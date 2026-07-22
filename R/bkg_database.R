# Everything related to accessing and maintaining the local, offline BKG
# address database: path resolution, low-level Parquet reads, and building /
# updating the database from raw BKG data.

# -----------------------------------------------------------------------------
# Path resolution
# -----------------------------------------------------------------------------

#' Build glob paths into the partitioned local Parquet address database
#'
#' @description Central helper that constructs the file glob(s) pointing to
#' the \code{ga/place_slug=<slug>/*.parquet} partitions for one or several
#' places. Used by both \code{\link{bkg_read}} and
#' \code{bkg_match_addresses_ddb} so that the on-disk layout only needs to be
#' known in one place.
#'
#' @param data_path \code{[character]} Path to the local Parquet database
#' directory (as created by \code{\link{bkg_build_database}}).
#' @param place_slugs \code{[character]} One or several place slugs (as
#' created by \code{\link{normalize_file}}).
#'
#' @returns A character vector of glob paths.
#'
#' @noRd
bkg_ga_parquet_glob <- function(data_path, place_slugs) {
  file.path(
    data_path, "ga",
    sprintf("place_slug=%s", place_slugs),
    "*.parquet",
    fsep = "/"
  )
}

#' Standard local storage location for the BKG address database
#'
#' @description Returns the platform-appropriate standard location for the
#' persistent local address database used by \code{\link{bkg_geocode_offline}},
#' following the conventions of \code{\link[tools]{R_user_dir}} (introduced in
#' R 4.0). This means the data lives in the standard per-package data
#' directory for the current platform (e.g. under \code{XDG_DATA_HOME} on
#' Linux, \code{~/Library/Application Support} on macOS, or \code{\%APPDATA\%}
#' on Windows), rather than in an arbitrary user-chosen folder.
#'
#' @details The location can be overridden by setting the environment
#' variable \code{BKGGEOCODER2_DB_PATH}, e.g. if the database should live on
#' a shared network location instead. This is mainly meant for advanced use
#' cases (e.g. a shared install for multiple users) or for testing.
#'
#' @returns \code{[character]} A single file path.
#'
#' @export
bkg_db_path <- function() {
  override <- Sys.getenv("BKGGEOCODER2_DB_PATH", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    return(override)
  }
  tools::R_user_dir("bkggeocoder2", which = "data")
}


# -----------------------------------------------------------------------------
# Reading the local database
# -----------------------------------------------------------------------------

#' Read local BKG address database
#'
#' @description Reads the locally stored, partitioned Parquet address
#' database built by \code{\link{bkg_build_database}} / updated by
#' \code{\link{bkg_update_database}}. Used internally by the offline
#' geocoding functions to retrieve either the ZIP/place lookup table or
#' address data for a set of places. Unlike previous versions of this
#' package, this function only ever reads local files -- no server access
#' or decryption is involved.
#'
#' @param place \code{[character]}
#'
#' One or several place slugs (as created by \code{\link{normalize_file}})
#' for which address data should be read. Ignored if \code{what = "places"}.
#' @param what \code{[character]}
#'
#' Type of dataset to be read. If \code{"places"}, reads the ZIP/place lookup
#' table. If \code{"addresses"}, reads address data for the places given in
#' \code{place}.
#' @param db_path \code{[character]}
#'
#' Path to the local Parquet database directory (as created by
#' \code{\link{bkg_build_database}}).
#' @param con A DuckDB connection. If \code{NULL}, a temporary connection is
#' created and closed automatically on exit.
#'
#' @returns A \code{data.table} containing the requested data.
#'
#' @noRd
bkg_read <- function(
    place = NULL,
    what = c("addresses", "places"),
    db_path = bkg_db_path(),
    con = NULL
) {
  what <- match.arg(what)

  if (what == "places") {
    paths <- file.path(db_path, "zip_places", "ga_zip_places.parquet")
  } else {
    if (is.null(place)) {
      cli::cli_abort("{.arg place} must be provided if {.code what = \"addresses\"}.")
    }
    paths <- bkg_ga_parquet_glob(db_path, place)
  }

  own_con <- is.null(con)
  if (own_con) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  sql_paths <- paste(DBI::dbQuoteString(con, paths), collapse = ",")
  view_name <- paste0("bkg_", sample.int(1e9, 1))

  out <- tryCatch({
    DBI::dbExecute(
      con,
      sprintf(
        "
        CREATE OR REPLACE TEMP VIEW %s AS
        SELECT *
        FROM read_parquet([%s])
        ",
        view_name,
        sql_paths
      )
    )

    dplyr::tbl(con, view_name) |>
      dplyr::collect() |>
      data.table::as.data.table()
  }, error = function(e) NULL)

  if (is.null(out)) {
    cli::cli_abort(c(
      "Cannot read {.val {what}} data from {.path {db_path}}.",
      "i" = paste(
        "Make sure the local address database exists.",
        "Use {.fun bkg_build_database} or {.fun bkg_update_database}",
        "to create/update it."
      )
    ))
  }

  out
}


# -----------------------------------------------------------------------------
# Building the database (internal worker)
# -----------------------------------------------------------------------------

#' Build the BKG address database (internal worker)
#'
#' @description Reads raw BKG address CSV files (one per federal state),
#' cleans and standardizes address components, and writes a partitioned
#' Parquet database that can be used for offline geocoding with
#' \code{\link{bkg_geocode_offline}}. This is the internal worker function
#' that does the actual heavy lifting; end users should call
#' \code{\link{bkg_update_database}} instead, which wraps this function
#' with staleness checks and backups and works for both the initial build
#' and later refreshes.
#'
#' @param address_data_path \code{[character]}
#'
#' Path to the directory containing the raw BKG address CSV files
#' (named \code{ga_<state>.csv}, e.g. \code{ga_nw.csv} for
#' Nordrhein-Westfalen).
#'
#' @param db_path \code{[character]}
#'
#' Path to the output directory where the partitioned Parquet database
#' will be written. The function creates two subdirectories:
#' \code{ga/} containing address data partitioned by place and
#' \code{zip_places/} containing a lookup table of places and zip codes.
#'
#' @details The function processes address data for all 16 German federal
#' states. For each state file, it:
#' \enumerate{
#'   \item Constructs the Regionalschlüssel (RS) from administrative key
#'     columns
#'   \item Standardizes street names (e.g. expands \code{str.} to
#'     \code{straße})
#'   \item Removes place name artifacts (e.g. \code{"Ortsteil unbekannt"})
#'   \item Creates composite address strings for record linkage
#'   \item Generates a normalized place slug for partitioning
#' }
#'
#' The output is written as a Snappy-compressed Parquet dataset partitioned
#' by \code{place_slug} using DuckDB. A separate Parquet lookup table
#' mapping places and zip codes is also created, as well as a persistent
#' DuckDB database file and a \code{version.json} metadata file.
#'
#' @returns Called for its side effect (writing Parquet files). Returns
#' \code{NULL} invisibly.
#'
#' @seealso \code{\link{bkg_update_database}}
#'
#' @encoding UTF-8
#' @md
#' @noRd
bkg_build_database_impl <- function(address_data_path, db_path) {

  laender_names <- c(
    "bb", "be", "bw", "by", "hb", "he", "hh", "mv",
    "ni", "nw", "rp", "sh", "sl", "sn", "st", "th"
  )

  cli::cli_h1("Building BKG address database")

  unlink(
    file.path(db_path, "ga"),
    recursive = TRUE,
    force = TRUE
  )

  unlink(
    file.path(db_path, "zip_places"),
    recursive = TRUE,
    force = TRUE
  )

  unlink(
    file.path(db_path, "bkg.duckdb"),
    force = TRUE
  )

  dir.create(db_path, recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE bkg_ga (
      street VARCHAR,
      house_number VARCHAR,
      house_number_add VARCHAR,
      zip_code VARCHAR,
      place VARCHAR,
      place_add VARCHAR,
      RS VARCHAR,
      x VARCHAR,
      y VARCHAR,
      whole_address VARCHAR,
      whole_address_add VARCHAR,
      place_slug VARCHAR
    )
  ")

  pb <- cli::cli_progress_bar(
    total = length(laender_names),
    format = "Reading state files [{cli::pb_bar}] {cli::pb_percent}"
  )

  for (idx in seq_along(laender_names)) {

    cli::cli_progress_update(id = pb, set = idx)

    i <- laender_names[[idx]]

    tmp <- data.table::fread(
      file.path(address_data_path, glue::glue("ga_{i}.csv")),
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = FALSE
    )

    tmp[
      ,
      RS := do.call(paste0, .SD),
      .SDcols = c("V4", "V5", "V6", "V7", "V8")
    ]

    tmp <- tmp[
      ,
      .(
        street = gsub(" \\(.*\\)\\b", "", V15),
        house_number = V11,
        house_number_add = tolower(V12),
        zip_code = V16,
        place = V20,
        place_add = gsub("Ortsteil unbekannt", "", V19),
        RS,
        x = gsub(",", ".", V13),
        y = gsub(",", ".", V14)
      )
    ]

    tmp[
      ,
      street := gsub(
        "Str[.]$",
        "Straße",
        gsub("str[.]$", "straße", street)
      )
    ]

    tmp[
      ,
      whole_address := paste0(
        street,
        " ",
        house_number,
        house_number_add,
        " ",
        zip_code,
        " ",
        place
      )
    ]

    tmp[
      ,
      whole_address_add := paste0(
        whole_address,
        " ",
        place_add
      )
    ]

    tmp[
      ,
      place_slug := normalize_file(place)
    ]

    DBI::dbAppendTable(
      con,
      "bkg_ga",
      tmp
    )

    rm(tmp)
    gc()
  }

  cli::cli_progress_done(id = pb)

  # --------------------------------------------------
  # Partitionierte Parquet-Dateien
  # --------------------------------------------------

  cli::cli_alert_info("Writing partitioned dataset")

  DBI::dbExecute(
    con,
    glue::glue("
      COPY bkg_ga
      TO '{normalizePath(
        file.path(db_path, 'ga'),
        winslash = '/',
        mustWork = FALSE
      )}'
      (
        FORMAT PARQUET,
        PARTITION_BY(place_slug),
        COMPRESSION snappy,
        OVERWRITE_OR_IGNORE
      )
    ")
  )

  # --------------------------------------------------
  # ZIP-Lookup
  # --------------------------------------------------

  cli::cli_alert_info("Writing ZIP lookup")

  dir.create(
    file.path(db_path, "zip_places"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  ga_zip_places <- DBI::dbGetQuery(
    con,
    "
    SELECT DISTINCT
      place,
      place_add,
      zip_code,
      place_slug
    FROM bkg_ga
    ORDER BY place
    "
  )

  arrow::write_parquet(
    ga_zip_places,
    file.path(
      db_path,
      "zip_places",
      "ga_zip_places.parquet"
    ),
    compression = "snappy"
  )

  # --------------------------------------------------
  # Persistente DuckDB-Datenbank
  # --------------------------------------------------

  cli::cli_alert_info("Writing DuckDB database")

  db_file <- normalizePath(
    file.path(db_path, "bkg.duckdb"),
    winslash = "/",
    mustWork = FALSE
  )

  DBI::dbExecute(
    con,
    glue::glue("
      ATTACH '{db_file}' AS pkgdb;
    ")
  )

  DBI::dbExecute(
    con,
    "
    CREATE TABLE pkgdb.bkg_ga AS
    SELECT *
    FROM bkg_ga
    ORDER BY
      place_slug,
      zip_code,
      street,
      house_number;
    "
  )

  DBI::dbWriteTable(
    con,
    DBI::Id(
      schema = "pkgdb",
      table = "ga_zip_places"
    ),
    ga_zip_places,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "
    CREATE INDEX idx_place_zip
    ON pkgdb.bkg_ga(place_slug, zip_code);
    "
  )

  DBI::dbExecute(
    con,
    "DETACH pkgdb"
  )

  # --------------------------------------------------
  # Version / Metadaten
  # --------------------------------------------------

  cli::cli_alert_info("Writing metadata")

  version <- list(
    version = format(Sys.Date(), "%Y-%m-%d"),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_addresses = DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM bkg_ga"
    )$n,
    n_places = DBI::dbGetQuery(
      con,
      "SELECT COUNT(DISTINCT place_slug) AS n FROM bkg_ga"
    )$n
  )

  jsonlite::write_json(
    version,
    file.path(
      db_path,
      "version.json"
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  cli::cli_alert_success("Done")

  invisible(NULL)
}


# -----------------------------------------------------------------------------
# Building / updating the database (public entry point)
# -----------------------------------------------------------------------------

#' Build or update the local BKG address database
#'
#' @description Creates or refreshes the local Parquet address database used
#' for offline geocoding (see \code{\link{bkg_geocode_offline}}). This is the
#' single entry point for managing the local database: if no database exists
#' yet at \code{db_path}, it is built from scratch; if one already exists, it
#' is refreshed from newer raw data (with staleness checking, so a rebuild
#' only happens if the raw data actually changed, unless \code{force = TRUE}).
#' An existing database is optionally backed up before being overwritten.
#'
#' @param address_data_path \code{[character]}
#'
#' Path to the directory containing the raw BKG address CSV files
#' (named \code{ga_<state>.csv}). This directory can, e.g., be a mounted
#' path to an internal server where newer raw data releases are placed.
#'
#' @param db_path \code{[character]}
#'
#' Path to the local Parquet database directory. Defaults to the standard,
#' platform-appropriate local data location returned by
#' \code{\link{bkg_db_path}} -- most users should not need to change this.
#'
#' @param force \code{[logical]}
#'
#' Whether to rebuild the database even if the raw data does not appear to
#' be newer than the existing database. Defaults to \code{FALSE}.
#'
#' @param backup \code{[logical]}
#'
#' Whether to keep a dated backup copy of an existing database directory
#' before it gets overwritten. Ignored on the initial build (there is
#' nothing to back up yet). Defaults to \code{TRUE}.
#'
#' @returns \code{TRUE} if the database was (re-)built, \code{FALSE} if it
#' was already up to date (invisibly).
#'
#' @examples
#' \dontrun{
#' # Initial build
#' bkg_update_database(address_data_path = "path/to/raw/bkg/csvs")
#'
#' # Later on, after new raw data has been placed in the same folder:
#' bkg_update_database(address_data_path = "path/to/raw/bkg/csvs")
#'
#' # Force a rebuild regardless of file timestamps
#' bkg_update_database(address_data_path = "path/to/raw/bkg/csvs", force = TRUE)
#' }
#'
#' @seealso \code{\link{bkg_db_path}}, \code{\link{bkg_geocode_offline}}
#'
#' @encoding UTF-8
#' @md
#' @export
bkg_update_database <- function(
    address_data_path,
    db_path = bkg_db_path(),
    force = FALSE,
    backup = TRUE
) {
  check_lgl(force)
  check_lgl(backup)

  if (!dir.exists(address_data_path)) {
    cli::cli_abort("{.path {address_data_path}} does not exist.")
  }

  version_file <- file.path(db_path, "version.json")
  old_version <- if (file.exists(version_file)) {
    jsonlite::read_json(version_file)
  } else NULL

  is_initial_build <- is.null(old_version)

  if (is_initial_build) {
    cli::cli_h2("Setting up local BKG address database")
    cli::cli_bullets(c(
      " " = "Location: {.path {db_path}}",
      "i" = paste(
        "This directory will hold the full local address database.",
        "Make sure there is enough disk space available."
      ),
      "i" = paste(
        "To use a different location, pass {.arg db_path} explicitly or set",
        "the {.envvar BKGGEOCODER2_DB_PATH} environment variable."
      )
    ))
    cli::cat_line()
  }

  if (!isTRUE(force) && !is_initial_build) {
    source_files <- list.files(
      address_data_path,
      pattern = "^ga_.*\\.csv$",
      full.names = TRUE
    )

    if (!length(source_files)) {
      cli::cli_abort(c(
        "No {.file ga_<state>.csv} files found in {.path {address_data_path}}."
      ))
    }

    newest_source <- max(file.mtime(source_files))
    db_created <- as.POSIXct(old_version$created)

    if (newest_source <= db_created) {
      cli::cli_inform(c(
        "i" = "Database is already up to date (created {.val {old_version$created}}).",
        "i" = "Set {.code force = TRUE} to rebuild anyway."
      ))
      return(invisible(FALSE))
    }
  }

  if (isTRUE(backup) && !is_initial_build && dir.exists(db_path)) {
    backup_path <- paste0(
      db_path, "_backup_", format(Sys.Date(), "%Y%m%d")
    )

    cli::cli_alert_info("Backing up existing database to {.path {backup_path}}")

    unlink(backup_path, recursive = TRUE, force = TRUE)
    dir.create(dirname(backup_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(db_path, dirname(backup_path), recursive = TRUE)
    file.rename(
      file.path(dirname(backup_path), basename(db_path)),
      backup_path
    )
  }

  dir.create(db_path, recursive = TRUE, showWarnings = FALSE)
  bkg_build_database_impl(address_data_path, db_path)

  new_version <- jsonlite::read_json(version_file)

  cli::cli_alert_success(paste0(
    "Database ",
    if (is_initial_build) "built" else "updated",
    " at {.path {db_path}}: ",
    "{old_version$version %||% 'none'} -> {new_version$version} ",
    "({new_version$n_addresses} addresses, {new_version$n_places} places)"
  ))

  invisible(TRUE)
}
