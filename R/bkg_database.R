# Everything related to accessing and maintaining the local, offline BKG
# address database: path resolution, low-level Parquet reads, and building /
# updating the database from raw BKG data.

# Path resolution ----

#' Build glob paths into the partitioned local Parquet address database
#'
#' @description Central helper that constructs the file glob(s) pointing to
#' the \code{ga/place_slug=<slug>/*.parquet} partitions for one or several
#' places. Used by \code{bkg_match_addresses_ddb} so that the on-disk layout
#' only needs to be known in one place.
#'
#' @param data_path \code{[character]} Path to the local Parquet database
#' directory (as created by \code{\link{bkg_update_database}}).
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

# Version metadata (read/write) ----
# Stored as DCF (the key: value format R uses for DESCRIPTION) rather than
# JSON, so no extra dependency is needed for a handful of fields.

#' @noRd
write_version_metadata <- function(version, path) {
  write.dcf(
    data.frame(
      version = version$version,
      created = version$created,
      n_addresses = version$n_addresses,
      n_places = version$n_places,
      n_dropped_katasterintern = version$n_dropped_katasterintern,
      stringsAsFactors = FALSE
    ),
    file = path
  )
}

#' @noRd
read_version_metadata <- function(path) {
  raw <- as.data.frame(read.dcf(path, all = TRUE), stringsAsFactors = FALSE)
  list(
    version = raw$version,
    created = raw$created,
    n_addresses = as.integer(raw$n_addresses),
    n_places = as.integer(raw$n_places),
    n_dropped_katasterintern = as.integer(raw$n_dropped_katasterintern)
  )
}




# Building the database (internal worker) ----

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
#' @param memory_limit \code{[character/NULL]}
#'
#' Caps how much memory DuckDB is allowed to use during the build (e.g.
#' \code{"4GB"}), set via \code{PRAGMA memory_limit}. Without a cap
#' (the default, \code{NULL}), DuckDB uses as much memory as it deems
#' useful for a given operation. Note that setting a cap is not a
#' guaranteed safety net: for a single large operation (e.g. partitioning
#' one big federal state's data by place), DuckDB may still raise an
#' out-of-memory error rather than spilling to disk, if the cap is too low
#' for that operation's minimum working set. If you hit that, raising
#' \code{memory_limit} (or leaving it at \code{NULL}) is more reliable than
#' lowering it further.
#'
#' @param threads \code{[integer/NULL]}
#'
#' Caps the number of threads DuckDB uses, set via \code{PRAGMA threads}.
#' Fewer threads lower peak memory somewhat (each parallel worker needs its
#' own working memory), at the cost of a slower build. Set to \code{NULL}
#' (the default) to leave DuckDB's own default (typically one thread per
#' CPU core) in place.
#'
#' @details The function processes address data for whichever state files
#' are present in \code{address_data_path} (normally all 16 German federal
#' states, but a subset works too, e.g. for testing). For each state file,
#' it:
#' \enumerate{
#'   \item Drops records with a "katasterinterne Hausnummer" (BKG quality
#'     code \code{"C"}), which is explicitly not an official house number
#'     and cannot correspond to a real, addressable location
#'   \item Constructs the Regionalschluessel (RS) from administrative key
#'     columns
#'   \item Standardizes street names (e.g. expands \code{str.} to
#'     \code{strasse})
#'   \item Removes place name artifacts (e.g. \code{"Ortsteil unbekannt"})
#'   \item Creates composite address strings for record linkage
#'   \item Generates a normalized place slug for partitioning
#' }
#'
#' The output is written as a Snappy-compressed Parquet dataset partitioned
#' by \code{place_slug} using DuckDB. A separate Parquet lookup table
#' mapping places and zip codes is also created, as well as a
#' \code{version.dcf} metadata file (which records how many records were
#' dropped for having a katasterinterne house number).
#'
#' @returns Called for its side effect (writing Parquet files). Returns
#' \code{NULL} invisibly.
#'
#' @seealso \code{\link{bkg_update_database}}
#'
#' @encoding UTF-8
#' @md
#' @noRd
bkg_build_database_impl <- function(
    address_data_path,
    db_path,
    memory_limit = NULL,
    threads = NULL
) {
  
  # Auto-discovers present state files instead of hardcoding all 16, so
  # partial data (e.g. one state, for testing) works too.
  csv_files <- list.files(address_data_path, pattern = "^ga_.*\\.csv$")
  
  if (!length(csv_files)) {
    cli::cli_abort(c(
      "No {.file ga_<state>.csv} files found in {.path {address_data_path}}."
    ))
  }
  
  laender_names <- sub("^ga_(.*)\\.csv$", "\\1", csv_files)
  
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
  
  # Cleans up a stale bkg.duckdb from older builds -- no longer written.
  unlink(
    file.path(db_path, "bkg.duckdb"),
    force = TRUE
  )
  
  dir.create(db_path, recursive = TRUE, showWarnings = FALSE)
  
  # Suppresses duckdb's one-time first-connection notice; unrelated to
  # our own verbose= setting.
  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  if (!is.null(memory_limit)) {
    DBI::dbExecute(con, sprintf("PRAGMA memory_limit='%s'", memory_limit))
  }
  
  if (!is.null(threads)) {
    DBI::dbExecute(con, sprintf("PRAGMA threads=%d", threads))
  }
  
  # Reads downstream always sort/rank explicitly (ORDER BY/QUALIFY), so
  # preserving insertion order here is pure overhead -- disabling it
  # lowers peak memory for the partitioned COPY below.
  DBI::dbExecute(con, "PRAGMA preserve_insertion_order=false")
  
  # Written to directly, per state, in the loop below.
  ga_path <- normalizePath(
    file.path(db_path, "ga"),
    winslash = "/",
    mustWork = FALSE
  )
  
  pb <- cli::cli_progress_bar(
    total = length(laender_names),
    format = "Reading state files [{cli::pb_bar}] {cli::pb_percent}"
  )
  
  n_dropped <- 0L
  n_addresses <- 0L
  zip_places_list <- vector("list", length(laender_names))
  
  # Each state writes directly to its own partitioned Parquet slice,
  # rather than accumulating all 16 into one table for a single final
  # COPY: profiling showed that final COPY dominating build time (~84%)
  # by shuffling/sorting tens of millions of rows at once, vs. one
  # smaller chunk per state.
  for (idx in seq_along(laender_names)) {
    
    cli::cli_progress_update(id = pb, set = idx)
    
    i <- laender_names[[idx]]
    
    # Only the columns actually used below are parsed. select= must be an
    # unnamed position vector (a named one means something different in
    # data.table); col.names= restores the friendly V<n> names.
    selected_cols <- c(3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16, 19, 20)
    
    tmp <- data.table::fread(
      file.path(address_data_path, sprintf("ga_%s.csv", i)),
      colClasses = "character",
      encoding = "UTF-8",
      showProgress = FALSE,
      select = selected_cols,
      col.names = paste0("V", selected_cols)
    )
    
    # Drops "katasterinterne Hausnummer" (V3 == "C") records -- not an
    # official house number, but an internal cadastre ID that would
    # corrupt address matching. Kept: "A", "B" (official), "P" (Post).
    n_before <- nrow(tmp)
    tmp <- tmp[V3 %in% c("A", "B", "P")]
    n_dropped <- n_dropped + (n_before - nrow(tmp))
    
    tmp[
      ,
      RS := do.call(paste0, .SD),
      .SDcols = c("V4", "V5", "V6", "V7", "V8")
    ]
    
    tmp <- tmp[
      ,
      .(
        street_raw = V15,
        house_number = V11,
        house_number_add = tolower(V12),
        zip_code = V16,
        place = V20,
        place_add_raw = V19,
        RS,
        x = gsub(",", ".", V13),
        y = gsub(",", ".", V14)
      )
    ]
    
    # street and place_add are cleaned via dedup-then-join: both repeat
    # heavily across rows, so cleaning each distinct value once avoids
    # redundant regex work.
    street_lookup <- data.table::data.table(street_raw = unique(tmp$street_raw))
    street_lookup[, street := gsub(" \\(.*\\)\\b", "", street_raw)]
    street_lookup[
      ,
      street := gsub(
        "Str[.]$",
        "Stra\u00dfe",
        gsub("str[.]$", "stra\u00dfe", street)
      )
    ]
    tmp[street_lookup, street := i.street, on = "street_raw"]
    tmp[, street_raw := NULL]
    
    place_add_lookup <- data.table::data.table(
      place_add_raw = unique(tmp$place_add_raw)
    )
    place_add_lookup[, place_add := gsub("Ortsteil unbekannt", "", place_add_raw)]
    tmp[place_add_lookup, place_add := i.place_add, on = "place_add_raw"]
    tmp[, place_add_raw := NULL]
    
    tmp[
      ,
      house_number_full := combine_house_number(house_number, house_number_add)
    ]
    
    tmp[
      ,
      whole_address := paste0(
        street,
        " ",
        house_number_full,
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
    
    # normalize_file()'s transliteration dominated build time (~42%) when
    # applied per row -- ~11,000 distinct places vs. millions of address
    # rows. Computed once per distinct place instead, then joined back.
    place_lookup <- data.table::data.table(place = unique(tmp$place))
    place_lookup[, place_slug := normalize_file(place)]
    tmp[place_lookup, place_slug := i.place_slug, on = "place"]
    
    n_addresses <- n_addresses + nrow(tmp)
    
    # Collected once per state, rather than re-querying it from one big
    # combined table at the end.
    zip_places_list[[idx]] <- unique(
      tmp[, .(place, place_add, zip_code, place_slug)]
    )
    
    # Not arrow::write_dataset(): it got progressively slower across
    # states (re-scans the growing target dataset on every call). Each
    # register+COPY here is scoped to a fresh, unrelated temp view instead.
    view_name <- paste0("tmp_state_", sample.int(1e9, 1))
    duckdb::duckdb_register(con, view_name, tmp)
    
    DBI::dbExecute(
      con,
      sprintf(
        "
    COPY (SELECT * FROM %s)
    TO '%s'
    (
      FORMAT PARQUET,
      PARTITION_BY(place_slug),
      COMPRESSION snappy,
      OVERWRITE_OR_IGNORE,
      FILENAME_PATTERN '%s_{i}'
    )
    ",
        view_name,
        ga_path,
        i 
      )
    )
    
    duckdb::duckdb_unregister(con, view_name)
    
    rm(tmp)
    gc()
  }
  
  cli::cli_progress_done(id = pb)
  
  if (n_dropped > 0) {
    cli::cli_alert_info(paste(
      "Dropped {.val {n_dropped}} record{?s} with a",
      "katasterinterne (non-official) house number."
    ))
  }
  
# ZIP-Lookup ----
  
  cli::cli_alert_info("Writing ZIP lookup")
  
  dir.create(
    file.path(db_path, "zip_places"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  ga_zip_places <- data.table::rbindlist(zip_places_list)
  data.table::setorder(ga_zip_places, place)
  
  arrow::write_parquet(
    ga_zip_places,
    file.path(
      db_path,
      "zip_places",
      "ga_zip_places.parquet"
    ),
    compression = "snappy"
  )
  
  # Version / Metadaten ----
  
  cli::cli_alert_info("Writing metadata")
  
  version <- list(
    version = format(Sys.Date(), "%Y-%m-%d"),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_addresses = n_addresses,
    n_places = data.table::uniqueN(ga_zip_places$place_slug),
    n_dropped_katasterintern = n_dropped
  )
  
  write_version_metadata(
    version,
    file.path(db_path, "version.dcf")
  )
  
  cli::cli_alert_success("Done")
  
  invisible(NULL)
}


# Building / updating the database (public entry point) ----

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
#' nothing to back up yet). Defaults to \code{FALSE}, since the local
#' address database can be large and a backup temporarily doubles the disk
#' space it uses; set to \code{TRUE} if you want a safety copy before a
#' refresh.
#'
#' @param memory_limit \code{[character/NULL]}
#'
#' Caps how much memory DuckDB is allowed to use while building the
#' database (e.g. \code{"4GB"}); passed through to the internal build
#' worker. Defaults to \code{NULL} (DuckDB's
#' own, uncapped default). Note that a cap is not a guaranteed safety net
#' against out-of-memory errors for a single large operation (e.g. one big
#' federal state) -- if you hit one, raising this (or leaving it at
#' \code{NULL}) tends to be more reliable than lowering it further.
#'
#' @param threads \code{[integer/NULL]}
#'
#' Caps the number of threads DuckDB uses; passed through to the internal
#' build worker. Fewer threads lower peak memory
#' somewhat but make the build slower. Set to \code{NULL} (the default)
#' to leave DuckDB's own default in place.
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
    backup = FALSE,
    memory_limit = NULL,
    threads = NULL
) {
  check_lgl(force)
  check_lgl(backup)
  
  if (!dir.exists(address_data_path)) {
    cli::cli_abort("{.path {address_data_path}} does not exist.")
  }
  
  version_file <- file.path(db_path, "version.dcf")
  old_version <- if (file.exists(version_file)) {
    read_version_metadata(version_file)
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
  bkg_build_database_impl(
    address_data_path, db_path,
    memory_limit = memory_limit, threads = threads
  )
  
  new_version <- read_version_metadata(version_file)
  
  cli::cli_alert_success(paste0(
    "Database ",
    if (is_initial_build) "built" else "updated",
    " at {.path {db_path}}: ",
    "{old_version$version %||% 'none'} -> {new_version$version} ",
    "({new_version$n_addresses} addresses, {new_version$n_places} places)"
  ))
  
  invisible(TRUE)
}