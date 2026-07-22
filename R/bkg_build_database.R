#' Build BKG address database
#'
#' @description Reads raw BKG address CSV files (one per federal state),
#' cleans and standardizes address components, and writes a partitioned
#' Parquet database that can be used for offline geocoding with
#' \code{\link{bkg_geocode_offline}}. Use this function for the initial
#' setup of the local database; for refreshing an already existing database
#' with newer raw data, use \code{\link{bkg_update_database}} instead.
#'
#' @param address_data_path \code{[character]}
#'
#' Path to the directory containing the raw BKG address CSV files
#' (named \code{ga_<state>.csv}, e.g. \code{ga_nw.csv} for
#' Nordrhein-Westfalen).
#'
#' @param db_target_path \code{[character]}
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
bkg_build_database <- function(address_data_path, db_target_path) {
  
  laender_names <- c(
    "bb", "be", "bw", "by", "hb", "he", "hh", "mv",
    "ni", "nw", "rp", "sh", "sl", "sn", "st", "th"
  )
  
  cli::cli_h1("Building BKG address database")
  
  unlink(
    file.path(db_target_path, "ga"),
    recursive = TRUE,
    force = TRUE
  )
  
  unlink(
    file.path(db_target_path, "zip_places"),
    recursive = TRUE,
    force = TRUE
  )
  
  unlink(
    file.path(db_target_path, "bkg.duckdb"),
    force = TRUE
  )
  
  dir.create(db_target_path, recursive = TRUE, showWarnings = FALSE)
  
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
        file.path(db_target_path, 'ga'),
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
    file.path(db_target_path, "zip_places"),
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
      db_target_path,
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
    file.path(db_target_path, "bkg.duckdb"),
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
      db_target_path,
      "version.json"
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  cli::cli_alert_success("Done")
  
  invisible(NULL)
}