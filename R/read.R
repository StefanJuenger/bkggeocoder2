#' Read encrypted envelopes as data.table
#'
#' @param .crypt Encrypted data to be read
#' @param key Key to decrypt data
#' @param password Password to decrypt data
#' @param ... Further arguments passed to fread
#'
#' @noRd
fread_encrypted <- function(.crypt, credentials_path, ...) {
  .decrypt <- openssl::decrypt_envelope(
    .crypt$data, .crypt$iv, .crypt$session,
    key = paste0(credentials_path, "/id_rsa"),
    password = readLines(paste0(credentials_path, "/pwd"))
  )

  data.table::fread(
    text = readBin(.decrypt, what = "character"),
    colClasses = "character",
    encoding = "UTF-8",
    ...
  )
}


#' Read BKG data
#'
#' @description Read and decrypt BKG datasets. This includes both address
#' datasets for individual places as well as a dataset containing place names
#' and their zip codes (depending on the \code{what} argument).
#'
#' @param place \code{[character]}
#'
#' Name of the place to be read and decrypted, e.g.,
#' \code{"Aachen"} or \code{"Berlin"}. Ignored if \code{what = "places"}.
#' @param what \code{[character]}
#'
#' Type of dataset to be read. If \code{"places"}, reads a dataset
#' containing all names and zip codes of German places. If \code{"addresses"}
#' reads in all addresses of a place specified in \code{"place"}.
#' @param ... Further arguments passed to \code{\link[data.table]{fread}}
#' @inheritParams bkg_geocode_offline
#'
#' @returns A data.table containing either names and zip codes of German
#' districts or full addresses of an individual place specified in
#' \code{what}.
#'
#' @export
bkg_read <- function(
    place,
    what = "addresses",
    data_from_server = FALSE,
    data_path = "../bkgdata",
    credentials_path = "../bkgcredentials/cert.pem",
    con = NULL,
    ...
) {
  
  dataset <- switch(
    what,
    places = "zip_places/ga_zip_places.parquet",
    addresses = sprintf(
      "ga/place_slug=%s/*.parquet",
      normalize_file(place)
    )
  )
  
  # dataset <- switch(
  #   what,
  #   places = "zip_places/ga_zip_places.parquet",
  #   addresses = sprintf("ga/%s.parquet", place)
  # )
  
  if (missing(place) && what != "places") {
    place <- dataset
  }
  
  paths <- if (data_from_server) {
    file.path(
      "https://10.6.13.50:8544",
      dataset,
      fsep = "/"
    )
  } else {
    file.path(data_path, dataset)
  }
  
  if (data_from_server) {
    verify_server(credentials_path)
  }
  
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }
  
  DBI::dbExecute(con, "
    CREATE SECRET noverify (
      TYPE HTTP,
      VERIFY_SSL 0
    )
  ")
  
  if (length(paths) > 1 && isTRUE(data_from_server)) {
    index <- DBI::dbGetQuery(
      con,
      sprintf(
        "
    SELECT file
    FROM read_parquet('%s')
    WHERE place_slug IN (%s)
    ",
        file.path(base_url, "ga", "index.parquet"),
        paste(DBI::dbQuoteString(con, place), collapse = ",")
      )
    )
    
    parquet_paths <- file.path(
      base_url,
      "ga",
      index$file,
      fsep = "/"
    )
    
    parquet_sql <- paste(
      DBI::dbQuoteString(con, parquet_paths),
      collapse = ","
    )
  }
  
  
  sql_paths <- paste(
    DBI::dbQuoteString(con, paths),
    collapse = ","
  )
  
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
    
    if (data_from_server) {
      cli::cli_abort(c(
        "Cannot access {.val {place}} from the local server under {.path https://10.6.13.50:8544/}.",
        "!" = "Verify that you are inside the GESIS intranet or set {.var data_from_server = FALSE}"
      ))
    } else {
      cli::cli_abort(
        "Cannot read {.val {place}} from {.path {data_path}}."
      )
    }
  }
  
  out
}

# bkg_read <- function(
#   place,
#   what = "addresses",
#   data_from_server = FALSE,
#   data_path = "../bkgdata",
#   credentials_path = "../bkgcredentials",
#   ...
# ) {
#   places_file <- "zip_places/ga_zip_places.csv.encryptr.bin"
# 
#   dataset <- switch(
#     what,
#     places = "zip_places/ga_zip_places.csv.encryptr.bin",
#     addresses = sprintf("ga/%s.csv.encryptr.bin", place)
#   )
# 
#   if (data_from_server) {
#     .crypt_file <- url(file.path("https://10.6.13.50:8000", utils::URLencode(dataset), fsep = "/"))
#     on.exit(close(.crypt_file))
#   } else {
#     .crypt_file <- file.path(data_path, dataset)
#   }
# 
#   .crypt <- tryCatch(
#     expr = readRDS(.crypt_file),
#     error = function(e) NULL,
#     warning = function(w) {
#       if (grepl("timeout", w, ignore.case = TRUE)) {
#         cli::cli_abort(c(
#           "Connection timed out. Address or place file could not be loaded.",
#           "i" = "Increase {.code options(timeout = ...)} to circumvent this issue."
#         ))
#       }
#     }
#   )
# 
#   if (is.null(.crypt)) {
#     if (missing(place)) place <- dataset
#     if (data_from_server) {
#       cli::cli_abort(c(
#         "Cannot access {.val {place}} from the local server under {.path https://10.6.13.50:8000/}.",
#         "!" = "Verify that you are inside the GESIS intranet or set {.var data_from_server = FALSE}"
#       ))
#     } else {
#       cli::cli_abort("Cannot read {.val {place}} from {.path {data_path}}.")
#     }
#   }
# 
#   fread_encrypted(.crypt, credentials_path, ...)
# }
