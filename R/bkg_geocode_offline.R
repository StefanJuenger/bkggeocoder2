# Public entry point for offline geocoding. The internal matching steps this
# function orchestrates (place matching, address matching, result cleaning)
# live in bkg_matching.R -- they are never meant to be called independently.
#' Geocoding of multiple addresses (offline, local database)
#'
#' @description Geocoding of multiple addresses using record linkage against
#' a local address/coordinate database (built from BKG data via
#' \code{\link{bkg_update_database}}). Unlike previous versions of this
#' package, this function works entirely offline against a local Parquet
#' database -- no server connection is required.
#'
#' @param .data \code{[data.frame]}
#'
#' Dataframe containing address data. The dataframe must contain columns
#' carrying the street name, house number, zip code and municipality name.
#' The corresponding column names or indices can be specified using the
#' \code{cols} argument.
#' @param cols \code{[numeric/character]}
#'
#' Names or indices of the columns containing relevant geocoding information.
#' Must be of length 3 or 4. If a length-3 vector is passed, the first column
#' is interpreted as a single character string containing street and house
#' number. By default, interprets the first four columns as street, house
#' number, zip code and municipality (in this order).
#' @param db_path \code{[character]}
#'
#' Path to the local Parquet address database (as created/refreshed by
#' \code{\link{bkg_update_database}}). Defaults to the standard local data
#' location returned by \code{\link{bkg_db_path}} -- most users should not
#' need to change this.
#' @param join_with_original \code{[logical]}
#'
#' Whether the input data should be joined with the output data. If
#' \code{FALSE}, input data is discarded. Defaults to \code{TRUE}.
#' @param crs \code{[various]}
#'
#' Any kind of object that can be parsed by \code{\link[sf]{st_crs}} that the
#' output data should be transformed to (e.g. EPSG code, WKT/PROJ4 character
#' string or object of class \code{crs}). Defaults to EPSG:3035.
#' @param identifiers \code{[character/logical]}
#'
#' Territorial identifiers to be included in the output. Can be one or
#' several of \code{"rs"}, \code{"nuts"} and \code{"inspire"}. \code{"rs"} is
#' short for Regionalschluessel and includes all variations of the official
#' municipality key of Germany. \code{"nuts"} includes all NUTS codes from
#' NUTS-1 to NUTS-3. \code{"inspire"} includes identifiers for the 100m and
#' 1km INSPIRE grids. If \code{TRUE}, includes all of the aforementioned
#' identifiers.
#' @param place_match_quality \code{[numeric]}
#'
#' Targeted quality of the place-matching step, corresponding to the
#' posterior probability used to determine a place match.
#' @param hierarchical_weight \code{[numeric]}
#'
#' Exponent that controls how strongly individual score components influence
#' the overall geocoding score. The final score is computed as
#' \code{place_score^hierarchical_weight * street_score^hierarchical_weight * house_number_score^hierarchical_weight}.
#' Lower values are more lenient towards imperfect matches (e.g. minor
#' spelling variations or house number additions like "1b" matched to "1"),
#' while higher values penalize them more strongly. Defaults to \code{0.5}.
#' @param house_number_penalty \code{[numeric]}
#'
#' Penalty subtracted from the house-number score if the input and matched
#' house number differ. Defaults to \code{0.1}.
#' @param verbose \code{[logical]}
#'
#' Whether to print informative messages and progress bars during the
#' geocoding process.
#'
#' @returns Returns an \code{sf} dataframe of class \code{GeocodingResults}
#' with the same number of rows as the input data. Each row contains the
#' geocoding result with score columns (\code{score}, \code{place_score},
#' \code{street_score}, \code{house_number_score}) and matched address
#' components. Rows where the place could not be matched have \code{NA}
#' scores and empty geometries. A summary of unmatched places is available
#' via \code{attr(result, "unmatched_places")}. Please note that original
#' columns retrieve the suffix \code{"_input"}.
#'
#' @details The function first matches the zip code and place information
#' from the data against the official names in the local address database
#' (first round of record linkage). This is done to filter out address
#' datasets that are not needed and lower the data size. You can play with
#' the quality by adjusting the \code{place_match_quality} parameter. In a
#' second step, the input addresses together with the matched results are
#' then again matched against the addresses in the local database (second
#' round of record linkage), using \code{\link[duckdb]{duckdb}}'s
#' \code{jaro_winkler_similarity()}. Again, you can play with the quality by
#' adjusting the \code{hierarchical_weight} parameter.
#'
#' The overall quality of the geocoding can be evaluated by looking at the
#' values of the column \code{score} (ranging from 0 to 1). In general, a
#' score above 0.9 can be considered a good match. If the score falls below
#' 0.8, the result might be questionable.
#'
#' @examples
#' \dontrun{
#' data(commaddr, package = "bkggeocoder2")
#'
#' gc <- bkg_geocode_offline(commaddr, cols = 2:5, place_match_quality = 0.7)
#' }
#'
#' @encoding UTF-8
#' @md
#'
#' @export
bkg_geocode_offline <- function(
    .data,
    cols = 1:4,
    db_path = bkg_db_path(),
    join_with_original = TRUE,
    crs = 3035,
    identifiers = "rs",
    place_match_quality = 0.8,
    hierarchical_weight = 0.5,
    house_number_penalty = 0.1,
    verbose = TRUE
) {
  
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.var data} must be a dataframe.")
  }
  
  if (length(cols) < 3 || length(cols) > 4) {
    cli::cli_abort("{.var cols} must be of length 3 or 4.")
  }
  
  version_file <- file.path(db_path, "version.dcf")
  
  if (!dir.exists(db_path) || !file.exists(version_file)) {
    cli::cli_abort(c(
      "No local address database found at:",
      " " = "{.path {db_path}}",
      "i" = "Build it first, e.g.:",
      " " = "{.code bkg_update_database(address_data_path = \"path/to/raw/csvs\")}"
    ))
  }
  
  if (place_match_quality > 1 || place_match_quality < 0) {
    cli::cli_abort("{.var place_match_quality} needs to be a value between 0 and 1.")
  }
  
  if (hierarchical_weight < 0) {
    cli::cli_abort("{.var hierarchical_weight} needs to be a non-negative value.")
  }
  
  if (isTRUE(verbose)) {
    cli::cli_h1("Starting offline geocoding")
    cli::cat_line()
    cli::cli_inform(c(
      "i" = "Number of distinct addresses: {.val {nrow(.data)}}",
      "i" = "Targeted quality of place-matching: {.val {place_match_quality}}",
      "i" = "Place weight exponent: {.val {hierarchical_weight}}")
    )
    
    cli::cli_h2("Subsetting data")
  }
  
  cols <- names(.data[cols])
  
  args <- as.list(environment())
  args$.data <- NULL
  
  .data <- cbind(data.frame(.iid = as.numeric(row.names(.data))), .data)
  
  # Matching ----
  # Suppresses duckdb's one-time first-connection notice; unrelated to
  # our own verbose= setting.
  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  place_result <- bkg_match_places_ddb(
    .data[c(".iid", cols)],
    cols = cols,
    db_path = db_path,
    place_match_quality = place_match_quality,
    con = con,
    verbose = verbose
  )
  
  unmatched_places <- attr(place_result, "unmatched_places")
  matched_rows <- place_result[place_result$place_matched_flag, ]
  unmatched_rows <- place_result[!place_result$place_matched_flag, ]
  
  if (isTRUE(verbose)) {
    cli::cli_h2("Geocoding input data")
  }
  
  messy_geocoded_data <- bkg_match_addresses_ddb(
    matched_rows,
    cols = cols,
    db_path = db_path,
    hierarchical_weight = hierarchical_weight,
    house_number_penalty = house_number_penalty,
    con = con,
    verbose = verbose
  )
  
  # Data Cleaning ----
  cleaned_data <- bkg_clean_matched_addresses(
    messy_geocoded_data,
    cols = cols,
    identifiers = identifiers,
    verbose = verbose
  )
  
  # Merge geocoded rows with unmatched rows (NA scores/geometry) ----
  if (nrow(unmatched_rows)) {
    empty_sf <- cleaned_data[0, ]
    unmatched_sf <- unmatched_rows[, ".iid", drop = FALSE]
    
    for (col in setdiff(names(empty_sf), c(".iid", "geometry"))) {
      unmatched_sf[[col]] <- NA
    }
    
    unmatched_sf <- sf::st_as_sf(
      unmatched_sf,
      geometry = sf::st_sfc(
        rep(list(sf::st_point()), nrow(unmatched_sf)),
        crs = sf::st_crs(cleaned_data)
      )
    )
    
    cleaned_data <- rbind(cleaned_data, unmatched_sf[, names(cleaned_data)])
  }
  
  if (isTRUE(join_with_original)) {
    # suffixes handles the rare case of a user column colliding with an
    # internal output name (e.g. a column literally called "score").
    cleaned_data <- merge(
      .data,
      cleaned_data,
      by = ".iid",
      all.x = TRUE,
      sort = TRUE,
      suffixes = c("", "_input")
    )
    
    cleaned_data <- sf::st_as_sf(tibble::as_tibble(cleaned_data))
  }
  
  # Remove internal id
  cleaned_data$.iid <- NULL
  
  cleaned_data <- sf::st_transform(cleaned_data, crs = crs)
  
  # Create Output ----
  structure(
    cleaned_data,
    unmatched_places = unmatched_places,
    call = match.call(),
    type = "offline",
    args = args,
    class = c("GeocodingResults", class(cleaned_data))
  )
}