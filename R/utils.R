"%||%" <- function(x, y) if (is.null(x)) y else x

check_lgl <- function(x) {
  obj <- deparse(substitute(x))
  if (!is.logical(x) || is.na(x)) {
    cli::cli_abort(sprintf("The argument %s must be a non-missing logical.", obj))
  }
}

normalize_file <- function(x) {
  x |>
    stringi::stri_trans_general("de-ASCII") |>
    tolower() |>
    gsub("[^a-z0-9]+", "_", x = _) |>
    gsub("^_|_$", "", x = _)
}

#' Canonical combination of a house number and its affix field
#'
#' @description R-side equivalent of the SQL \code{hn_combine()} helper used
#' in \code{bkg_matching.R} -- both must stay behaviorally identical, since
#' this one is applied once when building the local address database (to
#' compute \code{house_number_full}, and via that, \code{whole_address}),
#' while the SQL version is no longer needed for combining raw fields at
#' match time, only for reading the already-combined field.
#'
#' BKG's raw affix data (\code{house_number_add}) is stored inconsistently
#' for the exact same real address -- sometimes with a leading separator
#' ("/2"), sometimes as a bare number ("2"). This produces one canonical form
#' regardless of storage: numeric affixes get a canonical "/" separator
#' (e.g. "10" + "2"/"/2" -> "10/2"), alphabetic affixes are glued directly
#' (e.g. "10" + "a" -> "10a"), and an empty affix leaves the bare house
#' number unchanged.
#'
#' @param house_number \code{[character]}
#' @param house_number_add \code{[character]}
#'
#' @returns \code{[character]}
#'
#' @noRd
combine_house_number <- function(house_number, house_number_add) {
  house_number_add[is.na(house_number_add)] <- ""
  
  hn_add_clean <- trimws(gsub(
    "^[[:space:]/;+-]+", "", house_number_add
  ))
  
  data.table::fifelse(
    hn_add_clean == "",
    house_number,
    data.table::fifelse(
      grepl("^[0-9]", hn_add_clean),
      paste0(house_number, "/", hn_add_clean),
      paste0(house_number, hn_add_clean)
    )
  )
}


# -----------------------------------------------------------------------------
# Geospatial helpers
# -----------------------------------------------------------------------------

#' Create 1km and 100m INSPIRE IDs
#'
#' Create 1 km x 1 km and 100m x 100m INSPIRE IDs from coordinates
#'
#' @param data Object of class \code{sf} containing point geometries
#' @param type Character string for the requested ID type
#' @param column_name Output column name prefix. Defaults to "Gitter_ID_\{type\}".
#' @param combine Whether to combine the input data with the output values.
#' @return tibble
#'
#' @export

spt_create_inspire_ids <- function(
    data,
    type = c("1km", "100m"),
    column_name = "Gitter_ID_",
    combine = FALSE
) {
  
  if (sf::st_crs(data)$epsg != 3035) {
    data <- sf::st_transform(data, 3035)
  }
  
  coordinate_pairs <- tibble::as_tibble(sf::st_coordinates(data))
  
  id_name <- paste0(column_name, type)
  
  inspire_ids <- sprintf(
    "%sN%sE%s",
    type,
    # sprintf("%.0f", ...) rather than as.character(): the latter can
    # switch to scientific notation for round numbers (e.g. 3000000 ->
    # "3e+06") depending on getOption("scipen"), which would silently
    # corrupt the grid id. sprintf("%.0f", ...) always renders a fixed,
    # non-scientific integer string regardless of magnitude or options.
    substr(sprintf("%.0f", coordinate_pairs$Y), 1, 4 + (type == "100m")),
    substr(sprintf("%.0f", coordinate_pairs$X), 1, 4 + (type == "100m"))
  )
  
  if (isTRUE(combine)) {
    inspire_ids <- cbind(data, data.frame(id_name = inspire_ids))
    names(inspire_ids)[names(inspire_ids) == "id_name"] <- id_name
  } else {
    inspire_ids
  }
}