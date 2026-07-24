#' @keywords internal
#' @encoding UTF-8
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom data.table :=
## usethis namespace: end
NULL

# data.table's non-standard evaluation (bkg_build_database_impl() in
# bkg_database.R) references column names as bare symbols inside `tmp[...]`
# expressions, which R CMD check's static analysis can't tell apart from
# genuine undefined global variables. This is the standard, documented way
# to silence those "no visible binding" NOTEs for a data.table-based
# package (see `?data.table::.SD` and the data.table "importing data.table"
# vignette).
utils::globalVariables(c(
  ".", ".SD", "RS", "V3", "V11", "V12", "V13", "V14", "V15", "V16", "V19", "V20",
  "house_number", "house_number_add", "house_number_full",
  "place", "place_add", "place_slug", "street",
  "whole_address", "whole_address_add", "zip_code",
  "street_raw", "place_add_raw", "i.street", "i.place_add", "i.place_slug"
))


# -----------------------------------------------------------------------------
# Package data
# -----------------------------------------------------------------------------

#' Address data of community center addresses
#'
#' @description A dataset containing the addresses of community centers in
#' Germany. The dataset was extracted from OpenStreetMap on 2022-05-12 19:25.
#'
#' @format A tibble with 7976 rows and 6 columns:
#' \describe{
#'   \item{osm_id}{ID of the OSM object linked to the address}
#'   \item{addr.street}{Street name of the address}
#'   \item{addr.housenumber}{House number of the address}
#'   \item{addr.postcode}{Zip code of the address (PLZ)}
#'   \item{addr.city}{Place name of the address (Gemeinde)}
#'   \item{address}{Address string glued together from the other columns}
#' }
#'
#' @source https://www.openstreetmap.org/, https://overpass-turbo.eu/
"commaddr"