#' @keywords internal
#' @encoding UTF-8
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom data.table :=
## usethis namespace: end
NULL

# data.table's NSE (bare column-name symbols inside `tmp[...]` in
# bkg_build_database_impl()) look like undefined globals to R CMD
# check's static analysis; this is the standard way to silence those
# NOTEs (see ?data.table::.SD).
utils::globalVariables(c(
  ".", ".SD", "RS", "V3", "V11", "V12", "V13", "V14", "V15", "V16", "V19", "V20",
  "house_number", "house_number_add", "house_number_full",
  "place", "place_add", "place_slug", "street",
  "whole_address", "whole_address_add", "zip_code",
  "street_raw", "place_add_raw", "i.street", "i.place_add", "i.place_slug"
))


# Package data ----

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