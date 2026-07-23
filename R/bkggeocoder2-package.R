#' @keywords internal
#' @encoding UTF-8
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table data.table
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
  "whole_address", "whole_address_add", "zip_code"
))