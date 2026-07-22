#' Export geocoding results
#'
#' Export the output of \code{\link[bkggeocoder]{bkg_geocode_offline}} and
#' \code{\link[bkggeocoder]{bkg_geocode}}.
#'
#' @param .data \code{[GeocodingResults]}
#'
#' Output of \code{\link[bkggeocoder]{bkg_geocode}} or
#' \code{\link[bkggeocoder]{bkg_geocode_offline}} that should be exported.
#'
#' @param file \code{[character]}
#'
#' Path to the output file. The file type is guessed based on the file extension.
#' If the file extension is \code{csv}, the data is exported using
#' \code{\link[readr]{write_delim}}. If it is \code{xlsx}, the data is exported
#' using \code{\link[openxlsx]{writeData}}. If is is anything else, the file type
#' is guessed by \code{\link[sf]{st_write}} and must be supported by
#' \code{\link[sf]{st_drivers}}.
#'
#' @param overwrite \code{[logical]}
#'
#' Whether to overwrite \code{file}, if it already exists. Defaults to \code{TRUE}.
#'
#' @param ... Further arguments passed to \code{\link[readr]{write_delim}},
#' \code{\link[openxlsx]{writeData}} or \code{\link[sf]{st_write}}.
#'
#' @returns \code{file}, invisibly.
#'
#' @export
bkg_export_geocodes <- function(.data, file, overwrite = TRUE, ...) {
  if (!inherits(.data, "GeocodingResults")) {
    cli::cli_abort(c(
      "i" = "Expected object of class {.cls GeocodingResults}",
      "x" = "Got object of class {.cls {class(.data)[1]}}"
    ))
  }

  geometry_to_xy <- function(data) {
    if (inherits(data, "sf")) {
      coords <- sf::st_coordinates(data)
      data <- cbind.data.frame(coords, sf::st_drop_geometry(data))
    }
    data
  }

  if (!file.exists(file) || isTRUE(overwrite)) {
    if (grepl("\\.csv$", file)) {
      if (!requireNamespace("readr")) {
        cli::cli_abort(c(
          "The {.pkg readr} package is required to export to csv.",
          "i" = "Install it using {.code install.packages(\"readr\")}"
        ))
      }
      readr::write_delim(geometry_to_xy(.data), file, ...)

    } else if (grepl("\\.xlsx?$", file)) {
      if (!requireNamespace("openxlsx")) {
        cli::cli_abort(c(
          "The {.pkg openxlsx} package is required to export to Excel.",
          "i" = "Install it using {.code install.packages(\"openxlsx\")}"
        ))
      }
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "geocoded")
      openxlsx::writeData(wb, sheet = "geocoded", geometry_to_xy(.data), ...)

      unmatched <- attr(.data, "unmatched_places")
      if (!is.null(unmatched) && nrow(unmatched)) {
        openxlsx::addWorksheet(wb, "unmatched_places")
        openxlsx::writeData(wb, sheet = "unmatched_places", unmatched)
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

    } else {
      sf::st_write(.data, file, delete_dsn = TRUE, ...)
    }
  }

  invisible(file)
}
