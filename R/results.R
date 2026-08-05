# Everything related to consuming / presenting a GeocodingResults object:
# S3 methods (print, summary, plot) and exporting results to disk.

# S3 methods ----

#' Print geocoding results
#'
#' @param x Object of class \code{GeocodingResults}
#' @param n Maximum number of rows to display. Defaults to 10.
#' @param ... Further arguments passed on to
#' \code{\link[base:print.data.frame]{base::print.data.frame()}}
#'
#' @export
print.GeocodingResults <- function(x, n = 10, ...) {
  n_total <- nrow(x)
  has_score <- !is.na(x$score)
  n_place_matched <- sum(has_score)
  n_unmatched <- n_total - n_place_matched
  scores <- x$score[has_score]
  
  cat("Class:", strrep(" ", 5), "GeocodingResults", "\n")
  cat("Addresses:", strrep(" ", 2), n_total, "\n")
  cat("Geocoded:", strrep(" ", 3), n_place_matched, "/", n_total, "\n")
  
  if (length(scores)) {
    cat("Mean score:", strrep(" ", 1), round(mean(scores), 3), "\n")
  }
  
  cat("Type:", strrep(" ", 6), attr(x, "type"), "\n")
  
  if (n_unmatched) {
    cat("Unmatched:", strrep(" ", 2), n_unmatched, "address(es)\n")
  }
  
  cat("\n")
  
  display_cols <- intersect(
    c("score", "address_input", "address_cleaned", "address_output"),
    names(x)
  )
  
  if (length(display_cols)) {
    printed_df <- sf::st_drop_geometry(x[seq_len(min(n, n_total)), display_cols])
    class(printed_df) <- setdiff(class(printed_df), "GeocodingResults")
    print(printed_df, ...)
    if (n_total > n) {
      cat(sprintf("... and %d more rows\n", n_total - n))
    }
  }
  
  invisible(x)
}


#' Get a summary of geocoding results
#'
#' @param object Object of class \code{GeocodingResults}
#' @param ... Ignored.
#'
#' @export
summary.GeocodingResults <- function(object, ...) {
  n_total <- nrow(object)
  has_score <- !is.na(object$score)
  n_geocoded <- sum(has_score)
  scores <- object$score[has_score]
  unmatched <- attr(object, "unmatched_places")
  
  msg <- paste0(
    "Addresses in input data:         ", n_total, "\n",
    "Addresses geocoded:              ", n_geocoded, "\n",
    "Addresses not geocoded:          ", n_total - n_geocoded, "\n"
  )
  
  if (length(scores)) {
    msg <- paste0(
      msg, "\n",
      "Mean score:                      ", round(mean(scores), 3), "\n",
      "Median score:                    ", round(stats::median(scores), 3), "\n",
      "Standard deviation of score:     ", round(stats::sd(scores), 3), "\n",
      "Minimum score:                   ", round(min(scores), 3), "\n",
      "Maximum score:                   ", round(max(scores), 3), "\n"
    )
  }
  
  cat(msg)
  
  if (!is.null(unmatched) && nrow(unmatched)) {
    cat("\nUnmatched places:\n")
    print(unmatched)
  }
}


#' Plot geocoding score distribution
#'
#' @param x Object of class \code{GeocodingResults}
#' @param ... Further arguments passed on to \code{\link[graphics]{hist}}
#'
#' @export
plot.GeocodingResults <- function(x, ...) {
  scores <- x$score[!is.na(x$score)]
  
  if (!length(scores)) {
    cli::cli_warn("No scores to plot.")
    return(invisible(NULL))
  }
  
  graphics::hist(
    scores,
    main = "Distribution of geocoding scores",
    xlab = "Score",
    xlim = c(0, 1),
    ...
  )
}


#' Show a detailed, per-row comparison of address versions
#'
#' @description Prints the original (raw), cleaned (post-fix matching
#' key), and matched (database) version of \code{address_input}/
#' \code{address_cleaned}/\code{address_output} for one or more rows, one
#' value per line -- instead of a normal wide table. Console tibble
#' printing truncates a cell's text to fit the available width rather
#' than wrapping it across lines, which makes comparing three full
#' address strings side by side impractical once they're long. This
#' sidesteps the problem entirely by never putting them next to each
#' other in the first place.
#'
#' @param .data \code{[GeocodingResults]} Output of
#' \code{\link{bkg_geocode_offline}}, or any subset of it (e.g. already
#' filtered down to a handful of rows you want a closer look at).
#' @param id_col \code{[character/NULL]} Optional column used to label
#' each block (e.g. \code{"ID"}). Falls back to a plain row number if
#' omitted or not found.
#'
#' @returns \code{.data}, invisibly. Called for its printed side effect.
#'
#' @examples
#' \dontrun{
#' # A specific handful of rows
#' bkg_show_address_detail(gc[gc$ID %in% c("123", "456"), ], id_col = "ID")
#'
#' # Everything currently classified as needing review
#' bkg_show_address_detail(gc[gc$quality == "wrong_house_number", ], id_col = "ID")
#' }
#'
#' @export
bkg_show_address_detail <- function(.data, id_col = NULL) {
  if (inherits(.data, "sf")) {
    .data <- sf::st_drop_geometry(.data)
  }
  class(.data) <- setdiff(class(.data), "GeocodingResults")
  
  required_cols <- c("address_input", "address_cleaned", "address_output")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols)) {
    cli::cli_abort(paste(
      "{.arg .data} is missing column{?s} {.val {missing_cols}} --",
      "is this the output of {.fun bkg_geocode_offline}?"
    ))
  }
  
  if (!nrow(.data)) {
    cli::cli_inform("No rows to show.")
    return(invisible(.data))
  }
  
  has_id <- !is.null(id_col) && id_col %in% names(.data)
  
  for (i in seq_len(nrow(.data))) {
    label <- if (has_id) paste0("ID: ", .data[[id_col]][i]) else paste0("Row ", i)
    cat("\n", label, "\n", strrep("-", nchar(label)), "\n", sep = "")
    cat(sprintf("  %-10s %s\n", "Original:", .data$address_input[i]))
    cat(sprintf("  %-10s %s\n", "Cleaned:", .data$address_cleaned[i]))
    cat(sprintf("  %-10s %s\n", "Output:", .data$address_output[i]))
  }
  
  invisible(.data)
}


# Exporting results ----

#' Export geocoding results
#'
#' Export the output of \code{\link[bkggeocoder2]{bkg_geocode_offline}}.
#'
#' @param .data \code{[GeocodingResults]}
#'
#' Output of \code{\link[bkggeocoder2]{bkg_geocode_offline}} that should be
#' exported.
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