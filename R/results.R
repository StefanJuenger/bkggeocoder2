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
#' @param object \code{[GeocodingResults]}
#' @param quality \code{[logical]} Whether to include a breakdown by
#' quality tier (see \code{\link{bkg_classify}}), using an existing
#' \code{quality} column if present, or classifying with default
#' thresholds otherwise. Skipped silently if the score columns
#' \code{\link{bkg_classify}} needs aren't present. Defaults to \code{TRUE}.
#' @param ... Ignored.
#'
#' @export
summary.GeocodingResults <- function(object, quality = TRUE, ...) {
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
    q <- stats::quantile(scores, c(0.1, 0.25, 0.75, 0.9))
    msg <- paste0(
      msg, "\n",
      "Mean score:                      ", round(mean(scores), 3), "\n",
      "Median score:                    ", round(stats::median(scores), 3), "\n",
      "Standard deviation of score:     ", round(stats::sd(scores), 3), "\n",
      "Minimum score:                   ", round(min(scores), 3), "\n",
      "Maximum score:                   ", round(max(scores), 3), "\n",
      "10th / 90th percentile:          ", round(q[["10%"]], 3), " / ", round(q[["90%"]], 3), "\n",
      "25th / 75th percentile:          ", round(q[["25%"]], 3), " / ", round(q[["75%"]], 3), "\n"
    )
    
    component_cols <- c(
      "Place"        = "place_score",
      "Street"       = "street_score",
      "House number" = "house_number_score"
    )
    available <- component_cols[component_cols %in% names(object)]
    
    if (length(available)) {
      msg <- paste0(msg, "\nScore by component (mean / median):\n")
      for (label in names(available)) {
        vals <- object[[available[[label]]]]
        vals <- vals[!is.na(vals)]
        if (length(vals)) {
          msg <- paste0(
            msg,
            sprintf("  %-15s%.3f / %.3f\n", paste0(label, ":"), mean(vals), stats::median(vals))
          )
        }
      }
    }
  }
  
  cat(msg)
  
  if (isTRUE(quality)) {
    required_cols <- c("score", "place_score", "street_score", "house_number_score")
    if (all(required_cols %in% names(object))) {
      has_quality_col <- "quality" %in% names(object)
      quality_col <- if (has_quality_col) object$quality else bkg_classify(object)$quality
      tbl <- table(quality_col, useNA = "ifany")
      tbl <- tbl[order(-tbl)]
      
      cat("\nQuality breakdown:\n")
      for (i in seq_along(tbl)) {
        pct <- round(tbl[i] / sum(tbl) * 100, 1)
        display_name <- .bkg_quality_display_name(names(tbl)[i])
        cat(sprintf("  %-32s%6d (%5.1f%%)\n", display_name, tbl[i], pct))
      }
      
      if (!has_quality_col) {
        cat(paste(
          "\nNote: based on bkg_classify()'s DEFAULT thresholds -- these are",
          "a starting point, not a verdict. Check them for your data (see",
          "bkg_classify_interactive()).\n"
        ))
      }
    }
  }
  
  if (!is.null(unmatched) && nrow(unmatched)) {
    cat("\nUnmatched places:\n")
    print(unmatched)
  }
  
  invisible(object)
}


#' @noRd
.bkg_filter_by_threshold <- function(x, threshold, direction) {
  if (is.null(threshold)) return(x)
  keep <- if (direction == "below") x$score < threshold else x$score > threshold
  keep[is.na(keep)] <- FALSE
  x[keep, ]
}

#' @noRd
.bkg_threshold_subtitle <- function(threshold, direction, n) {
  if (is.null(threshold)) return(NULL)
  sprintf(
    "score %s %.2f (n = %d)",
    if (direction == "below") "<" else ">",
    threshold, n
  )
}

#' Get (or compute) the quality column, with the default-thresholds disclaimer
#'
#' @description Shared by \code{summary.GeocodingResults()},
#' \code{bkg_plot_quality()}, and \code{bkg_plot_map()} so the "was this
#' classified with defaults or did you already check it yourself" logic
#' only lives in one place.
#'
#' @param x \code{[GeocodingResults]}
#' @param quiet \code{[logical]} Suppress the console disclaimer (the
#' caller may prefer to show it another way, e.g. as a plot subtitle).
#'
#' @returns A list with \code{quality} (the column, existing or freshly
#' classified) and \code{is_default} (whether \code{\link{bkg_classify}}'s
#' defaults were used).
#'
#' @noRd
.bkg_quality_with_disclaimer <- function(x, quiet = FALSE) {
  required_cols <- c("score", "place_score", "street_score", "house_number_score")
  has_quality_col <- "quality" %in% names(x)
  
  if (!has_quality_col) {
    missing_cols <- setdiff(required_cols, names(x))
    if (length(missing_cols)) {
      cli::cli_abort(paste(
        "{.arg x} is missing column{?s} {.val {missing_cols}} needed to",
        "classify quality."
      ))
    }
    if (!quiet) {
      cli::cli_inform(paste(
        "Based on bkg_classify()'s DEFAULT thresholds -- a starting point,",
        "not a verdict. Check them for your data (see",
        "{.fun bkg_classify_interactive})."
      ))
    }
  }
  
  list(
    quality = if (has_quality_col) x$quality else bkg_classify(x)$quality,
    is_default = !has_quality_col
  )
}

#' @noRd
.bkg_filter_by_categories <- function(quality_col, categories) {
  if (is.null(categories)) {
    return(rep(TRUE, length(quality_col)))
  }
  unknown <- setdiff(categories, unique(quality_col))
  if (length(unknown)) {
    cli::cli_warn("{.val {unknown}} not present in the data -- ignoring.")
  }
  quality_col %in% categories
}

#' @noRd
.bkg_filter_by_subset <- function(x, subset_col, subset_value) {
  if (is.null(subset_col) && is.null(subset_value)) {
    return(x)
  }
  if (is.null(subset_col) || is.null(subset_value)) {
    cli::cli_abort("{.arg subset_col} and {.arg subset_value} must be supplied together.")
  }
  if (!subset_col %in% names(x)) {
    cli::cli_abort("{.arg subset_col} ({.val {subset_col}}) not found in the data.")
  }
  keep <- x[[subset_col]] %in% subset_value
  if (!any(keep)) {
    cli::cli_warn("No rows match {.arg subset_value} in column {.val {subset_col}}.")
  }
  x[keep, ]
}

#' Plot the distribution of geocoding scores
#'
#' @param x \code{[GeocodingResults]}
#' @param threshold \code{[numeric/NULL]} If given, only rows whose
#' \code{score} is below (or above, see \code{direction}) this value are
#' plotted. \code{NULL} (the default) plots everything.
#' @param direction \code{[character]} Whether \code{threshold} keeps
#' rows \code{"below"} (the default) or \code{"above"} it. Ignored if
#' \code{threshold} is \code{NULL}.
#' @param ... Further arguments passed on to \code{\link[graphics]{hist}}.
#'
#' @examples
#' \dontrun{
#' bkg_plot_score(gc)
#' bkg_plot_score(gc, threshold = 0.7, direction = "below")  # worst-scoring only
#' }
#'
#' @export
bkg_plot_score <- function(x, threshold = NULL, direction = c("below", "above"), ...) {
  direction <- match.arg(direction)
  x <- x[!is.na(x$score), ]
  x <- .bkg_filter_by_threshold(x, threshold, direction)
  
  if (!nrow(x)) {
    cli::cli_warn("No scores to plot.")
    return(invisible(NULL))
  }
  
  graphics::hist(
    x$score,
    main = "Distribution of geocoding scores",
    sub = .bkg_threshold_subtitle(threshold, direction, nrow(x)),
    xlab = "Score",
    xlim = c(0, 1),
    ...
  )
}

#' Plot the distribution of each score component side by side
#'
#' @description Histograms of \code{place_score}, \code{street_score},
#' and \code{house_number_score} next to each other -- useful for
#' spotting which component drags the overall score down.
#'
#' @param x \code{[GeocodingResults]}
#' @param threshold \code{[numeric/NULL]} If given, only rows whose
#' overall \code{score} is below (or above, see \code{direction}) this
#' value are plotted -- filters all three panels together, e.g. "show me
#' the component breakdown just for the worst-scoring addresses."
#' \code{NULL} (the default) plots everything.
#' @param direction \code{[character]} Whether \code{threshold} keeps
#' rows \code{"below"} (the default) or \code{"above"} it. Ignored if
#' \code{threshold} is \code{NULL}.
#' @param ... Further arguments passed on to \code{\link[graphics]{hist}}.
#'
#' @export
bkg_plot_components <- function(x, threshold = NULL, direction = c("below", "above"), ...) {
  direction <- match.arg(direction)
  x <- .bkg_filter_by_threshold(x, threshold, direction)
  
  component_cols <- c(
    "Place"        = "place_score",
    "Street"       = "street_score",
    "House number" = "house_number_score"
  )
  available <- component_cols[component_cols %in% names(x)]
  
  if (!length(available) || !nrow(x)) {
    cli::cli_warn("No component scores to plot.")
    return(invisible(NULL))
  }
  
  old_par <- graphics::par(mfrow = c(1, length(available)))
  on.exit(graphics::par(old_par))
  
  for (label in names(available)) {
    vals <- x[[available[[label]]]]
    vals <- vals[!is.na(vals)]
    if (length(vals)) {
      graphics::hist(
        vals,
        main = label,
        sub = .bkg_threshold_subtitle(threshold, direction, length(vals)),
        xlab = "Score",
        xlim = c(0, 1),
        ...
      )
    }
  }
  
  invisible(NULL)
}

#' Plot a bar chart of quality-tier counts
#'
#' @param x \code{[GeocodingResults]}
#' @param categories \code{[character/NULL]} If given, restricts the
#' plot to these quality tiers (e.g.
#' \code{c("wrong_street", "wrong_house_number")}). \code{NULL} (the
#' default) plots every tier.
#' @param ... Further arguments passed on to
#' \code{\link[graphics]{barplot}}.
#'
#' @details Uses an existing \code{quality} column if present (see
#' \code{\link{bkg_classify}}), or classifies with default thresholds
#' otherwise -- in the latter case, a disclaimer is printed and added as
#' a plot subtitle, since defaults are a starting point, not a verdict.
#'
#' @examples
#' \dontrun{
#' bkg_plot_quality(gc)
#' bkg_plot_quality(gc, categories = c("wrong_street", "wrong_house_number"))
#' }
#'
#' @export
bkg_plot_quality <- function(x, categories = NULL, ...) {
  res <- .bkg_quality_with_disclaimer(x)
  keep <- .bkg_filter_by_categories(res$quality, categories)
  
  if (!any(keep)) {
    cli::cli_warn("No rows match the requested {.arg categories}.")
    return(invisible(NULL))
  }
  
  tbl <- table(res$quality[keep], useNA = "ifany")
  tbl <- tbl[order(-tbl)]
  names(tbl) <- .bkg_quality_display_name(names(tbl))
  
  graphics::barplot(
    tbl,
    main = "Geocoding quality breakdown",
    sub = if (res$is_default) "Default thresholds -- verify for your data",
    ylab = "Count",
    las = 2,
    ...
  )
}

#' Plot geocoded points on a map
#'
#' @param x \code{[GeocodingResults]}
#' @param color_by \code{[character]} Whether points are colored by
#' \code{"score"} (continuous, red = low to green = high, the default) or
#' by \code{"quality"} tier (categorical -- see
#' \code{\link{bkg_plot_quality}}'s default-thresholds disclaimer, which
#' applies here too).
#' @param threshold \code{[numeric/NULL]} Only used when
#' \code{color_by = "score"}: if given, only rows whose score is below
#' (or above, see \code{direction}) this value are plotted.
#' @param direction \code{[character]} Whether \code{threshold} keeps
#' rows \code{"below"} (the default) or \code{"above"} it.
#' @param categories \code{[character/NULL]} Only used when
#' \code{color_by = "quality"}: restricts the plot to these quality tiers.
#' @param subset_col \code{[character/NULL]} Name of a column to subset
#' by (e.g. \code{"KRS"} for a Kreis, \code{"STA"} for a Bundesland, or
#' any identifier column in your data) -- lets you zoom in on a specific
#' geographic or logical group instead of plotting everything. Must be
#' supplied together with \code{subset_value}.
#' @param subset_value \code{[any/NULL]} Value(s) of \code{subset_col} to
#' keep (e.g. \code{"05111"}). Must be supplied together with
#' \code{subset_col}.
#' @param ... Further arguments passed on to \code{\link[base]{plot}}.
#'
#' @examples
#' \dontrun{
#' bkg_plot_map(gc)                                  # colored by score
#' bkg_plot_map(gc, color_by = "quality")             # colored by quality tier
#' bkg_plot_map(gc, threshold = 0.7, direction = "below")  # worst-scoring only
#' bkg_plot_map(gc, color_by = "quality", categories = "wrong_street")
#'
#' # Zoom in on a specific Kreis
#' bkg_plot_map(gc, subset_col = "KRS", subset_value = "05111")
#' }
#'
#' @export
bkg_plot_map <- function(x, color_by = c("score", "quality"), threshold = NULL,
                         direction = c("below", "above"), categories = NULL,
                         subset_col = NULL, subset_value = NULL, ...) {
  color_by <- match.arg(color_by)
  direction <- match.arg(direction)
  
  x <- .bkg_filter_by_subset(x, subset_col, subset_value)
  
  geom <- sf::st_geometry(x)
  has_geom <- !is.na(x$score) & !sf::st_is_empty(geom)
  
  if (!any(has_geom)) {
    cli::cli_warn("No geocoded geometries to plot.")
    return(invisible(NULL))
  }
  
  x_plot <- x[has_geom, ]
  
  if (color_by == "score") {
    x_plot <- .bkg_filter_by_threshold(x_plot, threshold, direction)
    if (!nrow(x_plot)) {
      cli::cli_warn("No rows match the requested {.arg threshold}.")
      return(invisible(NULL))
    }
    
    palette <- grDevices::colorRampPalette(c("red", "green"))(10)
    bins <- cut(x_plot$score, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE)
    
    plot(
      sf::st_geometry(x_plot),
      pch = 16,
      col = palette[as.integer(bins)],
      main = "Geocoded addresses (colored by score)",
      sub = .bkg_threshold_subtitle(threshold, direction, nrow(x_plot)),
      ...
    )
    graphics::legend(
      "topright",
      legend = c("low", "high"),
      fill = c(palette[1], palette[10]),
      title = "Score",
      bty = "n"
    )
  } else {
    res <- .bkg_quality_with_disclaimer(x_plot, quiet = TRUE)
    keep <- .bkg_filter_by_categories(res$quality, categories)
    
    if (!any(keep)) {
      cli::cli_warn("No rows match the requested {.arg categories}.")
      return(invisible(NULL))
    }
    
    x_plot <- x_plot[keep, ]
    quality_col <- as.character(res$quality[keep])
    tiers <- sort(unique(quality_col))
    tier_palette <- stats::setNames(
      grDevices::palette.colors(length(tiers), palette = "Okabe-Ito"),
      tiers
    )
    
    if (res$is_default) {
      cli::cli_inform(paste(
        "Based on bkg_classify()'s DEFAULT thresholds -- a starting point,",
        "not a verdict. Check them for your data (see",
        "{.fun bkg_classify_interactive})."
      ))
    }
    
    plot(
      sf::st_geometry(x_plot),
      pch = 16,
      col = tier_palette[quality_col],
      main = "Geocoded addresses (colored by quality)",
      sub = if (res$is_default) "Default thresholds -- verify for your data",
      ...
    )
    graphics::legend(
      "topright",
      legend = .bkg_quality_display_name(tiers),
      fill = tier_palette[tiers],
      title = "Quality",
      bty = "n",
      cex = 0.8
    )
  }
}

#' Plot geocoding results (S3 method)
#'
#' @description Thin wrapper dispatching to one of
#' \code{\link{bkg_plot_score}}, \code{\link{bkg_plot_components}},
#' \code{\link{bkg_plot_quality}}, or \code{\link{bkg_plot_map}} --
#' called for \code{plot(x)} to work out of the box, the way any other R
#' object's \code{plot()} method does. Each of the four has its own
#' parameters (only relevant to that one plot); call them directly for
#' full control instead of going through \code{type=} here.
#'
#' @param x \code{[GeocodingResults]}
#' @param type \code{[character]} Which of the four plots to draw:
#' \code{"score"} (default), \code{"components"}, \code{"quality"}, or
#' \code{"map"}.
#' @param ... Passed on to the underlying \code{bkg_plot_*()} function --
#' see its own help page for the parameters that actually apply.
#'
#' @seealso \code{\link{bkg_plot_score}}, \code{\link{bkg_plot_components}},
#' \code{\link{bkg_plot_quality}}, \code{\link{bkg_plot_map}}
#'
#' @export
plot.GeocodingResults <- function(x, type = c("score", "components", "quality", "map"), ...) {
  type <- match.arg(type)
  
  switch(
    type,
    score = bkg_plot_score(x, ...),
    components = bkg_plot_components(x, ...),
    quality = bkg_plot_quality(x, ...),
    map = bkg_plot_map(x, ...)
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