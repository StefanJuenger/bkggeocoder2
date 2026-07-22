"%||%" <- function(x, y) if (is.null(x)) y else x
"%??%" <- function(x, y) if (is.null(x) || all(is.na(x))) y else x
"%__%" <- function(x, y) if (length(x) == 0) y else x

#' Binds list of (sf) data.frames to a single data.frame. If the number of
#' columns differs, fills empty columns with NA
#' @param args List of data.frames or sf objects
#' @returns data.frame or sf data.frame
#' @noRd
rbind_list <- function(args) {
  nam <- lapply(args, names)
  unam <- unique(unlist(nam))
  len <- vapply(args, length, numeric(1))
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    if (nrow(args[[i]])) {
      nam_diff <- setdiff(unam, nam[[i]])
      if (length(nam_diff)) {
        args[[i]][nam_diff] <- NA
      }
    } else {
      next
    }
  }
  out <- do.call(rbind, args)
  rownames(out) <- NULL
  out
}


#' Simpler wrapper for regexec and regmatches
#'
#' @param x A character vector to be matched.
#' @param pattern Regex expression or term to be looked up in x
#' @param ... Further arguments passed to regexec.
#'
#' @noRd
match_regex <- function(x, pattern, ...) {
  matches <- regexec(pattern, x, ...)
  regmatches(x, matches)
}


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

normalize_key <- function(x) {
  x |>
    stringi::stri_trans_general("de-ASCII") |>
    tolower() |>
    gsub("[^a-z0-9]", "", x = _)
}


# -----------------------------------------------------------------------------
# Geospatial helpers
# -----------------------------------------------------------------------------

#' Create 1km and 100m INSPIRE IDs
#'
#' Create 1 km² and 100m X 100m INSPIRE IDs from coordinates
#'
#' @param data Object of class \code{sf} containing point geometries
#' @param type Character string for the requested ID type
#' @param column_name Output column name prefix. Defaults to "Gitter_ID_{type}".
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
    substr(as.character(coordinate_pairs$Y), 1, 4 + (type == "100m")),
    substr(as.character(coordinate_pairs$X), 1, 4 + (type == "100m"))
  )

  if (isTRUE(combine)) {
    inspire_ids <- cbind(data, data.frame(id_name = inspire_ids))
    names(inspire_ids)[names(inspire_ids) == "id_name"] <- id_name
  } else {
    inspire_ids
  }
}
