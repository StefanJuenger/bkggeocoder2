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

verify_server <- function (credentials_path) {
  live_fp <- tryCatch(
    system2(
      "openssl",
      c(
        "s_client",
        "-connect", "10.6.13.50:8544",
        "-servername", "10.6.13.50"
      ),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(live_fp)) {
    cli::cli_abort(c(
      "Connection timed out. Address or place file could not be loaded.",
      "i" = "Increase {.code options(timeout = ...)} to circumvent this issue."
    ))
  }
  
  live_fp <- system2(
    "openssl",
    c("x509", "-noout", "-fingerprint", "-sha256"),
    input = live_fp,
    stdout = TRUE
  )
  
  local_fp <- system2(
    "openssl",
    c(
      "x509",
      "-in", credentials_path,
      "-noout",
      "-fingerprint",
      "-sha256"
    ),
    stdout = TRUE
  )
  
  if (!identical(live_fp, local_fp)) {
    cli::cli_abort(c(
      "Cannot verify the authenticity of the local server certificate.",
      "!" = "The certificate fingerprint does not match {.path {credentials_path}}.",
      "i" = "Verify that you are connected to the GESIS intranet and that your certificate is up to date."
    ))
  }
}
