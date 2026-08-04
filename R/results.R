# Everything related to consuming / presenting a GeocodingResults object:
# S3 methods (print, summary, plot) and exporting results to disk.

# -----------------------------------------------------------------------------
# S3 methods
# -----------------------------------------------------------------------------

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
#' \code{\link{bkg_geocode_offline}} or \code{\link{bkg_geocode}}, or any
#' subset of it (e.g. already filtered down to a handful of rows you want
#' a closer look at).
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


# -----------------------------------------------------------------------------
# Quality triage
# -----------------------------------------------------------------------------
# Systematizes a common manual workflow: bucketing rows by their component
# scores into confidence tiers ("perfect", "semi_perfect",
# "wrong_house_number", "wrong_street", "wrong_place", "unmatched") to focus
# manual review on the rows that actually need it, instead of eyeballing
# every row.

#' Default score thresholds for \code{\link{bkg_classify}}
#'
#' @noRd
.bkg_default_quality_thresholds <- function() {
  list(
    perfect_place = 0.8,
    perfect_street = 0.95,
    perfect_house_number = 0.95,
    semi_place = 0.7,
    semi_street = 0.74,
    semi_house_number = 0.9,
    wrong_hn_street = 0.895,
    wrong_hn_house_number = 0.9
  )
}

# The four cascading rules, in order. Shared by bkg_classify() and
# bkg_classify_interactive() so the two can never drift apart --
# the interactive session is a preview loop around exactly the same masks
# the batch function commits directly.
.bkg_quality_rules <- function() {
  c("perfect", "semi_perfect", "wrong_house_number", "wrong_street")
}

#' Add the quality column, positioned right before score
#'
#' @description Column order is purely cosmetic (nothing downstream
#' depends on it), but \code{quality} reads much more naturally sitting
#' directly next to the score columns it was derived from, rather than
#' tacked on at the very end after RS/AGS/coordinates/etc.
#'
#' @noRd
.bkg_insert_quality_column <- function(.data, quality) {
  .data$quality <- quality
  nms <- setdiff(names(.data), "quality")
  score_idx <- match("score", nms)
  new_order <- append(nms, "quality", after = score_idx - 1)
  .data[new_order]
}

# Default output labels, keyed by internal rule name (plus "rest" and
# "unmatched", which aren't rules but always-possible outcomes). Rule
# names are fixed internal identifiers (used to look up thresholds and
# exclude_ids); labels are what actually shows up in the quality column,
# and can be freely renamed via the labels argument without touching any
# rule logic. "rest" defaults to "wrong_place" rather than a plain "rest"
# because, by construction, a row can only end up there if place_score
# fell short of semi_place -- every row that clears that bar is already
# caught by wrong_street (whose only condition is place_score itself) if
# nothing more specific matched first. So "rest" is, in the default case,
# specifically a place-match problem, which the label should say.
.bkg_default_quality_labels <- function() {
  list(
    perfect = "perfect",
    semi_perfect = "semi_perfect",
    wrong_house_number = "wrong_house_number",
    wrong_street = "wrong_street",
    rest = "wrong_place",
    unmatched = "unmatched"
  )
}

#' Boolean mask for one quality rule
#'
#' @noRd
.bkg_quality_rule_mask <- function(.data, rule, th, exclude_ids = list(), id_col = NULL) {
  place_score <- .data$place_score
  street_score <- .data$street_score
  house_number_score <- .data$house_number_score
  
  mask <- switch(
    rule,
    perfect = place_score >= th$perfect_place &
      street_score >= th$perfect_street &
      house_number_score >= th$perfect_house_number,
    semi_perfect = place_score >= th$semi_place &
      street_score >= th$semi_street &
      house_number_score >= th$semi_house_number,
    wrong_house_number = place_score >= th$semi_place &
      street_score >= th$wrong_hn_street &
      house_number_score < th$wrong_hn_house_number,
    wrong_street = place_score >= th$semi_place,
    cli::cli_abort("Unknown rule {.val {rule}}.")
  )
  mask[is.na(mask)] <- FALSE
  
  ids_to_exclude <- exclude_ids[[rule]]
  if (!is.null(id_col) && length(ids_to_exclude)) {
    mask <- mask & !(as.character(.data[[id_col]]) %in% ids_to_exclude)
  }
  
  mask
}

#' Comparison direction for one threshold parameter
#'
#' @description Every threshold is a ">=" cutoff (higher = stricter,
#' catches fewer rows) except \code{wrong_hn_house_number}, which is a
#' "<" cutoff (a row counts as a wrong house number if its score falls
#' \emph{below} this value) -- so for that one parameter, raising the
#' value makes the rule catch \emph{more} rows, not fewer. Surfaced in
#' the interactive prompt so this inversion is never a silent surprise.
#'
#' @noRd
.bkg_quality_param_comparison <- function(param) {
  if (identical(param, "wrong_hn_house_number")) "<" else ">="
}

#' Threshold names relevant to one quality rule
#'
#' @noRd
.bkg_quality_rule_params <- function(rule) {
  switch(
    rule,
    perfect = c("perfect_place", "perfect_street", "perfect_house_number"),
    semi_perfect = c("semi_place", "semi_street", "semi_house_number"),
    wrong_house_number = c("semi_place", "wrong_hn_street", "wrong_hn_house_number"),
    wrong_street = "semi_place",
    character(0)
  )
}

#' Default preview sort order for one quality rule
#'
#' @description For most rules, the most useful review order is
#' ascending \code{street_score} -- the weakest, most borderline matches
#' within the bucket surface first. Two exceptions:
#' \code{"wrong_house_number"} sorts by \emph{descending}
#' \code{house_number_score}, since that's the score the rule is
#' actually about -- best (least obviously wrong) first.
#' \code{"wrong_street"} sorts by \emph{descending} street score, for the
#' same reason: almost anything with a decent place score ends up there,
#' so the near-misses (most likely to actually be a genuine match)
#' surface first instead of the most obviously wrong ones. All of this is
#' just a starting point -- see the \code{[o]rder} option during an
#' interactive session to change it on the fly.
#'
#' @noRd
.bkg_quality_preview_sort <- function(rule) {
  switch(
    rule,
    wrong_house_number = list(col = "house_number_score", decreasing = TRUE),
    wrong_street = list(col = "street_score", decreasing = TRUE),
    list(col = "street_score", decreasing = FALSE)
  )
}

#' Build the bkg_classify() call that reproduces a given state
#'
#' @description Deliberately spells out every argument explicitly --
#' \code{thresholds} and \code{labels} always show the complete list,
#' not just entries that differ from the package's current defaults.
#' Showing only the diff would make the call's meaning depend on
#' whatever the package's defaults happen to be when it's later run,
#' which could silently change between package versions; a fully
#' explicit call reproduces the exact same result regardless.
#'
#' @noRd
.bkg_reproducible_quality_call <- function(th, exclude_ids, id_col, labels) {
  call_args <- list(.data = quote(.data))
  call_args$thresholds <- th
  call_args$exclude_ids <- lapply(exclude_ids, as.character)
  # A literal R NULL, if inserted directly as a list element's value,
  # gets silently dropped when as.call() below turns this into a
  # pairlist (a well-known R quirk: NULL list elements vanish on
  # conversion to pairlists/calls) -- quote(NULL) preserves it as the
  # *language object* representing the symbol NULL, so id_col = NULL
  # still shows up in the deparsed call when no id_col was given.
  call_args$id_col <- if (is.null(id_col)) quote(NULL) else id_col
  call_args$labels <- labels
  
  call_expr <- as.call(c(quote(bkg_classify), call_args))
  paste(deparse(call_expr, width.cutoff = 60L), collapse = "\n")
}

#' Classify offline geocoding results by quality
#'
#' @description Buckets each geocoded row into a data-quality category
#' based on its component scores (\code{place_score}, \code{street_score},
#' \code{house_number_score}), so unusually strong or weak matches can be
#' triaged without inspecting every row by hand. This systematizes a
#' common manual workflow -- cascading through score thresholds into
#' "perfect", "semi-perfect", "wrong house number", "wrong street", and
#' "rest" buckets -- as a single, reusable, adjustable step, so the
#' thresholds are named and visible in one place instead of scattered
#' across several ad hoc \code{dplyr::filter()} calls. See
#' \code{\link{bkg_classify_interactive}} for a guided version
#' that walks through each rule with a preview before committing to it.
#'
#' @param .data \code{[GeocodingResults]} Output of
#' \code{\link{bkg_geocode_offline}} or \code{\link{bkg_geocode}}.
#' @param thresholds \code{[list]}
#'
#' Named list overriding any subset of the default thresholds (see
#' Details); unspecified ones keep their default value.
#' @param exclude_ids \code{[list]}
#'
#' Named list keyed by rule name (\code{"perfect"}, \code{"semi_perfect"},
#' \code{"wrong_house_number"}, \code{"wrong_street"}), each holding IDs
#' that should be excluded from that specific rule -- e.g. a row that
#' scores well enough to look "perfect" but is known, from prior manual
#' review, to actually need a closer look. Excluded rows simply fall
#' through to the next rule (or end up in \code{"rest"}) instead of being
#' force-assigned elsewhere. Requires \code{id_col}.
#' @param id_col \code{[character]}
#'
#' Name of a column in \code{.data} that uniquely identifies each row.
#' Required if \code{exclude_ids} is used, ignored otherwise.
#' @param labels \code{[list]}
#'
#' Named list overriding any subset of the default output labels --
#' \code{perfect}, \code{semi_perfect}, \code{wrong_house_number},
#' \code{wrong_street}, \code{rest} (defaults to \code{"wrong_place"},
#' see Details), \code{unmatched}. The names on the left are fixed
#' internal rule identifiers (also used by \code{thresholds}/
#' \code{exclude_ids}); only the label text that ends up in the
#' \code{quality} column is renamed. Unspecified ones keep their default
#' label.
#'
#' @details Rules are applied in order, each one only catching rows not
#' already classified by an earlier (higher-confidence) rule:
#' \enumerate{
#'   \item \strong{unmatched}: \code{is.na(score)} -- no place could be
#'     matched at all (see \code{attr(.data, "unmatched_places")})
#'   \item \strong{perfect}: \code{place_score >= perfect_place} (default
#'     \code{0.8}), \code{street_score >= perfect_street} (\code{0.95}),
#'     \code{house_number_score >= perfect_house_number} (\code{0.95})
#'   \item \strong{semi_perfect}: \code{place_score >= semi_place}
#'     (\code{0.7}), \code{street_score >= semi_street} (\code{0.74}),
#'     \code{house_number_score >= semi_house_number} (\code{0.9})
#'   \item \strong{wrong_house_number}: \code{place_score >= semi_place},
#'     \code{street_score >= wrong_hn_street} (\code{0.895}) -- i.e. the
#'     street matched well -- but \code{house_number_score <
#'     wrong_hn_house_number} (\code{0.9})
#'   \item \strong{wrong_street}: \code{place_score >= semi_place}, but
#'     street/house number too weak to qualify for any rule above -- note
#'     this rule has no condition beyond \code{place_score} itself
#'   \item \strong{rest} (labeled \code{"wrong_place"} by default): every
#'     row that reaches this point without \code{exclude_ids} overrides
#'     necessarily has \code{place_score < semi_place} -- anything at or
#'     above that bar was already caught by \code{wrong_street}, whose
#'     only condition is \code{place_score} itself. So by construction,
#'     this bucket is specifically a place-match problem, which the
#'     default label reflects (use \code{labels = list(rest = "rest")} to
#'     get the old, less specific name back).
#' }
#'
#' These defaults come from a real manual triage workflow and are a
#' reasonable starting point, not a universal truth -- the right cutoffs
#' depend on the data source and how conservative you want to be. Treat
#' the resulting buckets as a way to prioritize manual review (start with
#' \code{"wrong_street"}/\code{"wrong_place"}, spot-check a sample of
#' \code{"perfect"}), not as a final, unreviewed verdict -- some rows will
#' always need a human look regardless of where the thresholds sit.
#'
#' @returns \code{.data}, with an added \code{quality} column (character:
#' \code{"perfect"}, \code{"semi_perfect"}, \code{"wrong_house_number"},
#' \code{"wrong_street"}, \code{"wrong_place"}, or \code{"unmatched"} by
#' default -- see \code{labels} to rename any of these).
#'
#' @seealso \code{\link{bkg_classify_interactive}},
#' \code{\link{bkg_quality_summary}}
#'
#' @examples
#' \dontrun{
#' gc <- bkg_geocode_offline(my_data, cols = 1:4)
#' gc <- bkg_classify(gc)
#' table(gc$quality)
#'
#' # Loosen the "perfect" bar a little, and pull two known-bad IDs out of it
#' gc <- bkg_classify(
#'   gc,
#'   thresholds = list(perfect_street = 0.9),
#'   exclude_ids = list(perfect = c("2690031", "2690097")),
#'   id_col = "ID"
#' )
#'
#' # Rename a category (e.g. back to the old, plainer name)
#' gc <- bkg_classify(gc, labels = list(rest = "rest"))
#' }
#'
#' @export
bkg_classify <- function(.data, thresholds = list(),
                         exclude_ids = list(), id_col = NULL,
                         labels = list()) {
  defaults <- .bkg_default_quality_thresholds()
  
  unknown <- setdiff(names(thresholds), names(defaults))
  if (length(unknown)) {
    cli::cli_abort(paste(
      "Unknown threshold name{?s}: {.val {unknown}}.",
      "See {.fun bkg_classify}'s Details for the valid names."
    ))
  }
  
  default_labels <- .bkg_default_quality_labels()
  unknown_labels <- setdiff(names(labels), names(default_labels))
  if (length(unknown_labels)) {
    # The "valid names" list is pre-collapsed into a single string before
    # The message uses c(...) (separate bullets), not paste() (one
    # combined string): paste() would merge both lines into a SINGLE glue
    # block, so {?s} in line 1 would still "see" the {valid_labels}
    # interpolation in line 2 and cli can't tell which one is "the"
    # pluralization quantity ("Multiple quantities for pluralization").
    # Each element of a c(...) vector is evaluated by cli independently.
    valid_labels <- paste(names(default_labels), collapse = ", ")
    cli::cli_abort(c(
      "Unknown label name{?s}: {.val {unknown_labels}}.",
      "i" = "Must be one of: {valid_labels}."
    ))
  }
  
  if (length(exclude_ids)) {
    if (is.null(id_col)) {
      cli::cli_abort("{.arg id_col} must be supplied when {.arg exclude_ids} is used.")
    }
    unknown_rules <- setdiff(names(exclude_ids), .bkg_quality_rules())
    if (length(unknown_rules)) {
      # Same c(...)-instead-of-paste() fix as above, plus: a dot-prefixed
      # function call directly inside {} (.bkg_quality_rules()) trips up
      # cli's own syntax -- a leading dot inside {} is interpreted as a
      # style directive, not as R code, regardless of parentheses.
      # Pre-computing into a plain-named local variable avoids both issues
      # at once.
      valid_rules <- paste(.bkg_quality_rules(), collapse = ", ")
      cli::cli_abort(c(
        "Unknown rule name{?s} in {.arg exclude_ids}: {.val {unknown_rules}}.",
        "i" = "Must be one of: {valid_rules}."
      ))
    }
  }
  if (!is.null(id_col) && !id_col %in% names(.data)) {
    cli::cli_abort("{.arg id_col} ({.val {id_col}}) not found in {.arg .data}.")
  }
  
  required_cols <- c("score", "place_score", "street_score", "house_number_score")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols)) {
    cli::cli_abort(paste(
      "{.arg .data} is missing column{?s} {.val {missing_cols}} --",
      "is this the output of {.fun bkg_geocode_offline} or",
      "{.fun bkg_geocode}?"
    ))
  }
  
  th <- utils::modifyList(defaults, thresholds)
  lbl <- utils::modifyList(default_labels, labels)
  
  quality <- rep(NA_character_, nrow(.data))
  quality[is.na(.data$score)] <- lbl$unmatched
  
  for (rule in .bkg_quality_rules()) {
    mask <- is.na(quality) &
      .bkg_quality_rule_mask(.data, rule, th, exclude_ids, id_col)
    quality[mask] <- lbl[[rule]]
  }
  
  quality[is.na(quality)] <- lbl$rest
  
  .bkg_insert_quality_column(.data, quality)
}

#' Interactively classify offline geocoding results by quality
#'
#' @description Walks through the same rules as
#' \code{\link{bkg_classify}}, one at a time: for each rule, shows
#' a preview of the rows it would catch with the current thresholds, and
#' lets you accept it, adjust its thresholds, exclude specific IDs from
#' it, or skip the rule entirely -- before moving on to the next one. At
#' the end, prints (and attaches as an attribute) the exact
#' \code{\link{bkg_classify}} call that reproduces the session's
#' result, so the outcome of an interactive review can be replayed
#' non-interactively later (updated data, a script for colleagues, etc.)
#' without repeating the manual review.
#'
#' @inheritParams bkg_classify
#' @param n_preview \code{[integer]} Number of rows to print per preview.
#'
#' @details Within each rule's preview, rows are sorted by
#' \code{street_score} -- ascending (weakest first) for every rule except
#' \code{"wrong_street"}, where descending order surfaces the near-misses
#' (most likely to actually be a genuine match) before the more obviously
#' wrong ones. This mirrors sorting by the score most relevant to that
#' rule's decision.
#'
#' @returns \code{.data} -- the same object \code{\link{bkg_geocode_offline}}
#' (or \code{\link{bkg_geocode}}) returned, with an added \code{quality}
#' column, and nothing else changed. The reproducible
#' \code{\link{bkg_classify}} call is printed to the console at
#' the end of the session, not attached to the returned object.
#'
#' Thin wrapper around interactive()
#'
#' @description Exists purely so tests can reliably mock this check via
#' \code{local_mocked_bindings()} -- \code{base}'s namespace is locked,
#' which can make directly mocking \code{base::interactive()} unreliable,
#' whereas mocking a binding within this package's own (unlocked)
#' namespace always works.
#'
#' @noRd
.bkg_is_interactive <- function() {
  interactive()
}

#' @seealso \code{\link{bkg_classify}}
#'
#' @export
bkg_classify_interactive <- function(.data, thresholds = list(),
                                     id_col = NULL, labels = list(),
                                     n_preview = 10) {
  if (!.bkg_is_interactive()) {
    cli::cli_abort(paste(
      "{.fun bkg_classify_interactive} requires an interactive R",
      "session -- use {.fun bkg_classify} directly in scripts."
    ))
  }
  
  required_cols <- c("score", "place_score", "street_score", "house_number_score")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols)) {
    cli::cli_abort(paste(
      "{.arg .data} is missing column{?s} {.val {missing_cols}} --",
      "is this the output of {.fun bkg_geocode_offline} or",
      "{.fun bkg_geocode}?"
    ))
  }
  if (!is.null(id_col) && !id_col %in% names(.data)) {
    cli::cli_abort("{.arg id_col} ({.val {id_col}}) not found in {.arg .data}.")
  }
  
  default_labels <- .bkg_default_quality_labels()
  unknown_labels <- setdiff(names(labels), names(default_labels))
  if (length(unknown_labels)) {
    valid_labels <- paste(names(default_labels), collapse = ", ")
    cli::cli_abort(c(
      "Unknown label name{?s}: {.val {unknown_labels}}.",
      "i" = "Must be one of: {valid_labels}."
    ))
  }
  lbl <- utils::modifyList(default_labels, labels)
  
  th <- utils::modifyList(.bkg_default_quality_thresholds(), thresholds)
  exclude_ids <- stats::setNames(
    replicate(length(.bkg_quality_rules()), character(0), simplify = FALSE),
    .bkg_quality_rules()
  )
  
  quality <- rep(NA_character_, nrow(.data))
  quality[is.na(.data$score)] <- lbl$unmatched
  
  display_cols <- intersect(
    c(id_col, "place_score", "street_score", "house_number_score",
      "address_input", "address_cleaned", "address_output"),
    names(.data)
  )
  
  cli::cli_h1("Interactive quality classification")
  cli::cli_inform(c(
    "i" = "For each rule: [a]ccept (or just Enter), [t]weak thresholds,",
    " " = "[e]xclude specific IDs, [p]review row count, [o]rder, [d]etail",
    " " = "view, [s]kip rule entirely.",
    "i" = "A reproducible call is printed at the end."
  ))
  
  n_rules <- length(.bkg_quality_rules())
  for (rule_idx in seq_along(.bkg_quality_rules())) {
    rule <- .bkg_quality_rules()[rule_idx]
    skip_rule <- FALSE
    sort_spec <- .bkg_quality_preview_sort(rule)
    
    repeat {
      mask <- is.na(quality) &
        .bkg_quality_rule_mask(.data, rule, th, exclude_ids, id_col)
      caught <- .data[mask, , drop = FALSE]
      
      if (nrow(caught) && sort_spec$col %in% names(caught)) {
        caught <- caught[order(caught[[sort_spec$col]], decreasing = sort_spec$decreasing), , drop = FALSE]
      }
      
      cli::cli_rule(left = sprintf("Step %d/%d: %s", rule_idx, n_rules, rule))
      cat(sprintf(
        "%d row(s), sorted by %s (%s)\n",
        nrow(caught), sort_spec$col,
        if (sort_spec$decreasing) "best first" else "worst first"
      ))
      
      if (nrow(caught)) {
        preview <- caught[seq_len(min(n_preview, nrow(caught))), display_cols]
        if (inherits(preview, "sf")) preview <- sf::st_drop_geometry(preview)
        # Subsetting a GeocodingResults object with `[` keeps its class,
        # which would otherwise make print() dispatch to
        # print.GeocodingResults() (the package's own summary-style
        # display) instead of a plain row table -- strip it first, same
        # as print.GeocodingResults() does internally for its own preview.
        class(preview) <- setdiff(class(preview), "GeocodingResults")
        if (inherits(preview, "tbl")) {
          print(preview, n = n_preview)
        } else {
          print(preview)
        }
        if (nrow(caught) > n_preview) {
          cli::cli_inform("... and {nrow(caught) - n_preview} more row(s).")
        }
      } else {
        cli::cli_inform("No rows match with the current thresholds/exclusions.")
      }
      
      choice <- tolower(trimws(readline(
        "[a]ccept / [t]hresholds / [e]xclude IDs / [p]review count / [o]rder / [d]etail view / [s]kip rule > "
      )))
      
      if (choice %in% c("a", "")) {
        break
      } else if (choice == "t") {
        for (p in .bkg_quality_rule_params(rule)) {
          op <- .bkg_quality_param_comparison(p)
          hint <- if (op == "<") {
            "score BELOW this counts -- raising it catches MORE rows"
          } else {
            "score AT/ABOVE this counts -- raising it catches FEWER rows"
          }
          cli::cli_inform("  {.val {p}} ({op} ; {hint})")
          val <- trimws(readline(sprintf("    new value [current: %s]: ", th[[p]])))
          if (nzchar(val)) {
            num <- suppressWarnings(as.numeric(val))
            if (is.na(num)) {
              cli::cli_inform("  Not a number, keeping {.val {th[[p]]}}.")
            } else {
              th[[p]] <- num
            }
          }
        }
      } else if (choice == "p") {
        val <- trimws(readline(sprintf("  new preview row count [current: %s]: ", n_preview)))
        if (nzchar(val)) {
          num <- suppressWarnings(as.integer(val))
          if (is.na(num) || num < 1) {
            cli::cli_inform("  Not a valid positive integer, keeping {.val {n_preview}}.")
          } else {
            n_preview <- num
          }
        }
      } else if (choice == "o") {
        sortable_cols <- intersect(
          c("place_score", "street_score", "house_number_score"),
          names(.data)
        )
        current_idx <- match(sort_spec$col, sortable_cols)
        cli::cli_inform("  Sort column:")
        for (i in seq_along(sortable_cols)) {
          marker <- if (i == current_idx) " (current)" else ""
          cli::cli_inform("    {i}) {sortable_cols[i]}{marker}")
        }
        col_input <- trimws(readline("  choice [number, Enter to keep]: "))
        if (nzchar(col_input)) {
          num <- suppressWarnings(as.integer(col_input))
          if (!is.na(num) && num >= 1 && num <= length(sortable_cols)) {
            sort_spec$col <- sortable_cols[num]
          } else {
            cli::cli_inform("  Not a valid choice, keeping {.val {sort_spec$col}}.")
          }
        }
        dir_input <- tolower(trimws(readline(sprintf(
          "  direction [b=best first/w=worst first, current: %s]: ",
          if (sort_spec$decreasing) "best first" else "worst first"
        ))))
        if (dir_input == "b") {
          sort_spec$decreasing <- TRUE
        } else if (dir_input == "w") {
          sort_spec$decreasing <- FALSE
        } else if (nzchar(dir_input)) {
          cli::cli_inform("  Not understood, keeping current direction.")
        }
      } else if (choice == "d") {
        if (nrow(caught) && all(c("address_input", "address_cleaned", "address_output") %in% names(caught))) {
          bkg_show_address_detail(
            caught[seq_len(min(n_preview, nrow(caught))), , drop = FALSE],
            id_col = id_col
          )
        } else {
          cli::cli_inform("Nothing to show (no rows, or missing address columns).")
        }
      } else if (choice == "e") {
        if (is.null(id_col)) {
          cli::cli_inform("No {.arg id_col} was supplied -- can't exclude by ID.")
        } else {
          ids_input <- readline("  ID(s) to exclude, comma-separated: ")
          ids <- trimws(strsplit(ids_input, ",")[[1]])
          ids <- ids[nzchar(ids)]
          exclude_ids[[rule]] <- unique(c(exclude_ids[[rule]], ids))
        }
      } else if (choice == "s") {
        skip_rule <- TRUE
        break
      } else {
        cli::cli_inform("Not understood, try again.")
      }
    }
    
    if (!skip_rule) {
      final_mask <- is.na(quality) &
        .bkg_quality_rule_mask(.data, rule, th, exclude_ids, id_col)
      quality[final_mask] <- lbl[[rule]]
    }
  }
  
  quality[is.na(quality)] <- lbl$rest
  .data <- .bkg_insert_quality_column(.data, quality)
  
  code <- .bkg_reproducible_quality_call(th, exclude_ids, id_col, lbl)
  cli::cli_h2("Reproducible code")
  cat("\n", code, "\n\n", sep = "")
  
  # Returned invisibly: this can be a very large object (thousands of
  # rows), and if the call isn't assigned to a variable, R's normal
  # auto-print would otherwise dump the whole thing right after the
  # reproducible-code block above, burying the one thing you actually
  # need to copy. Assign the result (e.g. `gc2 <- bkg_classify_interactive(...)`)
  # to keep working with it. Deliberately just `.data` -- the exact
  # object bkg_geocode_offline() returned, plus the quality column --
  # with no extra attributes tacked on; the reproducible call is only
  # ever shown via cat() above, never smuggled onto the object itself.
  invisible(.data)
}

#' Summarize the quality distribution of geocoding results
#'
#' @description Tabulates the \code{quality} categories produced by
#' \code{\link{bkg_classify}} -- counts and percentages, so you
#' get the same "how much of this needs review" overview as
#' \code{proportions(table(...))} without repeating that call everywhere.
#' If \code{.data} doesn't have a \code{quality} column yet,
#' \code{\link{bkg_classify}} is run first with default
#' thresholds.
#'
#' @param .data \code{[GeocodingResults]} Output of
#' \code{\link{bkg_geocode_offline}}/\code{\link{bkg_geocode}}, optionally
#' already run through \code{\link{bkg_classify}}.
#' @param ... Passed on to \code{\link{bkg_classify}} if
#' \code{.data} doesn't already have a \code{quality} column (e.g. a
#' custom \code{thresholds} list).
#'
#' @returns A \code{tibble} with columns \code{quality}, \code{n}, and
#' \code{pct}.
#'
#' @export
bkg_quality_summary <- function(.data, ...) {
  if (!"quality" %in% names(.data)) {
    .data <- bkg_classify(.data, ...)
  }
  
  tbl <- table(.data$quality, useNA = "ifany")
  tbl <- tbl[order(-tbl)]
  
  tibble::tibble(
    quality = names(tbl),
    n = as.integer(tbl),
    pct = round(as.numeric(tbl) / sum(tbl) * 100, 1)
  )
}


# -----------------------------------------------------------------------------
# Exporting results
# -----------------------------------------------------------------------------

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