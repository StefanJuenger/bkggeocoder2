# Classifies offline geocoding results into confidence tiers based on their
# component scores (bkg_classify()/bkg_classify_interactive()/
# bkg_quality_summary()). Split out from results.R because this subsystem has
# no dependency on the print/summary/plot/export methods there -- it only
# needs the score columns bkg_geocode_offline() produces.

# Quality triage ----
# Systematizes a manual workflow: bucketing rows by their component
# scores into confidence tiers, to focus manual review where it's
# actually needed instead of eyeballing every row.

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
# bkg_classify_interactive() so the two can never drift apart.
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

# Output labels, keyed by internal rule name (plus "rest"/"unmatched",
# always-possible outcomes that aren't rules). Freely renamable via the
# labels argument. "rest" defaults to "wrong_place": by construction it
# only catches place_score below semi_place, since wrong_street already
# catches everything above.
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

# Human-readable step titles for bkg_classify_interactive()'s header --
# a third concept, separate from the rule name (thresholds/exclude_ids
# lookup) and the label (quality column): a short value like
# "semi_perfect" is code-friendly for filtering but reads as jargon in a
# header meant to be skimmed once.
.bkg_default_quality_titles <- function() {
  list(
    perfect = "Perfect matches",
    semi_perfect = "Possible other perfect matches",
    wrong_house_number = "Wrong house numbers",
    wrong_street = "Wrong street names"
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
  # quote(NULL), not a literal NULL: NULL list elements are silently
  # dropped when as.call() converts this to a pairlist, which would make
  # id_col = NULL vanish from the deparsed call instead of showing it.
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
#' \code{\link{bkg_geocode_offline}}.
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
    # c(...) bullets, not paste(): paste() merges both lines into one
    # glue block, making {?s} ambiguous about which interpolated
    # collection is "the" pluralization quantity.
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
      # Same c(...) fix as above, plus: a dot-prefixed call directly
      # inside {} trips up cli's syntax (a leading dot means style
      # directive, not R code) -- a plain local variable avoids both.
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

#' Interactively classify offline geocoding results by quality
#'
#' @description Walks through the same rules as
#' \code{\link{bkg_classify}}, one at a time: for each rule, shows
#' a preview of the rows it would catch with the current thresholds, and
#' lets you accept it, adjust its thresholds, exclude specific IDs from
#' it, or skip the rule entirely -- before moving on to the next one. At
#' the end, prints the exact \code{\link{bkg_classify}} call that
#' reproduces the session's result, so the outcome of an interactive
#' review can be replayed non-interactively later (updated data, a
#' script for colleagues, etc.) without repeating the manual review.
#'
#' @inheritParams bkg_classify
#' @param titles \code{[list]}
#'
#' Named list overriding any subset of the default step titles shown in
#' each rule's header during the session (\code{perfect},
#' \code{semi_perfect}, \code{wrong_house_number}, \code{wrong_street}).
#' Purely cosmetic -- shown only in this interactive header, never
#' written anywhere or included in the reproducible
#' \code{\link{bkg_classify}} call (which has no \code{titles} argument).
#' Distinct from \code{labels}: a label is a short, code-friendly value
#' that ends up in the \code{quality} column (e.g. good for later
#' \code{dplyr::filter()}ing), while a title is a longer, human-readable
#' phrase meant to be read once during review. Unspecified ones keep
#' their default title.
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
#' returned, with an added \code{quality}
#' column, and nothing else changed. The reproducible
#' \code{\link{bkg_classify}} call is printed to the console at
#' the end of the session, not attached to the returned object.
#'
#' @seealso \code{\link{bkg_classify}}
#'
#' @export
bkg_classify_interactive <- function(.data, thresholds = list(),
                                     id_col = NULL, labels = list(),
                                     titles = list(), n_preview = 10) {
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
  
  default_titles <- .bkg_default_quality_titles()
  unknown_titles <- setdiff(names(titles), names(default_titles))
  if (length(unknown_titles)) {
    valid_titles <- paste(names(default_titles), collapse = ", ")
    cli::cli_abort(c(
      "Unknown title name{?s}: {.val {unknown_titles}}.",
      "i" = "Must be one of: {valid_titles}."
    ))
  }
  step_titles <- utils::modifyList(default_titles, titles)
  
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
      
      cli::cli_rule(left = sprintf(
        "%d/%d. (Check) %s", rule_idx, n_rules, step_titles[[rule]]
      ))
      cat(sprintf(
        "%d row(s), sorted by %s (%s)\n",
        nrow(caught), sort_spec$col,
        if (sort_spec$decreasing) "best first" else "worst first"
      ))
      
      if (nrow(caught)) {
        preview <- caught[seq_len(min(n_preview, nrow(caught))), display_cols]
        if (inherits(preview, "sf")) preview <- sf::st_drop_geometry(preview)
        # Strip the class so print() doesn't dispatch to
        # print.GeocodingResults() instead of a plain row table.
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
  
  # Invisible: an unassigned call would otherwise auto-print the whole
  # (possibly huge) object right after the code above. Just `.data` plus
  # the quality column -- no extra attributes.
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
#' \code{\link{bkg_geocode_offline}}, optionally
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

