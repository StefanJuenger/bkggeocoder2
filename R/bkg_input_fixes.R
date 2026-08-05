# Input-cleaning fixes applied to raw address components before matching
# (see apply_input_fixes(), called from bkg_matching.R). Split out since
# this registry has no dependency on the DuckDB matching engine.

# Shared R-side input normalization helpers ----

#' Strip formatting noise from a raw house number input
#'
#' @description Removes whitespace and periods that sometimes end up in
#' user-entered house numbers (e.g. "10 a", "10."), which would otherwise
#' lower the match score for no good reason -- unlike
#' \code{\link{collapse_house_number_range}}, this loses no real
#' information, so its result is used both for matching AND as the
#' displayed input value.
#'
#' @param x \code{[character]}
#'
#' @returns \code{[character]}
#'
#' @noRd
clean_house_number_input <- function(x) {
  gsub("[[:space:].]", "", x)
}

#' Strip a "Ortsteil" (OT) suffix from a raw place input
#'
#' @description Some address data sets append the district/sub-locality as
#' a suffix to the place name (e.g. "Musterstadt OT Nebendorf" or
#' "Musterstadt (OT Nebendorf)"), which is noise for place matching against
#' the official place name. This loses no information that
#' \code{\link{bkg_geocode_offline}} otherwise uses, so its result is used
#' both for matching AND as the displayed input value.
#'
#' @param x \code{[character]}
#'
#' @returns \code{[character]}
#'
#' @noRd
strip_place_suffix <- function(x) {
  x <- gsub(" \\(OT.*", "", x)
  x <- gsub(" OT.*", "", x)
  x
}

#' Expand abbreviated "str"/"str." to "strasse"
#'
#' @description Handles both "Hauptstr. 5" (abbreviation followed by
#' whitespace) and "Hauptstr"/"Hauptstr." (abbreviation at the very end of
#' the string, e.g. when street and house number are given in separate
#' columns). Old spellings ("strasse"/"Strasse") don't need separate
#' handling here: \code{nk()} further downstream already treats "ss" (its
#' escaped/transliterated form of sharp s) as equivalent for comparison, so
#' both spellings compare equally without any extra normalization at this
#' stage.
#'
#' @param x \code{[character]}
#'
#' @returns \code{[character]}
#'
#' @noRd
expand_street_abbreviation <- function(x) {
  gsub("str\\.?(?=$|\\s)", "stra\u00dfe", x, ignore.case = TRUE, perl = TRUE)
}

#' Expand trailing "tr"/"tr." street abbreviations (broad form) and trim
#' whitespace
#'
#' @description A second, more permissive pass at the same "str"/"str."
#' abbreviation \code{\link{expand_street_abbreviation}} already handles.
#' Where that function requires the abbreviation to be followed by
#' whitespace or the end of the string, this one matches "tr." anywhere
#' in the string, and a trailing "tr" without a period -- relying on the
#' fact that the German abbreviation is always spelled with a leading
#' "s" ("str."), which the replacement leaves untouched, so it only ever
#' needs to supply the "trasse"/"tra\u00dfe" part. In practice this mostly
#' runs as a no-op, since \code{expand_street_abbreviation()} (registered
#' before this one, see \code{.bkg_input_fixes}) already catches the
#' common cases -- it exists for the raw forms that lookahead misses
#' (e.g. an abbreviation followed directly by punctuation other than
#' whitespace). Also trims stray leading/trailing whitespace, which the
#' raw input occasionally carries.
#'
#' @param x \code{[character]}
#'
#' @returns \code{[character]}
#'
#' @noRd
expand_street_abbreviation_broad <- function(x) {
  x <- trimws(x)
  x <- gsub("tr\\.", "tra\u00dfe", x)
  x <- gsub("tr$", "tra\u00dfe", x)
  x
}

#' Collapse a house number range down to its first number
#'
#' @description House number ranges (e.g. "13-15") don't exist as a single
#' row in the reference data -- only the individual house numbers "13",
#' "14", "15" do. For MATCHING purposes, a range needs to be collapsed down
#' to its first number (mirroring how such ranges have always been
#' resolved); comparing the full range string via jaro_winkler instead can
#' match an unrelated house number that happens to share many characters
#' with the range string (e.g. "13-15" is closer to "135" than to "13" in
#' pure character-overlap terms). Inputs that are not a range (a plain
#' number, a number with a letter suffix, or a number with a "/"-style
#' affix) are returned unchanged.
#'
#' @param x \code{[character]} Raw house number input.
#'
#' @returns \code{[character]} \code{x}, with any range collapsed to its
#' first number.
#'
#' @noRd
collapse_house_number_range <- function(x) {
  gsub(
    "^([0-9]+[a-zA-Z]?)\\s*[-\u2013]\\s*[0-9]+[a-zA-Z]?$",
    "\\1",
    x
  )
}

# User-extensible input-fix registry ----
# The built-in fixes above cover known cases; this registry lets users
# layer their own cleaning functions on top, per component, without
# forking or monkey-patching the matching internals.

.bkg_default_input_fixes <- function() {
  list(
    street = list(
      expand_street_abbreviation = expand_street_abbreviation,
      expand_street_abbreviation_broad = expand_street_abbreviation_broad
    ),
    house_number = list(
      clean_house_number_input = clean_house_number_input,
      collapse_house_number_range = collapse_house_number_range
    ),
    place = list(
      strip_place_suffix = strip_place_suffix
    ),
    zip_code = list()  # no built-in defaults yet; see bkg_add_input_fix()
  )
}

.bkg_input_fixes <- list2env(
  .bkg_default_input_fixes(),
  envir = new.env(parent = emptyenv())
)

#' Reset the input-fix registry
#'
#' @description Resets \code{\link{bkg_add_input_fix}}'s registry back to
#' just the package's built-in fixes, discarding anything added in the
#' current R session. Useful when switching between datasets that need
#' different custom fixes, so a fix added for one dataset doesn't
#' accidentally keep applying to the next one processed in the same
#' session.
#'
#' @param component \code{[character]}
#'
#' Which component(s) to reset. One or several of \code{"street"},
#' \code{"house_number"}, \code{"zip_code"}, \code{"place"}. Defaults to
#' \code{NULL}, which resets all four.
#'
#' @returns \code{NULL}, invisibly.
#'
#' @examples
#' \dontrun{
#' bkg_add_input_fix(street = function(x) gsub("Str\\.?$", "stra\u00dfe", x))
#' # ... process dataset A ...
#'
#' bkg_reset_input_fixes()  # back to just the built-in fixes
#' bkg_add_input_fix(place = function(x) gsub("^Landkreis ", "", x))
#' # ... process dataset B, unaffected by dataset A's street fix ...
#'
#' bkg_reset_input_fixes("place")  # reset only one component
#' }
#'
#' @encoding UTF-8
#' @export
bkg_reset_input_fixes <- function(component = NULL) {
  valid <- c("street", "house_number", "zip_code", "place")
  
  if (is.null(component)) {
    component <- valid
  } else {
    component <- match.arg(component, valid, several.ok = TRUE)
  }
  
  defaults <- .bkg_default_input_fixes()
  for (comp in component) {
    .bkg_input_fixes[[comp]] <- defaults[[comp]]
  }
  
  invisible(NULL)
}

#' Convert a fix specification into a plain function
#'
#' @description Lets \code{\link{bkg_add_input_fix}} accept the more
#' compact one-sided formula syntax familiar from packages like purrr or
#' dplyr (e.g. \code{~ trimws(.x)}) as an alternative to spelling out
#' \code{function(x) trimws(x)} in full -- most useful when registering
#' several fixes for the same component at once, where the boilerplate of
#' repeating \code{function(x) ...} for each one adds up. Both \code{.x}
#' and \code{x} are available inside the formula as the placeholder for
#' the raw input; plain functions pass through unchanged.
#'
#' @param fix \code{[function/formula]}
#'
#' @returns \code{[function]}
#'
#' @noRd
as_fix_function <- function(fix) {
  if (is.function(fix)) {
    return(fix)
  }
  
  if (inherits(fix, "formula")) {
    if (length(fix) != 2) {
      cli::cli_abort(paste(
        "Formula fixes must be one-sided, e.g. {.code ~ trimws(.x)}",
        "-- not {.code lhs ~ rhs}."
      ))
    }
    expr <- fix[[2]]
    env <- environment(fix)
    return(function(.x) {
      eval(expr, envir = list(.x = .x, x = .x), enclos = env)
    })
  }
  
  cli::cli_abort(paste(
    "Each fix must be a function or a one-sided formula",
    "(e.g. {.code ~ trimws(.x)})."
  ))
}

#' Remove one or several patterns from an address component
#'
#' @description Builds a fix (for use with \code{\link{bkg_add_input_fix}})
#' that removes every occurrence of one or more regex patterns. Multiple
#' patterns are removed in the order given, all within a single fix --
#' equivalent to chaining several \code{gsub(pattern, "", x)} calls, but
#' without writing out a custom function.
#'
#' @param pattern \code{[character]} One or more regex patterns (or plain
#' strings, if \code{fixed = TRUE}) to remove.
#' @param ignore.case,fixed \code{[logical]} Passed through to
#' \code{\link[base]{gsub}}.
#'
#' @returns \code{[function]} A fix usable with
#' \code{\link{bkg_add_input_fix}}.
#'
#' @examples
#' \dontrun{
#' bkg_add_input_fix(place = bkg_fix_remove(c("^Landkreis ", "^Kreis ")))
#' }
#'
#' @export
bkg_fix_remove <- function(pattern, ignore.case = FALSE, fixed = FALSE) {
  function(x) {
    for (p in pattern) {
      x <- gsub(p, "", x, ignore.case = ignore.case, fixed = fixed)
    }
    x
  }
}

#' Replace one or several patterns in an address component
#'
#' @description Builds a fix (for use with \code{\link{bkg_add_input_fix}})
#' that replaces one or more regex patterns with corresponding
#' replacements, applied in order within a single fix. Patterns and
#' replacements can be given either as two separate vectors, or as a
#' single named vector (\code{c(pattern = replacement, ...)}) -- whichever
#' reads more naturally for the case at hand.
#'
#' @param pattern \code{[character]} One or more regex patterns, or a
#' named character vector where the names are the patterns and the values
#' are the replacements (in which case \code{replacement} is not needed).
#' @param replacement \code{[character/NULL]} One replacement per pattern,
#' or a single replacement recycled for every pattern. Ignored (and can be
#' omitted) if \code{pattern} is a named vector.
#' @param ignore.case,fixed \code{[logical]} Passed through to
#' \code{\link[base]{gsub}}.
#'
#' @returns \code{[function]} A fix usable with
#' \code{\link{bkg_add_input_fix}}.
#'
#' @examples
#' \dontrun{
#' # Two parallel vectors
#' bkg_add_input_fix(street = bkg_fix_replace(c("Str\\.$", "Weg\\.$"), c("stra\u00dfe", "weg")))
#'
#' # Same thing, as a single named vector
#' bkg_add_input_fix(street = bkg_fix_replace(c(
#'   "Str\\.$" = "stra\u00dfe",
#'   "Weg\\.$" = "weg"
#' )))
#'
#' # One replacement recycled across several patterns
#' bkg_add_input_fix(place = bkg_fix_replace(c("^Landkreis ", "^Kreis "), ""))
#' }
#'
#' @export
bkg_fix_replace <- function(pattern, replacement = NULL,
                            ignore.case = FALSE, fixed = FALSE) {
  if (!is.null(names(pattern)) && is.null(replacement)) {
    replacement <- unname(pattern)
    pattern <- names(pattern)
  }
  
  if (is.null(replacement)) {
    cli::cli_abort(paste(
      "{.arg replacement} must be supplied unless {.arg pattern} is a",
      "named vector."
    ))
  }
  
  if (length(replacement) == 1) {
    replacement <- rep(replacement, length(pattern))
  }
  
  if (length(pattern) != length(replacement)) {
    cli::cli_abort(paste(
      "{.arg pattern} and {.arg replacement} must have the same length",
      "(or {.arg replacement} must have length 1)."
    ))
  }
  
  function(x) {
    for (i in seq_along(pattern)) {
      x <- gsub(pattern[i], replacement[i], x,
                ignore.case = ignore.case, fixed = fixed)
    }
    x
  }
}

#' Trim whitespace from an address component
#'
#' @description Builds a fix (for use with \code{\link{bkg_add_input_fix}})
#' that trims leading/trailing whitespace. A thin wrapper around
#' \code{\link[base]{trimws}}, provided mainly so \code{trim} sits
#' alongside \code{\link{bkg_fix_remove}}/\code{\link{bkg_fix_replace}} as
#' a common, named building block instead of a one-off custom function.
#'
#' @param which \code{[character]} Passed through to
#' \code{\link[base]{trimws}}: \code{"both"}, \code{"left"}, or
#' \code{"right"}.
#'
#' @returns \code{[function]} A fix usable with
#' \code{\link{bkg_add_input_fix}}.
#'
#' @examples
#' \dontrun{
#' bkg_add_input_fix(zip_code = bkg_fix_trim())
#' }
#'
#' @export
bkg_fix_trim <- function(which = "both") {
  function(x) trimws(x, which = which)
}

#' Add a custom input-cleaning step
#'
#' @description Registers one or more functions that clean a raw address
#' component before matching, in addition to (after) the package's
#' built-in fixes. Each fix must take a character vector and return a
#' character vector of the same length. Registered fixes apply to every
#' subsequent call of \code{\link{bkg_geocode_offline}} in the current R
#' session.
#'
#' @param street,house_number,zip_code,place \code{[function/formula/list]}
#'
#' Optional cleaning fix(es) for the respective address component. Each
#' fix can be written as a plain function (\code{function(x) ...}), a
#' compact one-sided formula (\code{~ ...}, with \code{.x} or \code{x}
#' standing in for the input), or built from the common patterns
#' \code{\link{bkg_fix_remove}}, \code{\link{bkg_fix_replace}}, and
#' \code{\link{bkg_fix_trim}} -- all forms can be mixed freely. Pass a
#' single fix, or a list of several to register them all at once; they
#' run in the order given, after any already-registered fixes for that
#' component (unless \code{reset = TRUE}, see below). Only the
#' components you actually want to add a fix for need to be supplied.
#' @param reset \code{[logical]}
#'
#' If \code{TRUE}, drops any custom fixes previously added (via this
#' function) for each component being set in this call -- back to just
#' the package's built-in fixes for that component -- before adding the
#' new fix(es). The built-in fixes themselves are never removed; this is
#' a shorthand for calling \code{\link{bkg_reset_input_fixes}} followed
#' by a normal \code{bkg_add_input_fix()} call. Only affects the
#' component(s) actually passed in this call; any others keep whatever
#' they had. Defaults to \code{FALSE} (append to what's already there).
#'
#' @returns \code{NULL}, invisibly. Called for its side effect of
#' registering the fix(es).
#'
#' @examples
#' \dontrun{
#' # Plain function
#' bkg_add_input_fix(street = function(x) gsub("Str\\.?$", "stra\u00dfe", x))
#'
#' # Same thing, as a formula
#' bkg_add_input_fix(street = ~ gsub("Str\\.?$", "stra\u00dfe", .x))
#'
#' # Same thing again, using the remove/replace helpers, plus removing
#' # several other patterns in one call
#' bkg_add_input_fix(
#'   place = list(
#'     bkg_fix_remove(c("^Landkreis ", "^Kreis ")),
#'     bkg_fix_trim()
#'   )
#' )
#'
#' # Any of function, formula, or helper can be mixed in the same list
#' bkg_add_input_fix(
#'   place = list(
#'     bkg_fix_remove("^Landkreis "),
#'     ~ gsub("\\s+", " ", .x),
#'     function(x) trimws(x)
#'   )
#' )
#'
#' # Fixes for two different components at once
#' bkg_add_input_fix(
#'   place = bkg_fix_remove("^Landkreis "),
#'   zip_code = bkg_fix_trim()
#' )
#'
#' # Drop any custom fixes previously added for "street" (built-ins stay),
#' # then add just this one fix on top of the defaults
#' bkg_add_input_fix(street = ~ trimws(.x), reset = TRUE)
#' }
#'
#' @encoding UTF-8
#' @export
bkg_add_input_fix <- function(street = NULL, house_number = NULL,
                              zip_code = NULL, place = NULL,
                              reset = FALSE) {
  check_lgl(reset)
  
  fixes <- list(
    street = street, house_number = house_number,
    zip_code = zip_code, place = place
  )
  fixes <- fixes[!vapply(fixes, is.null, logical(1))]
  
  # Normalize each argument to a list, whether the user passed a single
  # bare function/formula or already a list of several.
  fixes <- lapply(fixes, function(fix) {
    if (is.function(fix) || inherits(fix, "formula")) list(fix) else fix
  })
  
  for (component in names(fixes)) {
    component_fixes <- fixes[[component]]
    
    if (!is.list(component_fixes)) {
      cli::cli_abort(paste(
        "{.arg {component}} must be a function, a formula, or a list of",
        "functions/formulas."
      ))
    }
    
    if (isTRUE(reset)) {
      .bkg_input_fixes[[component]] <- .bkg_default_input_fixes()[[component]]
    }
    
    for (fix in component_fixes) {
      idx <- length(.bkg_input_fixes[[component]]) + 1
      .bkg_input_fixes[[component]][[idx]] <- as_fix_function(fix)
    }
  }
  
  invisible(NULL)
}

#' Apply all registered fixes for one address component, in order
#'
#' @param x \code{[character]} Raw values for the given component.
#' @param component \code{[character]} One of \code{"street"},
#' \code{"house_number"}, \code{"zip_code"}, \code{"place"}.
#'
#' @returns \code{[character]}
#'
#' @noRd
apply_input_fixes <- function(x, component) {
  for (fix in .bkg_input_fixes[[component]]) {
    x <- fix(x)
  }
  x
}

