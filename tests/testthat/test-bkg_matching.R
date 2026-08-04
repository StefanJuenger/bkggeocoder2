test_that("collapse_house_number_range() collapses a range to its first number", {
  expect_equal(collapse_house_number_range("13-15"), "13")
  expect_equal(collapse_house_number_range("13 - 15"), "13")
  expect_equal(collapse_house_number_range("13a-15a"), "13a")
  expect_equal(collapse_house_number_range("13\u201315"), "13")
})

test_that("collapse_house_number_range() leaves non-ranges unchanged", {
  expect_equal(collapse_house_number_range("13"), "13")
  expect_equal(collapse_house_number_range("13a"), "13a")
  expect_equal(collapse_house_number_range("10/2"), "10/2")
  expect_equal(collapse_house_number_range(NA_character_), NA_character_)
})

test_that("collapse_house_number_range() is vectorized", {
  input <- c("13-15", "10/2", "8", NA)
  expect_equal(
    collapse_house_number_range(input),
    c("13", "10/2", "8", NA)
  )
})

test_that("clean_house_number_input() strips whitespace and periods without touching affixes", {
  expect_equal(clean_house_number_input("10 a"), "10a")
  expect_equal(clean_house_number_input("10."), "10")
  expect_equal(clean_house_number_input("10 . a"), "10a")
  expect_equal(clean_house_number_input("10/2"), "10/2")
})

test_that("strip_place_suffix() removes Ortsteil (OT) suffixes", {
  expect_equal(strip_place_suffix("Musterstadt OT Nebendorf"), "Musterstadt")
  expect_equal(strip_place_suffix("Musterstadt (OT Nebendorf)"), "Musterstadt")
  expect_equal(strip_place_suffix("Musterstadt"), "Musterstadt")
})

test_that("expand_street_abbreviation() expands 'str'/'str.' at end of string and before whitespace", {
  expect_equal(expand_street_abbreviation("Hauptstr."), "Hauptstraße")
  expect_equal(expand_street_abbreviation("Hauptstr"), "Hauptstraße")
  expect_equal(expand_street_abbreviation("Hauptstr. 5"), "Hauptstraße 5")
  expect_equal(expand_street_abbreviation("Hauptstr 5"), "Hauptstraße 5")
})

test_that("expand_street_abbreviation() leaves already-correct spellings alone", {
  expect_equal(expand_street_abbreviation("Musterstraße"), "Musterstraße")
  expect_equal(expand_street_abbreviation("Musterstrasse"), "Musterstrasse")
})

test_that("expand_street_abbreviation_broad() catches abbreviations the narrow lookahead misses", {
  # "tr\\." matches anywhere, not just before whitespace/end-of-string --
  # this is specifically for street+house-number concatenated in one cell
  # (the length(cols) == 3 case), which expand_street_abbreviation() alone
  # can't handle since nothing follows the abbreviation but a digit.
  expect_equal(expand_street_abbreviation_broad("Musterstr.5"), "Musterstraße5")
  expect_equal(expand_street_abbreviation_broad("Musterstr"), "Musterstraße")
})

test_that("expand_street_abbreviation_broad() trims stray whitespace", {
  expect_equal(expand_street_abbreviation_broad("  Musterstraße  "), "Musterstraße")
})

test_that("expand_street_abbreviation_broad() is a no-op once already fully expanded", {
  # Registered after expand_street_abbreviation() in the default fix
  # chain -- by the time this runs, the common cases are already done,
  # so this must not re-mangle an already-correct spelling.
  expect_equal(expand_street_abbreviation_broad("Musterstraße"), "Musterstraße")
})

# ------------------------------------------------------------------------
# Input-fix registry: bkg_add_input_fix(), apply_input_fixes(),
# bkg_reset_input_fixes(), as_fix_function(), and the
# bkg_fix_remove()/bkg_fix_replace()/bkg_fix_trim() verb helpers.
#
# .bkg_input_fixes is package-level mutable state shared across the whole
# test session -- every test that registers a fix restores it afterwards
# via withr::defer(), so leftover fixes can't leak into later tests.
# ------------------------------------------------------------------------

test_that("bkg_add_input_fix() registers a plain function that actually runs", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("street")
  bkg_add_input_fix(street = function(x) toupper(x))
  # toupper() is used here purely as an easy-to-verify marker fix -- note
  # base R's toupper() does NOT expand "ß" to "SS" (unlike nk() elsewhere
  # in the package, which handles German transliteration explicitly), so
  # this deliberately uses a string without "ß" to avoid that unrelated
  # ambiguity.
  expect_equal(apply_input_fixes("musterplatz", "street"), "MUSTERPLATZ")
})

test_that("bkg_add_input_fix() accepts one-sided formulas, with .x or x as the input", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("place")
  bkg_add_input_fix(place = ~ toupper(.x))
  expect_equal(apply_input_fixes("musterstadt", "place"), "MUSTERSTADT")
  
  bkg_reset_input_fixes("place")
  bkg_add_input_fix(place = ~ toupper(x))
  expect_equal(apply_input_fixes("musterstadt", "place"), "MUSTERSTADT")
})

test_that("as_fix_function() rejects two-sided formulas", {
  expect_error(
    bkg_add_input_fix(street = y ~ x),
    "one-sided"
  )
})

test_that("bkg_add_input_fix() accepts a list of several fixes, applied in order", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("place")
  bkg_add_input_fix(place = list(
    ~ gsub("^Landkreis ", "", .x),
    ~ trimws(.x)
  ))
  expect_equal(apply_input_fixes("Landkreis Musterstadt  ", "place"), "Musterstadt")
})

test_that("bkg_add_input_fix() allows mixing plain functions, formulas, and verb helpers", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("place")
  bkg_add_input_fix(place = list(
    bkg_fix_remove("^Landkreis "),
    ~ toupper(.x),
    function(x) trimws(x)
  ))
  expect_equal(apply_input_fixes("Landkreis Musterstadt ", "place"), "MUSTERSTADT")
})

test_that("bkg_add_input_fix() errors for something that isn't a function/formula/list", {
  expect_error(
    bkg_add_input_fix(street = "not a function"),
    "function.*formula"
  )
})

test_that("bkg_reset_input_fixes() with no argument resets all four components to the built-ins", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_add_input_fix(
    street = function(x) x,
    place = function(x) x,
    zip_code = function(x) x,
    house_number = function(x) x
  )
  bkg_reset_input_fixes()
  
  defaults <- .bkg_default_input_fixes()
  expect_equal(names(.bkg_input_fixes$street), names(defaults$street))
  expect_equal(names(.bkg_input_fixes$place), names(defaults$place))
  expect_equal(names(.bkg_input_fixes$zip_code), names(defaults$zip_code))
  expect_equal(names(.bkg_input_fixes$house_number), names(defaults$house_number))
})

test_that("bkg_reset_input_fixes() can target a single component", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes()
  bkg_add_input_fix(street = function(x) x, place = function(x) x)
  bkg_reset_input_fixes("street")
  
  defaults <- .bkg_default_input_fixes()
  expect_equal(names(.bkg_input_fixes$street), names(defaults$street))
  # place was untouched by the targeted reset -- still has the extra fix
  expect_length(.bkg_input_fixes$place, length(defaults$place) + 1)
})

test_that("bkg_add_input_fix(reset = TRUE) drops prior custom fixes but keeps built-ins", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("street")
  n_builtin <- length(.bkg_input_fixes$street)
  
  bkg_add_input_fix(street = function(x) x)
  expect_length(.bkg_input_fixes$street, n_builtin + 1)
  
  bkg_add_input_fix(street = ~ trimws(.x), reset = TRUE)
  expect_length(.bkg_input_fixes$street, n_builtin + 1)
  # the built-in default (expand_street_abbreviation()) is still applied,
  # not wiped out by reset = TRUE
  expect_equal(apply_input_fixes("Musterstr", "street"), "Musterstraße")
})

test_that("bkg_add_input_fix(reset = TRUE) only affects the component(s) passed in this call", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes()
  bkg_add_input_fix(place = function(x) x)
  n_place_before <- length(.bkg_input_fixes$place)
  
  bkg_add_input_fix(street = ~ trimws(.x), reset = TRUE)
  expect_length(.bkg_input_fixes$place, n_place_before)
})

test_that("bkg_fix_remove() removes several patterns in one fix", {
  fix <- bkg_fix_remove(c("^Landkreis ", "^Kreis "))
  expect_equal(fix("Landkreis Musterstadt"), "Musterstadt")
  expect_equal(fix("Kreis Musterstadt"), "Musterstadt")
  expect_equal(fix("Musterstadt"), "Musterstadt")
})

test_that("bkg_fix_replace() works with two parallel vectors", {
  fix <- bkg_fix_replace(c("str\\.$", "weg\\.$"), c("straße", "weg"))
  expect_equal(fix("Musterstr."), "Musterstraße")
  expect_equal(fix("Musterweg."), "Musterweg")
})

test_that("bkg_fix_replace() works with a single named vector (pattern = replacement)", {
  fix <- bkg_fix_replace(c("str\\.$" = "straße", "weg\\.$" = "weg"))
  expect_equal(fix("Musterstr."), "Musterstraße")
})

test_that("bkg_fix_replace() recycles a single replacement across several patterns", {
  fix <- bkg_fix_replace(c("^Landkreis ", "^Kreis "), "")
  expect_equal(fix("Landkreis Musterstadt"), "Musterstadt")
  expect_equal(fix("Kreis Musterstadt"), "Musterstadt")
})

test_that("bkg_fix_replace() errors on mismatched, non-recyclable lengths", {
  expect_error(
    bkg_fix_replace(c("a", "b"), c("x", "y", "z")),
    "same length"
  )
})

test_that("bkg_fix_replace() errors if no replacement is given and pattern isn't named", {
  expect_error(
    bkg_fix_replace(c("a", "b")),
    "replacement"
  )
})

test_that("bkg_fix_trim() must be called -- it returns a fix, it isn't one", {
  fix <- bkg_fix_trim()
  expect_type(fix, "closure")
  expect_equal(fix("  Musterstadt  "), "Musterstadt")
})

test_that("apply_input_fixes() runs built-in and custom fixes in registration order", {
  withr::defer(bkg_reset_input_fixes())
  
  bkg_reset_input_fixes("place")
  bkg_add_input_fix(place = list(
    ~ paste0(.x, "1"),
    ~ paste0(.x, "2")
  ))
  expect_equal(apply_input_fixes("x", "place"), "x12")
})

local_duckdb_con <- function(envir = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = envir)
  con
}

sql_eval <- function(con, expr) {
  DBI::dbGetQuery(con, sprintf("SELECT %s AS x", expr))$x
}

test_that("nk() transliterates German umlauts and sharp s in SQL", {
  con <- local_duckdb_con()
  expect_equal(sql_eval(con, nk("'Müllerstraße'")), "muellerstrasse")
  expect_equal(sql_eval(con, nk("'GRÖSSE'")), "groesse")
})

test_that("nk_plain() additionally strips all non-alphanumeric characters", {
  con <- local_duckdb_con()
  expect_equal(sql_eval(con, nk_plain("'Bad Homburg'")), "badhomburg")
  expect_equal(sql_eval(con, nk_plain("'10/2'")), "102")
})

test_that("nk_street() reduces 'strasse'/'weg' at word boundaries", {
  con <- local_duckdb_con()
  expect_equal(sql_eval(con, nk_street("'Musterstraße'")), "musterx")
  expect_equal(sql_eval(con, nk_street("'Bahnhofsweg'")), "bahnhofsy")
  expect_equal(sql_eval(con, nk_street("'Hauptstraße 12'")), "hauptx12")
})