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