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