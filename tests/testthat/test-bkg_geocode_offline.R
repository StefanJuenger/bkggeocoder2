# Builds the fixture database once per calling test. Explicitly passes
# envir = parent.frame() so the temp dir's withr::local_tempdir() cleanup is
# scheduled against the CALLING test_that() block, not against this helper's
# own (much shorter-lived) call frame.
build_fixture_db <- function(envir = parent.frame()) {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir(.local_envir = envir)
  bkg_build_database_impl(fixture_dir, db_dir)
  db_dir
}

valid_address <- data.frame(
  street       = "Teststra\u00dfe",
  house_number = "1",
  zip_code     = "12345",
  place        = "Musterstadt"
)

# ------------------------------------------------------------------------
# Input validation (none of these need a real database)
# ------------------------------------------------------------------------

test_that("bkg_geocode_offline() requires .data to be a data.frame", {
  expect_error(
    bkg_geocode_offline(list(street = "A"), db_path = withr::local_tempdir()),
    regexp = "data.*must be a dataframe"
  )
})

test_that("bkg_geocode_offline() requires cols to have length 3 or 4", {
  expect_error(
    bkg_geocode_offline(
      data.frame(a = 1, b = 2),
      cols = 1:2,
      db_path = withr::local_tempdir()
    ),
    regexp = "cols.*must be of length 3 or 4"
  )
  expect_error(
    bkg_geocode_offline(
      data.frame(a = 1, b = 2, c = 3, d = 4, e = 5),
      cols = 1:5,
      db_path = withr::local_tempdir()
    ),
    regexp = "cols.*must be of length 3 or 4"
  )
})

test_that("bkg_geocode_offline() aborts with a helpful message when no local database exists", {
  expect_error(
    bkg_geocode_offline(valid_address, db_path = withr::local_tempdir()),
    regexp = "No local address database found"
  )
})

test_that("bkg_geocode_offline() aborts when only an empty directory exists at db_path", {
  # dir.exists() is TRUE but version.dcf is missing -- a different branch
  # of the same guard condition than the fully-missing-directory case above.
  db_dir <- withr::local_tempdir()
  expect_error(
    bkg_geocode_offline(valid_address, db_path = db_dir),
    regexp = "No local address database found"
  )
})

# ------------------------------------------------------------------------
# Input validation that requires a valid database to be reached at all
# ------------------------------------------------------------------------

test_that("bkg_geocode_offline() validates place_match_quality is between 0 and 1", {
  db_dir <- build_fixture_db()
  
  expect_error(
    bkg_geocode_offline(valid_address, db_path = db_dir, place_match_quality = 1.5),
    regexp = "place_match_quality"
  )
  expect_error(
    bkg_geocode_offline(valid_address, db_path = db_dir, place_match_quality = -0.1),
    regexp = "place_match_quality"
  )
})

test_that("bkg_geocode_offline() validates hierarchical_weight is non-negative", {
  db_dir <- build_fixture_db()
  
  expect_error(
    bkg_geocode_offline(valid_address, db_path = db_dir, hierarchical_weight = -1),
    regexp = "hierarchical_weight"
  )
})

# ------------------------------------------------------------------------
# verbose = TRUE/FALSE
# ------------------------------------------------------------------------

#' Runs `expr` with both the output and message streams redirected into a
#' character vector, returning the combined text. Cleans up the sink/
#' connection via withr::defer() even if `expr` errors, so a failing
#' assertion here can't leave later tests writing into a stale sink.
capture_all_output <- function(expr, envir = parent.frame()) {
  captured <- character()
  con <- textConnection("captured", "w", local = TRUE)
  sink(con)
  sink(con, type = "message")
  withr::defer({
    sink(type = "message")
    sink()
    close(con)
  }, envir = envir)
  
  force(expr)
  paste(captured, collapse = "\n")
}

test_that("bkg_geocode_offline() prints progress messages when verbose = TRUE", {
  db_dir <- build_fixture_db()
  
  text <- capture_all_output(
    bkg_geocode_offline(valid_address, db_path = db_dir, verbose = TRUE)
  )
  
  expect_match(text, "Starting offline geocoding")
  expect_match(text, "Number of distinct addresses")
  expect_match(text, "Targeted quality of place-matching")
  expect_match(text, "Place weight exponent")
  expect_match(text, "Subsetting data")
})

test_that("bkg_geocode_offline() stays silent when verbose = FALSE", {
  db_dir <- build_fixture_db()
  
  text <- capture_all_output(
    bkg_geocode_offline(valid_address, db_path = db_dir, verbose = FALSE)
  )
  
  expect_equal(text, "")
})