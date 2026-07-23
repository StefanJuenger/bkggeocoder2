# ------------------------------------------------------------------------
# Shared fixture: a small, synthetic GeocodingResults object. These tests
# don't need a real local database -- the S3 methods and the exporter only
# care about the object's columns, attributes, and class, not about how it
# was produced.
# ------------------------------------------------------------------------

make_fake_results <- function(
    scores = c(0.95, 0.72, NA),
    address_input = NULL,
    address_output = NULL,
    unmatched_places = NULL
) {
  n <- length(scores)
  
  if (is.null(address_input)) {
    address_input <- paste("Straße", seq_len(n), "12345 Musterstadt")
  }
  if (is.null(address_output)) {
    address_output <- ifelse(
      is.na(scores), NA, paste("Straße", seq_len(n), "12345 Musterstadt")
    )
  }
  
  df <- data.frame(
    score = scores,
    address_input = address_input,
    address_output = address_output,
    stringsAsFactors = FALSE
  )
  
  geometry <- sf::st_sfc(
    lapply(scores, function(s) {
      if (is.na(s)) sf::st_point() else sf::st_point(c(1, 1))
    }),
    crs = 3035
  )
  
  sf_df <- sf::st_sf(df, geometry = geometry)
  
  structure(
    sf_df,
    unmatched_places = unmatched_places,
    type = "offline",
    class = c("GeocodingResults", class(sf_df))
  )
}

# ------------------------------------------------------------------------
# print.GeocodingResults()
# ------------------------------------------------------------------------

test_that("print.GeocodingResults() reports totals and unmatched count", {
  x <- make_fake_results()
  
  expect_output(print(x), "GeocodingResults")
  expect_output(print(x), "Addresses:\\s+3")
  expect_output(print(x), "Geocoded:\\s+2\\s*/\\s*3")
  expect_output(print(x), "Mean score:")
  expect_output(print(x), "Unmatched:\\s+1")
})

test_that("print.GeocodingResults() omits the unmatched line when everything matched", {
  x <- make_fake_results(scores = c(0.9, 0.8))
  out <- capture.output(print(x))
  expect_false(any(grepl("Unmatched:", out)))
})

test_that("print.GeocodingResults() omits the mean score line when nothing matched", {
  x <- make_fake_results(scores = c(NA, NA))
  out <- capture.output(print(x))
  expect_false(any(grepl("Mean score:", out)))
})

test_that("print.GeocodingResults() returns its input invisibly", {
  x <- make_fake_results()
  invisible(capture.output(result <- print(x)))
  expect_identical(result, x)
})

test_that("print.GeocodingResults() truncates and notes additional rows", {
  x <- make_fake_results(
    scores = rep(0.9, 12),
    address_input = paste("Straße", 1:12),
    address_output = paste("Straße", 1:12)
  )
  expect_output(print(x, n = 5), "and 7 more rows")
})

# ------------------------------------------------------------------------
# summary.GeocodingResults()
# ------------------------------------------------------------------------

test_that("summary.GeocodingResults() reports score statistics", {
  x <- make_fake_results(scores = c(0.9, 0.8, 0.7))
  out <- capture.output(summary(x))
  out_text <- paste(out, collapse = "\n")
  
  expect_match(out_text, "Addresses in input data:\\s+3")
  expect_match(out_text, "Addresses geocoded:\\s+3")
  expect_match(out_text, "Mean score:\\s+0.8")
})

test_that("summary.GeocodingResults() lists unmatched places when present", {
  unmatched <- data.frame(zip_code = "99999", place = "Nirgendwostadt")
  x <- make_fake_results(unmatched_places = unmatched)
  
  expect_output(summary(x), "Unmatched places:")
  expect_output(summary(x), "Nirgendwostadt")
})

test_that("summary.GeocodingResults() skips the unmatched section when there is none", {
  x <- make_fake_results(scores = c(0.9, 0.8), unmatched_places = NULL)
  out <- capture.output(summary(x))
  expect_false(any(grepl("Unmatched places:", out)))
})

# ------------------------------------------------------------------------
# plot.GeocodingResults()
# ------------------------------------------------------------------------

test_that("plot.GeocodingResults() draws a histogram when scores are present", {
  x <- make_fake_results(scores = c(0.9, 0.8, 0.7))
  
  path <- withr::local_tempfile(fileext = ".png")
  grDevices::png(path)
  withr::defer(grDevices::dev.off())
  
  expect_no_error(plot(x))
})

test_that("plot.GeocodingResults() warns and does nothing when there are no scores", {
  x <- make_fake_results(scores = c(NA, NA))
  expect_warning(result <- plot(x), "No scores to plot")
  expect_null(result)
})

# ------------------------------------------------------------------------
# bkg_export_geocodes()
# ------------------------------------------------------------------------

test_that("bkg_export_geocodes() rejects objects that aren't GeocodingResults", {
  expect_error(
    bkg_export_geocodes(data.frame(x = 1), "out.csv")
  )
})

test_that("bkg_export_geocodes() writes a csv with coordinates and data", {
  # Fully-matched fake data on purpose: geometry_to_xy()'s
  # sf::st_coordinates() call is untested here for a batch that also
  # contains unmatched (empty-geometry) rows -- see the note in the PR/
  # conversation about this being a currently-unverified edge case.
  x <- make_fake_results(
    scores = c(0.9, 0.8, 0.7),
    address_input = c("A", "B", "C"),
    address_output = c("A-out", "B-out", "C-out")
  )
  path <- withr::local_tempfile(fileext = ".csv")
  
  bkg_export_geocodes(x, path)
  
  expect_true(file.exists(path))
  # bkg_export_geocodes() calls readr::write_delim() with its default
  # delim (a single space), regardless of the .csv extension -- match
  # that explicitly here rather than relying on read_delim()'s guessing.
  written <- readr::read_delim(path, delim = " ", show_col_types = FALSE)
  expect_true(all(c("X", "Y", "score", "address_input") %in% names(written)))
  expect_equal(nrow(written), 3)
})

test_that("bkg_export_geocodes() writes an xlsx with a separate unmatched sheet", {
  unmatched <- data.frame(zip_code = "99999", place = "Nirgendwostadt")
  x <- make_fake_results(
    scores = c(0.9, 0.8, 0.7),
    address_input = c("A", "B", "C"),
    address_output = c("A-out", "B-out", "C-out"),
    unmatched_places = unmatched
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  
  bkg_export_geocodes(x, path)
  
  expect_true(file.exists(path))
  sheets <- openxlsx::getSheetNames(path)
  expect_true(all(c("geocoded", "unmatched_places") %in% sheets))
})

test_that("bkg_export_geocodes() omits the unmatched sheet when there is none", {
  x <- make_fake_results(
    scores = c(0.9, 0.8, 0.7),
    address_input = c("A", "B", "C"),
    address_output = c("A-out", "B-out", "C-out"),
    unmatched_places = NULL
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  
  bkg_export_geocodes(x, path)
  
  sheets <- openxlsx::getSheetNames(path)
  expect_false("unmatched_places" %in% sheets)
})

test_that("bkg_export_geocodes() falls back to sf::st_write() for other extensions", {
  x <- make_fake_results(
    scores = c(0.9, 0.8, 0.7),
    address_input = c("A", "B", "C"),
    address_output = c("A-out", "B-out", "C-out")
  )
  path <- withr::local_tempfile(fileext = ".geojson")
  
  bkg_export_geocodes(x, path)
  
  expect_true(file.exists(path))
  written <- sf::st_read(path, quiet = TRUE)
  expect_equal(nrow(written), 3)
})

test_that("bkg_export_geocodes() does not overwrite an existing file when overwrite = FALSE", {
  x <- make_fake_results(scores = c(0.9, 0.8, 0.7))
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("sentinel content", path)
  
  bkg_export_geocodes(x, path, overwrite = FALSE)
  
  expect_equal(readLines(path), "sentinel content")
})

test_that("bkg_export_geocodes() returns the file path invisibly", {
  x <- make_fake_results(scores = c(0.9, 0.8, 0.7))
  path <- withr::local_tempfile(fileext = ".csv")
  
  invisible(capture.output(result <- bkg_export_geocodes(x, path)))
  expect_equal(result, path)
})
