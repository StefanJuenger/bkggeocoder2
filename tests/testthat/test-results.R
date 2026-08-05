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


# ------------------------------------------------------------------------
# bkg_show_address_detail()
# ------------------------------------------------------------------------

test_that("bkg_show_address_detail() prints original, cleaned, and output per row", {
  x <- make_scored_results(
    id = 1,
    score = 0.9,
    place_score = 0.9,
    street_score = 0.9,
    house_number_score = 0.9,
    address_input = "Musterstr. 45b",
    address_cleaned = "Musterstraße 45b",
    address_output = "Musterstraße 45"
  )
  out <- capture.output(bkg_show_address_detail(x, id_col = "ID"))
  out_text <- paste(out, collapse = "\n")
  
  expect_match(out_text, "ID: 1")
  expect_match(out_text, "Musterstr\\. 45b")
  expect_match(out_text, "Musterstra\u00dfe 45b")
  expect_match(out_text, "Musterstra\u00dfe 45\\b")
})

test_that("bkg_show_address_detail() falls back to row numbers without id_col", {
  x <- make_scored_results(id = 99, score = 0.9, place_score = 0.9, street_score = 0.9, house_number_score = 0.9)
  expect_output(bkg_show_address_detail(x), "Row 1")
})

test_that("bkg_show_address_detail() errors when required columns are missing", {
  expect_error(
    bkg_show_address_detail(data.frame(x = 1)),
    "missing column"
  )
})

test_that("bkg_show_address_detail() handles zero rows without erroring", {
  x <- make_scored_results()[0, ]
  expect_message(bkg_show_address_detail(x), "No rows to show")
})

test_that("bkg_show_address_detail() returns its input invisibly, unchanged", {
  x <- make_scored_results(id = 1, score = 0.9, place_score = 0.9, street_score = 0.9, house_number_score = 0.9)
  invisible(capture.output(result <- bkg_show_address_detail(x, id_col = "ID")))
  expect_identical(result, x)
})
