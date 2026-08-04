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

# ------------------------------------------------------------------------
# Shared fixture: a small, synthetic, already-scored result set for
# bkg_classify()/bkg_classify_interactive()/bkg_quality_summary()/
# bkg_show_address_detail(). Unlike make_fake_results() above, these
# functions only ever look at the score columns (and, for exclude_ids/
# id_col, an identifier column) -- no sf geometry or GeocodingResults
# class required, so a plain tibble is enough.
# ------------------------------------------------------------------------

make_scored_results <- function(
    id = NULL,
    score = c(0.95, 0.8, 0.6, NA),
    place_score = c(0.95, 0.75, 0.6, NA),
    street_score = c(0.98, 0.9, 0.5, NA),
    house_number_score = c(0.97, 0.5, 0.5, NA),
    address_input = NULL,
    address_cleaned = NULL,
    address_output = NULL
) {
  n <- length(score)
  if (is.null(id)) id <- seq_len(n)
  if (is.null(address_input)) address_input <- paste("input", seq_len(n))
  if (is.null(address_cleaned)) address_cleaned <- paste("cleaned", seq_len(n))
  if (is.null(address_output)) address_output <- paste("output", seq_len(n))
  
  tibble::tibble(
    ID = id,
    score = score,
    place_score = place_score,
    street_score = street_score,
    house_number_score = house_number_score,
    address_input = address_input,
    address_cleaned = address_cleaned,
    address_output = address_output
  )
}

# ------------------------------------------------------------------------
# bkg_classify()
# ------------------------------------------------------------------------

test_that("bkg_classify() labels a clearly good match as perfect", {
  x <- make_scored_results(
    score = 0.95, place_score = 0.95, street_score = 0.97, house_number_score = 0.96
  )
  expect_equal(bkg_classify(x)$quality, "perfect")
})

test_that("bkg_classify() labels a decent-but-imperfect match as semi_perfect", {
  x <- make_scored_results(
    score = 0.8, place_score = 0.75, street_score = 0.8, house_number_score = 0.92
  )
  expect_equal(bkg_classify(x)$quality, "semi_perfect")
})

test_that("bkg_classify() labels a good street with a bad house number as wrong_house_number", {
  x <- make_scored_results(
    score = 0.8, place_score = 0.75, street_score = 0.9, house_number_score = 0.5
  )
  expect_equal(bkg_classify(x)$quality, "wrong_house_number")
})

test_that("bkg_classify() labels a decent place with a weak street as wrong_street", {
  x <- make_scored_results(
    score = 0.6, place_score = 0.75, street_score = 0.3, house_number_score = 0.3
  )
  expect_equal(bkg_classify(x)$quality, "wrong_street")
})

test_that("bkg_classify() labels a weak place score as wrong_place by default", {
  # By construction (see bkg_classify()'s Details), anything that fails
  # to clear wrong_street's bar (place_score >= semi_place, its only
  # condition) must have place_score below that bar -- this is
  # specifically a place-match problem, hence the default label.
  x <- make_scored_results(
    score = 0.3, place_score = 0.4, street_score = 0.9, house_number_score = 0.9
  )
  expect_equal(bkg_classify(x)$quality, "wrong_place")
})

test_that("bkg_classify() labels unmatched rows (NA score) as unmatched", {
  x <- make_scored_results(
    score = NA, place_score = NA, street_score = NA, house_number_score = NA
  )
  expect_equal(bkg_classify(x)$quality, "unmatched")
})

test_that("bkg_classify() inserts quality directly before score, not at the end", {
  x <- make_scored_results()
  result <- bkg_classify(x)
  idx <- match(c("quality", "score"), names(result))
  expect_equal(idx[1], idx[2] - 1)
})

test_that("bkg_classify() respects custom thresholds", {
  x <- make_scored_results(
    score = 0.8, place_score = 0.85, street_score = 0.9, house_number_score = 0.9
  )
  # With defaults, house_number_score 0.9 doesn't clear perfect_house_number (0.95)
  expect_equal(bkg_classify(x)$quality, "semi_perfect")
  # Loosen the bar and the same row becomes "perfect"
  result <- bkg_classify(x, thresholds = list(
    perfect_place = 0.8, perfect_street = 0.85, perfect_house_number = 0.85
  ))
  expect_equal(result$quality, "perfect")
})

test_that("bkg_classify() errors on an unknown threshold name", {
  x <- make_scored_results()
  expect_error(
    bkg_classify(x, thresholds = list(not_a_real_threshold = 1)),
    "Unknown threshold"
  )
})

test_that("bkg_classify() errors on an unknown label name", {
  x <- make_scored_results()
  expect_error(
    bkg_classify(x, labels = list(not_a_real_label = "x")),
    "Unknown label"
  )
})

test_that("bkg_classify() errors when required score columns are missing", {
  expect_error(
    bkg_classify(data.frame(x = 1)),
    "missing column"
  )
})

test_that("bkg_classify()'s default label for the catch-all bucket is wrong_place, not rest", {
  x <- make_scored_results(
    score = 0.3, place_score = 0.3, street_score = 0.9, house_number_score = 0.9
  )
  expect_equal(bkg_classify(x)$quality, "wrong_place")
  result <- bkg_classify(x, labels = list(rest = "rest"))
  expect_equal(result$quality, "rest")
})

test_that("bkg_classify() requires id_col when exclude_ids is used", {
  x <- make_scored_results()
  expect_error(
    bkg_classify(x, exclude_ids = list(perfect = "1")),
    "id_col"
  )
})

test_that("bkg_classify() excludes specific IDs from a rule, letting them fall through", {
  x <- make_scored_results(
    id = c(1, 2),
    score = c(0.95, 0.95),
    place_score = c(0.95, 0.95),
    street_score = c(0.97, 0.97),
    house_number_score = c(0.96, 0.96)
  )
  # Without exclusion, both rows clear "perfect"
  expect_equal(bkg_classify(x, id_col = "ID")$quality, c("perfect", "perfect"))
  
  # Excluding ID 1 from "perfect" doesn't force it all the way down --
  # it still clears "semi_perfect"'s (looser) bar and lands there instead
  result <- bkg_classify(x, exclude_ids = list(perfect = "1"), id_col = "ID")
  expect_equal(result$quality, c("semi_perfect", "perfect"))
})

test_that("bkg_classify() errors on an unknown rule name in exclude_ids", {
  x <- make_scored_results()
  expect_error(
    bkg_classify(x, exclude_ids = list(not_a_rule = "1"), id_col = "ID"),
    "Unknown rule"
  )
})

test_that("bkg_classify() errors when id_col isn't an actual column", {
  x <- make_scored_results()
  expect_error(
    bkg_classify(x, exclude_ids = list(perfect = "1"), id_col = "not_a_column"),
    "not found"
  )
})

# ------------------------------------------------------------------------
# bkg_classify_interactive()
# ------------------------------------------------------------------------
# The interactive session itself (readline() prompts) isn't exercised
# here -- it shares bkg_classify()'s exact rule-masking logic (see
# .bkg_quality_rule_mask() in bkg_matching.R), which the tests above
# already cover thoroughly. This only checks the one thing that's
# genuinely specific to this function: the interactivity guard.

test_that("bkg_classify_interactive() requires an interactive session", {
  # interactive() reflects the CALLING R session, not the test runner --
  # devtools::test_active_file()/test() run inline in the developer's own
  # (genuinely interactive) console, so interactive() would actually
  # return TRUE there and this test would otherwise trigger the real
  # readline() prompt loop, hanging the whole test run. Mocking the
  # package's own .bkg_is_interactive() wrapper (rather than
  # base::interactive() directly) sidesteps any reliability concerns
  # about mocking a binding in base's locked namespace.
  local_mocked_bindings(.bkg_is_interactive = function() FALSE)
  
  x <- make_scored_results()
  expect_error(
    bkg_classify_interactive(x),
    "interactive R"
  )
})

# ------------------------------------------------------------------------
# bkg_quality_summary()
# ------------------------------------------------------------------------

test_that("bkg_quality_summary() classifies first when there's no quality column yet", {
  x <- make_scored_results()
  result <- bkg_quality_summary(x)
  expect_true(all(c("quality", "n", "pct") %in% names(result)))
  expect_equal(sum(result$n), nrow(x))
})

test_that("bkg_quality_summary() percentages sum to 100", {
  x <- make_scored_results(
    id = 1:4,
    score = c(0.95, 0.8, 0.3, NA),
    place_score = c(0.95, 0.75, 0.3, NA),
    street_score = c(0.97, 0.8, 0.9, NA),
    house_number_score = c(0.96, 0.92, 0.9, NA)
  )
  result <- bkg_quality_summary(x)
  expect_equal(sum(result$pct), 100)
})

test_that("bkg_quality_summary() leaves an already-classified quality column alone", {
  x <- make_scored_results()
  x$quality <- "custom_label"
  result <- bkg_quality_summary(x)
  expect_equal(result$quality, "custom_label")
  expect_equal(result$n, nrow(x))
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