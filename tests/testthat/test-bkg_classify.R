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
