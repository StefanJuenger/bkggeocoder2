# ------------------------------------------------------------------------
# bkg_update_database(): workflow logic (staleness, force, backup)
# ------------------------------------------------------------------------
# These tests mock bkg_build_database_impl() so they run instantly and
# don't need any real address data -- they only exercise the surrounding
# decision logic (does a rebuild happen or not, is a backup made or not).

fake_build <- function(address_data_path, db_path, memory_limit = NULL, threads = NULL) {
  dir.create(db_path, recursive = TRUE, showWarnings = FALSE)
  write_version_metadata(
    list(
      version = format(Sys.Date(), "%Y-%m-%d"),
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      n_addresses = 1L,
      n_places = 1L,
      n_dropped_katasterintern = 0L
    ),
    file.path(db_path, "version.dcf")
  )
}

test_that("bkg_update_database() performs an initial build when no database exists", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  file.create(file.path(data_dir, "ga_tt.csv"))
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  
  result <- bkg_update_database(
    address_data_path = data_dir,
    db_path = db_dir,
    backup = FALSE
  )
  
  expect_true(result)
  expect_true(file.exists(file.path(db_dir, "version.dcf")))
})

test_that("bkg_update_database() skips rebuild when not stale", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  csv_path <- file.path(data_dir, "ga_tt.csv")
  file.create(csv_path)
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  # Make the existing database look newer than the source file
  Sys.setFileTime(csv_path, Sys.time() - 3600)
  
  build_called <- FALSE
  local_mocked_bindings(
    bkg_build_database_impl = function(...) build_called <<- TRUE
  )
  
  result <- bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  expect_false(result)
  expect_false(build_called)
})

test_that("bkg_update_database() rebuilds when the source is newer", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  csv_path <- file.path(data_dir, "ga_tt.csv")
  file.create(csv_path)
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  Sys.setFileTime(csv_path, Sys.time() + 5)
  
  build_called <- FALSE
  local_mocked_bindings(
    bkg_build_database_impl = function(...) {
      build_called <<- TRUE
      fake_build(...)
    }
  )
  
  result <- bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  expect_true(result)
  expect_true(build_called)
})

test_that("bkg_update_database() always rebuilds when force = TRUE", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  file.create(file.path(data_dir, "ga_tt.csv"))
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  build_called <- FALSE
  local_mocked_bindings(
    bkg_build_database_impl = function(...) {
      build_called <<- TRUE
      fake_build(...)
    }
  )
  
  result <- bkg_update_database(
    address_data_path = data_dir, db_path = db_dir, force = TRUE, backup = FALSE
  )
  
  expect_true(result)
  expect_true(build_called)
})

test_that("bkg_update_database() does not create a backup by default", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  csv_path <- file.path(data_dir, "ga_tt.csv")
  file.create(csv_path)
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir)
  
  Sys.setFileTime(csv_path, Sys.time() + 5)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir)
  
  backup_dir <- paste0(db_dir, "_backup_", format(Sys.Date(), "%Y%m%d"))
  expect_false(dir.exists(backup_dir))
})

test_that("bkg_update_database() creates a dated backup when backup = TRUE", {
  db_dir <- withr::local_tempdir()
  data_dir <- withr::local_tempdir()
  csv_path <- file.path(data_dir, "ga_tt.csv")
  file.create(csv_path)
  
  local_mocked_bindings(bkg_build_database_impl = fake_build)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = FALSE)
  
  Sys.setFileTime(csv_path, Sys.time() + 5)
  bkg_update_database(address_data_path = data_dir, db_path = db_dir, backup = TRUE)
  
  backup_dir <- paste0(db_dir, "_backup_", format(Sys.Date(), "%Y%m%d"))
  expect_true(dir.exists(backup_dir))
  expect_true(file.exists(file.path(backup_dir, "version.dcf")))
})

test_that("bkg_update_database() aborts if address_data_path does not exist", {
  expect_error(
    bkg_update_database(
      address_data_path = file.path(withr::local_tempdir(), "does-not-exist"),
      db_path = withr::local_tempdir()
    )
  )
})

test_that("bkg_build_database_impl() aborts if no ga_*.csv files are present", {
  data_dir <- withr::local_tempdir()
  expect_error(
    bkg_build_database_impl(data_dir, withr::local_tempdir())
  )
})

# ------------------------------------------------------------------------
# Full pipeline: build + offline geocoding against a synthetic fixture
# ------------------------------------------------------------------------
# This is a regression test for every house-number bug fixed in this
# package: numeric affixes ("10"+"2" -> "10/2"), affixes already containing
# a slash ("12"+"/4" -> "12/4"), house number ranges ("13-15" -> matches
# "13", not an unrelated coincidentally-similar number), and the exclusion
# of katasterinterne (non-official) house numbers (quality code "C").

test_that("building the database drops katasterinterne house numbers", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  version <- read_version_metadata(file.path(db_dir, "version.dcf"))
  
  expect_equal(version$n_addresses, 10)
  expect_equal(version$n_dropped_katasterintern, 1)
})

test_that("bkg_build_database_impl() accepts a memory_limit setting", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  expect_no_error(
    bkg_build_database_impl(fixture_dir, db_dir, memory_limit = "512MB")
  )
  expect_true(file.exists(file.path(db_dir, "version.dcf")))
})

test_that("bkg_build_database_impl() works with memory_limit = NULL (no cap)", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  expect_no_error(
    bkg_build_database_impl(fixture_dir, db_dir, memory_limit = NULL)
  )
})

test_that("bkg_build_database_impl() accepts a threads setting", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  expect_no_error(
    bkg_build_database_impl(fixture_dir, db_dir, threads = 1L)
  )
})

test_that("offline geocoding resolves house number affixes and ranges correctly", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = c("Teststraße", "Teststraße", "Teststraße"),
    house_number = c("10", "12", "13-15"),
    zip_code     = c("12345", "12345", "12345"),
    place        = c("Musterstadt", "Musterstadt", "Musterstadt")
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_equal(result$house_number_output, c("10/2", "12/4", "13"))
  expect_true(all(result$score > 0.8))
})

test_that("offline geocoding leaves unmatched places with NA scores", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststraße",
    house_number = "10",
    zip_code     = "99999",
    place        = "Nirgendwostadt"
  )
  
  # No warnings expected: sf::st_as_sf() would otherwise emit a harmless
  # but noisy "no non-missing arguments to min/max" warning when every
  # address fails place-matching (i.e. the intermediate frame it converts
  # has zero rows). See bkg_clean_matched_addresses() in bkg_matching.R.
  result <- expect_no_warning(
    bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  )
  
  expect_true(is.na(result$score))
  expect_true(nrow(attr(result, "unmatched_places")) >= 1)
})

test_that("offline geocoding matches on cleaned input but still displays the raw input", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststr.",
    house_number = "10 .",
    zip_code     = "12345",
    place        = "Musterstadt OT Nirgends"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  # Matching succeeded despite the messy input -- proves the invisible
  # cleaning (expand_street_abbreviation(), clean_house_number_input(),
  # strip_place_suffix()) is actually being applied.
  expect_true(result$score > 0.8)
  expect_equal(result$house_number_output, "10/2")
  
  # But the *_input columns show exactly what was entered, unedited.
  expect_equal(result$street_input, "Teststr.")
  expect_equal(result$house_number_input, "10 .")
  expect_equal(result$place_input, "Musterstadt OT Nirgends")
})

test_that("the *_input/*_output columns work with non-default column names too", {
  # Regression test: bkg_clean_matched_addresses() used to look up
  # street_input/house_number_input/zip_code_input/place_input (and the
  # _output equivalents) under hardcoded English names, regardless of what
  # the user's own columns were actually called. Whenever those didn't
  # literally match ("street", "house_number", "zip_code", "place"), the
  # columns silently went missing from the result (tibble() drops NULL
  # values without warning) -- this uses realistic German column names to
  # make sure that can't happen anymore.
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    strasse       = "Teststraße",
    hausnummer    = "10",
    postleitzahl  = "12345",
    ort           = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(
    addresses,
    cols = c("strasse", "hausnummer", "postleitzahl", "ort"),
    db_path = db_dir,
    verbose = FALSE
  )
  
  expect_true(all(c(
    "street_input", "house_number_input", "zip_code_input", "place_input",
    "street_output", "house_number_output", "zip_code_output", "place_output"
  ) %in% names(result)))
  
  expect_equal(result$street_input, "Teststraße")
  expect_equal(result$house_number_input, "10")
  expect_equal(result$place_input, "Musterstadt")
  expect_equal(result$house_number_output, "10/2")
})

# ------------------------------------------------------------------------
# Fuzzy zip-code blocking: a single-digit zip typo should still resolve
# via the fuzzy fallback in bkg_match_places_ddb(), rather than failing
# to match at all the way an exact-equality join would.
#
# Note: this fixture only has one place/zip combination, so it can only
# exercise the "no exact match at all, fuzzy recovery kicks in" path --
# not the "exact match must always outrank a same-block fuzzy competitor"
# safeguard, which needs a second place sharing the same 3-digit zip
# block to be meaningful. That would need extending the fixture data.
# ------------------------------------------------------------------------

test_that("offline geocoding recovers from a single zip-code typo via fuzzy blocking", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststraße",
    house_number = "10",
    zip_code     = "12346",  # one digit off from the real "12345"
    place        = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_false(is.na(result$score))
  expect_equal(result$zip_code_output, "12345")
  expect_equal(result$house_number_output, "10/2")
})

test_that("an exact zip code still matches normally (no regression from fuzzy blocking)", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststraße",
    house_number = "10",
    zip_code     = "12345",
    place        = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_equal(result$zip_code_output, "12345")
  expect_true(result$place_score > 0.99)
})

test_that("an exact zip match wins over a same-block fuzzy competitor with a better place-name score", {
  # Regression test for the actual vulnerability fuzzy zip blocking
  # introduced: jaro_winkler_similarity() between two zip codes sharing
  # the same 3-digit block is already ~0.8-0.92 (nowhere near as
  # discriminating as the old exact-equality join), which on its own
  # isn't enough of a gap to protect a correct-but-imperfectly-matched
  # place (one needing its Ortsteil suffix, via the fixture's zip 12301
  # "Musterstadt"/"Ortsteil Nirgendswo") against a same-block, different
  # zip place that happens to string-match the input's place name
  # perfectly (zip 12302, plain "Musterstadt", no suffix).
  #
  # Without the ORDER BY safeguard in bkg_match_places_ddb()'s QUALIFY
  # clause, 12302 (place_score 1.0 * zip_score ~0.92 = ~0.92) outscores
  # 12301 (place_score ~0.876 * zip_score 1.0 = ~0.876) on raw
  # total_score alone, even though the input's zip code is an exact,
  # unambiguous match for 12301. The fix makes any exact-zip candidate
  # with at least a plausible place score (>= 0.6) win outright,
  # regardless of how a fuzzy-zip competitor's total_score compares.
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststraße",
    house_number = "1",
    zip_code     = "12301",
    place        = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_equal(result$zip_code_output, "12301")
})

# ------------------------------------------------------------------------
# The *_cleaned columns: the value actually used for matching (after
# built-in and/or user-registered input fixes), distinct from both
# *_input (raw) and *_output (the matched database record).
# ------------------------------------------------------------------------

test_that("offline geocoding exposes cleaned (post-fix) values alongside input/output", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststr.",
    house_number = "10 .",
    zip_code     = "12345",
    place        = "Musterstadt OT Nirgends"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_true(all(c(
    "street_cleaned", "house_number_cleaned", "zip_code_cleaned",
    "place_cleaned", "address_cleaned"
  ) %in% names(result)))
  
  # Raw stays raw, cleaned reflects the applied built-in fixes, and is
  # distinct from either the raw input or the matched output.
  expect_equal(result$street_input, "Teststr.")
  expect_equal(result$street_cleaned, "Teststraße")
  expect_equal(result$place_input, "Musterstadt OT Nirgends")
  expect_equal(result$place_cleaned, "Musterstadt")
  expect_equal(result$house_number_input, "10 .")
  expect_equal(result$house_number_cleaned, "10")
})

test_that("address_cleaned sits alongside address_input/address_output, not identical to either", {
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  addresses <- data.frame(
    street       = "Teststr.",
    house_number = "10",
    zip_code     = "12345",
    place        = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_false(result$address_cleaned == result$address_input)
  expect_match(result$address_cleaned, "Teststra\u00dfe")
})

# ------------------------------------------------------------------------
# A custom fix registered via bkg_add_input_fix() should actually reach
# the matching stage -- this was the whole point of exposing *_cleaned:
# a bug in a user-registered fix is otherwise invisible until it distorts
# a score in a way that's hard to trace back to its cause.
# ------------------------------------------------------------------------

test_that("a custom input fix registered via bkg_add_input_fix() affects real matching", {
  withr::defer(bkg_reset_input_fixes())
  
  fixture_dir <- test_path("fixtures", "mini_bkg_raw")
  db_dir <- withr::local_tempdir()
  
  bkg_build_database_impl(fixture_dir, db_dir)
  
  # A deliberately aggressive custom fix: strips any trailing letter from
  # the house number before matching (mirrors the kind of well-meaning
  # but too-broad fix that motivated exposing house_number_cleaned in the
  # first place).
  bkg_reset_input_fixes("house_number")
  bkg_add_input_fix(house_number = ~ sub("[a-zA-Z]$", "", .x))
  
  addresses <- data.frame(
    street       = "Teststraße",
    house_number = "10a",
    zip_code     = "12345",
    place        = "Musterstadt"
  )
  
  result <- bkg_geocode_offline(addresses, db_path = db_dir, verbose = FALSE)
  
  expect_equal(result$house_number_input, "10a")
  expect_equal(result$house_number_cleaned, "10")
})
