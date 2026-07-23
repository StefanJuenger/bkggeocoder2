# ------------------------------------------------------------------------
# bkg_update_database(): workflow logic (staleness, force, backup)
# ------------------------------------------------------------------------
# These tests mock bkg_build_database_impl() so they run instantly and
# don't need any real address data -- they only exercise the surrounding
# decision logic (does a rebuild happen or not, is a backup made or not).

fake_build <- function(address_data_path, db_path) {
  dir.create(db_path, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      version = format(Sys.Date(), "%Y-%m-%d"),
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      n_addresses = 1L,
      n_places = 1L,
      n_dropped_katasterintern = 0L
    ),
    file.path(db_path, "version.json"),
    auto_unbox = TRUE
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
  expect_true(file.exists(file.path(db_dir, "version.json")))
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
  expect_true(file.exists(file.path(backup_dir, "version.json")))
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
  
  version <- jsonlite::read_json(file.path(db_dir, "version.json"))
  
  expect_equal(version$n_addresses, 8)
  expect_equal(version$n_dropped_katasterintern, 1)
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
