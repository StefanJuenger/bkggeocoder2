test_that("normalize_file() slugifies place names consistently", {
  expect_equal(normalize_file("Köln"), "koeln")
  expect_equal(normalize_file("Bad Homburg"), "bad_homburg")
  expect_equal(normalize_file("Sankt Augustin"), "sankt_augustin")
  expect_equal(normalize_file("  Leipzig  "), "leipzig")
  expect_equal(normalize_file("Rhein-Main"), "rhein_main")
})

test_that("normalize_file() has no leading/trailing underscores", {
  expect_false(startsWith(normalize_file("-Musterstadt-"), "_"))
  expect_false(endsWith(normalize_file("-Musterstadt-"), "_"))
})

test_that("combine_house_number() handles numeric affixes with canonical separator", {
  expect_equal(combine_house_number("10", "2"), "10/2")
  expect_equal(combine_house_number("10", "/2"), "10/2")
  expect_equal(combine_house_number("10", " /2"), "10/2")
  expect_equal(combine_house_number("10", "-2"), "10/2")
})

test_that("combine_house_number() glues alphabetic affixes directly", {
  expect_equal(combine_house_number("10", "a"), "10a")
  expect_equal(combine_house_number("46", "b"), "46b")
})

test_that("combine_house_number() leaves the bare number unchanged when there is no affix", {
  expect_equal(combine_house_number("10", ""), "10")
  expect_equal(combine_house_number("10", NA_character_), "10")
})

test_that("combine_house_number() is vectorized and mixes cases correctly", {
  house_number     <- c("10", "10", "10", "46")
  house_number_add <- c("2", "/2", "", "a")
  
  expect_equal(
    combine_house_number(house_number, house_number_add),
    c("10/2", "10/2", "10", "46a")
  )
})

test_that("bkg_db_path() respects the BKGGEOCODER2_DB_PATH override", {
  withr::local_envvar(BKGGEOCODER2_DB_PATH = "/tmp/some/custom/path")
  expect_equal(bkg_db_path(), "/tmp/some/custom/path")
})

test_that("bkg_db_path() falls back to the standard R_user_dir() location", {
  withr::local_envvar(BKGGEOCODER2_DB_PATH = NA)
  expect_equal(bkg_db_path(), tools::R_user_dir("bkggeocoder2", which = "data"))
})

test_that("spt_create_inspire_ids() produces the expected grid id format", {
  # Realistic EPSG:3035 coordinates for Germany (never perfectly round in
  # practice) -- see the regression test below for the round-number edge
  # case this deliberately avoids here.
  pt <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(4123456.78, 3012345.67)), crs = 3035)
  )
  
  id_1km <- spt_create_inspire_ids(pt, type = "1km")
  id_100m <- spt_create_inspire_ids(pt, type = "100m")
  
  expect_match(id_1km, "^1kmN\\d{4}E\\d{4}$")
  expect_match(id_100m, "^100mN\\d{5}E\\d{5}$")
})

test_that("spt_create_inspire_ids() handles perfectly round coordinates correctly", {
  # as.character(3000000) renders as "3e+06" in R (scientific notation is
  # shorter), which would have silently corrupted the grid id. This locks
  # in the sprintf("%.0f", ...) fix.
  pt <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(4000000, 3000000)), crs = 3035)
  )
  
  id_1km <- spt_create_inspire_ids(pt, type = "1km")
  
  expect_equal(id_1km, "1kmN3000E4000")
})
