# Synthetic GeocodingResults fixture ----
# No real database needed: S3 methods/exporter only care about columns,
# attributes, and class.

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

# Synthetic scored-results fixture ----
# For bkg_classify()/bkg_classify_interactive()/bkg_quality_summary()/
# bkg_show_address_detail() -- only score columns matter, no sf/
# GeocodingResults class needed, so a plain tibble is enough.

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
