bkg_match_addresses <- function(
  matched_data,
  cols,
  house_coordinates,
  opts,
  hierarchical_weight = 0.5,
  verbose
) {
  street <- cols[1]
  house_number <- ifelse(length(cols) == 4, cols[2], "")
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])

  # Prepare data ----
  if (verbose) {
    cli::cli_progress_step(
      msg = "Preparing data for address matching...",
      msg_done = "Prepared data for address matching.",
      msg_failed = "Could not prepare data for address matching."
    )
  }

  # Fix Mannheim square addresses
  matched_data[, street] <- gsub(
    "^([A-Z])([1-9])$", "\\1 \\2",
    matched_data[, street]
  )

  # Expand Str. to Straße
  fixed_street <- gsub(
    "str[\\.]?\\s", "straße ",
    matched_data[[street]],
    ignore.case = TRUE
  )

  # Create address string
  matched_data$whole_address <- trimws(paste0(
    fixed_street,
    if (house_number %in% colnames(matched_data)) {
      paste0(" ", matched_data[[house_number]])
    }
  ))

  matched_data$street <- fixed_street
  matched_data$house_number <- matched_data[[house_number]]

  house_coordinates <-
    house_coordinates[, house_number := paste0(house_number, house_number_add)]

  if (isTRUE(verbose)) {
    env <- environment()
    cli::cli_progress_bar(
      name = "Matching address data",
      total = nrow(matched_data),
      format = paste(
        "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} addresses |",
        "ETA {cli::pb_eta}"
      ),
      format_failed = "Failed at address {cli::pb_current}/{cli::pb_total}."
    )
  }

  # Match data with BKG data (record linkage) ----
  joined_data <- lapply(seq_len(nrow(matched_data)), function(i) {
    if (isTRUE(verbose)) cli::cli_progress_update(.envir = env)
    # Create a pairs object with matching places and zip codes
    within_place_string <- matched_data[i,]$place_slug
    within_zip_string <- matched_data[i,]$zip_code_matched

    within_place <-
      house_coordinates |>
      dplyr::filter(place_slug == within_place_string, zip_code == within_zip_string)

    # reduce to adjust weighting
    within_place$street_red <-
      gsub("straße\\b", "X", within_place$street, ignore.case = TRUE)

    within_place$street_red <-
      gsub("weg\\b", "Y", within_place$street_red, ignore.case = TRUE)

    matched_data$street_red <- NA
    matched_data[i, ]$street_red <-
      gsub("straße\\b", "X", matched_data[i, ]$street, ignore.case = TRUE)
    matched_data[i, ]$street_red <-
      gsub("weg\\b", "Y", matched_data[i, ]$street_red, ignore.case = TRUE)

    # normalize
    within_place$street_red <- normalize_key(within_place$street_red)

    matched_data[i, ]$street_red <-
      normalize_key(matched_data[i, ]$street_red)

    data_edited_pairs_street <- reclin2::pair(
      x = matched_data[i, ],
      y = within_place
    )

    # Compute string distance scores of the pairs
    reclin2::compare_pairs(
      data_edited_pairs_street,
      on = c("street_red"),
      default_comparator = dyn_comparator(hierarchical_weight, opts),
      inplace = TRUE
    )

    weight_street <- max(data_edited_pairs_street$street)

    # Select data below threshold using a greedy selection algorithm
    reclin2::select_greedy(
      data_edited_pairs_street,
      variable = "threshold",
      score = c("street_red"),
      threshold = 0,
      inplace = TRUE
    )

    selection_street <-
      data_edited_pairs_street[data_edited_pairs_street$threshold]

    # within street
    matched_street <-
      reclin2::link(
        selection_street,
        all_x = TRUE,
        all_y = FALSE
      ) |>
      cbind(score = weight_street)

    matched_street <- matched_street$street.y

    within_street <- within_place[within_place$street == matched_street,]

    data_edited_pairs_house_number <- reclin2::pair(
      x = matched_data[i, ],
      y = within_street
    )

    # Compute string distance scores of the pairs
    reclin2::compare_pairs(
      data_edited_pairs_house_number,
      on = c("house_number"),
      default_comparator = dyn_comparator(hierarchical_weight, opts),
      inplace = TRUE
    )

    weight_house_number <- max(data_edited_pairs_house_number$house_number)

    # Select data below threshold using a greedy selection algorithm
    reclin2::select_greedy(
      data_edited_pairs_house_number,
      variable = "threshold",
      score = c("house_number"),
      threshold = 0,
      inplace = TRUE
    )

    selection_house_number <-
      data_edited_pairs_house_number[data_edited_pairs_house_number$threshold]

    # Link results with original data
    data_linked <- reclin2::link(
      selection_house_number,
      all_x = TRUE,
      all_y = FALSE
    )

    # Hierarchical scoring: each component is dampened by the hierarchical_weight
    # exponent. The cascading match structure (place -> street -> house number)
    # already ensures that a wrong place drags everything down.
    # A floor of 0.5 prevents zero scores (e.g. missing house numbers) from
    # nullifying the entire result while still applying a moderate penalty.
    data_linked$place_score <- data_linked$score
    data_linked$street_score <- weight_street
    data_linked$house_number_score <- weight_house_number
    data_linked$score <-
      max(data_linked$place_score, 0.5)^hierarchical_weight *
      max(weight_street, 0.5)^hierarchical_weight *
      max(weight_house_number, 0.5)^hierarchical_weight

    data_linked
  })

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }

  joined_data <- do.call(rbind, joined_data)

  # Fix names
  for (name in c(street, house_number, zip_code, place)) {
    if (name %in% names(joined_data)) {
      names(joined_data)[names(joined_data) == name] <- paste0(name, ".x")
    }
  }
  
  # Fix scores ----
  # subtract 0.05 if housenumbers do not match
  regex_chr <- "[0-9]+[a-z]*"
  hn.x <- unlist(match_regex(joined_data$whole_address.x, regex_chr))
  hn.y <- match_regex(joined_data$whole_address.y, regex_chr)
  hn.y <- vapply(hn.y, function(x) if (!length(x)) NA_character_ else x, character(1))
  hn_mismatch <- !hn.x == hn.y & !is.na(hn.x) & !is.na(hn.y)
  incorrect_scores <- joined_data$score[hn_mismatch]
  joined_data$score[hn_mismatch] <- incorrect_scores - 0.05

  n_geocoded <- sum(!is.na(joined_data$RS))

  if (isTRUE(verbose)) {
    if (!n_geocoded) {
      cli::cli_warn("No address could be geocoded. Check your input!")
    } else if (n_geocoded == nrow(matched_data)) {
      cli::cli_alert_success("All place-matched addresses could be geocoded.")
    } else {
      cli::cli_alert_info(paste(
        "{.val {n_geocoded}} out of",
        "{.val {nrow(matched_data)}} place-matched address{?es} could be geocoded."
      ))
    }
  }

  joined_data
}
