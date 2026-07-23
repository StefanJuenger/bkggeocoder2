# Internal matching steps used by bkg_geocode_offline() (see
# bkg_geocode_offline.R): place matching -> address matching -> result
# cleaning. None of these functions are meant to be called independently.

# -----------------------------------------------------------------------------
# Shared SQL string normalization helpers
# -----------------------------------------------------------------------------
# Both matching steps below normalize strings inside DuckDB SQL rather than
# in R, so that reference (Parquet) and input data are always normalized by
# the exact same logic -- comparing anything else would silently bias the
# similarity scores.

# German-specific transliteration (mirrors the historical dyn_comparator()
# approach): umlaut expansion + accent stripping + lowercasing. No
# non-alphanumeric stripping yet -- see nk_plain()/nk_street() for that.
nk <- function(expr) {
  sprintf(
    paste0(
      "lower(strip_accents(",
      "replace(replace(replace(replace(replace(replace(replace(",
      "%s, 'ä','ae'), 'ö','oe'), 'ü','ue'), 'ß','ss'), 'Ä','Ae'), 'Ö','Oe'), 'Ü','Ue')",
      "))"
    ),
    expr
  )
}

# nk() + stripping all non-alphanumeric characters. Used wherever two
# strings should be compared as plain normalized keys (place names, house
# numbers) without any further domain-specific reduction.
nk_plain <- function(expr) {
  sprintf("regexp_replace(%s, '[^a-z0-9]', '', 'g')", nk(expr))
}

# Street normalization:
#   1. nk() handles umlauts+accents+lower ("straße" -> "strasse")
#   2. Reduce strasse/weg at WORD BOUNDARIES (\b)
#   3. Strip all non-alphanumeric chars
nk_street <- function(expr) {
  reduced <- sprintf(
    "regexp_replace(regexp_replace(%s, 'strasse\\b', 'x', 'g'), 'weg\\b', 'y', 'g')",
    nk(expr)
  )
  sprintf("regexp_replace(%s, '[^a-z0-9]', '', 'g')", reduced)
}

# -----------------------------------------------------------------------------
# Step 1: place matching (DuckDB)
# -----------------------------------------------------------------------------

#' Match data's places and zip codes against the local database (DuckDB)
#'
#' @noRd
bkg_match_places_ddb <- function(
    .data,
    cols,
    db_path,
    place_match_quality = 0.85,
    con,
    verbose
) {
  
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  
  # ---------------------------
  # Prepare input
  # ---------------------------
  
  .data[, zip_code] <- vapply(.data[, zip_code], function(zip) {
    zip <- as.character(zip)
    if (nchar(zip) == 4) paste0("0", zip) else zip
  }, FUN.VALUE = character(1))
  
  if (isTRUE(verbose)) {
    cli::cli_inform(
      "Found {.val {nrow(unique(.data[c(place, zip_code)]))}} distinct places."
    )
    cli::cli_progress_step("Matching places via DuckDB...")
  }
  
  # ---------------------------
  # Register input, reference lookup table lazily
  # ---------------------------
  # Unlike earlier versions of this package, the ZIP/place lookup table is
  # never pulled into R -- it is referenced directly via read_parquet() and
  # normalized in SQL, exactly like the address matching step below. This
  # also guarantees that both sides of the comparison are normalized by the
  # exact same logic (see nk_plain()).
  
  zip_places_path <- file.path(db_path, "zip_places", "ga_zip_places.parquet")
  
  input_tbl <- paste0("input_places_", sample.int(1e6, 1))
  duckdb::duckdb_register(con, input_tbl, .data)
  on.exit(
    DBI::dbExecute(con, sprintf("DROP VIEW IF EXISTS %s", input_tbl)),
    add = TRUE
  )
  
  # ---------------------------
  # SQL MATCHING (reclin2-like scoring)
  # ---------------------------
  
  sql <- sprintf("
    WITH ref AS (
      SELECT
        place,
        place_add,
        zip_code,
        place_slug,
        trim(concat(place, ' ', COALESCE(place_add, ''))) AS place_full,
        %s AS place_simple
      FROM read_parquet(%s)
    ),
    input_norm AS (
      SELECT
        *,
        %s AS place_simple
      FROM %s
    ),
    scored AS (
      SELECT
        input.*,
        ref.place_full AS place_matched,
        ref.zip_code AS zip_code_matched,
        ref.place_slug,
        -- Ort similarity: both sides normalized identically via nk_plain()
        jaro_winkler_similarity(input.place_simple, ref.place_simple) AS place_score,
        -- kombinierter Score (multiplikativ stabiler)
        (
          jaro_winkler_similarity(input.place_simple, ref.place_simple)
          *
          CASE
            WHEN input.\"%s\" = ref.zip_code THEN 1.0
            ELSE 0.2
          END
        ) AS total_score
      FROM input_norm input
      LEFT JOIN ref
        ON input.\"%s\" = ref.zip_code
    )
    SELECT *
    FROM scored
    QUALIFY ROW_NUMBER() OVER (
      PARTITION BY \".iid\"
      ORDER BY total_score DESC, place_matched
    ) = 1
  ",
                 nk_plain("concat(place, ' ', COALESCE(place_add, ''))"),
                 DBI::dbQuoteString(con, zip_places_path),
                 nk_plain(sprintf('"%s"', place)),
                 input_tbl,
                 zip_code,
                 zip_code
  )
  
  matched <- DBI::dbGetQuery(con, sql)
  
  # ---------------------------
  # Merge back
  # ---------------------------
  
  result <- merge(
    .data,
    matched[, c(".iid", "place_matched", "zip_code_matched",
                "place_slug", "total_score")],
    by = ".iid",
    all.x = TRUE
  )
  
  result$place_matched_flag <- !is.na(result$total_score) &
    result$total_score >= place_match_quality
  
  unmatched <- result[!result$place_matched_flag,
                      c(zip_code, place)]
  unmatched <- unmatched[!duplicated(unmatched), ]
  
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    
    n_matched <- sum(result$place_matched_flag)
    
    cli::cli_inform(
      "{.val {n_matched}} / {.val {nrow(.data)}} places matched."
    )
  }
  
  structure(result, unmatched_places = unmatched)
}


# -----------------------------------------------------------------------------
# Step 2: address matching (DuckDB)
# -----------------------------------------------------------------------------

#' Match addresses against the local BKG database (DuckDB)
#'
#' Reads Parquet partitions directly via DuckDB -- only for the relevant
#' places. Uses two-stage matching: first best street, then best house
#' number within that street. String normalization uses German-specific
#' transliteration + \code{strip_accents()} in SQL.
#'
#' @param matched_data Data frame of place-matched addresses.
#' @param cols Column names for address components.
#' @param db_path Path to the local Parquet database.
#' @param hierarchical_weight Exponent for score dampening.
#' @param house_number_penalty Penalty subtracted from the house-number
#' score if the input and matched house number differ.
#' @param con A DuckDB connection object.
#' @param verbose Whether to print progress.
#'
#' @noRd
bkg_match_addresses_ddb <- function(
    matched_data,
    cols,
    db_path,
    hierarchical_weight = 0.5,
    house_number_penalty = .05,
    con,
    verbose
) {
  street <- cols[1]
  house_number <- ifelse(length(cols) == 4, cols[2], "")
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  
  if (isTRUE(verbose)) {
    cli::cli_progress_step(
      msg = "Preparing data for address matching...",
      msg_done = "Prepared data for address matching.",
      msg_failed = "Could not prepare data for address matching."
    )
  }
  
  # Prepare input data in R ----
  # Fix Mannheim square addresses
  matched_data[, street] <- gsub(
    "^([A-Z])([1-9])$", "\\1 \\2",
    matched_data[, street]
  )
  
  # Expand Str. to Straße
  matched_data$street_clean <- gsub(
    "str[\\.]?\\s", "straße ",
    matched_data[[street]],
    ignore.case = TRUE
  )
  
  # Whole address for input
  matched_data$whole_address_in <- trimws(paste0(
    matched_data$street_clean,
    if (house_number %in% colnames(matched_data)) {
      paste0(" ", matched_data[[house_number]])
    }
  ))
  
  # House number as character
  if (house_number %in% colnames(matched_data)) {
    matched_data$hn_input <- as.character(matched_data[[house_number]])
  } else {
    matched_data$hn_input <- NA_character_
  }
  
  # House number ranges (e.g. "13-15") don't exist as a single row in the
  # reference data -- only the individual house numbers "13", "14", "15" do.
  # For MATCHING purposes only, collapse such a range down to its first
  # number (mirrors how ranges were always resolved to their first number
  # previously). The original, unmodified input (hn_input) is still used for
  # the displayed output and is unaffected by this. Comparing the full range
  # string via jaro_winkler instead can match an unrelated house number that
  # happens to share many characters with the range string (e.g. "13-15" is
  # closer to "135" than to "13" in pure character-overlap terms).
  matched_data$hn_input_key <- gsub(
    "^([0-9]+[a-zA-Z]?)\\s*[-\u2013]\\s*[0-9]+[a-zA-Z]?$",
    "\\1",
    matched_data$hn_input
  )
  
  # Build parquet paths for relevant places only ----
  relevant_slugs <- unique(matched_data$place_slug)
  parquet_paths <- bkg_ga_parquet_glob(db_path, relevant_slugs)
  parquet_sql <- paste(DBI::dbQuoteString(con, parquet_paths), collapse = ", ")
  
  if (isTRUE(verbose)) {
    cli::cli_progress_step(
      msg = "Matching addresses via DuckDB...",
      msg_done = "Address matching finished.",
      msg_failed = "Address matching failed."
    )
  }
  
  # Register input data ----
  input_tbl <- paste0("input_addr_", sample.int(1e6, 1))
  duckdb::duckdb_register(con, input_tbl, matched_data)
  on.exit(
    DBI::dbExecute(con, sprintf("DROP VIEW IF EXISTS %s", input_tbl)),
    add = TRUE
  )
  
  # String normalization uses the shared nk()/nk_street() helpers defined at
  # the top of this file, so both matching steps stay consistent.
  
  # Two-stage SQL matching ----
  # house_number_full is already the canonical, combined house number
  # (computed once when the database was built, see combine_house_number()
  # in utils.R) -- no need to recombine house_number/house_number_add here.
  sql <- sprintf("
    WITH addr AS (
      SELECT
        *,
        %s AS street_norm,
        %s AS hn_norm
      FROM read_parquet([%s])
    ),
    input_norm AS (
      SELECT
        *,
        %s AS street_norm,
        %s AS hn_norm
      FROM %s
    ),
    street_match AS (
      SELECT
        input.\".iid\",
        input.street_clean,
        input.hn_norm AS input_hn_norm,
        input.hn_input,
        input.hn_input_key,
        input.whole_address_in,
        input.\"%s\" AS zip_code_in,
        input.\"%s\" AS place_in,
        addr.street AS street_out,
        addr.zip_code AS zip_code_out,
        addr.place AS place_out,
        addr.place_slug,
        jaro_winkler_similarity(input.street_norm, addr.street_norm) AS street_score
      FROM input_norm input
      INNER JOIN addr
        ON input.place_slug = addr.place_slug
        AND input.zip_code_matched = addr.zip_code
      QUALIFY ROW_NUMBER() OVER (
        PARTITION BY input.\".iid\"
        ORDER BY street_score DESC
      ) = 1
    ),
    hn_match AS (
      SELECT
        sm.\".iid\",
        sm.street_clean,
        sm.hn_input,
        sm.hn_input_key,
        sm.whole_address_in,
        sm.zip_code_in,
        sm.place_in,
        sm.street_score,
        addr2.street AS street_out,
        addr2.house_number_full AS hn_out,
        addr2.zip_code AS zip_code_out,
        addr2.place AS place_out,
        addr2.whole_address AS whole_address_out,
        addr2.whole_address_add,
        addr2.RS,
        addr2.x,
        addr2.y,
        jaro_winkler_similarity(sm.input_hn_norm, addr2.hn_norm) AS house_number_score
      FROM street_match sm
      INNER JOIN addr addr2
        ON sm.place_slug = addr2.place_slug
        AND sm.zip_code_out = addr2.zip_code
        AND sm.street_out = addr2.street
      QUALIFY ROW_NUMBER() OVER (
        PARTITION BY sm.\".iid\"
        ORDER BY house_number_score DESC
      ) = 1
    )
    SELECT * FROM hn_match
  ",
                 nk_street("street"),
                 nk("house_number_full"),
                 parquet_sql,
                 nk_street("street_clean"),
                 nk("COALESCE(hn_input_key, '')"),
                 input_tbl,
                 zip_code, place
  )
  
  geocoded <- tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(e) {
      cli::cli_abort(c(
        "DuckDB address matching failed.",
        "x" = conditionMessage(e)
      ))
    }
  )
  
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }
  
  # Compute hierarchical score in R ----
  place_score <-
    matched_data$total_score[match(geocoded$.iid, matched_data$.iid)]
  geocoded$place_score <- place_score
  
  # Penalty for non-matching house numbers. hn_out is already the canonical
  # combination of house_number + house_number_add, computed once when the
  # database was built (see combine_house_number() in utils.R), and
  # hn_input_key is the range-collapsed matching key (see above) -- comparing
  # the raw hn_input here would flag every resolved range (e.g. "13-15" vs.
  # matched "13") as a mismatch.
  mismatch <- geocoded$hn_input_key != geocoded$hn_out
  mismatch[is.na(mismatch)] <- FALSE
  geocoded$house_number_score[mismatch] <-
    geocoded$house_number_score[mismatch] - house_number_penalty
  
  geocoded$house_number_score[geocoded$house_number_score < 0] <- 0
  
  geocoded$score <-
    pmax(geocoded$place_score, 0.5)^hierarchical_weight *
    pmax(geocoded$street_score, 0.5)^hierarchical_weight *
    pmax(geocoded$house_number_score, 0.5)^hierarchical_weight
  
  # Build result with .x/.y column names that bkg_clean_matched_addresses expects
  result <- data.frame(
    .iid = geocoded$.iid,
    score = geocoded$score,
    place_score = geocoded$place_score,
    street_score = geocoded$street_score,
    house_number_score = geocoded$house_number_score,
    whole_address.x = geocoded$whole_address_in,
    whole_address.y = geocoded$whole_address_out,
    whole_address_add = geocoded$whole_address_add,
    RS = geocoded$RS,
    x = geocoded$x,
    y = geocoded$y,
    check.names = FALSE
  )
  
  result[[paste0(street, ".x")]] <- geocoded$street_clean
  result[[paste0(street, ".y")]] <- geocoded$street_out
  if (house_number != "") {
    result[[paste0(house_number, ".x")]] <- geocoded$hn_input
    result[[paste0(house_number, ".y")]] <- geocoded$hn_out
  }
  result[[paste0(zip_code, ".x")]] <- geocoded$zip_code_in
  result[[paste0(zip_code, ".y")]] <- geocoded$zip_code_out
  result[[paste0(place, ".x")]] <- geocoded$place_in
  result[[paste0(place, ".y")]] <- geocoded$place_out
  
  # Merge back with input to keep .iid alignment ----
  result <- merge(
    matched_data[, ".iid", drop = FALSE],
    result,
    by = ".iid",
    all.x = TRUE
  )
  
  n_geocoded <- sum(!is.na(result$RS))
  
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
  
  result
}


# -----------------------------------------------------------------------------
# Step 3: cleaning up the matched result
# -----------------------------------------------------------------------------

bkg_clean_matched_addresses <- function(messy_data, cols, identifiers, verbose) {
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  
  if (isTRUE(identifiers)) {
    identifiers <- c("rs", "nuts", "inspire")
  }
  
  if (isTRUE(verbose)) {
    cli::cli_progress_step(
      msg = "Cleaning up geocoding output...",
      msg_done = "Cleaned up geocoding output.",
      msg_failed = "Failed to clean up geocoding output."
    )
  }
  
  is_out <- grepl("\\.y$", names(messy_data))
  is_inp <- grepl("\\.x$", names(messy_data))
  new_out <- gsub("\\.y$", "_output", names(messy_data)[is_out])
  new_in <- gsub("\\.x$", "_input", names(messy_data)[is_inp])
  names(messy_data)[is_out] <- new_out
  names(messy_data)[is_inp] <- new_in
  
  # Clean dataset
  clean_data <- tibble::tibble(
    .iid = messy_data$.iid,
    score = messy_data$score,
    place_score = messy_data$place_score,
    street_score = messy_data$street_score,
    house_number_score = messy_data$house_number_score,
    address_input = paste(
      messy_data$whole_address_input,
      messy_data[[paste0(zip_code, "_input")]],
      messy_data[[paste0(place, "_input")]]
    ),
    street_input = messy_data$street_input,
    house_number_input = messy_data$house_number_input,
    zip_code_input = messy_data$zip_code_input,
    place_input = messy_data$place_input,
    address_output = messy_data$whole_address_add,
    street_output = messy_data$street_output,
    house_number_output = messy_data$house_number_output,
    zip_code_output = messy_data$zip_code_output,
    place_output = messy_data$place_output,
    RS  = messy_data$RS,
    AGS = paste0(substr(messy_data$RS, 1, 5), substr(messy_data$RS, 10, 12)),
    VWG = substr(messy_data$RS, 1, 9),
    KRS = substr(messy_data$RS, 1, 5),
    RBZ = substr(messy_data$RS, 1, 3),
    STA = substr(messy_data$RS, 1, 2),
    x = messy_data$x,
    y = messy_data$y,
    address_date = "April, 2025",
    ags_date = "January 01, 2025",
    source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2025)"
  )
  
  clean_data <- sf::st_as_sf(
    clean_data,
    coords = c("x", "y"),
    crs = 25832,
    remove = TRUE,
    na.fail = FALSE
  )
  
  if ("inspire" %in% identifiers) {
    clean_data <- tibble::add_column(
      clean_data,
      Gitter_ID_1km = spt_create_inspire_ids(data = clean_data, type = "1km"),
      Gitter_ID_100m = spt_create_inspire_ids(data = clean_data, type = "100m"),
      .after = "STA"
    )
  }
  
  if ("nuts" %in% identifiers) {
    nuts <- merge(clean_data, nuts_ars, by = "KRS", all.x = TRUE)$nuts
    clean_data <- tibble::add_column(
      clean_data,
      nuts3 = nuts,
      nuts2 = substr(nuts, 1, 4),
      nuts1 = substr(nuts, 1, 3),
      .after = "STA"
    )
  }
  
  if (!"rs" %in% identifiers) {
    clean_data[c("RS", "VWG", "KRS", "RBZ", "STA", "AGS")] <- NULL
  }
  
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }
  
  clean_data
}


nuts_ars <- tibble::tibble(
  KRS = c("01001", "01002", "01003", "01004", 
          "01051", "01053", "01054", "01055", "01056", "01057", "01058", 
          "01059", "01060", "01061", "01062", "02000", "03101", "03102", 
          "03103", "03151", "03153", "03154", "03155", "03157", "03158", 
          "03159", "03241", "03251", "03252", "03254", "03255", "03256", 
          "03257", "03351", "03352", "03353", "03354", "03355", "03356", 
          "03357", "03358", "03359", "03360", "03361", "03401", "03402", 
          "03403", "03404", "03405", "03451", "03452", "03453", "03454", 
          "03455", "03456", "03457", "03458", "03459", "03460", "03461", 
          "03462", "04011", "04012", "05111", "05112", "05113", "05114", 
          "05116", "05117", "05119", "05120", "05122", "05124", "05154", 
          "05158", "05162", "05166", "05170", "05314", "05315", "05316", 
          "05334", "05358", "05362", "05366", "05370", "05374", "05378", 
          "05382", "05512", "05513", "05515", "05554", "05558", "05562", 
          "05566", "05570", "05711", "05754", "05758", "05762", "05766", 
          "05770", "05774", "05911", "05913", "05914", "05915", "05916", 
          "05954", "05958", "05962", "05966", "05970", "05974", "05978", 
          "06411", "06412", "06413", "06414", "06431", "06432", "06433", 
          "06434", "06435", "06436", "06437", "06438", "06439", "06440", 
          "06531", "06532", "06533", "06534", "06535", "06611", "06631", 
          "06632", "06633", "06634", "06635", "06636", "07111", "07131", 
          "07132", "07133", "07134", "07135", "07137", "07138", "07140", 
          "07141", "07143", "07211", "07231", "07232", "07233", "07235", 
          "07311", "07312", "07313", "07314", "07315", "07316", "07317", 
          "07318", "07319", "07320", "07331", "07332", "07333", "07334", 
          "07335", "07336", "07337", "07338", "07339", "07340", "08111", 
          "08115", "08116", "08117", "08118", "08119", "08121", "08125", 
          "08126", "08127", "08128", "08135", "08136", "08211", "08212", 
          "08215", "08216", "08221", "08222", "08225", "08226", "08231", 
          "08235", "08236", "08237", "08311", "08315", "08316", "08317", 
          "08325", "08326", "08327", "08335", "08336", "08337", "08415", 
          "08416", "08417", "08421", "08425", "08426", "08435", "08436", 
          "08437", "09161", "09162", "09163", "09171", "09172", "09173", 
          "09174", "09175", "09176", "09177", "09178", "09179", "09180", 
          "09181", "09182", "09183", "09184", "09185", "09186", "09187", 
          "09188", "09189", "09190", "09261", "09262", "09263", "09271", 
          "09272", "09273", "09274", "09275", "09276", "09277", "09278", 
          "09279", "09361", "09362", "09363", "09371", "09372", "09373", 
          "09374", "09375", "09376", "09377", "09461", "09462", "09463", 
          "09464", "09471", "09472", "09473", "09474", "09475", "09476", 
          "09477", "09478", "09479", "09561", "09562", "09563", "09564", 
          "09565", "09571", "09572", "09573", "09574", "09575", "09576", 
          "09577", "09661", "09662", "09663", "09671", "09672", "09673", 
          "09674", "09675", "09676", "09677", "09678", "09679", "09761", 
          "09762", "09763", "09764", "09771", "09772", "09773", "09774", 
          "09775", "09776", "09777", "09778", "09779", "09780", "10041", 
          "10042", "10043", "10044", "10045", "10046", "11000", "12051", 
          "12052", "12053", "12054", "12060", "12061", "12062", "12063", 
          "12064", "12065", "12066", "12067", "12068", "12069", "12070", 
          "12071", "12072", "12073", "13003", "13004", "13071", "13072", 
          "13073", "13074", "13075", "13076", "14511", "14521", "14522", 
          "14523", "14524", "14612", "14625", "14626", "14627", "14628", 
          "14713", "14729", "14730", "15001", "15002", "15003", "15081", 
          "15082", "15083", "15084", "15085", "15086", "15087", "15088", 
          "15089", "15090", "15091", "16051", "16052", "16053", "16054", 
          "16055", "16056", "16061", "16062", "16063", "16064", "16065", 
          "16066", "16067", "16068", "16069", "16070", "16071", "16072", 
          "16073", "16074", "16075", "16076", "16077"),
  nuts = c("DEF01", 
           "DEF02", "DEF03", "DEF04", "DEF05", "DEF06", "DEF07", "DEF08", 
           "DEF09", "DEF0A", "DEF0B", "DEF0C", "DEF0D", "DEF0E", "DEF0F", 
           "DE600", "DE911", "DE912", "DE913", "DE914", "DE916", "DE917", 
           "DE918", "DE91A", "DE91B", "DE91C", "DE929", "DE922", "DE923", 
           "DE925", "DE926", "DE927", "DE928", "DE931", "DE932", "DE933", 
           "DE934", "DE935", "DE936", "DE937", "DE938", "DE939", "DE93A", 
           "DE93B", "DE941", "DE942", "DE943", "DE944", "DE945", "DE946", 
           "DE947", "DE948", "DE949", "DE94A", "DE94B", "DE94C", "DE94D", 
           "DE94E", "DE94F", "DE94G", "DE94H", "DE501", "DE502", "DEA11", 
           "DEA12", "DEA13", "DEA14", "DEA15", "DEA16", "DEA17", "DEA18", 
           "DEA19", "DEA1A", "DEA1B", "DEA1C", "DEA1D", "DEA1E", "DEA1F", 
           "DEA22", "DEA23", "DEA24", "DEA2D", "DEA26", "DEA27", "DEA28", 
           "DEA29", "DEA2A", "DEA2B", "DEA2C", "DEA31", "DEA32", "DEA33", 
           "DEA34", "DEA35", "DEA36", "DEA37", "DEA38", "DEA41", "DEA42", 
           "DEA43", "DEA44", "DEA45", "DEA46", "DEA47", "DEA51", "DEA52", 
           "DEA53", "DEA54", "DEA55", "DEA56", "DEA57", "DEA58", "DEA59", 
           "DEA5A", "DEA5B", "DEA5C", "DE711", "DE712", "DE713", "DE714", 
           "DE715", "DE716", "DE717", "DE718", "DE719", "DE71A", "DE71B", 
           "DE71C", "DE71D", "DE71E", "DE721", "DE722", "DE723", "DE724", 
           "DE725", "DE731", "DE732", "DE733", "DE734", "DE735", "DE736", 
           "DE737", "DEB11", "DEB12", "DEB13", "DEB14", "DEB15", "DEB1C", 
           "DEB17", "DEB18", "DEB1D", "DEB1A", "DEB1B", "DEB21", "DEB22", 
           "DEB23", "DEB24", "DEB25", "DEB31", "DEB32", "DEB33", "DEB34", 
           "DEB35", "DEB36", "DEB37", "DEB38", "DEB39", "DEB3A", "DEB3B", 
           "DEB3C", "DEB3D", "DEB3E", "DEB3F", "DEB3G", "DEB3H", "DEB3I", 
           "DEB3J", "DEB3K", "DE111", "DE112", "DE113", "DE114", "DE115", 
           "DE116", "DE117", "DE118", "DE119", "DE11A", "DE11B", "DE11C", 
           "DE11D", "DE121", "DE122", "DE123", "DE124", "DE125", "DE126", 
           "DE127", "DE128", "DE129", "DE12A", "DE12B", "DE12C", "DE131", 
           "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE138", 
           "DE139", "DE13A", "DE141", "DE142", "DE143", "DE144", "DE145", 
           "DE146", "DE147", "DE148", "DE149", "DE211", "DE212", "DE213", 
           "DE214", "DE215", "DE216", "DE217", "DE218", "DE219", "DE21A", 
           "DE21B", "DE21C", "DE21D", "DE21E", "DE21F", "DE21G", "DE21H", 
           "DE21I", "DE21J", "DE21K", "DE21L", "DE21M", "DE21N", "DE221", 
           "DE222", "DE223", "DE224", "DE225", "DE226", "DE227", "DE228", 
           "DE229", "DE22A", "DE22B", "DE22C", "DE231", "DE232", "DE233", 
           "DE234", "DE235", "DE236", "DE237", "DE238", "DE239", "DE23A", 
           "DE241", "DE242", "DE243", "DE244", "DE245", "DE246", "DE247", 
           "DE248", "DE249", "DE24A", "DE24B", "DE24C", "DE24D", "DE251", 
           "DE252", "DE253", "DE254", "DE255", "DE256", "DE257", "DE258", 
           "DE259", "DE25A", "DE25B", "DE25C", "DE261", "DE262", "DE263", 
           "DE264", "DE265", "DE266", "DE267", "DE268", "DE269", "DE26A", 
           "DE26B", "DE26C", "DE271", "DE272", "DE273", "DE274", "DE275", 
           "DE276", "DE277", "DE278", "DE279", "DE27A", "DE27B", "DE27C", 
           "DE27D", "DE27E", "DEC01", "DEC02", "DEC03", "DEC04", "DEC05", 
           "DEC06", "DE300", "DE401", "DE402", "DE403", "DE404", "DE405", 
           "DE406", "DE407", "DE408", "DE409", "DE40A", "DE40B", "DE40C", 
           "DE40D", "DE40E", "DE40F", "DE40G", "DE40H", "DE40I", "DE803", 
           "DE804", "DE80J", "DE80K", "DE80L", "DE80M", "DE80N", "DE80O", 
           "DED41", "DED42", "DED43", "DED44", "DED45", "DED21", "DED2C", 
           "DED2D", "DED2E", "DED2F", "DED51", "DED52", "DED53", "DEE01", 
           "DEE02", "DEE03", "DEE04", "DEE05", "DEE07", "DEE08", "DEE09", 
           "DEE06", "DEE0A", "DEE0B", "DEE0C", "DEE0D", "DEE0E", "DEG01", 
           "DEG02", "DEG03", "DEG04", "DEG05", "DEG0N", "DEG06", "DEG07", 
           "DEG0P", "DEG09", "DEG0A", "DEG0B", "DEG0C", "DEG0D", "DEG0E", 
           "DEG0F", "DEG0G", "DEG0H", "DEG0I", "DEG0J", "DEG0K", "DEG0L", 
           "DEG0M")
)