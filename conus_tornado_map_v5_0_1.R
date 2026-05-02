# ============================================================
# CONUS TORNADO COUNTY MAP
# F/EF1+ tornadoes only
# By: Jon Sammons
#
# v4:   Correct county identification by FIPS. Tornadoes per 10K people.
# v4.1: Log-scale legend breaks for right-skewed count distributions.
# v4.2: Empirical Bayes smoothing. Gi* hotspot analysis replacing
#       Global Moran's I / LISA. Temporal hotspot classification.
#       Decade-by-decade Gi* timeline popups. Hotspot filter dropdown.
# v4.3: Count legend uses whole-number breaks. Breaks pooled across all
#       decades per season for consistent cross-decade colour scale.
# v5.0: Fully parameterized for reproducibility. Decades and census
#       columns are defined in a single configuration block at the top.
#       The classification logic, JavaScript dropdowns, and colour
#       scale pooling all derive from that config automatically.
#       Output HTML uses a versioned filename to avoid overwriting v4.x.
# ============================================================


# ============================================================
# USER CONFIGURATION — edit this block to change the analysis
# ============================================================

# decade_config maps each decade label to its NHGIS census column.
# Add, remove, or change entries here to match your data download.
# NHGIS column naming convention: A00AA{year} for total population.
# To download: https://www.nhgis.org -> Time Series Tables -> Total Population
# Select the census years that correspond to your study decades,
# choose County geography, and download. The column names appear
# in the codebook that comes with the download.
decade_config <- list(
  "1950s" = "A00AA1950",
  "1960s" = "A00AA1960",
  "1970s" = "A00AA1970",
  "1980s" = "A00AA1980",
  "1990s" = "A00AA1990",
  "2000s" = "A00AA2000",
  "2010s" = "A00AA2010",
  "2020s" = "A00AA2020"
)

# study_decades controls which decades are actually included in the
# analysis. Must be a subset of the keys in decade_config above.
# The order here determines the "early" vs "late" split used in the
# temporal hotspot classification.
study_decades <- c("1980s", "1990s", "2000s", "2010s")

# Tornado data year range — must span all study_decades
tornado_year_min <- 1980
tornado_year_max <- 2019

# Hotspot classification thresholds (as fractions of n_decades).
# Persistent = significant in at least this fraction of decades.
# If you have 4 decades: 0.75 * 4 = 3, matching the original rule.
persist_fraction <- 0.75   # e.g. 3 of 4, or 5 of 7
# Early/late split: first floor(n/2) decades = "early", rest = "late"
# For 4 decades: early = 1980s & 1990s, late = 2000s & 2010s (matches original)


# ============================================================
# FILE PATHS — update to match your local folder structure
# ============================================================
county_shp  <- "C:/filepath/to/your/folder/for/TIGER_COUNTY/tl_2024_us_county.shp"
# Shapefile location: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2024.html#list-tab-790442341

pop_csv     <- "C:/filepath/to/your/folder/for/NHGIS_pop_data/nhgis_ts_nominal_county.csv"
# CSV location: https://www.ipums.org/projects/ipums-nhgis/d050.V19.0
# Click GET DATA (Latest Version). Click Get Data again under Start Here.
# Click Time Series Tables. Click the green plus sign next to Total Population.
# Click Continue in your data cart. Select the census years that match your
# study_decades (e.g. 1980, 1990, 2000, 2010 for the default configuration).
# Select at least one geographic level — choose County only for this analysis.
# Download the CSV and point this path at it.
# NOTE: The NHGIS version number in the URL above may change as new releases
# are published. If the link is outdated, go to https://www.nhgis.org and
# navigate to Time Series Tables -> Total Population directly.

tornado_csv <- "C:/filepath/to/your/folder/for/conus_tornado_noaa/1950-2024_all_tornadoes.csv"
# 1950-2024 all tornadoes CSV location: https://www.spc.noaa.gov/wcm/#data
# Scroll approximately three-quarters down the page. Use Ctrl+F and search
# for "1950-2024" to locate the file quickly. Download the CSV and update
# this path. If you are using a different year range, download the
# corresponding file and update tornado_year_min / tornado_year_max above.

out_html    <- "C:/filepath/to/your/folder/for/conus_county_tornado_map_v5.html"
# Where you want to save the Leaflet interactive map.
# This filename is intentionally different from the v4.x output so that
# running this script does not overwrite a previously generated map.


# ============================================================
# 1) Libraries
# ============================================================
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(scales)
library(jsonlite)
library(tibble)
library(spdep)


# ============================================================
# 2) Derived constants (do not edit — computed from config)
# ============================================================
n_decades    <- length(study_decades)
n_early      <- floor(n_decades / 2)
n_late       <- n_decades - n_early
early_decades <- study_decades[seq_len(n_early)]
late_decades  <- study_decades[(n_early + 1):n_decades]
persist_threshold <- ceiling(persist_fraction * n_decades)

cat("Study decades:      ", paste(study_decades, collapse = ", "), "\n")
cat("Early decades:      ", paste(early_decades,  collapse = ", "), "\n")
cat("Late decades:       ", paste(late_decades,   collapse = ", "), "\n")
cat("Persist threshold:  ", persist_threshold, "of", n_decades, "decades\n")


# ============================================================
# 3) Helpers
# ============================================================
`%notin%` <- Negate(`%in%`)
safe_numeric <- function(x) suppressWarnings(as.numeric(x))

find_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

excluded_fips <- c("02", "15", "60", "66", "69", "72", "78")


# ============================================================
# 4) State lookup table
# ============================================================
state_lookup <- tibble(
  STATEFP = c(
    "01","04","05","06","08","09","10","11","12","13","16","17","18","19","20",
    "21","22","23","24","25","26","27","28","29","30","31","32","33","34","35",
    "36","37","38","39","40","41","42","44","45","46","47","48","49","50","51",
    "53","54","55","56"
  ),
  state_name = c(
    "Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
    "District of Columbia","Florida","Georgia","Idaho","Illinois","Indiana","Iowa",
    "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",
    "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
    "New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio",
    "Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota",
    "Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia",
    "Wisconsin","Wyoming"
  )
)


# ============================================================
# 5) Load county shapefile
# ============================================================
counties <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(
    STATEFP     = str_pad(as.character(STATEFP),  2, "left", "0"),
    COUNTYFP    = str_pad(as.character(COUNTYFP), 3, "left", "0"),
    GEOID       = paste0(STATEFP, COUNTYFP),
    county_name = coalesce(as.character(NAME), as.character(NAMELSAD))
  ) %>%
  filter(!STATEFP %in% excluded_fips) %>%
  left_join(state_lookup, by = "STATEFP") %>%
  mutate(state_name = coalesce(state_name, ""))


# ============================================================
# 6) Load NHGIS population
# Reads only the columns that correspond to study_decades,
# so missing census years in the CSV do not cause errors.
# ============================================================
pop_raw <- read_csv(pop_csv, show_col_types = FALSE) %>%
  mutate(
    STATEFP  = str_pad(as.character(STATEFP),  2, "left", "0"),
    COUNTYFP = str_pad(as.character(COUNTYFP), 3, "left", "0"),
    GEOID    = paste0(STATEFP, COUNTYFP)
  )

# Build a lookup of which census columns are available for study_decades.
# unname() is critical: unlist() preserves list names (decade labels) as
# vector names, and a named character vector passed to all_of() inside
# select() or pivot_longer() triggers dplyr's rename behaviour, causing
# columns to be renamed to the decade label before pivot_longer() can
# find them by their original census column name.
available_cols <- unname(unlist(decade_config[study_decades]))
missing_cols   <- available_cols[!available_cols %in% names(pop_raw)]
if (length(missing_cols) > 0) {
  warning("These census columns are missing from pop_csv and will be NA: ",
          paste(missing_cols, collapse = ", "))
}
use_cols <- available_cols[available_cols %in% names(pop_raw)]

pop_long <- pop_raw %>%
  select(GEOID, all_of(use_cols)) %>%
  pivot_longer(
    cols      = all_of(use_cols),
    names_to  = "pop_col",
    values_to = "population"
  ) %>%
  mutate(
    population = safe_numeric(population),
    decade     = names(decade_config)[match(pop_col, unlist(decade_config))]
  ) %>%
  filter(decade %in% study_decades) %>%
  select(GEOID, decade, population)


# ============================================================
# 7) Load tornadoes — F/EF1+ only, study years only
# ============================================================
tornadoes_raw <- read_csv(tornado_csv, show_col_types = FALSE)

state_col  <- find_first_existing(tornadoes_raw, c("stf",  "state_fips",  "STATEFP",  "statefp"))
county_col <- find_first_existing(tornadoes_raw, c("f1",   "county_fips", "COUNTYFP", "countyfp"))
year_col   <- find_first_existing(tornadoes_raw, c("yr",   "year",        "YR",        "YEAR"))
month_col  <- find_first_existing(tornadoes_raw, c("mo",   "month",       "MO",        "MONTH"))
mag_col    <- find_first_existing(tornadoes_raw, c("mag",  "MAG",         "om",        "OM", "f", "F"))

if (any(is.na(c(state_col, county_col, year_col, month_col)))) {
  stop("Required columns missing from tornado CSV. Available: ",
       paste(names(tornadoes_raw), collapse = ", "))
}

# Build decade assignment dynamically from study_decades
# Each label like "1980s" implies years 1980-1989
decade_starts <- as.integer(substr(study_decades, 1, 4))
decade_ends   <- decade_starts + 9

# Construct a named vector: start_year -> decade_label, for case_when
assign_decade <- function(year_vec) {
  result <- rep(NA_character_, length(year_vec))
  for (i in seq_along(study_decades)) {
    in_decade <- !is.na(year_vec) &
                 year_vec >= decade_starts[i] &
                 year_vec <= decade_ends[i]
    result[in_decade] <- study_decades[i]
  }
  result
}

tornadoes <- tornadoes_raw %>%
  mutate(
    state_fips  = str_pad(as.character(.data[[state_col]]),  2, "left", "0"),
    county_fips = str_pad(as.character(.data[[county_col]]), 3, "left", "0"),
    year_num    = suppressWarnings(as.integer(.data[[year_col]])),
    month_num   = suppressWarnings(as.integer(.data[[month_col]])),
    mag_num     = if (!is.na(mag_col)) safe_numeric(.data[[mag_col]]) else NA_real_,
    GEOID       = paste0(state_fips, county_fips)
  ) %>%
  filter(
    !is.na(mag_num), mag_num >= 1,
    !is.na(year_num), !is.na(month_num),
    year_num >= tornado_year_min, year_num <= tornado_year_max,
    str_length(GEOID) == 5,
    substr(GEOID, 1, 2) %notin% excluded_fips
  ) %>%
  mutate(
    decade = assign_decade(year_num),
    season = case_when(
      month_num %in% c(12, 1, 2)  ~ "Winter",
      month_num %in% c(3, 4, 5)   ~ "Spring",
      month_num %in% c(6, 7, 8)   ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  filter(!is.na(decade), !is.na(season))


# ============================================================
# 8) Summarize by county / decade / season
# ============================================================
county_summary <- tornadoes %>%
  group_by(GEOID, decade, season) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(pop_long, by = c("GEOID", "decade")) %>%
  mutate(
    count_per_10000 = if_else(
      !is.na(population) & population > 0,
      count / population * 10000,
      NA_real_
    )
  )


# ============================================================
# 9) Full grid + All Year season
# ============================================================
full_grid <- expand_grid(
  GEOID  = counties$GEOID,
  decade = study_decades,
  season = c("Winter", "Spring", "Summer", "Fall")
)

county_by_season <- full_grid %>%
  left_join(county_summary, by = c("GEOID", "decade", "season")) %>%
  left_join(pop_long, by = c("GEOID", "decade"), suffix = c("", "_pop")) %>%
  mutate(
    population      = coalesce(population, population_pop),
    count           = replace_na(count, 0),
    count_per_10000 = if_else(
      !is.na(population) & population > 0,
      count / population * 10000,
      NA_real_
    )
  ) %>%
  select(GEOID, decade, season, population, count, count_per_10000)

county_all_year <- county_by_season %>%
  group_by(GEOID, decade) %>%
  summarise(
    population = first(population),
    count      = sum(count, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    season          = "All",
    count_per_10000 = if_else(
      !is.na(population) & population > 0,
      count / population * 10000,
      NA_real_
    )
  ) %>%
  select(GEOID, decade, season, population, count, count_per_10000)

county_summary_full <- bind_rows(county_by_season, county_all_year)


# ============================================================
# 9b) Empirical Bayes smoothing of per-10K rate
# EBest() shrinks unstable rates toward the stratum mean.
# Counties with zero events may receive a small non-zero rate
# by borrowing from neighbors (Anselin, 2024a).
# Applied independently within each decade x season stratum.
# ============================================================
county_summary_full <- county_summary_full %>%
  group_by(decade, season) %>%
  mutate(
    count_per_10000 = {
      valid   <- !is.na(population) & population > 0
      eb_rate <- rep(NA_real_, n())
      if (sum(valid) > 1) {
        eb <- EBest(count[valid], population[valid])
        eb_rate[valid] <- eb$est * 10000
      }
      eb_rate
    }
  ) %>%
  ungroup()


# ============================================================
# 9c) Spatial weights + Gi* per decade x season
# Queen contiguity weights built once; Gi* computed per stratum.
# Row-standardized W-style weights (Anselin, 2024b).
# ============================================================
cat("Building spatial weights matrix...\n")
nb <- poly2nb(counties, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

all_seasons <- c("Winter", "Spring", "Summer", "Fall", "All")

gi_rows <- vector("list", n_decades * length(all_seasons))
idx <- 1L

for (dec in study_decades) {
  for (sea in all_seasons) {
    rates <- county_summary_full %>%
      filter(decade == dec, season == sea) %>%
      right_join(tibble(GEOID = counties$GEOID), by = "GEOID") %>%
      arrange(match(GEOID, counties$GEOID)) %>%
      pull(count_per_10000)

    rates_safe <- replace_na(rates, 0)

    gi_z <- tryCatch(
      as.numeric(localG(rates_safe, lw, zero.policy = TRUE)),
      error = function(e) rep(0, length(rates_safe))
    )
    gi_z[is.na(gi_z)] <- 0

    gi_rows[[idx]] <- tibble(
      GEOID    = counties$GEOID,
      decade   = dec,
      season   = sea,
      gi_score = round(gi_z, 3),
      gi_class = case_when(
        gi_z >  2.576 ~ "strong_hot",
        gi_z >  1.960 ~ "moderate_hot",
        gi_z < -2.576 ~ "strong_cold",
        gi_z < -1.960 ~ "moderate_cold",
        TRUE          ~ "none"
      )
    )
    idx <- idx + 1L
  }
}

gi_lookup <- bind_rows(gi_rows) %>%
  mutate(key = paste0(GEOID, "|", decade, "|", season))

cat("Gi* computation complete.\n")


# ============================================================
# 9d) Temporal hotspot classification
# Generalised to work with any number of study_decades.
# early_decades and late_decades are derived from the config.
# Thresholds scale with n_decades via persist_threshold.
# ============================================================
classify_hotspot <- function(gi_vec) {
  # gi_vec: named character vector with one entry per study decade,
  # in chronological order (matching study_decades)
  hot  <- gi_vec %in% c("strong_hot",  "moderate_hot")
  cold <- gi_vec %in% c("strong_cold", "moderate_cold")

  n_hot  <- sum(hot)
  n_cold <- sum(cold)

  # Early / late split indices
  early_idx <- seq_len(n_early)
  late_idx  <- (n_early + 1):n_decades

  eh <- sum(hot[early_idx]);  lh <- sum(hot[late_idx])
  ec <- sum(cold[early_idx]); lc <- sum(cold[late_idx])

  if (n_hot  >= persist_threshold)          return("Persistent Hotspot")
  if (eh == 0 & lh == n_late)              return("Emerging Hotspot")
  if (eh == n_early & lh == 0)             return("Historical Hotspot")
  if (n_hot  >= 1)                          return("Sporadic Hotspot")
  if (n_cold >= persist_threshold)          return("Persistent Coldspot")
  if (ec == 0 & lc == n_late)              return("Emerging Coldspot")
  if (ec == n_early & lc == 0)             return("Historical Coldspot")
  if (n_cold >= 1)                          return("Sporadic Coldspot")
  "No Pattern"
}

first_sig_decade <- function(gi_vec, type = "hot") {
  tgt <- if (type == "hot") c("strong_hot","moderate_hot") else c("strong_cold","moderate_cold")
  d   <- study_decades[gi_vec %in% tgt]
  if (length(d) > 0) d[1] else NA_character_
}

last_sig_decade <- function(gi_vec, type = "hot") {
  tgt <- if (type == "hot") c("strong_hot","moderate_hot") else c("strong_cold","moderate_cold")
  d   <- study_decades[gi_vec %in% tgt]
  if (length(d) > 0) d[length(d)] else NA_character_
}

# Build wide Gi* table with one column per study decade
gi_wide <- gi_lookup %>%
  select(GEOID, season, decade, gi_class) %>%
  pivot_wider(
    names_from   = decade,
    values_from  = gi_class,
    names_prefix = "gi_"
  ) %>%
  mutate(across(starts_with("gi_"), ~replace_na(., "none")))

# Column names for the gi_ columns, in study_decades order
gi_cols <- paste0("gi_", study_decades)

hotspot_class <- gi_wide %>%
  rowwise() %>%
  mutate(
    gi_vec         = list(c_across(all_of(gi_cols))),
    classification = classify_hotspot(gi_vec),
    first_hot_decade  = first_sig_decade(gi_vec, "hot"),
    last_hot_decade   = last_sig_decade(gi_vec,  "hot"),
    first_cold_decade = first_sig_decade(gi_vec, "cold"),
    last_cold_decade  = last_sig_decade(gi_vec,  "cold")
  ) %>%
  ungroup() %>%
  select(-gi_vec) %>%
  left_join(
    counties %>% st_drop_geometry() %>% select(GEOID, county_name, state_name),
    by = "GEOID"
  ) %>%
  mutate(
    county_name = coalesce(county_name, GEOID),
    state_name  = coalesce(state_name, ""),
    key         = paste0(GEOID, "|", season)
  ) %>%
  select(key, GEOID, season, county_name, state_name,
         classification, all_of(gi_cols),
         first_hot_decade, last_hot_decade,
         first_cold_decade, last_cold_decade)


# ============================================================
# 10) UI controls
# Decade dropdown is built dynamically from study_decades.
# ============================================================
decade_options <- lapply(seq_along(study_decades), function(i) {
  dec <- study_decades[i]
  if (i == 1) {
    tags$option(value = dec, dec, selected = "selected")
  } else {
    tags$option(value = dec, dec)
  }
})

metric_control <- tags$div(
  style = "background:white;padding:8px 10px;border-radius:4px;
            box-shadow:0 1px 5px rgba(0,0,0,0.35);
            font-family:Arial,sans-serif;font-size:13px;margin-bottom:8px;",
  tags$label(`for` = "metric_select",
             style = "display:block;font-weight:bold;margin-bottom:4px;", "Metric"),
  tags$select(id = "metric_select", style = "width:230px;",
              tags$option(value = "count",           "F/EF1+ tornado count"),
              tags$option(value = "count_per_10000", "F/EF1+ per 10,000 (EB smoothed)"),
              tags$option(value = "gi_score",        "Gi* Hotspot Score"),
              tags$option(value = "hotspot_class",   "Hotspot Classification")
  )
)

filter_control <- tags$div(
  id    = "filter_control_div",
  style = "background:white;padding:8px 10px;border-radius:4px;
            box-shadow:0 1px 5px rgba(0,0,0,0.35);
            font-family:Arial,sans-serif;font-size:13px;margin-bottom:8px;
            display:none;",
  tags$label(`for` = "filter_select",
             style = "display:block;font-weight:bold;margin-bottom:4px;",
             "Filter by Class"),
  tags$select(id = "filter_select", style = "width:230px;",
              tags$option(value = "All",                 "All Classifications", selected = "selected"),
              tags$option(value = "Persistent Hotspot",  "Persistent Hotspot"),
              tags$option(value = "Emerging Hotspot",    "Emerging Hotspot"),
              tags$option(value = "Historical Hotspot",  "Historical Hotspot"),
              tags$option(value = "Sporadic Hotspot",    "Sporadic Hotspot"),
              tags$option(value = "Persistent Coldspot", "Persistent Coldspot"),
              tags$option(value = "Emerging Coldspot",   "Emerging Coldspot"),
              tags$option(value = "Historical Coldspot", "Historical Coldspot"),
              tags$option(value = "Sporadic Coldspot",   "Sporadic Coldspot"),
              tags$option(value = "No Pattern",          "No Pattern")
  )
)

season_control <- tags$div(
  style = "background:white;padding:8px 10px;border-radius:4px;
            box-shadow:0 1px 5px rgba(0,0,0,0.35);
            font-family:Arial,sans-serif;font-size:13px;margin-bottom:8px;",
  tags$label(`for` = "season_select",
             style = "display:block;font-weight:bold;margin-bottom:4px;", "Season"),
  tags$select(id = "season_select", style = "width:230px;",
              tags$option(value = "All",    "All Year",          selected = "selected"),
              tags$option(value = "Spring", "Spring (Mar-May)"),
              tags$option(value = "Summer", "Summer (Jun-Aug)"),
              tags$option(value = "Fall",   "Fall (Sep-Nov)"),
              tags$option(value = "Winter", "Winter (Dec-Feb)")
  )
)

decade_control <- tags$div(
  style = "background:white;padding:8px 10px;border-radius:4px;
            box-shadow:0 1px 5px rgba(0,0,0,0.35);
            font-family:Arial,sans-serif;font-size:13px;",
  tags$label(`for` = "decade_select",
             style = "display:block;font-weight:bold;margin-bottom:4px;", "Decade"),
  do.call(tags$select, c(list(id = "decade_select", style = "width:230px;"), decade_options))
)


# ============================================================
# 11) Serialize geometry + tabular lookups
# study_decades_json is injected into JS so the timeline and
# colour-scale pooling work for any set of decades.
# ============================================================
county_geo <- counties %>%
  st_transform(3857) %>%
  st_simplify(dTolerance = 1500, preserveTopology = TRUE) %>%
  st_transform(4326) %>%
  select(GEOID, county_name, state_name, geometry)

geojson_file   <- tempfile(fileext = ".geojson")
st_write(county_geo, geojson_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
geojson_string <- paste(readLines(geojson_file, warn = FALSE), collapse = "")

data_lookup <- county_summary_full %>%
  left_join(
    counties %>% st_drop_geometry() %>% select(GEOID, county_name, state_name),
    by = "GEOID"
  ) %>%
  mutate(
    county_name = coalesce(county_name, GEOID),
    state_name  = coalesce(state_name, ""),
    key         = paste0(GEOID, "|", decade, "|", season)
  ) %>%
  select(key, GEOID, decade, season, county_name, state_name,
         population, count, count_per_10000)

data_lookup_json    <- toJSON(data_lookup, na = "null", digits = 4)
gi_lookup_json      <- toJSON(gi_lookup %>% select(key, GEOID, decade, season, gi_score, gi_class),
                              na = "null", digits = 3)
hotspot_class_json  <- toJSON(hotspot_class, na = "null", digits = 4)
study_decades_json  <- toJSON(study_decades)   # injected into JS as __DECADES__


# ============================================================
# 12) Base map
# ============================================================
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -96, lat = 38, zoom = 4) %>%
  addControl(metric_control,  position = "topright") %>%
  addControl(filter_control,  position = "topright") %>%
  addControl(season_control,  position = "topright") %>%
  addControl(decade_control,  position = "topright")


# ============================================================
# 13) JavaScript — all decade references driven by __DECADES__
# ============================================================
js_template <- r"[
function(el, x) {
  var map      = this;
  var geo      = __GEOJSON__;
  var rows     = __DATAROWS__;
  var giRows   = __GIROWS__;
  var hsRows   = __HSROWS__;
  var DECADES  = __DECADES__;   // injected from R: array of decade strings

  // Fast lookups
  var lkp   = {};
  var giLkp = {};
  var hsLkp = {};

  rows.forEach(function(r)   { lkp[r.key]   = r; });
  giRows.forEach(function(r) { giLkp[r.key] = r; });
  hsRows.forEach(function(r) { hsLkp[r.key] = r; });

  var legend        = null;
  var regionControl = null;

  // Region FIPS
  var TA_FIPS = ["48","40","20","31","08","46","38","19","29"];
  var DX_FIPS = ["28","01","05","47","22","13","45","37"];

  // ── Colour palettes ──────────────────────────────────────
  var RATE_COLORS = ["#ffffcc","#ffeda0","#fed976","#feb24c",
                     "#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026"];

  var GI_COLORS = {
    "strong_hot":    "#b2182b",
    "moderate_hot":  "#ef8a62",
    "none":          "#f7f7f7",
    "moderate_cold": "#67a9cf",
    "strong_cold":   "#2166ac"
  };

  var HS_COLORS = {
    "Persistent Hotspot":  "#b2182b",
    "Emerging Hotspot":    "#ef8a62",
    "Historical Hotspot":  "#8c510a",
    "Sporadic Hotspot":    "#fddbc7",
    "Persistent Coldspot": "#2166ac",
    "Emerging Coldspot":   "#67a9cf",
    "Historical Coldspot": "#4575b4",
    "Sporadic Coldspot":   "#abd9e9",
    "No Pattern":          "#d9d9d9"
  };

  var GI_LABEL = {
    "strong_hot":    "Strong Hotspot",
    "moderate_hot":  "Moderate Hotspot",
    "none":          "Not Significant",
    "moderate_cold": "Moderate Coldspot",
    "strong_cold":   "Strong Coldspot"
  };

  // ── Log-scale breaks ─────────────────────────────────────
  function getBreaks(vals) {
    var clean = vals.map(Number)
      .filter(function(v) { return isFinite(v) && v > 0; })
      .sort(function(a, b) { return a - b; });
    if (!clean.length) return [1, 2, 4, 8, 15, 25, 40, 60];
    var ceiling = clean[Math.min(Math.floor(clean.length * 0.98), clean.length - 1)];
    var logCeil = Math.log(ceiling + 1);
    var breaks  = [];
    for (var i = 1; i <= 8; i++) {
      breaks.push(Math.exp(logCeil * i / 8) - 1);
    }
    return breaks;
  }

  function getRateColor(val, breaks) {
    var v = Number(val);
    if (!isFinite(v) || v <= 0) return "#d9d9d9";
    for (var i = 0; i < breaks.length; i++) {
      if (v <= breaks[i]) return RATE_COLORS[i];
    }
    return RATE_COLORS[RATE_COLORS.length - 1];
  }

  function metricLabel(m) {
    if (m === "count")           return "F/EF1+ tornado count";
    if (m === "count_per_10000") return "F/EF1+ per 10,000 - EB smoothed";
    if (m === "gi_score")        return "Gi* Hotspot Score";
    return "Hotspot Classification";
  }

  function fmt(v, d) {
    var n = Number(v);
    if (v === null || v === undefined || !isFinite(n)) return "NA";
    return n.toLocaleString(undefined, {maximumFractionDigits: d || 0});
  }

  // ── Gi* dot badge ────────────────────────────────────────
  function giDot(cls) {
    var c   = GI_COLORS[cls] || "#d9d9d9";
    var bdr = (cls === "none") ? "border:1px solid #aaa;" : "";
    return "<span style='display:inline-block;width:10px;height:10px;" +
           "border-radius:50%;background:" + c + ";" + bdr +
           "margin-right:5px;vertical-align:middle;'></span>";
  }

  // ── Decade-by-decade timeline — driven by DECADES array ──
  function buildTimeline(geoid, season) {
    var html =
      "<table style='border-collapse:collapse;margin-top:6px;width:100%;font-size:11px;'>" +
      "<tr style='border-bottom:1px solid #ddd;'>" +
      "<th style='text-align:left;padding:2px 10px 2px 0;'>Decade</th>" +
      "<th style='text-align:left;padding:2px 0;'>Gi* Status</th></tr>";
    DECADES.forEach(function(dec) {
      var key  = geoid + "|" + dec + "|" + season;
      var gr   = giLkp[key];
      var cls  = gr ? gr.gi_class : "none";
      var lbl  = GI_LABEL[cls] || "Not Significant";
      var zTxt = gr ? " (z = " + fmt(gr.gi_score, 2) + ")" : "";
      html +=
        "<tr><td style='padding:3px 10px 2px 0;'>" + dec + "</td>" +
        "<td style='padding:3px 0 2px;'>" + giDot(cls) + lbl + zTxt + "</td></tr>";
    });
    html += "</table>";
    return html;
  }

  // ── Legend ───────────────────────────────────────────────
  function buildLegend(metric, breaks) {
    if (legend) map.removeControl(legend);
    legend = L.control({position: "bottomright"});
    legend.onAdd = function() {
      var div = L.DomUtil.create("div", "info legend");
      div.style.cssText =
        "background:white;padding:8px 10px;border-radius:4px;" +
        "box-shadow:0 1px 5px rgba(0,0,0,0.35);line-height:22px;" +
        "color:#333;font-family:Arial,sans-serif;font-size:12px;min-width:210px;";

      div.innerHTML = "<strong>" + metricLabel(metric) + "</strong><br>";

      function swatch(color, label, extra) {
        extra = extra || "";
        return "<i style='background:" + color + ";width:14px;height:14px;" +
               "float:left;margin-right:8px;opacity:0.9;margin-top:3px;" + extra + "'></i>" +
               label + "<br>";
      }

      if (metric === "count" || metric === "count_per_10000") {
        div.innerHTML += swatch("#d9d9d9", "No activity");
        for (var i = 0; i < RATE_COLORS.length; i++) {
          var lbl;
          var fmtB = (metric === "count")
            ? function(v) { return Math.max(1, Math.round(v)).toLocaleString(); }
            : function(v) { return Number(v).toFixed(2); };
          if (i === 0) {
            lbl = "> 0 to " + fmtB(breaks[0]);
          } else if (i === RATE_COLORS.length - 1) {
            lbl = "> " + fmtB(breaks[breaks.length - 1]);
          } else {
            lbl = fmtB(breaks[i-1]) + " \u2013 " + fmtB(breaks[i]);
          }
          div.innerHTML += swatch(RATE_COLORS[i], lbl);
        }

      } else if (metric === "gi_score") {
        var giOrder = ["strong_hot","moderate_hot","none","moderate_cold","strong_cold"];
        var giDesc  = [
          "Strong Hotspot  (p < 0.01)",
          "Moderate Hotspot  (p < 0.05)",
          "Not Significant",
          "Moderate Coldspot  (p < 0.05)",
          "Strong Coldspot  (p < 0.01)"
        ];
        for (var j = 0; j < giOrder.length; j++) {
          var ex = (giOrder[j] === "none") ? "border:1px solid #aaa;" : "";
          div.innerHTML += swatch(GI_COLORS[giOrder[j]], giDesc[j], ex);
        }

      } else {
        var hsOrder = [
          "Persistent Hotspot","Emerging Hotspot","Historical Hotspot","Sporadic Hotspot",
          "Persistent Coldspot","Emerging Coldspot","Historical Coldspot","Sporadic Coldspot",
          "No Pattern"
        ];
        hsOrder.forEach(function(k) {
          div.innerHTML += swatch(HS_COLORS[k], k);
        });
      }
      return div;
    };
    legend.addTo(map);
  }

  // ── Regional averages panel ──────────────────────────────
  function regionMean(fipsArr, decade, season, metric) {
    var total = 0, n = 0;
    rows.forEach(function(r) {
      if (r.decade !== decade || r.season !== season) return;
      if (fipsArr.indexOf(r.GEOID.substring(0, 2)) === -1) return;
      var v = Number(r[metric]);
      if (isFinite(v)) { total += v; n++; }
    });
    return n > 0 ? (total / n) : null;
  }

  function buildRegionPanel(decade, season, metric) {
    if (regionControl) { map.removeControl(regionControl); regionControl = null; }
    if (metric === "gi_score" || metric === "hotspot_class") return;

    regionControl = L.control({position: "bottomleft"});
    regionControl.onAdd = function() {
      var div = L.DomUtil.create("div", "region-panel");
      div.style.cssText =
        "background:white;padding:10px 14px;border-radius:4px;" +
        "box-shadow:0 1px 5px rgba(0,0,0,0.35);" +
        "font-family:Arial,sans-serif;font-size:12px;color:#333;min-width:320px;";

      var isRate      = (metric === "count_per_10000");
      var digits      = isRate ? 2 : 1;
      var colHead     = isRate ? "per 10,000 people" : "tornado count";
      var seasonLabel = (season === "All") ? "All Year" : season;
      var taVal = regionMean(TA_FIPS, decade, season, metric);
      var dxVal = regionMean(DX_FIPS, decade, season, metric);
      var taTxt = taVal !== null ? Number(taVal).toFixed(digits) : "NA";
      var dxTxt = dxVal !== null ? Number(dxVal).toFixed(digits) : "NA";

      div.innerHTML =
        "<strong style='font-size:13px;'>Regional Averages &mdash; " + colHead + "</strong><br>" +
        "<span style='font-size:11px;color:#666;'>Mean across counties &bull; " +
        decade + " &bull; " + seasonLabel + "</span>" +
        "<table style='border-collapse:collapse;margin-top:6px;width:100%;'>" +
          "<tr style='border-bottom:1px solid #ddd;'>" +
            "<th style='text-align:left;padding:3px 8px 3px 0;'>Region</th>" +
            "<th style='text-align:right;padding:3px 8px;'>Avg</th>" +
            "<th style='text-align:left;padding:3px 0;color:#666;font-weight:normal;'>States</th>" +
          "</tr>" +
          "<tr>" +
            "<td style='padding:4px 8px 2px 0;'>Tornado Alley</td>" +
            "<td style='text-align:right;padding:4px 8px 2px;font-weight:bold;'>" + taTxt + "</td>" +
            "<td style='padding:4px 0 2px;color:#666;'>TX, OK, KS, NE, CO, SD, ND, IA, MO</td>" +
          "</tr>" +
          "<tr>" +
            "<td style='padding:2px 8px 0 0;'>Dixie Alley</td>" +
            "<td style='text-align:right;padding:2px 8px 0;font-weight:bold;'>" + dxTxt + "</td>" +
            "<td style='padding:2px 0 0;color:#666;'>MS, AL, AR, TN, LA, GA, SC, NC</td>" +
          "</tr>" +
        "</table>";
      return div;
    };
    regionControl.addTo(map);
  }

  // ── GeoJSON layer ────────────────────────────────────────
  var renderer = L.canvas({padding: 0.5});

  var layer = L.geoJSON(geo, {
    renderer: renderer,
    style: {fillColor:"#d9d9d9", weight:0.5, color:"#666666", fillOpacity:0.8, opacity:1},
    onEachFeature: function(feat, lyr) {
      var geoid = feat.properties.GEOID;
      var cname = feat.properties.county_name || geoid;
      var sname = feat.properties.state_name  || "";

      lyr.bindTooltip(cname + ", " + sname, {sticky: true});

      lyr.on("click", function() {
        var metric = document.getElementById("metric_select").value;
        var season = document.getElementById("season_select").value;
        var decade = document.getElementById("decade_select").value;
        var rKey   = geoid + "|" + decade + "|" + season;
        var hsKey  = geoid + "|" + season;
        var row    = lkp[rKey]    || {};
        var hsRow  = hsLkp[hsKey] || {};
        var cn     = row.county_name  || hsRow.county_name  || cname;
        var sn     = row.state_name   || hsRow.state_name   || sname;

        var html = "<strong>" + cn + ", " + sn + "</strong><br>GEOID: " + geoid + "<br>";

        if (metric === "count" || metric === "count_per_10000") {
          html +=
            "Decade: "            + decade                     + "<br>" +
            "Season: "            + season                     + "<br>" +
            "F/EF1+ count: "      + fmt(row.count, 0)          + "<br>" +
            "Population: "        + fmt(row.population, 0)     + "<br>" +
            "F/EF1+ per 10,000: " + fmt(row.count_per_10000, 3);

        } else if (metric === "gi_score") {
          var giRow = giLkp[rKey] || {};
          var cls   = giRow.gi_class || "none";
          html +=
            "Season: " + season + "<br>" +
            "<b>Gi* Status (" + decade + "):</b> " +
            giDot(cls) + (GI_LABEL[cls] || "Unknown") +
            "  (z&nbsp;=&nbsp;" + fmt(giRow.gi_score, 2) + ")<br>" +
            "<b>Decade-by-decade timeline:</b>" +
            buildTimeline(geoid, season);

        } else {
          var cls       = hsRow.classification    || "No Pattern";
          var firstHot  = hsRow.first_hot_decade  || null;
          var lastHot   = hsRow.last_hot_decade   || null;
          var firstCold = hsRow.first_cold_decade || null;
          var lastCold  = hsRow.last_cold_decade  || null;

          html +=
            "Season: " + season + "<br>" +
            "<b>Classification:</b> " +
            "<span style='color:" + (HS_COLORS[cls] || "#333") + ";font-weight:bold;'>" +
            cls + "</span><br>";

          if (firstHot)
            html += "First hotspot decade: <b>" + firstHot + "</b><br>";
          if (lastHot && lastHot !== firstHot)
            html += "Last hotspot decade: <b>"  + lastHot  + "</b><br>";
          if (firstCold)
            html += "First coldspot decade: <b>" + firstCold + "</b><br>";
          if (lastCold && lastCold !== firstCold)
            html += "Last coldspot decade: <b>"  + lastCold  + "</b><br>";

          html += "<b>Decade-by-decade timeline:</b>" + buildTimeline(geoid, season);
        }

        lyr.bindPopup(html, {maxWidth: 340}).openPopup();
      });

      lyr.on({
        mouseover: function(e) {
          var metric = document.getElementById("metric_select").value;
          var season = document.getElementById("season_select").value;
          var gid    = e.target.feature.properties.GEOID;
          var cn2    = e.target.feature.properties.county_name || gid;
          var sn2    = e.target.feature.properties.state_name  || "";
          var tip    = cn2 + ", " + sn2;

          if (metric === "gi_score") {
            var decade = document.getElementById("decade_select").value;
            var gr     = giLkp[gid + "|" + decade + "|" + season] || {};
            var cls    = gr.gi_class || "none";
            tip += " \u2014 " + (GI_LABEL[cls] || "Not Significant");
          } else if (metric === "hotspot_class") {
            var hr  = hsLkp[gid + "|" + season] || {};
            var cls = hr.classification || "No Pattern";
            tip += " \u2014 " + cls;
            var since = hr.first_hot_decade || hr.first_cold_decade || null;
            if (since) tip += " (since " + since + ")";
          }

          e.target.setTooltipContent(tip);
          e.target.setStyle({weight:2, color:"#000000", fillOpacity:0.95});
        },
        mouseout: function(e) {
          e.target.setStyle({
            weight:      0.5,
            color:       "#666666",
            fillOpacity: 0.8,
            fillColor:   e.target._currentFillColor || "#d9d9d9"
          });
        }
      });
    }
  }).addTo(map);

  // ── Restyle layer ────────────────────────────────────────
  function redrawMap() {
    var metric    = document.getElementById("metric_select").value;
    var season    = document.getElementById("season_select").value;
    var decade    = document.getElementById("decade_select").value;
    var filterEl  = document.getElementById("filter_select");
    var filterVal = filterEl ? filterEl.value : "All";

    var decDiv = document.getElementById("decade_select").parentElement;
    if (metric === "hotspot_class") {
      decDiv.style.opacity       = "0.45";
      decDiv.style.pointerEvents = "none";
    } else {
      decDiv.style.opacity       = "1";
      decDiv.style.pointerEvents = "auto";
    }

    var filterDiv = document.getElementById("filter_control_div");
    if (filterDiv) {
      filterDiv.style.display = (metric === "hotspot_class") ? "block" : "none";
    }

    var breaks = null;
    if (metric === "count" || metric === "count_per_10000") {
      // Pool values across ALL decades (for the current season) so the
      // colour scale stays fixed when the user switches decades.
      var vals = [];
      DECADES.forEach(function(dec) {
        geo.features.forEach(function(f) {
          var row = lkp[f.properties.GEOID + "|" + dec + "|" + season];
          if (row) vals.push(row[metric]);
        });
      });
      breaks = getBreaks(vals);
      if (metric === "count") {
        breaks = breaks.map(function(b) { return Math.max(1, Math.round(b)); });
        breaks = breaks.filter(function(v, i, a) { return i === 0 || v !== a[i - 1]; });
        while (breaks.length < RATE_COLORS.length) {
          breaks.push(breaks[breaks.length - 1] + 1);
        }
      }
    }

    layer.eachLayer(function(lyr) {
      var geoid = lyr.feature.properties.GEOID;
      var color = "#d9d9d9";

      if (metric === "count" || metric === "count_per_10000") {
        var row = lkp[geoid + "|" + decade + "|" + season];
        color = getRateColor(row ? row[metric] : null, breaks);

      } else if (metric === "gi_score") {
        var gr = giLkp[geoid + "|" + decade + "|" + season];
        color  = GI_COLORS[gr ? gr.gi_class : "none"] || "#d9d9d9";

      } else {
        var hr  = hsLkp[geoid + "|" + season];
        var cls = hr ? hr.classification : "No Pattern";
        color = (filterVal !== "All" && cls !== filterVal)
                  ? "#eeeeee"
                  : (HS_COLORS[cls] || "#d9d9d9");
      }

      lyr._currentFillColor = color;
      lyr.setStyle({fillColor: color});
    });

    buildLegend(metric, breaks);
    buildRegionPanel(decade, season, metric);
  }

  document.getElementById("metric_select").addEventListener("change", redrawMap);
  document.getElementById("season_select").addEventListener("change", redrawMap);
  document.getElementById("decade_select").addEventListener("change", redrawMap);
  var filterEl = document.getElementById("filter_select");
  if (filterEl) filterEl.addEventListener("change", redrawMap);

  redrawMap();
}
]"

# Substitute all placeholders
js_code <- gsub("__GEOJSON__",  geojson_string,     js_template, fixed = TRUE)
js_code <- gsub("__DATAROWS__", data_lookup_json,   js_code,     fixed = TRUE)
js_code <- gsub("__GIROWS__",   gi_lookup_json,     js_code,     fixed = TRUE)
js_code <- gsub("__HSROWS__",   hotspot_class_json, js_code,     fixed = TRUE)
js_code <- gsub("__DECADES__",  study_decades_json, js_code,     fixed = TRUE)

m <- m %>% onRender(js_code)


# ============================================================
# 14) Save
# ============================================================
out_dir <- dirname(out_html)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveWidget(m, file = out_html, selfcontained = TRUE)

cat("\nMap written to:\n", out_html, "\n")
cat("HTML exists:", file.exists(out_html), "\n")
if (file.exists(out_html)) browseURL(out_html)


# ============================================================
# 15) Validation
# ============================================================
cat("\nStudy configuration:\n")
cat("  Decades analyzed: ", paste(study_decades, collapse = ", "), "\n")
cat("  Early decades:    ", paste(early_decades,  collapse = ", "), "\n")
cat("  Late decades:     ", paste(late_decades,   collapse = ", "), "\n")
cat("  Persist threshold:", persist_threshold, "of", n_decades, "\n")

cat("\nSample counties:\n")
print(counties %>% st_drop_geometry() %>% select(GEOID, county_name, state_name) %>% head(8))

cat("\nF/EF1+ tornado records used:", nrow(tornadoes), "\n")
cat("Lookup rows (incl. All Year):", nrow(county_summary_full), "\n")
cat("Gi* rows computed:           ", nrow(gi_lookup), "\n")
cat("Hotspot classification rows: ", nrow(hotspot_class), "\n")

cat("\nHotspot classification breakdown (All Year):\n")
print(
  hotspot_class %>%
    filter(season == "All") %>%
    count(classification, sort = TRUE)
)

cat("\nCounties missing county_name:", sum(is.na(counties$county_name) | counties$county_name == ""), "\n")
cat("Counties missing state_name: ", sum(is.na(counties$state_name)  | counties$state_name  == ""), "\n")
cat("\nDone.\n")
