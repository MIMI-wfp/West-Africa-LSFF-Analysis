# =============================================================================
# Food Vehicle Coverage & Consumption Analysis — Core Functions
# =============================================================================
# These helpers work for ANY food vehicle (rice, wheat flour, oil, millet, etc.)
# across any set of countries with household survey data.
# =============================================================================

############################################################
# Install + Load Required Packages (pacman)
############################################################

# # Install pacman if not already installed
# if (!requireNamespace("pacman", quietly = TRUE)) {
#   install.packages("pacman")
# }
# 
# # Use pacman to install & load all required packages
# pacman::p_load(
#   tidyverse,        # Data manipulation + plotting
#   survey,           # Survey design
#   srvyr,            # Tidy survey analysis
#   sf,               # Spatial data
#   biscale,          # Bivariate maps
#   cowplot,          # Plot composition
#   rnaturalearth,    # Country Admin boundaries
#   rnaturalearthdata # Additional Natural Earth datasets
# )

# -----------------------------------------------------------------------------
# 1. Load & pre-process household food consumption data
# -----------------------------------------------------------------------------
# Divides quantities by Adult Female Equivalent (AFE) to normalise to per-adult
# consumption, then merges in household metadata.
#
# Args:
#   hh_info_path : path to CSV with household info (must contain: hhid, afe,
#                  adm1, survey_wgt, ea, res)
#   cons_path    : path to CSV with consumption records (must contain: hhid,
#                  item_code, quantity_g)
#
# Returns: a tibble with one row per hhid × item_code
# -----------------------------------------------------------------------------
process_food_consumption <- function(hh_info_path, cons_path) {
  hh_info <- read.csv(hh_info_path)

  read.csv(cons_path) %>%
    left_join(hh_info, by = "hhid") %>%
    mutate(quantity_g = quantity_g / afe) %>%
    select(hhid, item_code, quantity_g, adm1, survey_wgt, ea, res)
}


# -----------------------------------------------------------------------------
# 2. Calculate survey-weighted REACH (% of households consuming the vehicle)
# -----------------------------------------------------------------------------
# Args:
#   data          : output of process_food_consumption()
#   vehicle_codes : integer vector of item_codes that define the vehicle
#   ...           : column names (unquoted): hhid_col, adm1_col,
#                   survey_wgt_col, ea_col, res_col, item_code_col
#
# Returns: tibble with adm1 and <vehicle>_reach_pct
# -----------------------------------------------------------------------------
calculate_reach <- function(data,
                            vehicle_codes,
                            vehicle_name  = "vehicle",
                            hhid_col      = hhid,
                            adm1_col      = adm1,
                            survey_wgt_col = survey_wgt,
                            ea_col        = ea,
                            res_col       = res,
                            item_code_col = item_code) {

  reach_col <- paste0(vehicle_name, "_reach_pct")

  consumers <- data %>%
    filter({{ item_code_col }} %in% vehicle_codes) %>%
    mutate(.consumed = 1L)

  all_hh <- data %>%
    filter(!({{ hhid_col }} %in% consumers[[rlang::as_name(enquo(hhid_col))]])) %>%
    mutate(.consumed = 0L) %>%
    bind_rows(consumers)

  hh_status <- all_hh %>%
    group_by({{ hhid_col }}, {{ adm1_col }},
             {{ survey_wgt_col }}, {{ ea_col }}, {{ res_col }}) %>%
    summarise(.consumed = max(.consumed), .groups = "drop")

  hh_status %>%
    as_survey_design(ids    = {{ ea_col }},
                     strata = {{ res_col }},
                     weights = {{ survey_wgt_col }},
                     nest   = TRUE) %>%
    group_by({{ adm1_col }}) %>%
    summarise(!!reach_col := survey_mean(.consumed, proportion = TRUE) * 100) %>%
    select(-ends_with("_se"))
}


# -----------------------------------------------------------------------------
# 3. Calculate survey-weighted mean INTAKE (g/day) among consumers
# -----------------------------------------------------------------------------
# Args:
#   data          : output of process_food_consumption()
#   vehicle_codes : integer vector of item_codes that define the vehicle
#   vehicle_name  : string label used to name the output column
#   ...           : column names (unquoted)
#
# Returns: tibble with adm1 and mean_<vehicle>_g
# -----------------------------------------------------------------------------
calculate_intake <- function(data,
                             vehicle_codes,
                             vehicle_name   = "vehicle",
                             adm1_col       = adm1,
                             quantity_col   = quantity_g,
                             survey_wgt_col = survey_wgt,
                             ea_col         = ea,
                             res_col        = res,
                             item_code_col  = item_code) {

  intake_col <- paste0("mean_", vehicle_name, "_g")

  data %>%
    filter({{ item_code_col }} %in% vehicle_codes) %>%
    as_survey_design(ids    = {{ ea_col }},
                     strata = {{ res_col }},
                     weights = {{ survey_wgt_col }},
                     nest   = TRUE) %>%
    group_by({{ adm1_col }}) %>%
    summarise(!!intake_col := survey_mean({{ quantity_col }})) %>%
    select(-ends_with("_se"))
}


# -----------------------------------------------------------------------------
# 4. Build the combined reach × intake spatial table for one country
# -----------------------------------------------------------------------------
# Args:
#   reach_data    : output of calculate_reach()
#   intake_data   : output of calculate_intake()
#   shapefile     : sf object with an `adm1` column
#   vehicle_name  : string label (must match what was passed to the above fns)
#   reach_breaks  : numeric vector of break points for reach % bins
#   intake_breaks : numeric vector of break points for intake g bins
#   intake_labels : character vector of labels (length = length(intake_breaks)-1)
#
# Returns: an sf object ready for bi_class()
# -----------------------------------------------------------------------------
build_reach_intake_sf <- function(reach_data,
                                  intake_data,
                                  shapefile,
                                  vehicle_name   = "vehicle",
                                  reach_breaks   = c(0, 25, 50, 75, 100),
                                  intake_breaks  = c(-Inf, 75, 149, 300, Inf),
                                  intake_labels  = c("<75", "75-149", "150-300", ">300")) {

  reach_col  <- paste0(vehicle_name, "_reach_pct")
  intake_col <- paste0("mean_", vehicle_name, "_g")

  reach_data %>%
    left_join(intake_data, by = "adm1") %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
    mutate(
      reach_bins  = cut(.data[[reach_col]],
                        breaks          = reach_breaks,
                        include.lowest  = TRUE),
      intake_bins = cut(.data[[intake_col]],
                        breaks          = intake_breaks,
                        labels          = intake_labels,
                        include.lowest  = TRUE),
      adm1 = as.character(adm1)
    ) %>%
    select(adm1, all_of(intake_col), reach_bins, intake_bins) %>%
    left_join(shapefile, by = "adm1") %>%
    st_as_sf()
}


# -----------------------------------------------------------------------------
# 5. Draw a bivariate choropleth map for one country
# -----------------------------------------------------------------------------
# Args:
#   sf_data      : output of build_reach_intake_sf() after bi_class()
#   outline_sf   : sf object used for country/adm1 border overlay
#   country_name : string for the subtitle
#   palette      : biscale palette name (default "BlueGold")
#   dim          : bivariate grid dimension (default 4)
#   lwd          : border line width
#
# Returns: a list with $map and $legend ggplot objects
# -----------------------------------------------------------------------------
make_bivariate_map <- function(sf_data,
                               outline_sf,
                               country_name,
                               vehicle_label = "Vehicle",
                               palette       = "BlueGold",
                               dim           = 4,
                               lwd           = 0.5) {

  bi_data   <- bi_class(sf_data, x = reach_bins, y = intake_bins, dim = dim)
  break_vals <- bi_class_breaks(sf_data, x = reach_bins, y = intake_bins, dim = dim)

  map_plot <- ggplot() +
    geom_sf(data    = bi_data,
            mapping = aes(fill = bi_class),
            color   = NA, show.legend = FALSE) +
    bi_scale_fill(pal = palette, dim = dim) +
    bi_theme() +
    geom_sf(data = outline_sf, fill = NA, color = "black", lwd = lwd) +
    labs(subtitle = paste("Coverage and Consumption of", vehicle_label, "in", country_name))

  legend_plot <- bi_legend(
    pal    = palette,
    dim    = dim,
    xlab   = "Higher Reach (%) ",
    ylab   = paste0("Higher Consumption (g) "),
    size   = 8,
    breaks = break_vals
  )

  list(map = map_plot, legend = legend_plot, bi_data = bi_data)
}


# -----------------------------------------------------------------------------
# 6. Combine map + legend into a single ggdraw object
# -----------------------------------------------------------------------------
assemble_map <- function(map_obj,
                         legend_x = 0.65, legend_y = 0.2,
                         legend_w = 0.45, legend_h = 0.2) {
  ggdraw() +
    draw_plot(map_obj$map) +
    draw_plot(map_obj$legend,
              x = legend_x, y = legend_y,
              width  = legend_w, height = legend_h)
}

# =============================================================================
# Country Configuration
# =============================================================================
# Define each country's:
#   - file paths for hh_info and food consumption CSVs
#   - item codes for each vehicle
#   - shapefile source (via rnaturalearth or a local .shp)
#   - adm1 name remapping (survey names → shapefile names)
# To add a new vehicle, add item codes under each country's `vehicles` list.
# =============================================================================

COUNTRY_CONFIG <- list(
  
  senegal = list(
    iso3           = "SEN",
    hh_info_path   = "data/sen_ehcvm2122_hh_info.csv",
    cons_path      = "data/sen_food_consumption.csv",
    shapefile_type = "ne_states",     # "ne_states" | "ne_countries" | "local"
    shapefile_path = NULL,            # used when shapefile_type = "local"
    ne_country     = "Senegal",
    adm1_recode    = c(
      "Kédougou" = "Kedougou",
      "Sédhiou"  = "Sedhiou",
      "Thiès"    = "Thies"
    ),
    vehicles = list(
      rice       = c(1, 2, 3, 4)
    )
  ),
  
  niger = list(
    iso3           = "NER",
    hh_info_path   = "data/ner_ehcvm2122_hh_info.csv",
    cons_path      = "data/ner_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Niger",
    adm1_recode    = c("Tillabéri" = "Tillaberi"),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  mali = list(
    iso3           = "MLI",
    hh_info_path   = "data/mli_ehcvm2122_hh_info.csv",
    cons_path      = "data/mli_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Mali",
    adm1_recode    = c(
      "Timbuktu" = "Tombouctou",
      "Ségou"    = "Segou"
    ),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  togo = list(
    iso3           = "TGO",
    hh_info_path   = "data/tgo_ehcvm2122_hh_info.csv",
    cons_path      = "data/tgo_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Togo",
    adm1_recode    = c("Centre" = "Centrale"),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  benin = list(
    iso3           = "BEN",
    hh_info_path   = "data/ben_ehcvm2122_hh_info.csv",
    cons_path      = "data/ben_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Benin",
    adm1_recode    = c(
      "Ouémé"   = "Oueme",
      "Atakora" = "Atacora",
      "Kouffo"  = "Couffo"
    ),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  guinea_bissau = list(
    iso3           = "GNB",
    hh_info_path   = "data/gnb_ehcvm2122_hh_info.csv",
    cons_path      = "data/gnb_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Guinea-Bissau",
    adm1_recode    = c(
      "Gabú"   = "Gabu",
      "Bafatá" = "Bafata",
      "Bissau" = "SAB",
      "Bolama" = "Bolama Bijagos"
    ),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  burkina_faso = list(
    iso3           = "BFA",
    hh_info_path   = "data/bfa_ehcvm2122_hh_info.csv",
    cons_path      = "data/bfa_food_consumption.csv",
    shapefile_type = "local",
    shapefile_path = "shapefiles/bfa/bf_admin1_hno_2022.shp",
    shapefile_name_col = "adm1_name1",
    ne_country     = NULL,
    adm1_recode    = c("Boucle du Mouhoun" = "Boucle du Mouhoum"),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  cote_divoire = list(
    iso3           = "CIV",
    hh_info_path   = "data/civ_ehcvm2122_hh_info.csv",
    cons_path      = "data/civ_food_consumption.csv",
    shapefile_type = "local",
    shapefile_path = "shapefiles/civ/civ_admbnda_adm1_cntig_ocha_itos_20180706.shp",
    shapefile_name_col = "ADM1_FR",
    ne_country     = NULL,
    adm1_recode    = c(
      "District Autonome D'Abidjan"        = "Autonome d'Abidjan",
      "District Autonome De Yamoussoukro"  = "Yamoussoukro",
      "Me"                                 = "La Me",
      "Grands Ponts"                       = "Grands-Ponts"
    ),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  nigeria = list(
    iso3           = "NGA",
    hh_info_path   = "data/nga_lss1819_hh_info.csv",
    cons_path      = "data/nga_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Nigeria",
    # Nigeria requires an adm1→state crosswalk loaded separately (see analysis script)
    adm1_recode    = c(
      "Federal Capital Territory" = "Fct",
      "Nassarawa"                 = "Nasarawa"
    ),
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  ),
  
  ghana = list(
    iso3           = "GHA",
    hh_info_path   = "data/gha_glss17_hh_info.csv",
    cons_path      = "data/gha_food_consumption.csv",
    shapefile_type = "ne_states",
    ne_country     = "Ghana",
    adm1_recode    = c(),             # no recoding needed
    vehicles = list(
      rice        = c(1, 2, 3, 4)
    )
  )
  
)


# =============================================================================
# Vehicle display labels (used in map subtitles and legend axes)
# =============================================================================
VEHICLE_LABELS <- list(
  rice        = "Rice"
)


# =============================================================================
# Countries not in the analysis that should appear grey on the combined map
# =============================================================================
GREY_COUNTRIES_ISO3 <- c("GMB", "GIN", "LBR", "SLE")


# =============================================================================
# West Africa countries for the labelled inset map
# =============================================================================
WA_COUNTRIES <- c(
  "Senegal", "Nigeria", "Ghana", "Ivory Coast", "Mali",
  "Burkina Faso", "Niger", "Sierra Leone", "Liberia",
  "Guinea", "Guinea-Bissau", "Gambia", "Togo", "Benin"
)

# =============================================================================
# Food Vehicle Analysis — Main Runner
# =============================================================================
# Usage:
#   1. Set `VEHICLE` to any key defined in COUNTRY_CONFIG$<country>$vehicles
#   2. Set `COUNTRIES` to the subset of countries you want to run
#   3. Source this script — it will produce per-country and combined maps
# =============================================================================

source("01_functions.R")
source("02_config.R")


# ── USER SETTINGS ─────────────────────────────────────────────────────────────

VEHICLE  <- "rice"          # change to e.g. "wheat_flour", "oil", "salt" …

COUNTRIES <- names(COUNTRY_CONFIG)   # or a subset: c("senegal", "niger", "mali")

REACH_BREAKS  <- c(0, 25, 50, 75, 100)
INTAKE_BREAKS <- c(-Inf, 75, 149, 300, Inf)
INTAKE_LABELS <- c("<75", "75-149", "150-300", ">300")

PALETTE <- "BlueGold"
DIM     <- 4

# ── HELPERS ───────────────────────────────────────────────────────────────────

#' Load the adm1 shapefile for a country config entry
load_shapefile <- function(cfg) {
  if (cfg$shapefile_type == "ne_states") {
    sf <- ne_states(country = cfg$ne_country, returnclass = "sf") %>%
      select(name, geometry) %>%
      rename(adm1 = name)
  } else if (cfg$shapefile_type == "ne_countries") {
    sf <- ne_countries(country = cfg$ne_country, returnclass = "sf") %>%
      select(name, geometry) %>%
      rename(adm1 = name)
  } else {
    # local shapefile — name column defined in config
    sf <- st_read(cfg$shapefile_path, quiet = TRUE) %>%
      select(all_of(cfg$shapefile_name_col), geometry) %>%
      rename(adm1 = all_of(cfg$shapefile_name_col))
  }
  
  if (length(cfg$adm1_recode) > 0) {
    sf <- sf %>% mutate(adm1 = recode(adm1, !!!cfg$adm1_recode))
  }
  
  sf
}

# ── MAIN LOOP ─────────────────────────────────────────────────────────────────

results     <- list()   # stores per-country outputs for later combination
bi_sf_list  <- list()   # bi_class sf objects for the combined map

for (country_key in COUNTRIES) {
  
  cfg <- COUNTRY_CONFIG[[country_key]]
  message("Processing: ", country_key, " | vehicle: ", VEHICLE)
  
  # Skip if this vehicle isn't configured for this country
  if (!VEHICLE %in% names(cfg$vehicles)) {
    warning("  ⚠ Vehicle '", VEHICLE, "' not configured for ", country_key, " — skipping.")
    next
  }
  
  vehicle_codes <- cfg$vehicles[[VEHICLE]]
  vehicle_label <- VEHICLE_LABELS[[VEHICLE]] %||% VEHICLE
  
  # 1. Load consumption data
  cons_data <- process_food_consumption(cfg$hh_info_path, cfg$cons_path)
  
  # Special case: Nigeria needs adm1 → state crosswalk
  # (keep here so all other countries remain unaffected)
  if (country_key == "nigeria") {
    # Expects files: data/nga_lss1819_hh_info_extra.csv with adm1 + state cols
    # or adjust to your actual crosswalk source
    crosswalk <- read.csv("data/nga_adm1_state_crosswalk.csv") %>%
      select(adm1, state) %>%
      distinct()
    
    cons_data <- cons_data %>%
      left_join(crosswalk, by = "adm1") %>%
      mutate(adm1 = state) %>%
      select(-state)
  }
  
  # 2. Calculate reach & intake
  reach  <- calculate_reach(cons_data,
                            vehicle_codes = vehicle_codes,
                            vehicle_name  = VEHICLE)
  
  intake <- calculate_intake(cons_data,
                             vehicle_codes = vehicle_codes,
                             vehicle_name  = VEHICLE)
  
  # 3. Load shapefile & recode
  shp <- load_shapefile(cfg)
  
  # 4. Build spatial table
  reach_intake_sf <- build_reach_intake_sf(
    reach_data     = reach,
    intake_data    = intake,
    shapefile      = shp,
    vehicle_name   = VEHICLE,
    reach_breaks   = REACH_BREAKS,
    intake_breaks  = INTAKE_BREAKS,
    intake_labels  = INTAKE_LABELS
  )
  
  # 5. Make bivariate map for this country
  map_obj <- make_bivariate_map(
    sf_data      = reach_intake_sf,
    outline_sf   = shp,
    country_name = cfg$ne_country %||% country_key,
    vehicle_label = vehicle_label,
    palette      = PALETTE,
    dim          = DIM
  )
  
  country_bivariate <- assemble_map(map_obj)
  
  # Store
  results[[country_key]] <- list(
    reach         = reach,
    intake        = intake,
    sf            = reach_intake_sf,
    map_obj       = map_obj,
    bivariate_map = country_bivariate,
    outline       = shp
  )
  
  bi_sf_list[[country_key]] <- map_obj$bi_data
}


# ── COMBINED WEST AFRICA MAP ──────────────────────────────────────────────────

combined_sf  <- bind_rows(bi_sf_list) %>% st_as_sf()
grey_sf      <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(iso_a3 %in% GREY_COUNTRIES_ISO3) %>%
  st_transform(st_crs(combined_sf))
west_africa_sf <- ne_countries(country = WA_COUNTRIES, returnclass = "sf")

# Assemble combined main map
main_map <- ggplot() +
  geom_sf(data    = combined_sf,
          mapping = aes(fill = bi_class),
          color   = NA, show.legend = FALSE) +
  bi_scale_fill(pal = PALETTE, dim = DIM) +
  bi_theme() +
  geom_sf(data = grey_sf, fill = "white", color = "black") +
  # Draw country outlines on top
  lapply(results, function(r) {
    geom_sf(data = r$outline, fill = NA, color = "black", size = 0.5)
  })

# Labelled West Africa inset
inset_wa <- ggplot() +
  geom_sf(data = west_africa_sf, fill = "white", color = "black", size = 0.5) +
  geom_sf_text(data = west_africa_sf, aes(label = name),
               size = 2.5, check_overlap = FALSE) +
  theme_void()

# Shared legend (use last country's break values; all use same bins)
last_country <- tail(names(results), 1)
last_sf      <- results[[last_country]]$sf
break_vals   <- bi_class_breaks(last_sf, x = reach_bins, y = intake_bins, dim = DIM)

shared_legend <- bi_legend(
  pal    = PALETTE,
  dim    = DIM,
  xlab   = "Higher Reach (%) ",
  ylab   = paste0("Higher ", VEHICLE_LABELS[[VEHICLE]] %||% VEHICLE, " Consumption (g) "),
  size   = 8,
  breaks = break_vals
)

# Final combined map
vehicle_label <- VEHICLE_LABELS[[VEHICLE]] %||% VEHICLE
final_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_wa,      x = 0.02, y = 0.01, width = 0.25, height = 0.30) +
  draw_plot(shared_legend, x = 0.73, y = 0.01, width = 0.35, height = 0.30)

print(final_map)

# ── SAVE OUTPUTS ──────────────────────────────────────────────────────────────

output_dir <- file.path("outputs", VEHICLE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save per-country maps
for (country_key in names(results)) {
  ggsave(
    filename = file.path(output_dir, paste0(country_key, "_", VEHICLE, ".png")),
    plot     = results[[country_key]]$bivariate_map,
    width    = 8, height = 6, dpi = 300, bg = "white"
  )
}

# Save combined West Africa map
ggsave(
  filename = file.path(output_dir, paste0("west_africa_", VEHICLE, ".png")),
  plot     = final_map,
  width    = 12, height = 6, dpi = 300, bg = "white"
)

message("Done! Outputs saved to: ", output_dir)

