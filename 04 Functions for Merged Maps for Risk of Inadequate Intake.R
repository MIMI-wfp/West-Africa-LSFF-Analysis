# Author: Uchenna Agu
# Purpose: Generate merged West Africa Risk Maps

# Recode names function
recode_names <- function(df, recode_list){
  df %>% mutate(name = recode(name, !!!recode_list))
}

# Merge with nutrient data
merge_nutrient <- function(adm_sf, nutrient_df){
  left_join(adm_sf, nutrient_df, by = c("name" = "adm1"))
}

# Create country outline
create_outline <- function(merged_sf){
  st_union(st_make_valid(merged_sf))
}

# Generic map plotting function
plot_nutrient <- function(merged_sf, country_outline, nutrient_col, title){
  ggplot() +
    geom_sf(data = merged_sf, aes_string(fill = nutrient_col), color = NA) +
    geom_sf(data = country_outline, fill = NA, color = "black", size = 1) +
    scale_fill_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous"),
                         limits = c(0, 100),
                         name = "At Risk (%)") +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(hjust = 0.5)
    )
}

#------------------- Load Data -------------------#

# List of countries and their RDS or shapefile paths
countries <- list(
  sen = list(file = "sen_adm1.Rds", recode = c("Kédougou" = "Kedougou", "Sédhiou"="Sedhiou", "Thiès"="Thies")),
  ner = list(file = "ner_adm1.Rds", recode = c("Tillabéri" = "Tillaberi")),
  nga = list(file = "nga_adm1.Rds", recode = c("Federal Capital Territory" = "Abuja")),
  mli = list(file = "mli_adm1.Rds", recode = c("Timbuktu" = "Tombouctou", "Ségou"="Segou")),
  gnb = list(file = "gnb_adm1.Rds", recode = c("Gabú" = "Gabu", "Bafatá"="Bafata", "Bissau"="SAB", "Bolama"="Bolama Bijagos")),
  tgo = list(file = "tgo_adm1.Rds", recode = c("Centre" = "Centrale")),
  ben = list(file = "ben_adm1.Rds", recode = c("Ouémé" = "Oueme", "Atakora"="Atacora", "Kouffo"="Couffo")),
  bfa = list(file = "bf_admin1_hno_2022.shp", recode = c("Boucle du Mouhoun"="Boucle du Mouhoum")),
  civ = list(file = "civ_admbnda_adm1_cntig_ocha_itos_20180706.shp", recode = c(
    "District Autonome D'Abidjan" = "Autonome d'Abidjan",
    "District Autonome De Yamoussoukro" = "Yamoussoukro",
    "Me" = "La Me",
    "Grands Ponts" = "Grands-Ponts"
  )),
  gha = list(file = "gha_adm1.Rds", recode = NULL)
)

# Load and process each country
adm_list <- lapply(names(countries), function(ctry){
  path <- countries[[ctry]]$file
  rec <- countries[[ctry]]$recode
  
  adm <- if(grepl("\\.Rds$", path)){
    readRDS(path)
  } else {
    st_read(path) %>% select(starts_with("ADM") | geometry) %>% rename(name = contains("ADM"))
  }
  
  if(!is.null(rec)) adm <- recode_names(adm, rec)
  
  adm
})
names(adm_list) <- names(countries)

# Merge nutrient data
# Assuming *_inadq_proportion_adm1 objects exist with same naming convention
for(ctry in names(adm_list)){
  nut_df <- get(paste0(ctry, "_inadq_proportion_adm1"))
  adm_list[[ctry]] <- merge_nutrient(adm_list[[ctry]], nut_df)
  assign(paste0(ctry, "_outline"), create_outline(adm_list[[ctry]]))
}

# Combine all countries for regional plotting
combined_merged <- do.call(rbind, adm_list)

#------------------- West Africa Base & Grey Countries -------------------#
wa_countries <- c("Senegal","Nigeria","Ghana","Ivory Coast","Mali","Burkina Faso",
                  "Niger","Sierra Leone","Liberia","Guinea","Guinea-Bissau","Gambia","Togo","Benin")
west_africa_base <- ne_countries(country = wa_countries, returnclass = "sf")

grey_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  subset(iso_a3 %in% c("GMB","GIN","LBR","SLE")) %>%
  st_transform(st_crs(combined_merged))

#------------------- Map Function for West Africa -------------------#
generate_nutrient_map <- function(data_sf, nutrient_col, map_title){
  # Main map
  main_map <- ggplot() +
    geom_sf(data = data_sf, aes_string(fill = nutrient_col), color = NA) +
    geom_sf(data = grey_countries, fill = "grey90", color = "black") +
    lapply(names(adm_list), function(ctry){
      geom_sf(data = get(paste0(ctry, "_outline")), fill = NA, color = "black", size = 1)
    }) +
    scale_fill_gradientn(colors = wes_palette("Zissou1", 100, type = "continuous"),
                         limits = c(0,100), name = "At Risk (%)") +
    labs(title = map_title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size=16),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(hjust = 0.5)
    )
  
  # West Africa inset
  inset_wa <- ggplot() +
    geom_sf(data = west_africa_base, fill = "white", color = "black", size = 0.5) +
    geom_sf_text(data = west_africa_base, aes(label = name), size = 2.5) +
    theme_void()
  
  # Africa overview inset
  bbox_wa <- st_bbox(west_africa_base)
  africa_overview <- ggplot() +
    geom_sf(data = ne_countries(continent="Africa", returnclass="sf"), fill="white", color="black", size=0.3) +
    annotate("rect", xmin=bbox_wa["xmin"], xmax=bbox_wa["xmax"],
             ymin=bbox_wa["ymin"], ymax=bbox_wa["ymax"], color="red", fill=NA, size=1) +
    theme_void()
  
  # Combine with cowplot
  final_plot <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(inset_wa, x=0.01, y=0.01, width=0.3, height=0.3) +
    draw_plot(africa_overview, x=0.74, y=0.01, width=0.3, height=0.3)
  
  return(final_plot)
}

#------------------- Generate Maps -------------------#
p_b12 <- generate_nutrient_map(combined_merged, "vitb12_mcg_prop", "Risk of Inadequate Vitamin B12 Intake")
p_fol <- generate_nutrient_map(combined_merged, "folate_mcg_prop", "Risk of Inadequate Folate Intake")
p_vita <- generate_nutrient_map(combined_merged, "vita_rae_mcg_prop", "Risk of Inadequate Vitamin A Intake")
p_zn <- generate_nutrient_map(combined_merged, "zn_mg_prop", "Risk of Inadequate Zinc Intake")
p_fe <- generate_nutrient_map(combined_merged, "fe_mg_prop", "Risk of Inadequate Iron Intake")

# Example display
p_b12

