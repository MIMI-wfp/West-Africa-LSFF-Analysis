############################################################
Authors: Uchenna Agu, Kevin Tang
# 0. PACKAGES
############################################################

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, survey, srvyr, sf, biscale, cowplot,
  rnaturalearth, rnaturalearthdata, wesanderson
)

############################################################
# 1. LOAD INPUT DATA
#    Assumes base_ai already exists
#    Assumes wa_hh_info.Rds exists
############################################################

wa_hh_info <- readRDS("wa_hh_info.Rds")

############################################################
# 2. EAR + CV SETTINGS
############################################################

ear <- c(
  vita_rae_mcg = 490,
  folate_mcg  = 250,
  vitb12_mcg  = 2,
  zn_mg       = 10.2
)

cv <- c(
  vita_rae_mcg = 0.20,
  folate_mcg   = 0.10,
  vitb12_mcg   = 0.10,
  zn_mg        = 0.125
)

############################################################
# 3. IRON INADEQUACY FUNCTION (BIOAVAILABILITY 10%)
############################################################

fe_full_prob <- function(data, bio_avail = 10) {
  
  data %>%
    rename(intake = fe_mg) %>%
    rowwise() %>%
    mutate(
      prob_inad = as.numeric(case_when(
        # ---- 10% BIOAVAILABILITY ----
        bio_avail == 10 & intake <= 7.5  ~ 1,
        bio_avail == 10 & intake <= 8.4  ~ 0.96,
        bio_avail == 10 & intake <= 9.4  ~ 0.93,
        bio_avail == 10 & intake <= 10.7 ~ 0.85,
        bio_avail == 10 & intake <= 11.8 ~ 0.75,
        bio_avail == 10 & intake <= 12.9 ~ 0.65,
        bio_avail == 10 & intake <= 13.9 ~ 0.55,
        bio_avail == 10 & intake <= 15.1 ~ 0.45,
        bio_avail == 10 & intake <= 16.6 ~ 0.35,
        bio_avail == 10 & intake <= 18.7 ~ 0.25,
        bio_avail == 10 & intake <= 22.5 ~ 0.15,
        bio_avail == 10 & intake <= 26.7 ~ 0.08,
        bio_avail == 10 & intake <= 31.5 ~ 0.04,
        bio_avail == 10 & intake >  31.5 ~ 0,
        TRUE ~ NA_real_
      ))
    ) %>%
    ungroup() %>%
    mutate(pa_fe = 1 - prob_inad) %>%
    select(iso3, hhid, pa_fe)
}

############################################################
# 4. COMPUTE IRON PA VALUES
############################################################

fe_probs <- fe_full_prob(
  data = base_ai %>% select(iso3, hhid, fe_mg),
  bio_avail = 10
)

############################################################
# 5. COMPUTE MPA
############################################################

mpa_result <- base_ai %>%
  rowwise() %>%
  mutate(
    pa_vita   = pnorm((vita_rae_mcg - ear["vita_rae_mcg"]) / (ear["vita_rae_mcg"] * cv["vita_rae_mcg"])),
    pa_folate = pnorm((folate_mcg   - ear["folate_mcg"])   / (ear["folate_mcg"]   * cv["folate_mcg"])),
    pa_b12    = pnorm((vitb12_mcg   - ear["vitb12_mcg"])   / (ear["vitb12_mcg"]   * cv["vitb12_mcg"])),
    pa_zn     = pnorm((zn_mg        - ear["zn_mg"])        / (ear["zn_mg"]        * cv["zn_mg"]))
  ) %>%
  ungroup() %>%
  left_join(fe_probs, by = c("iso3", "hhid")) %>%
  mutate(
    mpa  = rowMeans(cbind(pa_vita, pa_folate, pa_b12, pa_zn, pa_fe), na.rm = TRUE),
    mpa  = 1 - mpa,               # convert to inadequacy score
    flag = if_else(mpa > 0.5, 1L, 0L)
  ) %>%
  select(iso3, hhid, mpa, flag)

############################################################
# 6. MERGE WITH HOUSEHOLD WEIGHTS + ADM1 INFO
############################################################

merged_wa_hh_mpa <- mpa_result %>%
  left_join(wa_hh_info, by = c("iso3", "hhid"))

############################################################
# 7. SURVEY DESIGN
############################################################

survey_design <- merged_wa_hh_mpa %>%
  as_survey_design(
    ids     = ea,
    strata  = res,
    weights = survey_wgt,
    nest    = TRUE
  )

############################################################
# 8. ADM1‑LEVEL MEAN MPA
############################################################

mpa_mean_adm1_all <- survey_design %>%
  group_by(iso3, adm1) %>%
  summarise(
    mpa_mean = survey_mean(mpa, na.rm = TRUE, vartype = NULL)
  )

############################################################
# 9. LOAD & HARMONIZE ADM1 SHAPEFILES
############################################################

# All your existing readRDS shapes:
sen_adm1 <- readRDS("sen_adm1.Rds") %>% mutate(iso3="SEN")
ner_adm1 <- readRDS("ner_adm1.Rds") %>% mutate(iso3="NER")
nga_adm1 <- readRDS("nga_adm1.Rds") %>% mutate(iso3="NGA")
mli_adm1 <- readRDS("mli_adm1.Rds") %>% mutate(iso3="MLI")
gnb_adm1 <- readRDS("gnb_adm1.Rds") %>% mutate(iso3="GNB")
tgo_adm1 <- readRDS("tgo_adm1.Rds") %>% mutate(iso3="TGO")
ben_adm1 <- readRDS("ben_adm1.Rds") %>% mutate(iso3="BEN")
gha_adm1 <- readRDS("gha_adm1.Rds") %>% mutate(iso3="GHA")

# CIV and BFA use shapefiles:
civ_adm1 <- st_read("shapefiles/civ_adm1.shp") %>%
  rename(name = ADM1_FR) %>%
  mutate(iso3 = "CIV")

bfa_adm1 <- st_read("shapefiles/bfa_adm1.shp") %>%
  rename(name = adm1_name1) %>%
  mutate(iso3 = "BFA") %>%
  st_make_valid()

# Recodes (cleaned)
civ_adm1 <- civ_adm1 %>%
  mutate(name = recode(name,
                       "District Autonome D'Abidjan" = "Autonome d'Abidjan",
                       "District Autonome De Yamoussoukro" = "Yamoussoukro",
                       "Me" = "La Me",
                       "Grands Ponts" = "Grands-Ponts"))

gnb_adm1 <- gnb_adm1 %>% mutate(name = recode(name,
                                              "Gabú"="Gabu",
                                              "Bafatá"="Bafata",
                                              "Bissau"="SAB",
                                              "Bolama"="Bolama Bijagos"))

mli_adm1 <- mli_adm1 %>% mutate(name = recode(name,
                                              "Timbuktu"="Tombouctou",
                                              "Ségou"="Segou"))

ner_adm1 <- ner_adm1 %>% mutate(name = recode(name, "Tillabéri"="Tillaberi"))
sen_adm1 <- sen_adm1 %>% mutate(name = recode(name,
                                              "Kédougou"="Kedougou",
                                              "Sédhiou"="Sedhiou",
                                              "Thiès"="Thies"))
tgo_adm1 <- tgo_adm1 %>% mutate(name = recode(name, "Centre"="Centrale"))
ben_adm1 <- ben_adm1 %>% mutate(name = recode(name,
                                              "Ouémé"="Oueme",
                                              "Atakora"="Atacora",
                                              "Kouffo"="Couffo"))
nga_adm1 <- nga_adm1 %>% mutate(name = recode(name,
                                              "Abuja"="Fct",
                                              "Nassarawa"="Nasarawa"))

############################################################
# 10. CREATE COUNTRY OUTLINES
############################################################

make_outline <- function(shp) st_union(shp)

nga_country_outline <- make_outline(nga_adm1)
ben_country_outline <- make_outline(ben_adm1)
gnb_country_outline <- make_outline(gnb_adm1)
mli_country_outline <- make_outline(mli_adm1)
ner_country_outline <- make_outline(ner_adm1)
sen_country_outline <- make_outline(sen_adm1)
tgo_country_outline <- make_outline(tgo_adm1)
bfa_country_outline <- make_outline(bfa_adm1)
civ_country_outline <- make_outline(civ_adm1)
gha_country_outline <- make_outline(gha_adm1)

############################################################
# 11. MERGE ALL ADM1 + MPA RESULTS
############################################################

combined_adm1 <- bind_rows(
  nga_adm1, ben_adm1, gnb_adm1, mli_adm1, ner_adm1,
  sen_adm1, tgo_adm1, bfa_adm1, civ_adm1, gha_adm1
)

combined_adm1 <- combined_adm1 %>%
  left_join(mpa_mean_adm1_all, by = c("iso3", "name"="adm1"))

############################################################
# 12. BACKGROUND GREY REGIONS
############################################################

grey_countries <- ne_countries(scale="medium", returnclass="sf") %>%
  filter(iso_a3 %in% c("GMB","GIN","LBR","SLE")) %>%
  st_transform(st_crs(combined_adm1))

############################################################
# 13. INSETS
############################################################

wa_countries <- c("Senegal", "Nigeria","Ghana","Ivory Coast","Mali",
                  "Burkina Faso","Niger","Sierra Leone","Liberia",
                  "Guinea","Guinea-Bissau","Gambia","Togo","Benin")

west_africa_base <- ne_countries(country = wa_countries, returnclass = "sf")

inset_westafrica <- ggplot() +
  geom_sf(data = west_africa_base, fill="white", color="black", size=0.5) +
  geom_sf_text(data = west_africa_base, aes(label = name), size = 2.5) +
  theme_void()

bbox_west <- st_bbox(west_africa_base)

africa_map <- ggplot() +
  geom_sf(data = ne_countries(continent="Africa", returnclass="sf"),
          fill="white", color="black", size=0.3) +
  annotate("rect",
           xmin=bbox_west["xmin"], xmax=bbox_west["xmax"],
           ymin=bbox_west["ymin"], ymax=bbox_west["ymax"],
           color="red", fill=NA, size=1) +
  theme_void()

############################################################
# 14. MAIN MAP
############################################################

main_map <- ggplot() +
  geom_sf(data = combined_adm1, aes(fill = mpa_mean), color = NA) +
  geom_sf(data = grey_countries, fill="white", color="black") +
  geom_sf(data = nga_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = ben_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = gnb_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = mli_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = ner_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = sen_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = tgo_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = bfa_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = civ_country_outline, fill=NA, color="black", size=1) +
  geom_sf(data = gha_country_outline, fill=NA, color="black", size=1) +
  scale_fill_gradientn(
    colors = wes_palette("Zissou1", n = 100, type="continuous"),
    limits = c(0, 1),
    name   = "Prob. Inadequacy"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

############################################################
# 15. FINAL MAP (WITH INSETS)
############################################################

final_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_westafrica, x=0.01, y=0.01, width=0.27, height=0.30) +
  draw_plot(africa_map, x=0.71, y=0.01, width=0.3, height=0.3)

print(final_map)

############################################################
# 16. SAVE OUTPUTS
############################################################

ggsave("wa_MPI_main.png",
       plot = final_map, width = 10, height = 6,
       dpi = 300, bg="white")

ggsave("wa_MPI_main_no_inset.png",
       plot = main_map, width = 10, height = 6,
       dpi = 300, bg="white")

