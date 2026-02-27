# =====================================================================
# Author: Uchenna Agu
# Purpose: Create ADM1 and ADM2 weighted inadequacy proportions
# =====================================================================

# -------------------------------
# Load household-level metadata
# -------------------------------
wa_country_hh_info <- read.csv(wa_country_hh_file)

wa_country_hh <- wa_country_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

# ---------------------------------------------
# Merge apparent intakes with HH geographic info
# ---------------------------------------------
wa_country_base_ai_fe <- wa_country_base_ai %>%
  left_join(wa_country_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)

# ---------------------------------------------
# Compute iron inadequacy (probability-based)
# ---------------------------------------------
wa_country_fe_inadequacy_adm1 <- fe_full_prob(
  wa_country_base_ai_fe,
  group1     = adm1,
  bio_avail  = 10,
  hh_weight  = "hh_weight"
)

wa_country_fe_inadequacy_adm2 <- fe_full_prob(
  wa_country_base_ai_fe,
  group1     = adm2,
  bio_avail  = 10,
  hh_weight  = "hh_weight"
)

# -----------------------------------------------------
# Create 0/1 deficiency flags for all micronutrients
# -----------------------------------------------------
wa_country_targets <- apply_nutrient_deficiency_flag(wa_country_base_ai)

wa_country_target_hh_info <- wa_country_targets %>%
  left_join(wa_country_hh_info, by = "hhid") %>%
  select(
    -pc_expenditure, -sep_quintile, -res_quintile,
    -year, -month, -afe, -fe_mg
  )

# -----------------------------
# Build survey design object
# -----------------------------
wa_country_survey <- wa_country_target_hh_info %>%
  as_survey_design(
    ids     = ea,
    strata  = res,
    weights = survey_wgt
  )

# -----------------------------------------------------------------
# Compute ADM1-level weighted inadequacy proportions
# -----------------------------------------------------------------
wa_country_inadq_proportion_adm1 <- wa_country_survey %>%
  group_by(adm1) %>%
  summarise(
    across(
      .cols = c(
        energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg,
        vitb6_mg, folate_mcg, vitb12_mcg, zn_mg
      ),
      .fns   = ~ survey_mean(. == 1, na.rm = TRUE) * 100,
      .names = "{.col}_prop"
    )
  ) %>%
  left_join(wa_country_fe_inadequacy_adm1, by = c("adm1" = "subpopulation"))

# -----------------------------------------------------------------
# Compute ADM2-level weighted inadequacy proportions
# -----------------------------------------------------------------
wa_country_inadq_proportion_adm2 <- wa_country_survey %>%
  group_by(adm2) %>%
  summarise(
    across(
      .cols = c(
        energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg,
        vitb6_mg, folate_mcg, vitb12_mcg, zn_mg
      ),
      .fns   = ~ survey_mean(. == 1, na.rm = TRUE) * 100,
      .names = "{.col}_prop"
    )
  ) %>%
  left_join(wa_country_fe_inadequacy_adm2, by = c("adm2" = "subpopulation"))

# ==============================================================
# AUTOMATIC PIPELINE FOR 10 WEST AFRICAN COUNTRIES
# ==============================================================

country_list <- list(
  "sen" = list(hh_file = "sen_ehcvm2122_hh_info.csv", ai = sen_base_ai),
  "civ" = list(hh_file = "civ_ehcvm2122_hh_info.csv", ai = civ_base_ai),
  "bfa" = list(hh_file = "bfa_ehcvm2122_hh_info.csv", ai = bfa_base_ai),
  "mli" = list(hh_file = "mli_ehcvm2122_hh_info.csv", ai = mli_base_ai),
  "ben" = list(hh_file = "ben_ehcvm2122_hh_info.csv", ai = ben_base_ai),
  "gnb" = list(hh_file = "gnb_ehcvm2122_hh_info.csv", ai = gnb_base_ai),
  "tgo" = list(hh_file = "tgo_ehcvm2122_hh_info.csv", ai = tgo_base_ai),
  "ner" = list(hh_file = "ner_ehcvm2122_hh_info.csv", ai = ner_base_ai),
  "nga" = list(hh_file = "nga_lss1819_hh_info.xlsx", ai = nga_base_ai),
  "gha" = list(hh_file = "gha_glss17_hh_info.csv", ai = gha_base_ai)
)

results_adm1 <- list()
results_adm2 <- list()

for (country in names(country_list)) {
  
  message("Processing ", country, "...")
  
  # Pull inputs
  hh_file <- country_list[[country]]$hh_file
  base_ai <- country_list[[country]]$ai
  
  # Apply generic template
  wa_country_hh_info <- if (grepl(".xlsx", hh_file)) readxl::read_excel(hh_file) else read.csv(hh_file)
  
  wa_country_hh <- wa_country_hh_info %>%
    select(hhid, adm1, adm2, survey_wgt)
  
  wa_country_base_ai_fe <- base_ai %>%
    left_join(wa_country_hh, by = "hhid") %>%
    select(hhid, adm1, adm2, survey_wgt, fe_mg)
  
  wa_country_fe_inadequacy_adm1 <- fe_full_prob(wa_country_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")
  wa_country_fe_inadequacy_adm2 <- fe_full_prob(wa_country_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")
  
  wa_country_targets <- apply_nutrient_deficiency_flag(base_ai)
  
  wa_country_target_hh_info <- wa_country_targets %>%
    left_join(wa_country_hh_info, by="hhid") %>%
    select(-pc_expenditure, -sep_quintile, -res_quintile,
           -year, -month, -afe, -fe_mg)
  
  wa_country_survey <- wa_country_target_hh_info %>%
    as_survey_design(ids=ea, strata=res, weights=survey_wgt)
  
  adm1_result <- wa_country_survey %>%
    group_by(adm1) %>%
    summarise(across(
      c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg,
        vitb6_mg, folate_mcg, vitb12_mcg, zn_mg),
      ~ survey_mean(. == 1, na.rm = TRUE) * 100,
      .names="{.col}_prop"
    )) %>%
    left_join(wa_country_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))
  
  adm2_result <- wa_country_survey %>%
    group_by(adm2) %>%
    summarise(across(
      c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg,
        vitb6_mg, folate_mcg, vitb12_mcg, zn_mg),
      ~ survey_mean(. == 1, na.rm = TRUE) * 100,
      .names="{.col}_prop"
    )) %>%
    left_join(wa_country_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))
  
  # Store
  results_adm1[[country]] <- adm1_result
  results_adm2[[country]] <- adm2_result
}