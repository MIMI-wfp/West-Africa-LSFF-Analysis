############################################################
# Mean Probability of Adequacy (MPA) Calculation Script
# Assumes: base_ai already exists in the environment from previous scripts
# Required columns:
#   iso3, hhid, vita_rae_mcg, folate_mcg,
#   vitb12_mcg, fe_mg, zn_mg
############################################################


############################################################
# 1. EARs (Estimated Average Requirements) and CVs
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
# 2. Iron Probability of Adequacy Function
#    FAO/WHO threshold-based approach, bioavailability = 10%
############################################################

fe_full_prob <- function(data, bio_avail = 10) {
  
  data %>%
    rename(intake = fe_mg) %>%
    rowwise() %>%
    mutate(
      prob_inad = as.numeric(case_when(
        
        # -------- Iron bioavailability = 10% --------
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
# 3. Compute Probability of Adequacy (PA) for Iron
############################################################

fe_probs <- fe_full_prob(
  data = base_ai %>% select(iso3, hhid, fe_mg),
  bio_avail = 10
)


############################################################
# 4. Compute PA for all other nutrients + join iron PA
############################################################

mpa_result <- base_ai %>%
  rowwise() %>%
  mutate(
    pa_vita   = pnorm((vita_rae_mcg - ear["vita_rae_mcg"]) /
                        (ear["vita_rae_mcg"] * cv["vita_rae_mcg"])),
    
    pa_folate = pnorm((folate_mcg   - ear["folate_mcg"]) /
                        (ear["folate_mcg"]   * cv["folate_mcg"])),
    
    pa_b12    = pnorm((vitb12_mcg   - ear["vitb12_mcg"]) /
                        (ear["vitb12_mcg"]   * cv["vitb12_mcg"])),
    
    pa_zn     = pnorm((zn_mg        - ear["zn_mg"]) /
                        (ear["zn_mg"]        * cv["zn_mg"]))
  ) %>%
  ungroup() %>%
  left_join(fe_probs, by = c("iso3", "hhid")) %>%
  mutate(
    mpa = rowMeans(
      cbind(pa_vita, pa_folate, pa_b12, pa_zn, pa_fe),
      na.rm = TRUE
    ),
    flag = if_else(mpa < 0.5, 1L, 0L)   # 1 = inadequate micronutrient intake
  ) %>%
  select(iso3, hhid, mpa, flag)


############################################################
# 5. OPTIONAL: Convert MPA -> inadequacy score (1 - MPA)
############################################################

mpa_result <- mpa_result %>%
  mutate(mpa = 1 - mpa)   # higher = worse


############################################################
# 6. OPTIONAL: Merge with Household Info
# Requires file in working directory:
#   wa_hh_info.Rds
############################################################

if (file.exists("wa_hh_info.Rds")) {
  wa_hh_info <- readRDS("wa_hh_info.Rds")
  merged_wa_hh_mpa <- mpa_result %>%
    left_join(wa_hh_info, by = c("iso3", "hhid"))
}

############################################################
# End of MPA Calculation Script
############################################################
