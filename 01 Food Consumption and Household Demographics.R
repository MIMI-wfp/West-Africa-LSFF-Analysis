# Authors: Uchenna Agu
# Use: Food Consumption and Household Demography extraction script for West Africa (EHCVM)
# Data Sources: Enquête Harmonisée sur le Conditions de Vie des Ménages, 2021-20122

#============================================================= Load required libraries ================================================================

rq_packages <- c("tidyverse", "haven", "srvyr", "glue", "readxl", "gridExtra", "devtools")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#===================================================== Part 1: Calculate Food Consumption Quantities ==================================================

# Read in food consumption, household roster, non standard units and edible portions file

wa_country <- read.csv("raw_data/wa_country_ehcvm2122/s07b_me_wa_country2021.csv", stringsAsFactors = FALSE)
nsu <- read.csv("raw_data/wa_country_ehcvm2122/ehcvm_nsu_wa_country2021_nat.csv")
wa_country_foods <- read.csv("raw_data/wa_country_ehcvm2122/mapping.csv", stringsAsFactors = FALSE)

# Select required variables from the food consumption data, also create a new hhid for downstream merging

wa_country_cons <- wa_country %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c)

# Select required variables from the NSU file

wa_country_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID)

# Join consumption data with NSU data and calculate daily consumption quatities

wa_country_cons_df <- wa_country_cons %>%
  left_join(wa_country_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID")) %>%
  mutate(quantity_g = (s07bq03a * poids)/7)

# Select needed variables from edible portions file

wa_country_foods <- wa_country_foods %>%
  select(s07bq01, edible_portion)

# Join food consumption data and edible portions file, apply correctly

wa_country_cons_df <- wa_country_cons_df %>%
  left_join(wa_country_foods, by ="s07bq01") %>%
  mutate(quantity_g = quantity_g * edible_portion) %>%
  select(hhid,s07bq01,quantity_g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

# Save CSV: 
write_csv(wa_country_cons_df, "processed_data/wa_country_ehcvm2122/wa_country_food_consumption_v1.csv")

#======================================================== Part 2: Calculate AFEs ====================================================================

# List of assumptions made for AFE calculations: 
# 1 AFE = 2291kcal/day as calculated using the FAO/WHO/UNU (2004) equations for a 55kg female
# PAL = 1.76 (active/moderately active lifestyle) - reference: table 5.1 FAO/WHO/UNU (2004)
# Average men's weight = 65kg (Assumed)
# Average women's weight = 55kg (Assumed)
# Average energy cost of lactation = 505kcal for first 6-months of lactation
# 460kcal after 6-months of lactation (Chapter 7 in FAO/WHO/UNU (2004))
# Average total energy cost of a preganancy = 77,100kcal (reference: table 6.3 FAO/WHO/UNU (2004))
# Average length of a pregnancy = 280days
# Therefore average daily energy cost during pregnancy = 275kcal/day
# There is no data in the NPS-5 to determine pregnancy trimester

# Create function to read csv and generate hhid 

read_and_hhid <- function(file_path) {
  df <- read_csv(file_path)  %>%
    mutate(hhid = paste0(grappe, "_", menage))
  
  return(df)
}

# Create a function to generalize and automate the script across the countries

process_country <- function(country_code) {
  # Define file paths using country code
  
  s00_file <- glue("Data/s00_me_{country_code}2021.csv")
  s01_file <- glue("Data/s01_me_{country_code}2021.csv")
  s03_file <- glue("Data/s03_me_{country_code}2021.csv")
  welfare_file <- glue("Data/ehcvm_welfare_{country_code}2021.csv")
  ind_file <- glue("Data/ehcvm_individu_{country_code}2021.csv")
  adm1_list <- glue("Data/adm1_list_{country_code}.csv")
  adm2_list <- glue("Data/adm2_list_{country_code}.csv")
  output_file <- glue("Data/{country_code}_ehcvm2122_hh_info.csv")
  output_plots <- glue("Data/{country_code}_plots.csv")
  
  # READ IN DEMOGRAPHIC DATA:
  
  demographic <- read_and_hhid(ind_file) %>%
    select(hhid, numind, sexe, age, resid) %>% 
    filter(resid == 1) %>%  # only for individuals residing in the household
    select(hhid, numind, sexe, age)
  
  #-------------------------------------------------------------------------------
  
  # IDENTIFY INDIVUDUALS FALLING INTO EACH DEMOGRAPHIC GROUP:
  
  # CHILDREN UNDER 2: 
  u2 <- demographic %>% 
    filter(age < 2)
  
  # Get age in months: 
  u2_age <- read_and_hhid(s01_file)  %>% 
    rename(numind = membres__id,
           day_birth = s01q03a,
           month_birth = s01q03b,
           year_birth = s01q03c)
  
  ref_date_df <- read_and_hhid(s00_file) %>%
    mutate(
      ref_date = coalesce(s00q25b, s00q24b, s00q23b)
    ) %>%
    select(hhid, ref_date)
  
  u2 <- u2 %>% 
    left_join(u2_age, by= c('hhid', 'numind')) %>%
    left_join(ref_date_df, by = "hhid") %>%
    select(hhid, numind, age, day_birth, month_birth, year_birth, ref_date) %>% 
    mutate(
      birth_date = make_date(year_birth, month_birth, day_birth), 
      age_months = interval(birth_date, ref_date) %/% months(1)
    ) 
  
  rm(u2_age, ref_date_df)
  
  
  # LACTATING AND PREGNANT WOMEN: 
  women_subset <- read_and_hhid(s03_file) %>% 
    rename(numind = membres__id,
           lact_m = s03q47,          # Woman had a baby in the past 12 months
           pregnant = s03q49) %>%    # woman is pregnant at the moment of the interview
    filter(lact_m == 1 | pregnant == 1) # filter twice to optimize performance
  
  women_subset <- demographic %>%
    left_join(women_subset, by = c('hhid', 'numind')) %>%
    select(hhid, numind, sexe, age, lact_m, pregnant) %>% 
    filter(lact_m == 1 | pregnant == 1) 
  
  # DEMOGRAPHIC ALL OTHERS: 
  demographic_others <- demographic %>% 
    anti_join(u2, by = c("hhid", "numind")) %>% 
    anti_join(women_subset, by = c("hhid", "numind")) %>% 
    filter(age > 1)
  #-------------------------------------------------------------------------------
  
  # ESTIMATE ENERGY REQUIREMENTS AND AFE's FOR THOSE AGED < 24-months:
  
  # Assign energy requirements for different age groups - SOURCE: 
  # Book - Complementary feeding of young Children in Developing Countries, 
  # Table 10, page 51.
  # WHO 1998, edited by Kenneth Brown, Kathryn Dewey, Lindsay Allen
  
  u2 <- u2 %>%
    mutate(TEE = case_when(
      age_months <= 2 ~ 0,   # only breast feeding - no food intake
      age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76 kcal per day for 3-5 months of age
      age_months >= 6 & age_months <= 8 ~ 269,  # 269 kcal per day for 6-8 months of age
      age_months >= 9 & age_months <= 11 ~ 451,   # 451 kcal per day for 9-11 months of age
      age_months >= 12 ~ 746, # 746 kcal per day for those aged 12-months - 2years
      is.na(age_months) ~ 746)) # 746 kcal for those without a birth certificate, assuming they can be older
  
  # AFE calculation for children below 2 years old:
  afeu2 <- u2 %>%
    mutate(afe = TEE/2291) %>% # 1AFE = 2291kcal
    select(hhid, numind, afe)
  
  #rm(u2)
  
  # ESTIMATING TEE FOR THOSE AGED >2YEARS:
  
  tee_calc <- demographic %>%
    mutate(ind_weight = ifelse(sexe == 1, 65, 55)) %>% # Assumed average weight of men = 65kg
    # Assumed average weight of women = 55kg
    filter(age >= 2) %>%  # Remove under 2's as these have already been calculated above
    mutate(PAL = ifelse(age > 18, 1.76, NA))   # Set a PAL at 1.76 for all over 18's:
  
  # TEE FOR CHILDREN (2-18 years old) (formula from tables 4.5 and 4.6 in Human energy requirements
  # Report from FAO/WHO/UNU (2001)):
  tee_calc <- tee_calc %>% 
    mutate(TEE = case_when(    sexe == 1 & age == 2 ~ 950,
                               sexe == 1 & age == 3 ~ 1125,
                               sexe == 1 & age == 4 ~ 1250,
                               sexe == 1 & age == 5 ~ 1350,
                               sexe == 1 & age == 6 ~ 1475,
                               sexe == 1 & age == 7 ~ 1575,
                               sexe == 1 & age == 8 ~ 1700,
                               sexe == 1 & age == 9 ~ 1825,
                               sexe == 1 & age == 10 ~ 1975,
                               sexe == 1 & age == 11 ~ 2150,
                               sexe == 1 & age == 12 ~ 2350,
                               sexe == 1 & age == 13 ~ 2550,
                               sexe == 1 & age == 14 ~ 2775,
                               sexe == 1 & age == 15 ~ 3000,
                               sexe == 1 & age == 16 ~ 3175,
                               sexe == 1 & age == 17 ~ 3325,
                               sexe == 1 & age == 18 ~ 3400,
                               sexe == 2 & age == 2 ~ 850,
                               sexe == 2 & age == 3 ~ 1050,
                               sexe == 2 & age == 4 ~ 1150,
                               sexe == 2 & age == 5 ~ 1250,
                               sexe == 2 & age == 6 ~ 1325,
                               sexe == 2 & age == 7 ~ 1425,
                               sexe == 2 & age == 8 ~ 1550,
                               sexe == 2 & age == 9 ~ 1700,
                               sexe == 2 & age == 10 ~ 1850,
                               sexe == 2 & age == 11 ~ 2000,
                               sexe == 2 & age == 12 ~ 2150,
                               sexe == 2 & age == 13 ~ 2275,
                               sexe == 2 & age == 14 ~ 2375,
                               sexe == 2 & age == 15 ~ 2450,
                               sexe == 2 & age > 15 & age <= 18 ~ 2500))
  
  # TEE FOR ADULTS (Formula from table 5.2 in FAO/WHO/UNU (2004)):
  tee_calc <- tee_calc %>% 
    mutate(BMR = case_when( # Firstly need to calculate BMR for different age categories:
      sexe == 1 & age >18 & age <= 30 ~ 15.057 * ind_weight + 692.2,
      sexe == 1 & age >30 & age < 60 ~ 11.472 * ind_weight + 873.1,
      sexe == 1 & age >= 60 ~ 11.711 * ind_weight + 587.7,
      sexe == 2 & age >18 & age <= 30 ~ 14.818 * ind_weight + 486.6,
      sexe == 2 & age >30 & age < 60 ~ 8.126 * ind_weight + 845.6, 
      sexe == 2 & age >= 60 ~ 9.082 * ind_weight + 658.5,
      TRUE ~ NA)) %>% # Get TEE by multiplying BMR by PAL for over 18's: 
    mutate(TEE = ifelse(age > 18, BMR * PAL, TEE)) # 
  
  # x <- as.data.frame(table(tee_calc$TEE, uwa_countryA = "ifany"))  checking distribution of possible values
  
  # ENERGY REQUIREMENTS FOR PREGNANT WOMEN: 
  
  afe_preg <- women_subset %>%
    filter(pregnant == 1) %>% # the  women that are pregnant and have had a baby 
    # in the past 12 months will be counted as pregnant
    left_join(tee_calc %>% select(hhid, numind, TEE),
              by = c("hhid", "numind")) %>% 
    select(-ends_with(".y")) %>%
    rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
    mutate(TEE = TEE + 275) %>% # Usual energy requirements +275 kcal/day: 
    mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291 kcal/day
    select(hhid, numind, afe)
  
  # ENERGY REQUIREMENT FOR LACTATING WOMEN:
  
  afe_lact <- women_subset %>%
    filter(lact_m == 1 & pregnant == 2) %>%
    left_join(tee_calc %>% select(hhid, numind, TEE),
              by = c("hhid", "numind")) %>% 
    select(-ends_with(".y")) %>%
    rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
    mutate (TEE = TEE + 483) %>% 
    mutate(afe = TEE / 2291) %>% # AFE = Total energy expenditure / 2291kcal/day
    select(hhid, numind, afe)
  
  # rm(women_subset)
  
  # CALCULATE AFE FOR ALL OTHER INDIVIDUALS: 
  afe_other <- demographic_others %>% 
    left_join(tee_calc %>% select(hhid, numind, TEE),
              by = c("hhid", "numind")) %>% 
    #  select(-ends_with(".y"), -resid) %>%
    #  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
    # Calculate AFE:
    mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291kcal/day
    select(hhid, numind, afe)
  
  # CALCULATE TOTAL AFE PER HOUSEHOLD: 
  
  hh_afe <- bind_rows(afeu2, afe_lact, afe_preg, afe_other) %>% 
    group_by(hhid) %>% 
    summarise(afe = sum(afe, na.rm = TRUE))
  
  # Save csv
  write_csv(hh_afe, "processed_data/wa_country_ehcvm2122/hh_afe.csv")
  
  #===========================================================Part 3: Clean Consumption Quantities=======================================================================
  # READ DATA: 
  
  food_consumption <- read_csv("processed_data/wa_country_ehcvm2122//wa_country_ehcvm2122_food_consumption_v1.csv")
  hh_afe <- read_csv("processed_data/wa_country_ehcvm2122/hh_afe.csv")
  
  # GET CONSUMPTION QUANTITIES IN GRAMS/DAY/AFE: 
  
  quantities_gdafe <- food_consumption %>% 
    left_join(hh_afe, by = "hhid") %>%
    mutate(quantity_gdafe = quantity_g / afe)
  
  # HANDLING OF EXTREME OUTLIERS: 
  # (Defined as 3 standard deviations above the log transformed mean intake for 
  # each food item)
  
  # Log transform the quantity_gdafe:
  quantities_gdafe <- quantities_gdafe %>% 
    mutate(log_quantity_gdafe = log10(quantity_gdafe))
  
  # Calculate the cutpoint for each item_code:
  cutpoint <- quantities_gdafe %>% 
    group_by(item_code) %>% 
    summarise(mean_log = mean(log_quantity_gdafe, na.rm = TRUE),
              sd_log = sd(log_quantity_gdafe, na.rm = TRUE)) %>% 
    mutate(cutpoint = mean_log + 3 * sd_log) %>% 
    dplyr::select(item_code, cutpoint)
  
  # Apply cutpoints to the data:
  quantities_gdafe <- quantities_gdafe %>% 
    left_join(cutpoint, by = "item_code") %>% 
    mutate(quantity_gdafe = case_when(
      log_quantity_gdafe > cutpoint ~ NA_real_,
      TRUE ~ quantity_gdafe)) %>% 
    dplyr::select(-log_quantity_gdafe, -cutpoint) %>% 
    # Replace NA values with median reported intake 
    group_by(item_code) %>%
    mutate(quantity_gdafe = ifelse(is.na(quantity_gdafe), 
                                   median(quantity_gdafe, na.rm = TRUE), 
                                   quantity_gdafe))
  
  # UPDATE FOOD CONSUMPTION DATAFRAME WITH CLEANED VALUES:
  food_consumption <- food_consumption %>% 
    dplyr::select(-quantity_g) %>% 
    left_join(quantities_gdafe %>% 
                dplyr::select(hhid, item_code, afe, quantity_gdafe) %>% 
                # Note that values need to be grams consumed per day - to remain 
                # consistent with other countries on the MIMI database:
                mutate(quantity_g = quantity_gdafe * afe) %>% 
                dplyr::select(-c(quantity_gdafe, afe)),
              by = c("hhid", "item_code"))
  
  
  # PREPARE FINAL FOOD_CONSUMPTION DATAFRAME FOR MIMI DATABASE: 
  food_consumption <- food_consumption %>% 
    mutate(quantity_100g = quantity_g / 100) %>% 
    dplyr::select(-item_name)
  
  # Save csv
  write_csv(food_consumption, "processed_data/wa_country_ehcvm2122//wa_country_ehcvm2122_food_consumption.csv") 
  
  #========================================================== Part 4: Extract Household Information==================================================================
  hh_info <- read_and_hhid(welfare_file) 
  adm2_date <- read_and_hhid(s00_file)
  adm1_list_ds <- read_csv(adm1_list)
  adm2_list_ds <- read_csv(adm2_list)
  
  # SELECT RELEVANT HOUSEHOLD INFO DATA: 
  
  hh_info <- hh_info %>% 
    select(hhid, hhweight, region, grappe, milieu, hgender, hage, heduc, dtot, # for Benin, it's not region but department
           eqadu1,eqadu2, hhsize) %>% 
    rename(adm1 = region,
           survey_wgt = hhweight,
           res = milieu, 
           sex_head = hgender,
           age_head = hage,
           educ_head = heduc,
           pc_expenditure = dtot,
           ea = grappe) %>% 
    mutate(res = case_when(res == 2 ~ "Rural",
                           res == 1 ~ "Urban",
                           TRUE ~ NA_character_),
           adm1 = adm1_list_ds$category[match(adm1, adm1_list_ds$value)],
           # The adm1_list data frames for each country were created from the World Bank's 
           # repository, by printing the statistical summary for s00q01 on pdf and asking 
           # ChatGPT to create csv files with two columns: one for the values and another 
           # for the categories. These outputs from ChatGPT had to be checked and minor
           # errors were corrected
           sex_head = case_when(sex_head == 2 ~ "Female",
                                sex_head == 1 ~ "Male",
                                TRUE ~ NA_character_),
           educ_head = case_when(educ_head == 1 ~ "None",
                                 educ_head == 2 ~ "Kindergarden",
                                 educ_head == 3 ~ "Elementary",
                                 educ_head == 4 ~ "Secondary, 1st Level - General",
                                 educ_head == 5 ~ "Secondary, 1st Level - Technical",
                                 educ_head == 6 ~ "Secondary, 2nd Level - General",
                                 educ_head == 7 ~ "Secondary, 2nd Level - Technical",
                                 educ_head == 8 ~ "Some Post-Secondary",
                                 educ_head == 9 ~ "Higher Education",
                                 TRUE ~ NA_character_)) #,
  
  adm2_date <- adm2_date %>% 
    select(hhid, s00q02, s00q25b, s00q24b, s00q23b) %>%
    rename(adm2 = s00q02) %>%   
    mutate(adm2 = adm2_list_ds$category[match(adm2, adm2_list_ds$value)],
           int_date = coalesce(s00q25b, s00q24b, s00q23b),
           month = month(int_date),
           year = year(int_date)
    )
  
  # CALCULATE CONSUMPTION QUINTILES: 
  
  # Firstly extract total consumption (spatially and temporally adjusted): 
  hh_consumption <- hh_info %>% 
    select(hhid, survey_wgt, res, pc_expenditure)
  
  # Create tbl_svy object: 
  svy_hh_consumption <- hh_consumption %>% 
    as_survey_design(weights = survey_wgt)
  
  # Calculate consumption quintiles cut-points: 
  consumption_quantiles <- svy_hh_consumption %>% 
    summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))
  
  urban_quantiles <- svy_hh_consumption %>% 
    filter(res == "Urban") %>%
    summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))
  
  rural_quantiles <- svy_hh_consumption %>% 
    filter(res == "Rural") %>%
    summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))
  
  # Apply cut-points to data: 
  hh_consumption <- hh_consumption %>% 
    mutate(sep_quintile = case_when(pc_expenditure < consumption_quantiles$consumption_q20 ~ 1,
                                    pc_expenditure >= consumption_quantiles$consumption_q20 & 
                                      pc_expenditure < consumption_quantiles$consumption_q40 ~ 2,
                                    pc_expenditure >= consumption_quantiles$consumption_q40 & 
                                      pc_expenditure < consumption_quantiles$consumption_q60 ~ 3,
                                    pc_expenditure >= consumption_quantiles$consumption_q60 & 
                                      pc_expenditure < consumption_quantiles$consumption_q80 ~ 4,
                                    pc_expenditure >= consumption_quantiles$consumption_q80 ~ 5,
                                    TRUE ~ NA_real_)) %>% 
    mutate(res_quintile = case_when(res == "Urban" & 
                                      pc_expenditure < urban_quantiles$consumption_q20 ~ 1,
                                    res == "Urban" & pc_expenditure >= urban_quantiles$consumption_q20 & 
                                      pc_expenditure < urban_quantiles$consumption_q40 ~ 2,
                                    res == "Urban" & 
                                      pc_expenditure >= urban_quantiles$consumption_q40 & 
                                      pc_expenditure < urban_quantiles$consumption_q60 ~ 3,
                                    res == "Urban" & 
                                      pc_expenditure >= urban_quantiles$consumption_q60 & 
                                      pc_expenditure < urban_quantiles$consumption_q80 ~ 4,
                                    res == "Urban" & 
                                      pc_expenditure >= urban_quantiles$consumption_q80 ~ 5,
                                    res == "Rural" & 
                                      pc_expenditure < rural_quantiles$consumption_q20 ~ 1,
                                    res == "Rural" & 
                                      pc_expenditure >= rural_quantiles$consumption_q20 & 
                                      pc_expenditure < rural_quantiles$consumption_q40 ~ 2,
                                    res == "Rural" & 
                                      pc_expenditure >= rural_quantiles$consumption_q40 & 
                                      pc_expenditure < rural_quantiles$consumption_q60 ~ 3,
                                    res == "Rural" & 
                                      pc_expenditure >= rural_quantiles$consumption_q60 & 
                                      pc_expenditure < rural_quantiles$consumption_q80 ~ 4,
                                    res == "Rural" & 
                                      pc_expenditure >= rural_quantiles$consumption_q80 ~ 5,
                                    TRUE ~ NA_real_))
  
  rm(consumption_quantiles, urban_quantiles, rural_quantiles, svy_hh_consumption)
  
  # Join all relevant variables to hh_info: 
  hh_info <- hh_info %>% 
    left_join(hh_afe, by = "hhid") %>% 
    left_join(adm2_date, by = "hhid") %>% 
    left_join(hh_consumption %>% 
                dplyr::select(hhid, sep_quintile, res_quintile),
              by = "hhid") %>% 
    select(hhid, adm1, adm2, res, pc_expenditure, sep_quintile, res_quintile, 
           year, month, survey_wgt, afe, ea) 
  
  write_csv(hh_info, output_file)
  
  return(list(
    hh_info = hh_info,
    u2 = u2,
    women_subset = women_subset,
    demographic_others = demographic_others
  ))
  
}

# Write data
write_csv(hh_info, "processed_data/wa_country_ehcvm2122/wa_country_ehcvm2122_hh_info.csv")