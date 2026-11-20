# ============================================================================
# ===== Processing PHS HOSPITAL STATS (SMR01 AND SMR04) =====
# ============================================================================

# Liz Richardson
# Oct 2026

# NOTES ON THE INDICATOR

# 1 adult MH indicator: 
# ind_id = 30006 = "Rate of alcohol-related hospitalisation from mental and behavioural disorders due to alcohol use, per 100,000 adults."

# Whether rate of patients or stays was not specified in MHI spec. 
# I opted to use stays, as this is the headline stat the ARHS team use in their publications. 

# OCT 2026:
# Data provided by Scott Kilgariff, mailto:phs.alcohol@phs.scot
# Patients/stays from 16 years and older (alc team confirmed that age_grp 4 = 16-19 years)
# Only Scottish residents have been included
# Only those patients/stays where the sex is known have been included
# Separate RDS files by DZ01, DZ11, and DZ22 (same individuals in each). REQUEST THIS FORMAT GOING FORWARD, TO AVOID HAVING TO CHANGE THE CODE.
# We use DZ11 data for main and popgrp data time series, and add in DZ01 data when analysing inequalities pre 2014.

# Statistical disclosure control applied to numerator after rate calculations : CHECK FINAL OUTPUT WITH ALCOHOL TEAM BEFORE PUBLISHING.
# Method used in disclosure control should align with that used in total alcohol related admissions indicator & alcohol team publication not be published. 


##########################################################
### Functions/packages -----
##########################################################

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA
source("//PHI_conf/ScotPHO/Profiles/Code/stat_disclosure_alcohol_stays.R") # statistical disclosure methodology - confidential - do not share

##########################################################
### Paths -----
##########################################################

# File paths
arhs_data_folder <- paste0(profiles_data_folder, "/Received Data/Alc-related hosp stays - mental and behavioural disorders/")
data_datazone2001 <- "IR2026-00790-output_dz2001.RDS"
data_datazone2011 <- "IR2026-00790-output_dz2011.RDS"
data_datazone2022 <- "IR2026-00790-output_dz2022.RDS"


##########################################################
### Read in data -----
##########################################################

# Numerator data

get_numerators <- function(filename) {
  df <- readRDS(paste0(arhs_data_folder, filename)) %>%
    mutate(sex_grp = ifelse(sex=="Female", 2, 1),
           year = as.numeric(substr(financial_year, 1, 4))) %>%
    select(year, starts_with("datazone"), sex_grp, age_grp, numerator = stays) 
}

arhs_dz01 <- get_numerators(data_datazone2001)
arhs_dz11 <- get_numerators(data_datazone2011)
arhs_dz22 <- get_numerators(data_datazone2022)


# Save the raw numerator data for feeding to the processing functions

# Main data - uses datazone2011 throughout the series (back to 2002)

arhs_main <- arhs_dz11 %>%
  select(year, datazone=datazone2011, sex_grp, age_grp, numerator) 

# Save overall
saveRDS(arhs_main, 
        file.path(profiles_data_folder, "Prepared Data", "arhs_main_raw.rds"))

# Save subsets by sex
saveRDS(arhs_main %>% filter(sex_grp==1), 
        file.path(profiles_data_folder, "Prepared Data", "arhs_main_m_raw.rds"))

saveRDS(arhs_main %>% filter(sex_grp==2), 
        file.path(profiles_data_folder, "Prepared Data", "arhs_main_f_raw.rds"))

# Deprivation data - uses datazone2001 if year<2014, and datazone2011 after this 
# (datazone2022 will be included when SIMD2026 and population data available)
arhs_simd_dz01 <- arhs_dz01 %>%
  filter(year<2014) %>%
  rename(datazone = datazone2001)
arhs_simd_dz11 <- arhs_dz11 %>%
  filter(year>=2014) %>%
  rename(datazone = datazone2011)
arhs_simd <- rbind(arhs_simd_dz01, arhs_simd_dz11)

# Save overall
saveRDS(arhs_simd,
        file.path(profiles_data_folder, "Prepared Data", "arhs_simd_raw.rds"))

# Save subsets by sex
saveRDS(arhs_simd %>% filter(sex_grp==1),
        file.path(profiles_data_folder, "Prepared Data", "arhs_simd_m_raw.rds"))
saveRDS(arhs_simd %>% filter(sex_grp==2),
        file.path(profiles_data_folder, "Prepared Data", "arhs_simd_f_raw.rds"))



##########################################################
### Main data processing -----
##########################################################

# Run main_analysis to produce total population rates
main_analysis(filename="arhs_main",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "financial",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2024, 
              pop = "DZ11_pop_16+", 
              epop_total = 165800, # 82900 x 2 sexes (because age grp 4 starts at 16y)
              epop_age = "16+", 
              test_file = FALSE, 
              QA = FALSE, 
              police_div = TRUE)

popgrp_total <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Total")

# Run main_analysis to produce rates by sex
main_analysis(filename="arhs_main_m",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "financial",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2024, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # because starts at 16y
              epop_age = "16+", 
              test_file = FALSE, 
              QA = FALSE, 
              police_div = TRUE)

popgrp_m <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Male")

main_analysis(filename="arhs_main_f",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "financial",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2024, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # because starts at 16y
              epop_age = "16+", 
              test_file = FALSE, 
              QA = FALSE, 
              police_div = TRUE)

popgrp_f <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Female")


# Combine data
arhs_main_rates <- rbind(popgrp_total,
                         popgrp_f,
                         popgrp_m) |>
  apply_stats_disc_mandb() |> # statistical disclosure applied to final values
  # remove small geographies
  filter(!(substr(code, 1, 3) %in% c("S02", "S99"))) #IZs and localities, these have some counts < 5
# now smallest numerator is 15



##########################################################
### Deprivation data processing -----
##########################################################

deprivation_analysis(filename = "arhs_simd",
                     measure = "stdrate",
                     time_agg = 1,
                     yearstart = 2002,
                     yearend = 2024,
                     year_type = "financial",
                     pop_sex = "all",
                     pop_age = c(16,150),
                     epop_age = "16+",
                     epop_total = 165800,
                     ind_id = 30006,
                     test_file = FALSE, QA = FALSE)
arhs_simd_result <- deprivation_analysis_result %>% mutate(sex="Total")

deprivation_analysis(filename = "arhs_simd_f",
                     measure = "stdrate",
                     time_agg = 1,
                     yearstart = 2002,
                     yearend = 2024,
                     year_type = "financial",
                     pop_sex = "female",  
                     pop_age = c(16,150),
                     epop_age = "16+",
                     epop_total = 82900,
                     ind_id = 30006,
                     test_file = FALSE, QA = FALSE)
arhs_simd_result_F <- deprivation_analysis_result %>% mutate(sex="Female")

deprivation_analysis(filename = "arhs_simd_m",
                     measure = "stdrate",
                     time_agg = 1,
                     yearstart = 2002,
                     yearend = 2024,
                     year_type = "financial",
                     pop_sex = "male",
                     pop_age = c(16,150),
                     epop_age = "16+",
                     epop_total = 82900,
                     ind_id = 30006,
                     test_file = FALSE, QA = FALSE)
arhs_simd_result_M <- deprivation_analysis_result %>% mutate(sex="Male")

# Combine data
arhs_simd_rates <- rbind(arhs_simd_result,
                         arhs_simd_result_F,
                         arhs_simd_result_M) |>
  # Keep only Scotland
  filter(substr(code, 1, 3) == "S00") |> 
  apply_stats_disc_mandb() # statistical disclosure applied to final values (num<3 become 0, other values rounded to nearest 3)
# smallest numerator 216


##########################################################
### Prepare final files -----
##########################################################


# 1 - main data (ie data behind summary/trend/rank tab)

main_data <- arhs_main_rates %>% 
  filter(split_value == "Total") %>% 
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) 
# SMALLEST NUMERATOR = 57

# Save 
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny.csv"), row.names = FALSE)


# 2 - population groups data (ie data behind population groups tab)

pop_grp_data <- arhs_main_rates %>% # includes the contents of main data, as the 'total' for each split
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value) 
# SMALLEST NUMERATOR = 15

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny_popgrp.csv"), row.names = FALSE)


# 3 - SIMD data (ie data behind deprivation tab)

# Save
write_rds(arhs_simd_rates, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.rds"))
write.csv(arhs_simd_rates, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.csv"), row.names = FALSE)
# SMALLEST NUMERATOR = 216

## SEND THE DATA TO THE ALCOHOL TEAM TO CHECK FOR DISCREPANCIES WITH THEIR PUBLISHED FIGURES BEFORE WE PUBLISH ##

##########################################################
### QA -----
##########################################################

run_qa(type = "main", filename = "ARHS_mental_and_behav", test_file = FALSE) 
# any differences across nested geogs are <=3 (explained by the rounding)
# differences between previous file and the update run in Oct 2026 are because we're now producing the data for financial years rather than calendar. Creates numerator diffs.
run_qa(type = "popgrp", filename = "ARHS_mental_and_behav", test_file = FALSE)
run_qa(type = "deprivation", filename = "ARHS_mental_and_behav", test_file = FALSE) 
