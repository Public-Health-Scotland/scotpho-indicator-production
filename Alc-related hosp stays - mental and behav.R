# ============================================================================
# ===== Processing PHS HOSPITAL STATS (SMR01 AND SMR04) =====
# ============================================================================

# NOTES ON THE INDICATOR

# 1 adult MH indicator: 
# ind_id = 30006 = "Rate of alcohol-related hospitalisation from mental and behavioural disorders due to alcohol use, per 100,000 adults."

# Whether rate of patients or stays was not specified. I opted to use stays, as this is the headline stat the ARHS team use in their publications. 

# Data provided by Scott Kilgariff, mailto:phs.alcohol@phs.scot, in Sept 2025
# rds file at DZ11 level

# Please see attached for the data you have requested for patients/stays from 16 years and older.
# •	For the SIMD quintile figures I have used a top age group of 85+ whilst for the rest of the figures I have used a top age group of 90+ when calculating EASR.
# •	SIMD has been applied as follows:
# o	1996 to 2003 – SIMD 2004 has been used
# o	2004 to 2006 – SIMD 2006 has been used
# o	2007 to 2009 – SIMD 2009(v2) has been used
# o	2010 to 2011 – SIMD 2012 has been used (NB differs from the ISD GPD guidance which applies SIMD2012 to 2010-2013 data)
# o	2012 to 2016 – SIMD 2016 has been used
# o	2017+ – SIMD 2020(v2) has been used
# •	Only Scottish residents have been included
# •	Only those patients/stays where the sex is known have been included
# •	You’ll see in the attached that some of the population figures per group have decimal values. This is due to removing 0.2 of the 15-19 age group to create the 16-19 age group.
# •	o_lower is the lower end of the 95% CI for the number of observed cases
# •	o_upper is the higher end of the 95% CI for the number of observed cases


##########################################################
### Functions/packages -----
##########################################################

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA


##########################################################
### Read in data -----
##########################################################

# File location
arhs_data_folder <- paste0(profiles_data_folder, "/Received Data/Alc-related hosp stays - mental and behavioural disorders/")
file <- "IR2025-00668-output.RDS"

arhs <- readRDS(paste0(arhs_data_folder, file)) %>%
  mutate(sex_grp = ifelse(sex=="Female", 2, 1)) %>%
  select(year, datazone2011, sex_grp, age_grp, numerator = stays)


# Save the raw data for feeding to the processing functions

# Total population
saveRDS(arhs, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_raw.rds"))

# Subsets by sex
arhs_m <- arhs %>% filter(sex_grp==1)
saveRDS(arhs_m, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_M_raw.rds"))

arhs_f <- arhs %>% filter(sex_grp==2)
saveRDS(arhs_f, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_F_raw.rds"))



##########################################################
### Main analysis (used for pop group files too) -----
##########################################################

# Run main_analysis to produce total population rates
main_analysis(filename="ARHS_mental_and_behav",
                            measure = "stdrate",
                            geography = "datazone11",
                            year_type = "calendar",
                            ind_id = 30006, 
                            time_agg = 1, 
                            yearstart=2002, 
                            yearend=2023, 
                            pop = "DZ11_pop_16+", 
                            epop_total = 165800, # 82900 x 2 sexes if starts at 16y
                            epop_age = "16+", # waiting for Scott to confirm
                            test_file = FALSE, 
                            QA = TRUE, 
                            police_div = TRUE)

popgrp_total <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Total")

# Run main_analysis to produce rates by sex
main_analysis(filename="ARHS_mental_and_behav_M",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2023, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # if starts at 16y
              epop_age = "16+", # waiting for Scott to confirm
              test_file = FALSE, 
              QA = TRUE, 
              police_div = TRUE)

popgrp_m <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Male")

main_analysis(filename="ARHS_mental_and_behav_F",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2023, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # if starts at 16y
              epop_age = "16+", # waiting for Scott to confirm
              test_file = FALSE, 
              QA = TRUE, 
              police_div = TRUE)

popgrp_f <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Female")


# Combine data
stays <- rbind(popgrp_total,
               popgrp_f,
               popgrp_m) %>%
# remove small geographies
  filter(!(substr(code, 1, 3) %in% c("S02", "S99"))) #IZs and localities, these have some counts < 5
# now smallest numerator is 15



##########################################################
### Deprivation analysis -----
##########################################################

# Total population
deprivation_analysis(filename="ARHS_mental_and_behav", 
                                 time_agg = 1, 
                                 yearstart=2002, 
                                 yearend=2023,
                                 year_type = "calendar",
                                 measure = "stdrate",
                                 pop_sex = "all", # c("male", "female", "all")
                                 epop_total = 165800, # if starts at 16y, both sexes
                                 epop_age = "16+", # waiting for Scott to confirm
                                 pop_age = c(16, 150), # wait for age confirmation
                                 ind_id = 30006, 
                                 QA = TRUE, 
                                 test_file = FALSE)

arhs_simd_total <- deprivation_analysis_result %>%
  mutate(sex = "Total")


# Male population
deprivation_analysis(filename="ARHS_mental_and_behav_M", 
                     time_agg = 1, 
                     yearstart=2002, 
                     yearend=2023,
                     year_type = "calendar",
                     measure = "stdrate",
                     pop_sex = "male", # c("male", "female", "all")
                     epop_total = 82900, # if starts at 16y, both sexes
                     epop_age = "16+", # waiting for Scott to confirm
                     pop_age = c(16, 150), # wait for age confirmation
                     ind_id = 30006, 
                     QA = TRUE, 
                     test_file = FALSE)

arhs_simd_m <- deprivation_analysis_result %>%
  mutate(sex = "Male")

# Female population
deprivation_analysis(filename="ARHS_mental_and_behav_F", 
                     time_agg = 1, 
                     yearstart=2002, 
                     yearend=2023,
                     year_type = "calendar",
                     measure = "stdrate",
                     pop_sex = "female", # c("male", "female", "all")
                     epop_total = 82900, # if starts at 16y, both sexes
                     epop_age = "16+", # waiting for Scott to confirm
                     pop_age = c(16, 150), # wait for age confirmation
                     ind_id = 30006, 
                     QA = TRUE, 
                     test_file = FALSE)

arhs_simd_f <- deprivation_analysis_result %>%
  mutate(sex = "Female")


# Combine data
stays_simd <- rbind(arhs_simd_total,
                    arhs_simd_f,
                    arhs_simd_m) 
# smallest numerators for non-Scotland geogs are 1-4.
# Smallest numerator for Scotland is 590 

# NEED DZ01 TO PRODUCE DEP DATA BACK FURTHER THAN 2014
# SEND DATA TO SCOTT BEFORE PUBLISHING


##########################################################
### Prepare final files -----
##########################################################


# 1 - main data (ie data behind summary/trend/rank tab)

main_data <- stays %>% 
  filter(split_value == "Total") %>% 
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  unique() 

# Save 
write_rds(main_data, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny.csv"), row.names = FALSE)


# 2 - population groups data (ie data behind population groups tab)

pop_grp_data <- stays %>% # includes the contents of main data, as the 'total' for each split
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value,) 

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny_popgrp.csv"), row.names = FALSE)


# 3 - SIMD data (ie data behind deprivation tab)

# Save
write_rds(stays_simd, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny_ineq.rds"))
write.csv(stays_simd, paste0(profiles_data_folder, "Data to be checked/ARHS_mental_and_behav_shiny_ineq.csv"), row.names = FALSE)





##########################################################
### QA -----
##########################################################

run_qa(type = "main", filename = "ARHS_mental_and_behav", test_file = FALSE) 
run_qa(type = "popgrp", filename = "ARHS_mental_and_behav", test_file = FALSE)
run_qa(type = "deprivation", filename = "ARHS_mental_and_behav", test_file = FALSE) 
