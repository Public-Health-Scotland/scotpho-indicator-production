# To-do contains indicators saved in test output location- this should be updated to non-test folder when new profile designed is published. 

#  ScotPHO indicators: 2 indicator outputs from this script 
#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE can only be generated at Scotland, NHS board and CA level.  It is NOT possibly to generate at smaller geographies as robust data on SAH (self assessed health is not available)

# Population splits by SIMD quintile and Urban/rural split for male/female HLE available at Scotland level only.

# HLE data published annually by NRS - usually in December - check website to see if new data has been published
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland
# it may take some time before this data is then available in statistics.gov 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") #Normal indicator functions

# Data queried directly from statistics.gov 
# install the dev tools/opendata scotland r package which communicates with the statistics.gov website api - if you don't already have them.
# install.packages("devtools") #commented out as only needs to be run once and you may already have the packages installed.
# devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov

# Extracts for Life Expectancy data saved in left expectancy network folder.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
} else {
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
}

###############################################.
# Extract Healthy life expectancy data ----
## by CA & NHS board
###############################################.

# see structure and variables of this dataset
ods_structure("healthy-life-expectancy") 

# Set filter parameters to use in open data extraction
# reminder: there is a limit on number of rows that can be extracted using this particular method - in case the extracted data doesn't look complete.
age_select <- c("0-years") #include filter in a extraction as it is needed to ensure number of rows to extract is within limits however (annoyingly initial extratction returns '0 years' and '90 years' so need to filter again)
#urban_rural <- c("all")

# extract data
hle_data_raw <- ods_dataset("healthy-life-expectancy", 
                            #urbanRuralClassification = urban_rural,
                            age = age_select)

# prepare data
hle <- hle_data_raw %>% 
  
  # clean column names
  clean_names() %>%
  
  # filter again as this will select correctly filter only '0 years' (ie healthy life expectancy at birth) otherwise you have '90 years' 
  filter(age == age_select) %>%  
  
  # pivot measure types to wide format
  pivot_wider(names_from = measure_type, values_from = value) %>%
  
  # rename columns
  rename("code" = ref_area, 
         "trend_axis" = ref_period,
         "rate" = count,
         "lowci" = `95-lower-confidence-limit`,
         "upci" = `95-upper-confidence-limit`) %>% 

         # create single split name column
  mutate(split_name = case_when(urban_rural_classification != "all" ~ "Urban/Rural",
                                simd_quintiles != "all" ~ "Scottish Index of Multiple Deprivation",
                                urban_rural_classification == "all" & simd_quintiles == "all" ~ "Total"),
         
         # create single split value column
         split_value = case_when(split_name == "Urban/Rural" ~ urban_rural_classification,
                                 split_name == "Scottish Index of Multiple Deprivation" ~ simd_quintiles,
                                 split_name == "Total" ~ "Total"),
         
         # tidy split values
         split_value = str_to_sentence(split_value), # capitalises first letter
         split_value = str_replace_all(split_value, c("-" = " ",
                                                      "1 most deprived" = "1 - most deprived",
                                                      "5 least deprived" = "5 - least deprived",
                                                      "Large urban areas" = "1 Large urban areas",
                                                      "Other urban areas" = "2 Other urban areas",
                                                      "Accessible small towns" = "3 Acessible small towns",
                                                      "Remote small towns" = "4 Remote small towns",
                                                      "Accessible rural" = "5 Accessible rural",
                                                      "Remote rural" = "6 Remote rural")),
         # recode standard geo code for scotland to the ScotPHO dictionary where S00000001 is Scotland
         code = ifelse(code == "S92000003", "S00000001", code),
         
         # Create new columns
         numerator = NA, # insert column where numerator would ordinarily be - there is no numerator for LE
         ind_id = case_when(sex == "female" ~ 99101,
                            sex == "male" ~ 99102),
         def_period = paste0(trend_axis, " (3 year aggregate)"),
         year = as.numeric(substr(trend_axis, 1, 4)) +1) %>% # its 3 year average so take first year of time period then add 1 to find mid-year

  # remove irrelevant columns
  select(!c(age, simd_quintiles, urban_rural_classification)) %>% 
  
  arrange(ind_id, code, year, split_name, split_value)


###############################################.
## Generate Male healthy life expectancy shiny files ----
###############################################.

# 1. main data (ie data behind summary/trend/rank tab)
hle_male_main <- hle %>% 
  filter(sex == "male",
         split_name == "Total") %>%
  select(-c(sex, split_name, split_value))
  

write_csv(hle_male_main, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_male_shiny.csv"))
write_rds(hle_male_main, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_male_shiny.rds"))

# 2. population groups data (ie data behind population groups tab)
hle_male_popgrp <- hle %>% 
  filter(sex == "male",
         split_name != "Total") %>%
  select(-sex)

write_csv(hle_male_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_male_shiny_popgrp.csv"))
write_rds(hle_male_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_male_shiny_popgrp.rds"))

# make a copy of the popgrp data as a basis for the subsequent deprivation analysis
# this analysis will remove the SIMD data and overwrite the healthy_life_expectancy_male_shiny_popgrp files
write_rds(hle_male_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_male_shiny_popgrp_ORIGINAL.rds"))

###############################################.
## Generate Female healthy life expectancy files ----
###############################################.

# 1. main data (ie data behind summary/trend/rank tab)
hle_female_main <- hle %>% 
  filter(sex == "female",
         split_name == "Total") %>%
  select(-c(sex, split_name, split_value))

write_csv(hle_female_main, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_female_shiny.csv"))
write_rds(hle_female_main, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_female_shiny.rds"))

# 2. population groups data (ie data behind population groups tab)
hle_female_popgrp <- hle %>% 
  filter(sex == "female",
         split_name != "Total") %>%
  select(-sex)

write_csv(hle_female_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_female_shiny_popgrp.csv"))
write_rds(hle_female_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_female_shiny_popgrp.rds"))

# make a copy of the popgrp data as the _shiny_popgrp files will get overwritten during the subsequent deprivation analysis (after SIMD data have been removed)
write_rds(hle_female_popgrp, file = paste0(data_folder, "Test Shiny Data/healthy_life_expectancy_female_shiny_popgrp_ORIGINAL.rds"))

######################################################.
# Prepare data for Deprivation tab ----
######################################################.

# ER: I was unable to install opendatascot, so used the files that had already been prepared

# This function opens the popgroup and maindata files that were saved above.
# The non-SIMD data are retained in the popgroup file.
# The SIMD data are extracted, and totals are added to these from the maindata file.
# The deprivation analysis function is then run, producing an _ineq.rds file -> Data to be checked

split_popgrp_file <- function(indicator){

  # Read in the data processed above
  # Check these are the paths to the latest data
  popgroup_df <- readRDS(paste0(data_folder, "Test Shiny Data/", indicator, "_shiny_popgrp_ORIGINAL.rds")) # use the copy so that it won't get overwritten by this function
  maindata_df <- read_csv(paste0(data_folder, "Data to be checked/", indicator, "_shiny.csv")) # the rds had the wrong ind_id, so use csv until fixed
  
  # get SIMD data from the popgroup df:
  simd_df <- popgroup_df %>%
    filter(split_name=="Scottish Index of Multiple Deprivation") %>%
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin",
           quintile = substr(quintile, 1, 1)) %>%
    select(-split_name)
  
  # no totals here, so get from maindata df:
  simd_totals <- simd_df %>%
    select(ind_id, code, year, trend_axis, def_period) %>%
    distinct()  %>%
    merge(y=maindata_df, by=c("ind_id", "code", "year", "trend_axis", "def_period")) %>%
    mutate(quintile = "Total",
           quint_type="sc_quin")
  
  # combine these:
  simd_df <- rbind(simd_df, simd_totals) 

  # Save intermediate SIMD file
  write_rds(simd_df, file = paste0(data_folder, "Prepared Data/", indicator, "_shiny_depr_raw.rds"))
  write.csv(simd_df, file = paste0(data_folder, "Prepared Data/", indicator, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  # Get ind_id argument for the analysis function 
  ind_id <- unique(simd_df$ind_id)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(indicator, "_shiny_depr"), 
                                 pop = "depr_pop_allages", # these are all-age indicators, with no sex split for SIMD
                                 ind_id, 
                                 indicator)
  
  # Remove SIMD data from original popgrp file and resave
  popgroup_df <- popgroup_df %>%
    filter(split_name!="Scottish Index of Multiple Deprivation") 
  
  write_csv(popgroup_df, file = paste0(data_folder, "Test Shiny Data/", indicator, "_shiny_popgrp.csv")) 
  write_rds(popgroup_df, file = paste0(data_folder, "Test Shiny Data/", indicator, "_shiny_popgrp.rds"))

}

# run the function:
split_popgrp_file("healthy_life_expectancy_female")
split_popgrp_file("healthy_life_expectancy_male")



# END
