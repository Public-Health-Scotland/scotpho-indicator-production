# HEALTHY LIFE EXPECTANCY PROFILES INDICATOR USING REVISED 2025 ONS/NRS METHODOLOGY

#  ScotPHO indicators: 2 indicator outputs from this script 
#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE can only be generated at Scotland, NHS board and CA level.  It is currently NOT possibly to generate at smaller geographies as robust data on SAH (self assessed health is not available)
# Population splits by SIMD quintile and Urban/rural split for male/female HLE are not currently available


# HLE data published annually by NRS - july 2025 first release under new methodology - check website to see if new data has been published
# https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy/#
# New data series not available in statistics.gov at time of script writing


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") #doesn't use the functions, but quick way of getting packages and folders
library(readxl)

# Extracts for Life Expectancy data saved in left expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/HLE data with CI/"


###############################################.
# Read in source data ----
###############################################.

# Open data downloaded from NRS website latest HLE publication pages
# (data comes from ONS pivot table manipulated to include time series hle at birth data for all geographies and both sexes) 

ons_data <- read_excel((paste0(source_network,"ons-data-tables (from NRS HLE Publication July 2025).xlsx")), sheet = "pivot_extract") %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  mutate(trend_axis =paste0(substr(period,1,4),"-",substr(period,9,12)),
         def_period = paste0(trend_axis, "(3 year aggregate)"),
         year = as.numeric(substr(trend_axis,1,4))+1, # year for LE is mid-point of aggregate years (this helps line up data series when comparing le & hle which aren't always same periods)
         code = case_when(`area code` == "S92000003" ~ "S00000001", TRUE ~ `area code`),
         split_value=sex, # required for pop group file
         numerator = NA,
         ind_id = case_when(sex == "Female" ~ 99101,
                            sex == "Male" ~ 99102))|>  #required by shiny app but is null for HLE
  rename("rate" = "hle",
         "lowci" = "lci",
         "upci" = "uci")


##########################################################################.
##1. Generate Main Data files for healthy life expectancy shiny files ----
##########################################################################.

hle_main_file <- function(indicator, sex_filter){
  
  maindata_df <- ons_data %>%
    filter(sex==sex_filter) %>%
    select(ind_id,year,code, numerator, rate, upci, lowci, numerator, trend_axis, def_period)
  
  if (sex_filter=="Female"){
    hle_main_file_female <<- maindata_df #saving sex specific dataframe to posit environment so it can be visually checked
  } else if (sex_filter=="Male") {
    hle_main_file_male <<- maindata_df #saving sex specific dataframe to posit environment so it can be visually checked
  }
  
  write_csv(maindata_df, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.csv"))
  write_rds(maindata_df, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.rds"))
  
}

# run the function for each of the sexes:
hle_main_file(indicator="healthy_life_expectancy_female", sex_filter ="Female")
hle_main_file(indicator="healthy_life_expectancy_male", sex_filter="Male")

#run QA reports for main data
run_qa("healthy_life_expectancy_female", type="main")
run_qa("healthy_life_expectancy_male", type="main")



###################################################################################.
##2. Generate Population Group shiny files for HLE ----
###################################################################################.


# function to prepare male and female file files  
hle_pop_file <- function(indicator, sex, ind_id ){
  
    hle_popgrp <- ons_data %>%
      mutate(split_name="Sex",
             ind_id=ind_id) |>
      select(ind_id,year,code, split_name, split_value,numerator, rate, upci, lowci, numerator, trend_axis, def_period)
    
    if (sex=="Female"){
    hle_popgrp_female <<- hle_popgrp #save sex specific dataframe as this will be overwritten by next function call
    
    } else if (sex=="Male") {
    hle_popgrp_male <<- hle_popgrp #save sex specific dataframe as this will be overwritten by next function call
    }
  
  write_csv(hle_popgrp, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny_popgrp.csv"))
  write_rds(hle_popgrp, file = paste0(profiles_data_folder, "/Data to be checked/",  indicator, "_shiny_popgrp.rds"))
  
}

# run the function for each of the sexes:
hle_pop_file(indicator="healthy_life_expectancy_female", sex ="Female", ind_id=99101)
hle_pop_file(indicator="healthy_life_expectancy_male", sex ="Male", ind_id=99102)


#run QA reports for main data
run_qa("healthy_life_expectancy_female", type="popgrp")
run_qa("healthy_life_expectancy_male", type="popgrp")