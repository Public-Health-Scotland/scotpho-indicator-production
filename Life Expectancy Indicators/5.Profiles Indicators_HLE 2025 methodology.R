# HEALTHY LIFE EXPECTANCY PROFILES INDICATOR USING REVISED 2025 ONS/NRS METHODOLOGY

#  ScotPHO indicators: 2 indicator outputs from this script 
#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE can only be generated at Scotland, NHS board and CA level.  It is currently NOT possibly to generate at smaller geographies as robust data on SAH (self assessed health is not available)
# Population splits by SIMD quintile and Urban/rural split for male/female HLE are not currently available


# HLE data published annually by NRS - july 2025 first release under new methodology - check website to see if new data has been published
# https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy/#


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") #doesn't use the functions, but quick way of getting packages and folders
library(readxl)

# Extracts for Life Expectancy data saved in left expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"

###############################################.
# Read in source data ----
###############################################.

# Data not yet included in any open data platforms so need to source either from NRS or ONS publication files
# Depending on the formatting (which has been inconsistent) it might be easier to obtain data from NRS or the ONS publication files
# i've left script for reading in via different options so easier to pick and choose from year to year.

# Feb:2026 revised metholdology HLE data not yet updated on SG open data so currently sourcing data from publication files either from NRS or ONS
# (data comes from ONS pivot table manipulated to include time series hle at birth data (<1 year) for all geographies and both sexes) 
# ugly process but quickest to manually manipulate the ONS pivot table then read in data

#scotland data
nrs_hle_scot <- read_excel((paste0(source_network,"healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 1",skip = 3) |>
  clean_names()|>
  filter(age_group=="<1") #only interested in HLE at birth

#nhs board data
nrs_hle_ca <- read_excel((paste0(source_network,"healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 2",skip = 3) |>
  clean_names()|>
  filter(age_group=="<1")

#nhs board data
nrs_hle_board <- read_excel((paste0(source_network,"healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 3",skip = 3) |>
  clean_names()|>
  filter(age_group=="<1")

nrs_hle <- rbind(nrs_hle_scot,nrs_hle_ca,nrs_hle_board)

nrs_hle <-nrs_hle|>
  mutate(trend_axis =paste0(substr(period,1,4),"-",substr(period,9,12)),
         def_period = paste(trend_axis, "(3 year aggregate)"),
         year = as.numeric(substr(trend_axis,1,4))+1, # year for LE is mid-point of aggregate years (this helps line up data series when comparing le & hle which aren't always same periods)
         code = case_when(area_code == "S92000003" ~ "S00000001", TRUE ~ area_code),
         split_value=sex, # required for pop group file
         numerator = NA,
         ind_id = case_when(sex == "Females" ~ 99101,
                            sex == "Males" ~ 99102)) |>  #required by shiny app but is null for HLE
  rename("rate" = "healthy_life_expectancy",
         "lowci" = "lower_95_percent_confidence_interval",
         "upci" = "upper_95_percent_confidence_interval")

#remove raw files
rm(nrs_hle_board,nrs_hle_ca,nrs_hle_scot)

# alternative script should it be easier to source from ONS publication files
#   ons_data <- read_excel((paste0(source_network,"/HLE data with CI/ons-data-tables (from NRS HLE Publication July 2025).xlsx")), sheet = "pivot_extract") %>%
  # setNames(tolower(names(.))) %>%  #variables to lower case
  # mutate(trend_axis =paste0(substr(period,1,4),"-",substr(period,9,12)),
  #        def_period = paste(trend_axis, "(3 year aggregate)"),
  #        year = as.numeric(substr(trend_axis,1,4))+1, # year for LE is mid-point of aggregate years (this helps line up data series when comparing le & hle which aren't always same periods)
  #        code = case_when(`area code` == "S92000003" ~ "S00000001", TRUE ~ `area code`),
  #        split_value=sex, # required for pop group file
  #        numerator = NA,
  #        ind_id = case_when(sex == "Female" ~ 99101,
  #                           sex == "Male" ~ 99102))|>  #required by shiny app but is null for HLE
  # rename("rate" = "hle",
  #        "lowci" = "lci",
  #        "upci" = "uci")


##########################################################################.
##1. Generate Main Data files for healthy life expectancy shiny files ----
##########################################################################.

hle_main_file <- function(data, indicator, sex_filter){
  
  maindata_df <- data |>
    filter(sex==sex_filter) %>%
    select(ind_id, year, code, numerator, rate, upci, lowci, numerator, trend_axis, def_period)
  
  if (sex_filter=="Females"){
    hle_main_file_female <<- maindata_df #saving sex specific dataframe to posit environment so it can be visually checked

  } else if (sex_filter=="Males") {
    hle_main_file_male <<- maindata_df #saving sex specific dataframe to posit environment so it can be visually checked
  }
  
  write_csv(maindata_df, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.csv"))
  write_rds(maindata_df, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.rds"))
}

# run the function for each of the sexes:
hle_main_file(data=nrs_hle, indicator="healthy_life_expectancy_female", sex_filter ="Females")
hle_main_file(data=nrs_hle, indicator="healthy_life_expectancy_male", sex_filter="Males")

#run QA reports for main data
run_qa("healthy_life_expectancy_female", type="main")
run_qa("healthy_life_expectancy_male", type="main")



###################################################################################.
##2. Generate Population Group shiny files for HLE ----
###################################################################################.


# function to prepare male and female file files  
hle_pop_file <- function(data, indicator, sex, ind_id ){
  
    hle_popgrp <- data %>%
      mutate(split_name="Sex",
             ind_id=ind_id) |>
      select(ind_id,year,code, split_name, split_value,numerator, rate, upci, lowci, numerator, trend_axis, def_period)
    
    if (sex=="Female"){
    hle_popgrp <- hle_popgrp |> #save sex specific dataframe as this will be overwritten by next function call
      mutate(ind_id = 99101) 
    
    } else if (sex=="Male") {
    hle_popgrp <- hle_popgrp |> #save sex specific dataframe as this will be overwritten by next function call
    mutate(ind_id = 99102) 
    }
  
  write_csv(hle_popgrp, file = paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny_popgrp.csv"))
  write_rds(hle_popgrp, file = paste0(profiles_data_folder, "/Data to be checked/",  indicator, "_shiny_popgrp.rds"))
  
}

# run the function for each of the sexes:
hle_pop_file(data = nrs_hle, indicator="healthy_life_expectancy_female", sex ="Female", ind_id=99101)
hle_pop_file(data = nrs_hle,  indicator="healthy_life_expectancy_male", sex ="Male", ind_id=99102)


#run QA reports for main data
run_qa("healthy_life_expectancy_female", type="popgrp")
run_qa("healthy_life_expectancy_male", type="popgrp")