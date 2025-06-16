# ~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicators:
# 1570 - Smoking prevalence, aged 16+ years
# 1571 - Smoking prevalence, aged 16-34 years
# 1572 - Smoking prevalence, aged 16-64 years
# 1573 - Smoking prevalence, aged 65+ years
# 1574 - Smoking prevalence, aged 35-64 years 
# 1575 - Smoking prevalence, men aged >16 years
# 1576 - Smoking prevalence, women aged >16 years


# These indicators are sourced from the Scottish Health Survey (SHeS)
# They replace similar indicators which used to be sourced from the Scottish Household Survey
# Decision was made to remove old indicators from the tool and replace with these due to;
# different source, different time periods provided (now aggregate years instead of single years)
# and slightly different age splits (e.g. now 16+ instead of >16, 65+ instead of >65)
# See 'smoking prevalence.R' script in Archived folder for details on old prevalence indicators

# Note that the format of the data provided by the SHS team has varied over the years
# making it difficult to automate the process for these indicators
# It is highly likely that the steps below may need changed with each update!

# The source of these indicators is the Scottish Health Survey.
# Historically we used to use the Scottish Household Survey for smoking prevalence 
# but in 2023 the SG recommended we switch to the health survey as a more reliable source of health data


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filepaths/ packages/ functions  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# packages 
library(dplyr) # for data wrangling functions
library(data.table) # for function to combine files
library(janitor) # for function to clean column names
library(stringr) # for replacing strings


# Source main analysis script which enable QA reports to be called and run
source("functions/main_analysis.R")


# filepaths 
data_folder <- "/PHI_conf/ScotPHO/Profiles/Data" # data folder 
data_received <- file.path(data_folder, "Received Data", "Smoking prevalence data") # smoking prevalence sub folder 


# ~~~~~~~~~~~~~~~~~~~~~~~
# Read in data ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# get names of the data files containing data 
# at board, council and Scotland level in 5 year aggregates
files <- list.files(data_received, pattern = "HB_LA")


# read in and combine data files, adding a filename column
# the files provided to us are split up by age groups
data <- data.table::rbindlist(lapply(files, function(x) {
  read.csv(file.path(data_received, x)) |>
    mutate(filename = x) # create filename column as contains age group in it - no age group cols in datasets!
}), use.names = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~


# clean column names and filter on smoker status 
data <- data |>
  janitor::clean_names() |>
  filter(smoking_status == "Current smoker")



# create an age group column using the filename
data <- data |>
  mutate(age_grp = case_when(
    grepl("16_64", filename) ~ "16 to 64",
    grepl("16_34", filename) ~ "16 to 34",
    grepl("35_64", filename) ~ "35 to 64",
    grepl("16andover", filename) ~ "16+",
    grepl("65andover", filename) ~ "65+", TRUE ~ "other")
  ) 


# create one areaname column as the dataset contains multiple
data <- data |>
  rename(areatype = geography) |>
  mutate(areaname = case_when(
    areatype == "Scotland" ~ "Scotland",
    areatype == "Health Board" ~ paste("NHS", health_board),
    areatype == "Local Authority" ~ local_authority, TRUE ~ "other")
  ) |>
  select(-c(health_board, local_authority))




# 2013-2016 data uses numbers from 1-32 instead of local authority names
# assuming that these data relate to the alphabetical order of the local
# authorities in the dataset, we can create a lookup which assigns each number an LA
la_index <- data |>
  filter(areatype == "Local Authority") |>
  filter(!grepl("[1-9]", areaname)) |>
  select(areatype, areaname) |>
  unique() |>
  mutate(number = as.character(row_number())) |>
  rename(correct_areaname = areaname)


# replace the numbers for affected year 2013-2026 with the local authority name 
data <- left_join(data, la_index, by = c("areatype", "areaname" = "number")) |>
  mutate(areaname = if_else(grepl("[1-9]", areaname), correct_areaname, areaname)) |>
  select(-correct_areaname)


# get geography code lookup 
geo_lookup <- readRDS(file.path(data_folder, "Lookups", "Geography", "codedictionary.rds")) |>
  filter(grepl("^S00|^S12|^S08", code)) # only include CA, HB and Scotland codes


# replace the name of Edinburgh city to ensure it matches with our geography lookup
data <- data |>
  mutate(areaname = stringr::str_replace(areaname, "Edinburgh City", "City of Edinburgh"))

# replace blank sex with 'All'
data <- data |>
  mutate(sex = stringr::str_replace(sex, " ", "All"))


# join lookup with data to create geography code column 
data <- left_join(data, geo_lookup, by = "areaname")


# select and rename required columns 
data <- data |>
  select(year, 
         code,
         sex,
         age_grp,
         rate = percent,
         lowci = lower_ci,
         upci = upper_ci)


# add metadata cols (not we cannot use helper functions because the number
# of years the data is aggregated by is not consistent)
data <- data |>
  mutate(
    numerator = NA,
    trend_axis = year,
    def_period = paste0(year, "; survey years"),
    year = as.numeric(paste0("20", substr(year, 8, 9)))
  )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create and QA final files -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# function to prepare final files for each indicator 
prepare_indicator <- function(data, age_group, sex_group, ind_id){
  
  # filter on age and sex
  x <- data |>
    filter(age_grp == age_group & sex == sex_group) |>
    select(-c(age_grp, sex)) |>
    mutate(ind_id = ind_id) # create indicator id column
  
  # define filename 
  filename <- paste0(ind_id, "_smoking_prev_", age_group, "_", sex_group) 
  full_filename <- paste0(filename, "_shiny") # final filename must end in _shiny
  
  # save final files
  saveRDS(x, paste0(data_folder, "/Data to be checked/", filename, ".rds"))
  write.csv(x, paste0(data_folder, "/Data to be checked/", filename, ".csv"), row.names = FALSE)
  
  # run QA checks
  run_qa(filename, type = "main", test_file = FALSE)
  
}


# run functions to update each indicator and QA the files
prepare_indicator(data=data, ind_id = 1570, age_group = "16+", sex_group = "All")
prepare_indicator(data, ind_id = 1571, age_group = "16 to 34", sex_group = "All")
prepare_indicator(data, ind_id = 1572, age_group = "16 to 64", sex_group = "All")
prepare_indicator(data, ind_id = 1573, age_group = "65+", sex_group = "All")
prepare_indicator(data, ind_id = 1574, age_group = "35 to 64", sex_group = "All")
prepare_indicator(data, ind_id = 1575, age_group = "16+", sex_group = "Male")
prepare_indicator(data, ind_id = 1576, age_group = "16+", sex_group = "Female")


## Create a population group split file for the smoking prevalence in 16+ indicator
## the 16+ indicator features in the care and wellbeing profile and would be helpful to provide age and sex breakdowns

pop_groups_age <- data |>
  filter(sex=="All") |> #only provide age breakdown for 'all' sex not for males/females
  filter(age_grp %in% c("16+","16 to 34","35 to 64","65+"))|>
  # Create 'split-name' & split_value columns based
  mutate(split_value= case_when(age_grp=="16+" ~ "All (16+)", TRUE ~age_grp),
         split_name = "Age") |>
  # Remove columns no longer needed  
  select(!c(age_grp, sex))


pop_groups_sex <- data |>
  filter(age_grp %in% c("16+"))|>
  # Create 'split-name' & split_value columns based
  mutate(split_value= case_when(sex=="All" ~ "All (16+)", TRUE ~ sex),
         split_name = "Sex")|>
  # Remove columns no longer needed  
  select(!c(age_grp, sex))

                                
pop_groups_data <- bind_rows(pop_groups_age,pop_groups_sex) |>
  mutate(ind_id=1570) |>
  filter(!is.na(code)) # exclude where geography is unknown
  

# save final files
saveRDS(pop_groups_data, paste0(data_folder, "/Data to be checked/1570_smoking_prev_16+_shiny_popgrp.rds"))
write.csv(pop_groups_data, paste0(data_folder, "/Data to be checked/1570_smoking_prev_16+_shiny_popgrp.csv"), row.names = FALSE)

#run QA report
run_qa(filename="1570_smoking_prev_16+", type="popgrp", test_file = FALSE)


## END

