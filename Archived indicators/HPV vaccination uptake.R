# ~~~~~~~~~~~~~~~~~~~~~
# Archived -----
# ~~~~~~~~~~~~~~~~~~~~~~

# This indicator was archived in August 2024 due to outdated criteria.
# It has been replaced with 2 new indicators which better reflect the current JCVA guidelines
# and vaccination schedule in schools (see script named 'HPV vaccine uptake in S1 pupils'):
# 99144 - Uptake of HPV vaccine in S1 females
# 99145 - Uptake of HPV vaccine in S1 pupils 

# IMPORTANT: This indicator still needs updated following any boundary changes,
# such as new localities, datazones etc. as it is still in the tool, just not being
# updated with more recent years data.

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 13032 - Uptake of the HPV vaccine in S3 girls

# A data request is sent to the child health team following the release of HPV immunisations publication
# publication link: https://publichealthscotland.scot/publications/hpv-immunisation-statistics-scotland/ 
# (usually published in November)

# The team usually sent on 1 year worth of data each year
# the file should then saved in the 'raw data' subfolder within the HPC vaccine uptake' folder

# script outline:
# Step 1 - Prepare latest data
# Step 2 - Create trend data
# Step 3 - Run analysis functions 


# ~~~~~~~~~~~~~~~~~~~~~~~
# Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/deprivation_analysis.R")


# ~~~~~~~~~~~~~~~~~~~~~~
# Filepaths ----
# ~~~~~~~~~~~~~~~~~~~~~~
scotpho_folder <- "/PHI_conf/ScotPHO/Profiles/Data" # scotpho folder 
hpv_subfolder <- "Received Data/HPV vaccine uptake" # HPV vaccine uptake sub-folder
data_path <- file.path(scotpho_folder, hpv_subfolder) # full path to folder 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1 - Prepare latest data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in latest recieved data
# tweak filename to name of latest extract
hpv_new <- readRDS(file.path(data_path, "raw data", "2024 - HPV uptake.rds"))


# clean latest data
hpv_new <- hpv_new |>
  # clean column names 
  janitor::clean_names() |>
  # select and rename required columns 
  select(
    year = schoolyear_ending,
    datazone = geography,
    numerator = numerator_female,
    denominator = denominator_female
  ) |>
  # adjust year column from school ending year to school starting year
  # e.g year 2024 should be 2023 as analysis functions expect starting years for school year and fin year data
  mutate(year = year - 1) |>
  # only select datazone level data (or NAs to include in Scotland totals) 
  # the extract includes some additional levels but these will be calculated via analysis functions
  filter(grepl('S01', datazone11) | is.na(datazone11)) |>
  # note this particular extract has 3 identical NA rows which is an error in data provided -  only select 1 of them
  unique()


# save formatted file
# change filename to match school year of latest data
saveRDS(hpv_new, file.path(data_path, "formatted", "HPV_data_formatted_2023-24.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2 - Create trend data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get filepaths of all formatted files for each school year
formatted_files <- list.files(file.path(data_path, "formatted"), full.names = TRUE)


# read in formatted files and combine into one dataframe
hpv_trend <- data.table::rbindlist(lapply(formatted_files, function(x) {
  data <- readRDS(x) |>
    mutate(across(c("numerator", "denominator", "year"), as.numeric))
}), use.names = TRUE)


# save temp files for use in analysis functions 
saveRDS(hpv_trend, file.path(scotpho_folder, "Prepared Data", "hpv_uptake_raw.rds"))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3 - Run analysis functions  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# update the 'yearend' arguments to match the latest year in dataset

# create 'main' indicator dataset
main_analysis(filename = "hpv_uptake", 
              geography = "datazone11", 
              measure = "percent", 
              yearstart = 2009, 
              yearend = 2023, 
              time_agg = 3,
              year_type = "school",
              ind_id = 13032
)

# create 'deprivation' indicator dataset
deprivation_analysis(filename = "hpv_uptake", 
                     measure = "percent", 
                     time_agg = 3, 
                     yearstart= 2014, 
                     yearend = 2023, 
                     year_type = "school", 
                     ind_id = 13032)


## END