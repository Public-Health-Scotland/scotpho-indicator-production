# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following 2 indicators:
# 21005 - Child dental health P1
# 21006 - Child dental health P7


# A data request is sent to the dental team following the release of National Dental Inspection Programme (NDIP) publication
# publication link: https://publichealthscotland.scot/publications/national-dental-inspection-programme/ 
# (usually published in October)


# The team usually sent on 1 year worth of data each year
# files should then saved in the 'Child dental health' folder

# Missing data:
# no 2020/21 data for Child Dental health P1, due to the pandemic
# no 2020/21 and 2021/22 data for Child Dental health P7, due to the pandemic
# no 2021/22 P1 data for NHS Western Isles and NHS Highland


# script changes required each year:
# change file path to read in latest data extracts
# update year in clean_data() function
# update school year when saving temporry RDS files for analysis functions
# update end_year parameter in analysis functions

# script outline:
# Step 1 - Prepare latest data
# Step 2 - create trend data
# Step 3 - Run analysis functions 


# ~~~~~~~~~~~~~~~~~~~~~~~
# functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~
source("1.indicator_analysis.R") # Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


# ~~~~~~~~~~~~~~~~~~~~~~
# filepaths ----
# ~~~~~~~~~~~~~~~~~~~~~~
scotpho_folder <- "/PHI_conf/ScotPHO/Profiles/Data" # scotpho folder 
dental_health_subfolder <- "Received Data/Child dental health" # child dental health sub-folder
data_path <- file.path(scotpho_folder, dental_health_subfolder) # full path to folder 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1 - Prepare latest data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### read in latest recieved data ----
# tweak filename to name of latest extract
# note: the number of rows to skip may need tweaked each year - these are rows containing info about the IR
p1_new <- read_excel(file.path(data_path, "NDIP_P1_2023-24.xlsx"), skip = 4)
p7_new <- read_excel(file.path(data_path, "NDIP_P7_2023-24.xlsx"), skip = 4)


### clean data ----

# data cleaning function
clean_data <- function(data, starting_school_year){
  data <- data |>
    # clean colum names 
    janitor::clean_names() |>
    # select and rename required columns 
    select(
      datazone = datazone_2011,
      numerator = number_of_c_letters_issued,
      denominator = total_inspected
    ) |>
    # add year column 
    mutate(year = starting_school_year) |>
    # summarise data
    group_by(year, datazone) |>
    summarise_all(sum, na.rm = T) |>
    ungroup()
  
  return(data)
}

# apply function to both datasets
# change year to match starting school year of latest data
p1_new <- clean_data(data = p1, starting_school_year = 2023)
p7_new <- clean_data(data = p7, starting_school_year = 2023)


# save clean formatted files
# change filename to match school year of latest data
saveRDS(p1_new, file.path(data_path, "formatted", "P1_data_formatted_2023-24.rds"))
saveRDS(p7_new, file.path(data_path, "formatted", "P7_data_formatted_2023-24.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create trend data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function to read in and combine single years data to create trend data
combine_files <- function(files) {
  combined <- data.table::rbindlist(lapply(files, function(x) {
    data <- readRDS(x) |>
      mutate(across(c("numerator", "denominator", "year"), as.numeric))
  }), use.names = TRUE)
  return(combined)
}


p1_trend <- combine_files(files = list.files(path = file.path(data_path, "formatted"), pattern = "P1", full.names = TRUE))
p7_trend <- combine_files(files = list.files(path = file.path(data_path, "formatted"), pattern = "P7", full.names = TRUE))


## save temp files for use in analysis functions 
saveRDS(p1_trend, file.path(scotpho_folder, "Prepared Data", "child_dental_p1_raw.rds"))
saveRDS(p7_trend, file.path(scotpho_folder, "Prepared Data", "child_dental_p7_raw.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run analysis functions  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Child dental health P1
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2023, time_agg = 1) 


analyze_second(filename = "child_dental_p1", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21005, year_type = "school", pop="DZ11_pop_5")


analyze_deprivation(filename="child_dental_p1", measure="perc_pcf",  
                    yearstart= 2014, yearend = 2023, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_5", ind_id = 21005)



# Child dental health P7
analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2023, time_agg = 1)


analyze_second(filename = "child_dental_p7", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21006, year_type = "school", pop="DZ11_pop_11")

analyze_deprivation(filename="child_dental_p7", measure="perc_pcf",  
                    yearstart= 2014, yearend = 2023, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_11", ind_id = 21006)



## END  