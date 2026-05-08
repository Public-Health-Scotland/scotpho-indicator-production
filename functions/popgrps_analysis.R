
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# script summary ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. load packages 
# 2. source helper functions
# 3. define function arguments
# 4. check validity of function arguments
# 5. set file paths
# 6. read and validate data (e.g. checking all relevant cols present)
# 7. filter by defined time period
# 8. aggregate by geography level (e.g. building up datazones to larger geographies)
# 9. add population figures (conditional for crude/standardised rates)
# 10. aggregate over multiple years (conditional)
# 11. calculate rates
# 12. add metadata columns (e.g. indicator id)
# 13. save final file
# 14. run QA 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# these small functions are used within the function below
source("functions/helper functions/check_file_exists.R") # to check file exists before attempting to read in 

# ~~~~~~~~~~~~~~~~~~~~~~~
# file paths (derived when script sourced)----
# this filepath object will be created if the main_analysis script is sourced (before and functions are called)
# it will mean file path declarations sourcing indicator data can be shortened
# ~~~~~~~~~~~~~~~~~~~~~~~~

# path to ScotPHO folder - within here there are different sub-folders
# for saving data at different stages of indicator production process
profiles_data_folder <- "/PHI_conf/ScotPHO/Profiles/Data"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# understanding this function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

popgrps_analysis <- function(filename,
                          measure = c("percent", "stdrate", "crude", "perc_pcf"),
                          geography = c("scotland", "board", "council", "intzone11", "datazone11", "multiple"),
                          year_type = c("financial", "calendar", "survey", "snapshot", "school"),
                          ind_id, time_agg, yearstart, yearend, 
                          pop = NULL, epop_total = NULL, epop_age = NULL, crude_rate = NULL, test_file = FALSE, QA = TRUE, police_div = FALSE,
                          NA_means_suppressed = FALSE, splits){
 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check function arguments ---
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ensure arguments with finite choices are valid
  # this section should only work when calling the entire function, not running individually
  geography <- rlang::arg_match(geography)
  measure <- rlang::arg_match(measure)
  year_type <- rlang::arg_match(year_type)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~
  # file paths ----
  # file paths only generated when main analysis function is called.
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  
  # path to ScotPHO folder - within here there are different sub-folders
  # for saving data at different stages of indicator production process
  # profiles_data_folder <- "/PHI_conf/ScotPHO/Profiles/Data"
  
  # paths to ScotPHOs lookup folders - we create these lookups ourselves
  # repo: https://github.com/Public-Health-Scotland/scotpho-lookups 
  geography_lookups <- file.path(profiles_data_folder, "Lookups", "Geography")
  population_lookups <- file.path(profiles_data_folder, "Lookups", "Population")
  
  # folder where 'prepared data' should be saved, to be passed through this function.
  # (as data has to be in a particular format before it can be analysed)
  input_folder <- file.path(profiles_data_folder, "Prepared Data")
  
  # folder where final indicator file is saved at the end of this function 
  output_folder <- if(test_file){
    file.path(profiles_data_folder, "Test Shiny Data") # for newly developed indicators that have never been in the profiles tool
  } else {
    file.path(profiles_data_folder, "Data to be checked") # for established indicators already in the profiles tool 
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read and validate prepared data ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # check data file exists before attempting to read in
  full_filename <- paste0(filename, "_popgrps_raw.rds")
  check_file_exists(input_folder, full_filename)
  
  # read in data file
  data <- readRDS(file.path(input_folder, full_filename)) |>
    janitor::clean_names()
  
  # check required columns are present, named correctly and of correct class
  # The no. of cols and col names expected varies depending on geography level and measure type of the indicator.
  # data <- validate_columns(data, measure, geography)
  # 
  # # step complete
  # cli::cli_alert_success("'Read and validate prepared data' step complete")
  
}
