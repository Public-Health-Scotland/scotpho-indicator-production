# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) # data wrangling
library(tidyr) # pivoting data longer
library(cli) # formatting custom error messages
library(RcppRoll) # for calculating rolling averages
library(utils) # for printing questions in console
library(varhandle) # for checking columns that need to be converted to numeric don't contain special characters
library(janitor) # helps cleaning imported variable names
library(rmarkdown) # for running rmarkdown QA checks
library(plotly) # used in rmarkdown
library(htmltools) # used in rmarkdown
library(shiny) # used in rmarkdown
library(flextable) # used in rmarkdown
library(ggplot2) # used in rmarkdown
library(purrr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# these small functions are used within the function below
source("functions/helper functions/check_file_exists.R") # to check file exists before attempting to read in 
source("functions/helper functions/create_quintile_data.R") # to check file exists before attempting to read in 
source("functions/helper functions/calculate_inequality_measures.R") # for calculating percent and confidence intervals
source("functions/helper functions/check_year_parameters.R") # for checking years in the dataset before filtering on them
source("functions/helper functions/calculate_percent.R") # for calculating percent and confidence intervals
source("functions/helper functions/calculate_crude_rate.R") # for calculating crude rates and confidence intervals
source("functions/helper functions/calculate_easr.R") # for european age-sex standarised rates and confidence intervals
source("functions/helper functions/create_def_period_column.R") # for creating definition period column 
source("functions/helper functions/create_trend_axis_column.R") # for creating trend axis column 
source("functions/helper functions/run_rmarkdown_QA.R") # for creating trend axis column 
source("functions/helper functions/validate_deprivation_columns.R") # for creating trend axis column 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Understanding this function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To understand how this function works you can run it line by line from this script (don't run the 'check function arguments' section)
# First, run all the code above to a. load the packages and b. source the small functions which are used within this larger function
# For a more in-depth understanding on how to calculate rates such as crude rates, standarised rates etc. check the code in the smaller functions.

# You'll also need to create some variables to use as placeholders for the functions arguments.
# un-comment and run the below variables before running lines of code
# This will prepare the 'Early deaths from CHD' indicator as an example:

# filename = "deaths_CHD_depr"
# measure = "stdrate"
# time_agg = 3
# yearstart = 2002
# yearend = 2022
# year_type = "calendar"
# pop_age = c(0,74)
# epop_age = "normal"
# epop_total = 182000
# ind_id = 20105
# test_file = TRUE


#' `analyse_deprivation` takes a formatted rds file containing raw datazone level data for single years. Aggregates by SIMD quintiles/deciles, geography and time period and calculates
#' a rate for each quintile, an SII, RII and PAR. Saves final file in RDS and CSV format and makes final result available in global environment called `analyse_deprivation_result`
#' The file created in this function is used in the 'Deprivation' tab of the ScotPHO profiles tool dashboard.
#' The following columns must be in the input file:
#' `year`, `datazone`, `numerator` +
#' `denominator` if measure is `percent` +
#' `age_grp` and `sex_grp` if measure is `stdrate`
#'
#'@param filename name of the rds file to read in. File should end in '_raw.rds' but this shouldn't be added to the argument.
#'@param measure type of rate to calculate - one of `percent`, `stdrate` or `crude`,
#'@param time_agg number of years to aggregate the data by. 
#'@param year_type type of year data refers to, for creating time period columns - one of `financial`, `calendar`, `survey`, `snapshot` or `school`.
#'@param pop_age age range of population for denominator. Numeric vector with start and end age i.e. c(0, 15) Arg only required if measure is `stdrate` or `crude` and if indicator is not for all ages.
#'@param pop_sex sex of population for denominator - one of `male`, `female` or `all`. Arg only required if measure is `stdrate` or `crude`.
#'@param yearstart start year to filter data by - 4-digit number
#'@param yearend end year to filter data by - 4-digit number
#'@param ind_id unique numeric id for indicator. Should match that assigned to the indicator in the technical document
#'@param test_file whether to save file to test folder or not - either `TRUE` or `FALSE`. Se to `FALSE` by default
#'@param QA = whether to run QA checks on the dataset - either `TRUE` or `FALSE`. Set to `TRUE` by default
#'@param epop_age only applicable to standardised rates. Should be one of `normal`, `16+`, `<16`, `0to25`, `11to25` or `15to25`
#'@param epop_total only applicable if measure is `stdrate`
#'@param crude_rate only applicable if measure is`crude rate`. Size of the population to use.



# deprivation_analysis(filename = "teen_preg", measure="crude", time_agg = 3, crude_rate = 1000,
#                      yearstart = 2014, yearend = 2022, year_type = "calendar",
#                      pop_age=c(15,19), pop_sex = "female", ind_id = 21001)
# 
# deprivation_analysis(filename = "hpv_uptake", measure = "percent", time_agg = 3,
#                      yearstart = 2014, yearend = 2022, year_type = "school",
#                      ind_id = 13032)
# 
# deprivation_analysis(filename="drug_deaths_depr", measure="stdrate", time_agg=5, 
#                     yearstart= 2006, yearend=2022, year_type = "calendar", epop_age="normal",
#                     epop_total =200000, ind_id = 4121, pop_sex = "all")



deprivation_analysis <- function(filename, yearstart, yearend, time_agg, 
                                 year_type = c("financial", "calendar", "survey", "snapshot", "school"),
                                 measure = c("percent", "crude", "stdrate"),
                                 pop_sex = c("male", "female", "all"),
                                 epop_age = NULL, epop_total = NULL, pop_age = NULL,
                                 pop_pcf = NULL, crude_rate = NULL, 
                                 ind_id,  QA = TRUE, test_file = FALSE){

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check function arguments ---
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ensure arguments with finite choices are valid
  measure <- rlang::arg_match(measure)
  year_type <- rlang::arg_match(year_type)
  pop_sex <- rlang::arg_match(pop_sex)
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~
  # File paths ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~
  
  # path to ScotPHO folder - within here there are different sub-folders
  # for saving data at different stages of indicator production process
  scotpho_folder <- "/PHI_conf/ScotPHO/Profiles/Data"
  
  # paths to ScotPHOs lookup folders - we create these lookups ourselves
  # repo: https://github.com/Public-Health-Scotland/scotpho-lookups 
  geography_lookups <- file.path(scotpho_folder, "Lookups", "Geography")
  population_lookups <- file.path(scotpho_folder, "Lookups", "Population")
  
  # folder where 'prepared data' should be saved, to be passed through this function.
  # (as data has to be in a particular format before it can be analysed)
  input_folder <- file.path(scotpho_folder, "Prepared Data")
  
  # folder where final indicator file is saved at the end of this function 
  output_folder <- if(test_file){
    file.path(scotpho_folder, "Test Shiny Data") # for newly developed indicators that have never been in the profiles tool
  } else {
    file.path(scotpho_folder, "Data to be checked") # for established indicators already in the profiles tool 
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read and validate prepared data ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # check data file exists before attempting to read in
  full_filename <- paste0(filename, "_raw.rds")
  check_file_exists(input_folder, full_filename)
  
  # read in data file
  data <- readRDS(file.path(input_folder, full_filename)) |>
    janitor::clean_names()

  # check required columns are present, named correctly and of correct class
  # The no. of cols varies depending on measure type of the indicator
  data <- validate_deprivation_columns(data, measure)
  
  # step complete
  cli::cli_alert_success("'Read and validate prepared data' step complete")
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign each geography to an SIMD quintile/decile  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # This step assigns each geography level to an SIMD quintile/decile:
  # It does so by joining the data with an SIMD lookup so that each datazone is assigned to:
  # a health board, council area and SIMD decile and quintile (at local and national level) 
  
  # Note there are 10 SIMD deciles (1 = 10% most deprived, 10 = 10% least deprived)
  # and 5 SIMD quintiles (1 = 20% most deprived, 5 = 20% least deprived)
  
  # Deciles and quintiles can either be national or local
  # E.g. a particular datazone might fall within SIMD quintile 5 at national level 
  # (i.e. be in the 20% least deprived area in Scotland - see 'sc_quin' column)
  # but it may only fall within SIMD quintile 3 at local level (i.e. it's not within the 20% least deprived areas
  # within the particular council/health board that the datazone belongs to - see 'hb_quin' and 'ca_quin' columns)
  
  
  # get simd lookup
  simd_lookup <- readRDS(file.path(geography_lookups, "simd_datazone_lookup.rds"))
  
  # join data with lookup
  data <- left_join(data, simd_lookup, by = c("datazone", "year"))

  
  # step complete
  cli::cli_alert_success("'Assign each datazone to an SIMD quintile/decile' step complete")
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate by geography and SIMD quintiles/deciles  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # This step involves aggregating the data up to Health board, council and Scotland level by:
  # SIMD quintile (health boards, councils, scotland level only)
  # SIMD decile (scotland level only)
  
  # This will produce a numerator for every geography, simd quintile/decile and year
  # (and a denominator if the measure is percentage - otherwise denominator (i.e. population figures) is added in the next step)
  
  # Note that we only calculate deprivation data for these 3 geography levels because 
  # aggregating by smaller geography levels (i.e. intermediate zones, HSC localities) 
  # would likely result in small numbers/volatility of rates/statistical disclosure issues

    simd_data <- rbind(
   aggregate_by_simd(data, geo_col = "scotland", simd_col = "sc_decile"), # scotland data, split by scotland deciles
    aggregate_by_simd(data, geo_col = "scotland", simd_col = "sc_quin"), # scotland data, split by scotland quintiles
    aggregate_by_simd(data, geo_col = "hb", simd_col = "hb_quin"), # health board data, split by board quintiles
    aggregate_by_simd(data, geo_col = "hb", simd_col = "sc_quin"), # health board data, split by sotland quintiles
    aggregate_by_simd(data, geo_col = "ca", simd_col = "ca_quin"), # council area data, split by council quintiles
    aggregate_by_simd(data, geo_col = "ca", simd_col = "sc_quin") # council area data, split by scotland quintiles
  )

  # create overall totals
   totals <- simd_data |>
    mutate(quintile = "Total") |>
     group_by(across(any_of(c("year", "code", "quint_type", "quintile", "sex_grp", "age_grp")))) |>
     summarise_all(sum) |>
     ungroup()

  # combine totals with SIMD splits
  simd_data <- simd_data |>
    rbind(totals)
    
    
  # remove any data that wasn't assigned to a datazone in the prepared datafile
  # that therefore couldn't be assigned to any SIMD quintiles/deciles
    simd_data <- simd_data |>
    filter(!is.na(code))
  

   # step complete
   cli::cli_alert_success("'Aggregate by geography and SIMD quintiles/deciles' step complete")
   

   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Add population figures  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   # This step is only applicable if the measure type is a standardised rate or a crude rate
   # where population figures are required to calculate the rate. It reads in the
   # SIMD datazone level population lookup file from scotphos population lookups folder
   # and filters the sex and age range according to the population of interest

   if(measure %in% c("stdrate", "crude")){

     if(!is.null(pop_age)){
     lower_age = pop_age[1] # minimum age
     upper_age = pop_age[2] # maximum age
     }
     
     #convert specified sex to code for lookup - this is used to filter population lookup
     if(pop_sex == "male") {
       pop_sex = 1
     } else if(pop_sex == "female") {
       pop_sex = 2
     } else {
       pop_sex = NULL  # No filtering on sex if "all"
     }

     # what vars to summarise the population lookup by
     grouping_vars <- c("year", "code", "quintile", "quint_type",
                        if(measure == "stdrate") c("age_grp", "sex_grp"))
     

     # read in the simd population lookup, filter by age group and summarise
         population_lookup <- readRDS(file.path(population_lookups, "simd_population_lookup.rds")) |>
         filter(year >= yearstart) 
         
         
    # if indicator is not for all ages, filter by age range. Otherwise do not filter
      if (!is.null(pop_age)){
        population_lookup <- population_lookup |> 
          filter(age >= lower_age & age <= upper_age)
      }
      
       
       # if indicator is for a single sex, filter pop lookup by that sex. Otherwise do not filter
       if (!is.null(pop_sex)) {
         population_lookup <- population_lookup |>
           filter(sex_grp == pop_sex)
       }
       
      
       population_lookup <- population_lookup |>
         mutate(age_grp = as.character(age_grp),
                sex_grp = as.character(sex_grp)) |>
         select(-age) |>
         group_by(across(all_of(grouping_vars))) |>
         summarise(denominator = sum(denominator), .groups = "drop")
    
         
    # join data with population lookup to add population column to use as denominator
   simd_data <- right_join(simd_data, population_lookup, by = grouping_vars)


   cli::cli_alert_success("'Add population figures' step complete")

    }
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Aggregate by time period  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   
   # Up until this stage, the numerators and denominators are calculated for each geography, for a single year only.
   # However, we often need to combine years in order to publish data if the figures are small or sensitive. This step
   # aggregates the data according to what number has been passed to the 'time_agg' argument of the function.
   
   # check year parameters are sensible
   check_year_parameters(simd_data, yearend, year_type)
   
   # filter years
   simd_data <- simd_data |>
     filter(year >= yearstart & year <= yearend)
   
   # replace NAs with 0 before aggregating data by time period
   simd_data <- simd_data |>
     tidyr::replace_na(list(numerator = 0,
                            denominator = 0))
   
   
   # determine sort order or variables before aggregating
   var_order <- if(measure == "stdrate"){
     c("code", "quintile", "quint_type", "sex_grp", "age_grp", "year")
   } else {
     c("code", "quintile", "quint_type", "year")
   }
   
   
   # aggregate by time period
   simd_data <- simd_data |>
     arrange(across(all_of(var_order))) |> # arrange data by var order
     group_by(across(all_of(setdiff(var_order, "year")))) |>
     # calculating rolling averages
     mutate(numerator = RcppRoll::roll_meanr(numerator, time_agg),
            denominator = RcppRoll::roll_meanr(denominator, time_agg)
     ) |>
     filter(!is.na(denominator)) |>
     ungroup()
   
   
   # step complete
   cli::cli_alert_success("'Aggregate by time period' step complete - aggregated by {time_agg} year{?s}")
   

   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Calculate rate ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   # By this stage there is a numerator and denominator for every geography and time period
   # (and age and sex if the measure is a standardised rate)
   # This step calculates the rate and confidence intervals
   # Note each rate has it's own function. If you want to undertand how these rates are calculated
   # you can look at the code for those functions.
   
   
   if(measure == "percent"){
     simd_data <- calculate_percent(simd_data) # calculate percent
   } else if(measure == "crude"){
     simd_data <- calculate_crude_rate(simd_data, crude_rate) # calculate crude rate
   } else if(measure == "stdrate"){
     simd_data <- calculate_easr(simd_data, epop_total, epop_age) # calculate standarised rate
   }
   
   
   # step complete
   cli::cli_alert_success("'Calculate rate' step complete")
   
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Calculate measures of inequality  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   # This step calculates, for every geography and time period:
   # an SII (slope index of inequality)
   # an RII (relative index of inequality)
   # and PAR (partial attributable risk)
   # To understand how these measures are calculated, See the functions R script.
   
   simd_data <- calculate_inequality_measures(simd_data)
   
   # step complete
   cli::cli_alert_success("'Calculate inequality measures (SII/RII/PAR)' step complete")
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Add some metadata columns  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   simd_data <- simd_data |>
     # create indicator id column - whatever id has been passed to the 'ind_id' argument should match
     # the id assigned to the indicator in our teams technical document
     mutate(ind_id = ind_id) |>
     # create trend axis column (used to create axis labels on trend charts)
     create_trend_axis_column(year_type, time_agg) |>
     # create definition period column (used to show time period for charts looking at a single year)
     create_def_period_column(year_type, time_agg)
   
   # Step complete
   cli::cli_alert_success("'Add some metadata columns' step complete.")
   
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Save final file  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   # the number of years to subtract from the 'year' column after calculating rolling averages
   # E.g. if 3 year rolling average (2017-2019) shift the year column back a year (2018)
   # note the numeric year column is only used in the shiny app to filter, for example on the max year for an indicator
   # the columns created in the step above provide a more accurate description of the time period covered
   
   # Note this step is technically not required and has no impact on the calculation of rolling averages or rates
   # but is being kept in due to having been used in legacy analysis functions
   # It will ensure that any automatated QA's that compares old data file (prepared via old functions) and new data file
   # (prepared by this new function) are comparing the correct years against eachother.
   
   year_fix <- case_when(
     time_agg < 3 ~ 0,
     time_agg == 3 ~ 1,
     time_agg == 5 ~ 2,
     TRUE ~ NA_real_
   )
   

   # select final columns
   simd_data <- simd_data |>
     mutate(year = year - year_fix) |> # adjust year column
     select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
               least_rate, par_rr, count))
   
   
   # save the data as both an RDS file
   saveRDS(simd_data, paste0(output_folder, "/", filename, "_ineq.rds"))

   # make results available in global environment
   deprivation_analysis_result <<- simd_data
   
   # Step complete
   cli::cli_alert_success("Final file saved in '{output_folder}'")
   
   
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # Run QA  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   if(QA){
     cli::cli_alert_info("Running QA")
     run_qa(type = "deprivation", filename={{filename}})
   }
   
   
   # all steps finished
   cli::cli_alert_success("All steps complete :) See file in {output_folder}")
   
}
   
   
   

  
 