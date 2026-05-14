
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
# packages specifically for indicators
library(odbc) # for reading oracle databases (needed for many indicator data extractions)
library(stringr) #used for string manipulation/searching
library(readr) #used to write out rds files

#packages required for function
library(dplyr) # data wrangling
library(tidyr) # pivoting data longer
library(cli) # formatting custom error messages
library(RcppRoll) # for calculating rolling averages
library(utils) # for printing questions in console
library(varhandle) # for checking columns that need to be converted to numeric don't contain special characters
library(janitor) # helps cleaning imported variable names
library(purrr) # for running iterative processes
library(rmarkdown) # for running rmarkdown QA checks
library(plotly) # used in rmarkdown
library(htmltools) # used in rmarkdown
library(shiny) # used in rmarkdown
library(flextable) # used in rmarkdown
library(ggplot2) # used in rmarkdown
library(hablar) # sum_ function from hablar keeps NA when there should be NA

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# these small functions are used within the function below
source("functions/helper functions/check_file_exists.R") # to check file exists before attempting to read in 
source("functions/helper functions/validate_popgrps_columns.R") # for checking all required columns are present, named correctly and of correct class
source("functions/helper functions/check_year_parameters.R") # for checking years in the dataset before filtering on them
#source("functions/helper functions/check_denominator_years.R") # for checking years present in population denominator files
source("functions/helper functions/calculate_percent.R") # for calculating percent and confidence intervals
source("functions/helper functions/calculate_perc_pcf.R") # for calculating percent and confidence intervals
source("functions/helper functions/calculate_crude_rate.R") # for calculating crude rates and confidence intervals
source("functions/helper functions/calculate_easr.R") # for european age-sex standarised rates and confidence intervals
source("functions/helper functions/create_def_period_column.R") # for creating definition period column 
source("functions/helper functions/create_trend_axis_column.R") # for creating trend axis column 
# source("functions/helper functions/get_population_lookup.R") # for reading in correct population lookup if required
source("functions/helper functions/run_rmarkdown_QA.R") # for running QA rmarkdown doc
# source("functions/helper functions/create_agegroups.R") # converts single year age field to 5 year ageband - used in indicator data manipulation
source("functions/helper functions/create_geo_parents.R") # creates lookup which details the parent areas of smaller geographies (for QA checks)

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
  data <- validate_popgrps_columns(data, measure, geography, splits)

  # step complete
  cli::cli_alert_success("'Read and validate prepared data' step complete")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # filter by time period ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # check year parameters are sensible and all required years are present
  check_year_parameters(data, yearend, year_type)
  
  # filter time period
  data <- data |>
    filter(year >= yearstart & year <= yearend)
  
  # step complete
  cli::cli_alert_success("'Filter by time period' step complete - filtered between {yearstart} and {yearend}.")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate by geography level ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # this step ensures there is a numerator calculated for every possible geography level that can be created
  # (and a denominator if the measure is percentage - otherwise denominator (i.e. population figures) is added in the next step)

  # The number of geography levels that can be created for an indicator varies depending on the 'base'
  # geography level of the raw data file. E.g. since datazones are the building blocks of scotlands geographies, we can
  # aggregate data to many different geography types if the data is available at that granular level. However, if the data is for
  # instance only available at council level, we can only aggregate by council, health board and scotland.
  # See code below to understand which levels can be created for each base geography:

  if(geography != "multiple"){

    # read in geography lookup
    geography_lookup <- readRDS(file.path(geography_lookups, "DataZone11_All_Geographies_Lookup.rds")) |>
      mutate(scotland = "S00000001") # create scotland column

    # identify which columns to select from the lookup, depending on the base geography level of the data
    area_types <- switch(geography,
                         "scotland" =  c("scotland"),
                         "board" = c("hb2019", "scotland"),
                         "council" = c("ca2019", "hscp2019", "hb2019", "adp", "scotland"),
                         "intzone11" = c("intzone2011", "ca2019", "hb2019", "scotland", "adp", "hscp2019"),
                         "datazone11" = c("datazone2011", "intzone2011", "ca2019", "hb2019", "scotland", "adp", "hscp2019", "hscp_locality")
    )

    # if police division is set to true then add to geography list, default is false so will skip this step
    if (police_div==TRUE){
      area_types <- c(area_types,"pd")
    }

    # select those columns from the lookup
    geography_lookup <- geography_lookup |>
      select(all_of(area_types)) |>
      unique()


    # check if there any NAs in the geography code column - even though these cannot
    # be mapped to any local areas, they are used within the Scotland totals
    na_vals <- data |>
      filter(is.na(!!sym(area_types[1]))) |>
      rename("code" = !!sym(area_types[1])) |>
      mutate(code = "S00000001")

    # remove them from the dataset temporarily
    data <- data |>
      filter(!is.na(!!sym(area_types[1])))


    # join the geography lookup to the data so there is a column for each geography level
    data <- left_join(x = data, y = geography_lookup, by = area_types[1])


    # pivot the data into a 'longer' format so there is just one geography column called 'code'
    data <- data |>
      select(-contains("datazone")) |> # remove datazone column if this was the base geography - we don't publish at this level
      tidyr::pivot_longer(cols = any_of(area_types), values_to = "code", names_to = NULL)


    # add NA values back into the dataset and assign them the scotland geography code
    # to ensure they're included when data is summarised by geography in next step
    if(nrow(na_vals > 0)){
      data <- rbind(data, na_vals)
    }

  }

  # and finally, aggregate the data by each geography code
  if (NA_means_suppressed==FALSE) {

    data <- data |>
      group_by(across(any_of(c("code", "year", "age_grp", "sex_grp", names(splits))))) |>
      summarise_all(sum, na.rm = T) |>
      ungroup()
  }

  if (NA_means_suppressed==TRUE) {

    data <- data |>
      group_by(across(any_of(c("code", "year", "age_grp", "sex_grp", names(splits))))) |>
      summarise_all(sum_) |> #sum_ function from hablar better than sum here, as it still ignores NA when there is some data to sum, but retains NA if there are no counts, e.g., when suppressed data. sum turns suppressed counts into 0, when they should be NA.
      # means that aggregated totals that include some suppressed data will be calculated. The metadata should note that suppression exists in the data.
      ungroup()

  }

  # step complete
  cli::cli_alert_success("'Aggregate by geography level' step complete.")

#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Conditional step - Add population figures  ----
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   # This step is only applicable if the measure type is a standardised rate or a crude rate
#   # where population figures are required to calculate the rate. It reads in the correct
#   # population lookup file from scotphos population lookups folder, depending on whatever
#   # was passed to the 'pop' argument of the function.
#   
#   # add population lookups if required
#   if(!is.null(pop)){
#     
#     pop_lookup <- get_population_lookup(folder = population_lookups, pop, measure)
#     
#     # check what geography levels are in the dataset
#     geo_codes <- paste0("^", unique(stringr::str_sub(data$code, 1, 3)))
#     geo_codes <- paste(geo_codes, collapse = "|")
#     
#     # filter the population lookup so it only include required geo levels
#     pop_lookup <- pop_lookup |>
#       filter(grepl(geo_codes, code))
#     
#     # show warning if population lookup doesn't have enough years in it
#     # i.e. if your trying to update an indicator but new population estimates have not yet
#     # been published and added to our lookups to facilitate this
#     pop_max_year <- max(pop_lookup$year)
#     data_max_year <- max(data$year)
#     
#     if(data_max_year > pop_max_year){
#       
#       cli::cli_alert_warning("'Population lookup only contains population estimates up to {pop_max_year}. Unable to attach population estimates for {data_max_year}")
#     }
#     
#     # filter by time period
#     pop_lookup <- pop_lookup |>
#       filter(year >= yearstart & year <= yearend)
#     
#     
#     # identify which variables to join data by - the population lookups used for
#     # standardised rates also include age and sex splits
#     joining_vars <- c("code", "year", if(measure == "stdrate") c("age_grp", "sex_grp"))
#     
#     # full_join keeps all groups in the pop_lookup file and indicator data file
#     # full_join selected as a fail safe to try and prevent cases where either events in areas where apparently no population (which might indicate a problem with population lookup)
#     # or to keep an eye on areas with population but apparently no events, this might be legitimate for events that are rare or it might signify incomplete event/case data.
#     
#     #should this be full or left - based on population lookup?
#     data <- full_join(x = data, y = pop_lookup, by = joining_vars) 
#     
#     # check year parameters are sensible and all required years are present
#     check_denominator_years(data, yearend, yearstart)
#     
#     
#     # step complete
#     cli::cli_alert_success("'Add population figures' step complete.")
#     
#   }
#   
#   
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate splits and add totals  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
  #first capitalise columns with the split names if not done already. They are mapped before they are "pivoted" longer into split_name as it's more efficient to change once than on every row
  #note make_clean_names is used instead of standard clean_names() as the latter is only applicable to df cols and this works for vectors i.e. the list of split col names
  name_map <- setNames(janitor::make_clean_names(names(splits), case = "sentence"), names(splits))

  
  data <- purrr::map_dfr(names(splits), function(split) { #using purrr's map_dfr to run the code below across each of the split columns specified in the named list "splits"
    
    # create the split_name and split_value columns - rather than pivoting, summarised output will populate these columns
    data_split <- data |>
      mutate(split_name = name_map[[split]], #using name_map[[]] to apply sentence case when populating the split_name column
             split_value = .data[[split]]) #applying the data within the split column to the new split_value column
      
    # next aggregate each of the splits e.g. get male and female totals for all age groups, or age group totals for both sexes combined
    splits_aggregated <- data_split |>
      group_by(year, code, split_name, split_value) |>
      summarise(numerator = sum(numerator), denominator = sum(denominator), .groups = "drop")
    
    # finally calculate totals i.e. males + females, all age groups combined. This should match the output of the main analysis function
    totals <- data_split |>
      group_by(year, code, split_name) |>
      summarise(numerator = sum(numerator), denominator = sum(denominator), .groups = "drop") |>
      mutate(split_value = "All")
    
    # append totals onto main aggregated data
    bind_rows(splits_aggregated, totals)
  }) #close mapping
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate by time period ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Up until this stage, the numerators and denominators are calculated for each geography, for a single year only.
  # However, we often need to combine years in order to publish data if the figures are small or sensitive. This step
  # aggregates the data according to what number has been passed to the 'time_agg' argument of the function.

  # determine sort order or variables before aggregating
  var_order <- if(measure == "stdrate"){
    c("code", "sex_grp", "age_grp", "year")
  } else {
    c("code", "year")
  }

  if (NA_means_suppressed==FALSE) {

    # replace NAs with 0 before aggregating data by time period
    data <- data |>
      tidyr::replace_na(list(numerator = 0, # should est_pop be included here too? I don't have indicators with this column in...
                             denominator = 0))

    # aggregate by time period
    data<- data |>
      arrange(across(all_of(var_order))) |> # arrange data by var order
      group_by(across(any_of(c("code", "split_name", "split_value")))) |>
      # calculating rolling averages
      mutate(across(any_of(c("numerator", "denominator", "est_pop")), ~ RcppRoll::roll_meanr(., time_agg))) |>
      ungroup()
  }

  if (NA_means_suppressed==TRUE) { # additional na.rm=TRUE is the only difference here. Maybe possible to simplify?

    # aggregate by time period
    data<- data |>
      arrange(across(all_of(var_order))) |> # arrange data by var order
      group_by(across(any_of(c("code", "split_name", "split_value")))) |>
      # calculating rolling averages
      mutate(across(any_of(c("numerator", "denominator", "est_pop")), ~ RcppRoll::roll_meanr(., time_agg, na.rm=TRUE))) |>
      ungroup()
  }

  data <- data |>
    filter(!is.na(denominator) | is.nan(denominator)) |> # want to keep NaN but drop NA
    mutate(across(any_of(c("numerator", "denominator", "est_pop")), ~ ifelse(is.nan(.), NA, .))) #NaN result if time_agg is 1 and an NA is encountered. Reset as NA.
  #Want to keep suppressed data as NA so that these are available to show as empty cells on the dashboard, and are included in data downloads from there

  # step complete
  cli::cli_alert_success("'Aggregate by time period' step complete - aggregated by {time_agg} year{?s}")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate rate ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # By this stage there is a numerator and denominator for every geography and time period,
  # age and sex if the measure is a standardised rate,
  # and split_name and split_value
  # This step calculates the rate and confidence intervals
  # Note each rate has its own function. If you want to undertand how these rates are calculated
  # you can look at the code for those functions.


  if(measure == "percent"){
    data <- calculate_percent(data) # calculate percent
  } else if(measure == "crude"){
    data <- calculate_crude_rate(data, crude_rate) # calculate crude rate
  } else if(measure == "stdrate"){
    data <- calculate_easr(data, epop_total, epop_age) # calculate standarised rate
  } else if(measure == "perc_pcf"){
    data <- calculate_perc_pcf(data)
  }


  # step complete
  cli::cli_alert_success("'Calculate rate' step complete")


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add some metadata columns  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data <- data |>
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
  year_fix <- case_when(
    time_agg < 3 ~ 0,
    time_agg == 3 ~ 1,
    time_agg == 5 ~ 2,
    TRUE ~ NA_real_
  )

  # # select final columns
  data <- data |>
    mutate(year = year - year_fix) |> # adjust year column
    select(ind_id, year, code, split_name, split_value, numerator, rate, upci, lowci, trend_axis, def_period) |>
    arrange(year, code, split_name, split_value)

  # save the data as both an RDS and CSV file
 #saveRDS(data, paste0(output_folder, "/", filename, "_shiny_popgrps.rds"))
 #write.csv(data, paste0(output_folder, "/", filename, "_shiny_popgrps.csv"), row.names = FALSE)


  # make results available in global environment
  popgrps_analysis_result <<- data

  # Step complete
  cli::cli_alert_success("Final files saved.")


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run QA  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(QA){
    cli::cli_alert_info("Running QA")
    run_qa(type = "popgrp", filename={{filename}},test_file={{test_file}})
  }


  # all steps finished
  cli::cli_alert_success("All steps complete :)")
  
}
