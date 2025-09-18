# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following 6 indicators:
# 20902 - Population living in the 20% most access deprived datazones
# 20701 - Population income deprived
# 20702 - Working age population employment deprived
# 13003 - Young people living in the 20% most access deprived datazones 
# 13005 - Young people living in the 20% most crime deprived datazones
# 13004 - Young people living in the 20% most income deprived datazones

# We apply population weighting to calculate quintiles separately for each domain
# i.e. ranking the DZs according to their domain deprivation scores alongside a cumulative total of DZ populations. 
# The cut-off for the most deprived DZs is the point at which 20% of the population has been included.
# For a more thorough explanation of methodology, refer to the metadata in the techdoc.

# Each year when updating this indicator, the 'simd_list' below needs reviewed to 
# ensure there is a version of SIMD assigned to the year you are trying to update. 
# If there are no new versions of SIMD (they're only released every few years), 
# then simply extend the period in 'trends_from' for the most recent version of SIMD.


# ~~~~~~~~~~~~~~~~~~~~~~~~
# Dependencies ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(phslookups) # for get_simd_datazone() function to read in SIMD versions from cl-out

# uncomment and run below to install phslookups package:
# remotes::install_github("Public-Health-Scotland/phslookups")



# ~~~~~~~~~~~~~~~~~~~~~~~
# SIMD versions ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Details about each version of SIMD - matches Table 4 of PHS deprivation guidance:
# https://publichealthscotland.scot/media/24056/2023-12-phs-deprivation-guidance-v35.pdf

# dz_year = whether datazones are 2001/2011 based
# pop_year = year of population used in the index
# trend_years = period each version of SIMD should be applied to

simd_info <- list(
  # SIMD 2004 pop_year should be 2001 but our pop lookups only start at 2002
  "2004" = list(dz_year = "2001", pop_year = 2002, trend_years = 2002:2003),
  "2006" = list(dz_year = "2001", pop_year = 2004, trend_years =  2004:2006),
  "2009v2" = list(dz_year = "2001", pop_year = 2007, trend_years = 2007:2009),
  "2012" = list(dz_year = "2001", pop_year = 2010, trend_years = 2010:2013),
  "2016" = list(dz_year = "2011", pop_year = 2014, trend_years = 2014:2016),
  "2020v2" = list(dz_year = "2011", pop_year = 2017, trend_years = 2017:2023)
)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population estimates data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Early versions of SIMD are based on 2001 DZs and more recent versions are based on 2011 DZs
# Therefore use pop estimates based on both 2001 and 2011 DZs for different years of this indicators time series
pop_data <- list(
  "2001" = "DZ01_pop_basefile.rds",
  "2011" = "DZ11_pop_basefile.rds"
  )
  

# read in both pop files, aggregate to get totals per DZ and year for different age groups 
# We calculate population weighted quintiles based on the total population (i.e. the 'all_ages' column)
# However, the indicators themselves use different age groups to identify populations living in most deprived quintile
pop_data <- imap(pop_data, ~ {
  readRDS(file.path(profiles_data_folder, "Lookups/Population/", .x)) |>
    group_by(across(contains("datazone")), year) |>
    summarise(
      all_ages = sum(denominator), # total pop
      under25 = sum(denominator[age >= 0 & age <= 25]), # 0-25 pop (i.e. 'young people')
      working_age = sum(denominator[age >= 16 & age <= 64]), # 16-64 pop (i.e. 'working age')
      .groups = "drop"
    ) |>
    rename(datazone = 1)
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get HSCP localities -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# localities are not an  official geography with set of standard codes 
# so are not part of any of the SIMD lookups - have to add them on as a column 
localities <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "DataZone11_HSCLocality_Lookup.rds")) |>
  select(datazone = datazone2011, hscp_locality)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in SIMD lookups  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Read in each SIMD version and save into a named list of dataframes
# For each version:
simd_data <- imap(simd_info, ~ {
  
  
  ## 1. get SIMD lookup 
    phslookups::get_simd_datazone(simd_version = .y) |>
    select(
      # select geography cols
      datazone = 1,
      any_of(c(
        "intzone2011" = matches("^intzone2011$", ignore.case = TRUE),
        "ca2019" = matches("^CA2019$", ignore.case = TRUE),
        "hscp2019" = matches("^HSCP2019$", ignore.case = TRUE),
        "hb2019" = matches("^HB2019$", ignore.case = TRUE)
      )),
      # select domain cols
      any_of(c(
        "access" = ends_with("_access_rank"),
        "income" = ends_with("_inc_rank"),
        "crime" = ends_with("_crime_rank"),
        "employment" = ends_with("_emp_rank"),
        "education" = ends_with("educ_rank"),
        "housing" = ends_with("house_rank")
      ))
    ) |>
    # add localities column 
    left_join(localities, by = "datazone") |>
    # add scotland column 
    mutate(scotland = "S00000001") |>
    # move columns so all geo cols next to eachother
    relocate(c("hscp_locality", "scotland"), .after = "hb2019")
  
})



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate population weighted quintiles -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step 1 ----
# For each SIMD version, get pop estimates for the year that SIMD is based on 
# e.g. for SIMD2020v2, take 2011 pop estimates and filter on 2017
simd_index_pop <- imap(simd_info, ~ {
  pop_data[[.x$dz_year]] |> 
    filter(year == .x$pop_year) |>
    select(datazone, 
           index_year = year, 
           index_pop = all_ages)
})

## Step 2 ----
# For each SIMD, join with pop index year data
# e.g. for SIMD 2020v2, add 2017 pop estimates as a column 
simd_data <- imap(simd_data, ~ {
  left_join(.x, simd_index_pop[[.y]], by = "datazone")
})


## Step 3 ----
# For each SIMD pop index data, divide the total population into 5 equal intervals
# e.g. for SIMD2020v2, create intervals between 0 and 5,424,800 (total population in 2017)
# results: 0 1084960 2169920 3254880 4339840 5424800
quint_breaks <- imap(simd_data, ~ {
  seq(from = 0, to = sum(.x$index_pop), length.out = 6)
})


## Step 4 ----
# For each SIMD, calculate quintiles for each domain using breaks created above
# e.g for SIMD2020v2, go through each of the 6 domains one at a time and:
# order data from most (rank number 1) to least deprived
# calculate cumulative population total and:
# once DZ population hits 1084960, assign this first batch of DZs to Q1
# Once DZ population hits 2169920, assign next batch of DZs to Q2 etc. etc.
simd_data <- imap(simd_data, ~ {
    reduce(
      c("access", "income", "crime", "employment", "education", "housing"),
      function(df, domain) {
        
        # check if domain exists first (e.g. SIMD2004 doesn't have crime domain)
        if (domain %in% names(df)) {
          df |>
            arrange(!!sym(domain)) |>
            mutate("{domain}_quint" := cut(cumsum(index_pop),
                                           breaks = quint_breaks[[.y]],
                                           labels = 1:5,
                                           include.lowest = TRUE))
        } else {
          df
        }
        
      },
      .init = .x
    )
    
})


## Step 5 ----
# apply correct versions of SIMD to correct population data
# e.g. e.g. for SIMD2020v2, take 2011 pop estimates and filter from years 2017 - 2023
# and join pop data with that version of SIMD
simd_data <- imap(simd_info, ~ {
  
  pop_data[[.x$dz_year]] |>
    filter(year %in% .x$trend_years) |>
    left_join(simd_data[[.y]], by = "datazone")
    
})


## Step 6: Convert data list into a single dataframe
# and add a column to signify which SIMD version has been applied
# note there will be NAs for SIMD2004 crime domain as doesn't exist
# and NAs for IZs/localities until 2015 as they cannot be mapped to 2001 dzs
result <- imap_dfr(simd_data, ~ mutate(.x, simd_version = .y))



# pivot data longer to create 1 geography col
result <- result |>
  pivot_longer(
    cols = c("datazone", "intzone2011", "hscp_locality", "ca2019", "hscp2019", "hb2019", "scotland"), 
    names_to = "geo_type", 
    values_to = "code"
  ) |>
  filter(!is.na(code) & geo_type != "datazone") # this remove DZ data and IZ/locality data pre-2014


# tidy global env.
rm(localities, pop_data, quint_breaks, simd_data, simd_index_pop, simd_info)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create indicators-----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Function to create indicator data for different domains/populations 
create_indicator_data <- function(df = result, 
                                domain = c("access", "income", "crime", "employment", "education", "housing"), 
                                population = c("all_ages", "under25", "working_age"),
                                ind_id, yearstart, yearend
                                ){
  
  # name of domain col 
  domain_col <- paste0(domain, "_quint")
  
  
  df <- df |>
    # select cols
    select(code, year, quintile = {{domain_col}}, denominator = {{population}}) |>
    # create numerator
    mutate(numerator = if_else(quintile == 1, denominator, 0)) |>
    select(-quintile) |>
    # aggregate numerator and denominator per year/geography
    group_by(code, year) |>
    summarise_all(sum) |>
    ungroup()

  # save temp file
  filename <- paste0(ind_id, "_", domain, "_deprived_", population)
  folder <- paste0(profiles_data_folder, "/Prepared Data/")
  saveRDS(df, paste0(folder, filename, "_raw.rds"))

  cli_alert(text = "'{filename}' saved in {folder}")

  # run analysis function
  main_analysis(ind_id = ind_id, filename = filename, measure = "percent",
                geography = "multiple", year_type = "calendar", time_agg = 1,
                yearstart = yearstart, yearend = yearend)
}




##  20902 - Population living in the most access deprived datazones ----
create_indicator_data(ind_id = 20902, domain = "access", population = "all_ages", yearstart = 2002, yearend = 2023)


## 13003 - Young people living in the most access deprived datazones ----
create_indicator_data(ind_id = 13003, domain = "access", population = "under25", yearstart = 2002, yearend = 2023)


## 13004 - Young people living in the most income deprived datazones ----
create_indicator_data(ind_id = 13004, domain = "income", population = "under25", yearstart = 2002, yearend = 2023)


## 13005 - Young people living in the most crime deprived datazones ----
create_indicator_data(ind_id = 13005, domain = "crime", population = "under25", yearstart = 2002, yearend = 2023)


##  20701 - Population income deprived  ----
# i.e. Population living in the most income deprived datazones in Scotland?
create_indicator_data(ind_id = 20701, domain = "income", population = "all_ages", yearstart = 2002, yearend = 2023)


##  20702 - Working age population living in the most employment deprived datazones ----
# Working age population employment deprived?
create_indicator_data(ind_id = 20702, domain = "employment", population = "working_age", yearstart = 2002, yearend = 2023)



# ~~~~~~~~~~~~~~~~~~~~~~~~
# Notes re QA ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# A few points when looking at QA results:

# 1. Results should confirm that IZ/locality data only available for 2014 onwards

# 2. Scotland trend will always be roughly 20% since the 20% most deprived areas hold approx 20% of the scottish pop 
# (although rate may rise slightly in between pop index years before resetting again). 

# 3.IZ/locality trends may produce odd looking trends - this is to be expected e.g. where an IZ is comprised of a small
# number of DZs where they may fall within the 20% most deprived in one version of SIMD but not another.



## END






