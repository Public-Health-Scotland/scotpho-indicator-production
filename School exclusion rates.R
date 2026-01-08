
### 1. notes -----

# this script updates the following indicator: 13016 - School exclusion rate

# numerator source (no. of exclusions): https://www.gov.scot/publications/school-exclusion-statistics/ INCLUDES: PUBLICLY-FUNDED LOCAL AUTHORITY SCHOOLS ONLY. NOT INCLUDING GRANT-AIDED SCHOOLS. 
# denominator source (no. of pupils):   https://www.gov.scot/publications/pupil-census-supplementary-statistics/  (Table 5.1: Schools by local authority)
# update file names and ranges below when new data from links above are saved in "data received" folder.
# Latest data (2022/23) published March 2024
# SIMD time trend data to 2022/23 provided in 2025: AAE0015_exclusions_by_simd.xlsx

#################################################################################
### 1. Required packages/functions -----
#################################################################################

source("functions/main_analysis.R") # needed for the QA
source("functions/deprivation_analysis.R") # needed for the QA
library(hablar) # for sums that ignore NA only if other values are present, otherwise it returns NA
library(readxl) # for reading in excel files

# Read in geography lookup
geography_lookups <- file.path(profiles_data_folder, "Lookups", "Geography")
geo_lookup <- readRDS(file.path(geography_lookups, "opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# create lookup for higher geogs: get simd lookup, and aggregate (lowest geog is CA)
higher_geog_lookup <- readRDS(file.path(geography_lookups, "simd_datazone_lookup.rds")) %>%
  select(year, code = ca, hb, hscp, pd, scotland) %>%
  unique()

# Importing and processing the standalone SIMD data file, provided by SG in Aug 2025: 

get_simd_data <- function(tab_name) {
  
  df <- read_excel(path = exclusions_simd, sheet = tab_name) %>%
    mutate(year = as.numeric(substr(tab_name, nchar(tab_name)-3, nchar(tab_name))) - 1) # years in the tab are the end of the sch year, not the start
  names(df) <- c("areaname", "1", "2", "3", "4", "5", "NA", "Total", "year")
  df_name <- paste0("tab_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
}

# Functions to aggregate CA data to higher geogs

# SIMD data:
aggregate_higher_simd <- function(df, geog) {
  
  df <- df %>%
    select(year, quintile, code=geog, numerator, denominator) %>%
    group_by(year, quintile, code) %>%
    summarise(numerator = sum(numerator, na.rm=TRUE),
              denominator = sum(denominator, na.rm=TRUE)) %>%
    ungroup()
  
}

# Non-SIMD data:
aggregate_higher <- function(df, geog) {
  
  df <- df %>%
    select(year, code=geog, numerator, denominator) %>%
    group_by(year, code) %>%
    summarise(numerator = sum(numerator, na.rm=TRUE),
              denominator = sum(denominator, na.rm=TRUE)) %>%
    ungroup()
  
}


#################################################################################
### 2. Read in data -----
#################################################################################

# file paths:

# the folder where the data are saved
exclusions_folder <- paste0(profiles_data_folder, "/Received Data/School Exclusion Rates/")

# counts of exclusions (numerators)
exclusions2022 <- paste0(exclusions_folder, "Exclusions_202223.xlsx") # Numerators for Scotland and LAs
exclusions2024 <- paste0(exclusions_folder, "School+exclusions+2024-25+Corrected+December+2025.xlsx")

# SIMD data 2010-2023 (counts and denominators): could be used for Scotland and CA aggregates too.
exclusions_simd <- paste0(exclusions_folder, "AAE0015_exclusions_by_simd.xlsx") 

# pupil counts (denominators)
census2022 <- paste0(exclusions_folder, "Pupils_Census_2022.xlsx") # Denominators for Scotland, LAs, and SIMD 2022 
census2020 <- paste0(exclusions_folder, "Pupils_in_Scotland_2020.xlsx") # For SIMD denominators for 2020
census2024 <- paste0(exclusions_folder, "Pupil+census+supplementary+statistics+2024+-+March.xlsx")

#################################
## 2a. Import SIMD data 
#################################

# Get the data from the SIMD file
sheets <- readxl::excel_sheets(exclusions_simd)

for (tab in sheets) {
  get_simd_data(tab)
}

# combine the numerator tabs and the denominator tabs
numerator_data <- mget(ls(pattern = "tab_cases_"), .GlobalEnv) %>% # gets the dataframes starting with tab_cases_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  pivot_longer(-c(areaname, year), names_to="quintile", values_to = "numerator")

denominator_data <- mget(ls(pattern = "tab_denominators_"), .GlobalEnv) %>% # gets the dataframes starting with tab_denominators_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  pivot_longer(-c(areaname, year), names_to="quintile", values_to = "denominator")

# remove unwanted df
rm(list=ls(pattern="tab_"))

simd_scot_and_ca <- numerator_data %>%
  merge(y = denominator_data, by = c("areaname", "year", "quintile"), all = TRUE) %>% # checked: no extra rows added, perfect match
  mutate(areaname = ifelse(areaname=="All local authorities", "Scotland", areaname),
         areatype = ifelse(areaname=="Scotland", "Scotland", "Council area"),
         areaname = gsub(" and ", " & ", areaname)) %>%
  # remove data where denominator == 0
  filter(denominator>0) %>%
  merge(y = geo_lookup, by = c("areaname", "areatype"), all.x=TRUE) %>%
  select(-areatype, -areaname) %>%
  filter(quintile != "NA") 


# add higher geogs
simd_higher <- simd_scot_and_ca %>%
  filter(code!="S00000001") %>%
  # join data with lookup
  left_join(higher_geog_lookup, by = c("code", "year"))

# Run the aggregating function
simd_hb <- aggregate_higher_simd(simd_higher, "hb")
simd_hscp <- aggregate_higher_simd(simd_higher, "hscp")
simd_pd <- aggregate_higher_simd(simd_higher, "pd")

# combine all simd data
simd_all <- rbind(simd_scot_and_ca,
                  simd_hb,
                  simd_pd,
                  simd_hscp) %>%
  mutate(quint_type = "sc_quin",
         trend_axis = paste0(year, "/", as.character(substr(year+1, 3, 4))),
         def_period = paste0("School year (", trend_axis, ")"),
         ind_id = 13016) %>%
  calculate_crude_rate(., 1000)

# calculate the inequality measures
simd_all <- simd_all |>
  calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
  select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields

# save the data as RDS file
saveRDS(simd_all, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_ineq.rds"))

# Total counts (which include the pupils where SIMD is not known) compared with the published totals for Scotland and LAs.
# Differences of 1 to 3 exclusions out of >20,000 (0.01%), which would make little difference to percentages, 
# but also the published data have a longer time series, so still need to read that data in. 


#################################################################################
# 2b. Main data prep
#################################################################################

# Get Scotland and CA data (numerators and denominators), 
# ...then aggregate and calculate the indicator.

# Numerators:

# exclusions by council area (time series from 2007/08)
ca_exclusions <- read_xlsx(exclusions2022, sheet = "Table 2.1", skip = 3) |> 
  head(32) %>% #select first 32 rows (excludes all LA totals, as we get these for a longer time series from sheet Table 1.1)
  mutate_at(c(2:11), as.numeric) %>% # some suppressed values converted to NA
  pivot_longer(-c("Local Authority"), names_to = "year", values_to = "numerator") 

# exclusions scotland level (time series from 2002/03)
scotland_exclusions <- read_xlsx(exclusions2022, sheet = "Table 1.1", skip = 3) |> 
  filter(`Exclusion type` == "All cases of exclusion") |> #excludes breakdown of exclusion types
  mutate_at(c(2:16), as.numeric) %>% 
  select(-`Exclusion type`) |> 
  mutate("Local Authority" = "All local authorities") %>%
  pivot_longer(-c("Local Authority"), names_to = "year", values_to = "numerator") 


# Denominators:

# total pupils by council area 
total_pupils <- read_xlsx(census2022, sheet = "Table 5.2", skip = 4) |>  
  head(33) |> # we want 'All local authorities' rather than 'Scotland' here, as the Scotland figure includes Grant-aided schools that are not included in the exclusions stats.
  mutate_at(c(2:23), as.numeric) %>% 
  pivot_longer(-c(`Local Authority`), names_to = "year", values_to = "denominator") %>%
  mutate(year = as.numeric(substr(year, 1, 4)))

# Combine
exclusions_ca_scot <- rbind(ca_exclusions, scotland_exclusions) %>%
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  mutate(`Local Authority` = gsub(" and ", " & ", `Local Authority`)) %>% # for matching with denoms
  merge(y = total_pupils, by=c("Local Authority", "year"), all.x=T) %>% # add denoms
  rename(areaname = `Local Authority`) %>%
  mutate(areatype = ifelse(areaname == "All local authorities", "Scotland", "Council area"),
         areaname = ifelse(areaname == "All local authorities", "Scotland", areaname)) %>%
  # add the geog codes, 
  merge(y=geo_lookup, by=c("areaname", "areatype"), all.x=TRUE) %>%  # kept all.x to check the match... produced a full match...
  select(-starts_with("area"))

# Could run the main_analysis function on the CA data to produce all aggregations, including Scotland, 
# but the Scotland time trend goes back further, and we want to keep that data.
# This code ensures that.

# add higher geogs
exclusions_higher <- exclusions_ca_scot %>%
  filter(code!="S00000001") %>%
  # join data with lookup
  left_join(higher_geog_lookup, by = c("code", "year"))

# Run the aggregating function
exclusions_hb <- aggregate_higher(exclusions_higher, "hb")
exclusions_hscp <- aggregate_higher(exclusions_higher, "hscp")
exclusions_pd <- aggregate_higher(exclusions_higher, "pd")

# combine all data
exclusions_all <- rbind(exclusions_ca_scot,
                        exclusions_hb,
                        exclusions_pd,
                        exclusions_hscp) %>%
  mutate(trend_axis = paste0(year, "/", as.character(substr(year+1, 3, 4))),
         ind_id = 13016,
         def_period = paste0("School year (", trend_axis, ")")) %>%
  calculate_crude_rate(., 1000)


### Prepare main data file -----

main_data <- exclusions_all %>% 
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  unique() %>%
  arrange(code, year)

# Save
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_shiny.csv"), row.names = FALSE) 


##########################################################
### 4. Run QA reports -----
##########################################################

# # Run QA reports 
run_qa(type ="main",filename="school_exclusions", test_file=FALSE)
run_qa(type = "deprivation", filename="school_exclusions", test_file=FALSE)

##END
