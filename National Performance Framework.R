#new indicators introduced for care and wellbeing portfolio

###################################################################################################################################################################
####### JUNE 2025: ##########
####### NB. Persistent poverty now produced in a separate script (Persistent poverty.R), as more recent data are available from a different source. ###############
###################################################################################################################################################################


# TO DO: enable automated checking at end of the indicator production
#         create a deprivation data file output (for any indicators that may have sutiable data)

###   Update ScotPHO Care and Wellbeing indicators: 
#   99121: Health risk behaviours
#   99123: Gender balance in organisations (for minority ethnic population)

# Indicator 99117 (Young peoples mental wellbeing) was previously produced here, but as this is sourced from the SHeS, and hasn't been updated in the NPF data file, we're
# now producing it with other SHeS indicators: starting with extraction of the microdata in the ScotPHO_survey_data repo, and then finishing the processing in this repo.

# Data source is the National Performance Framework open data on statistics.gov.scot
# 2024 update (August 2024): https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx
# (N.B. THERE ARE OTHER SCOTPHO INDICATORS IN THIS FILE THAT COULD BE USED TO DE-DUPLICATE OTHER SCRIPTS. NOT READ IN CURRENTLY.)
# (N.B.2 THE DATA AREN'T CONSISTENTLY PRESENTED IN THIS FILE, SO ADD NEW INDICATORS WITH CARE)



### Functions/packages ----
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA
library(readxl) # to read in excel spreadsheets


### Lookups ----

# bring in LA dictionary and include LA codes 
# NB the current 2 indicators not available at sub-national level, but others extracted from this file in the future could be.
la_lookup <- readRDS(paste0(profiles_lookups, "/Geography/CAdictionary.rds"))%>%
  mutate(geographylevel="Local Authority")
hb_lookup <- readRDS(paste0(profiles_lookups, "/Geography/HBdictionary.rds"))%>%
  mutate(geographylevel="Health Board")

area_lookup <-rbind(la_lookup, hb_lookup)


rm(hb_lookup, la_lookup)


### 1 - Read in data -----

# Specify url of the NPF file to download from stats.gov
url <- "https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx"

# Specify file name and where to save file to
file_name <- "NPF_database_2024.xlsx"
file_path <- paste0(profiles_data_folder, "/Received Data/")

# Download file
download.file(url = url, destfile = paste(file_path, file_name, sep = ""))

# Read in data file
dat <- read_xlsx(paste0(file_path, file_name))


### 2 - Prepare data  -----
# N.B. If additional indicators are read in from this file make sure that this processing captures all the possible permutations... 

data <- dat %>%
  
  # Clean column names
  clean_names() %>% 
  
  # Select relevant indicators 
  filter(indicator %in% c("Persistent poverty", 
                          "Health risk behaviours",
                          "Gender balance in organisations",
                        # Sourced from elsewhere now:  
                          # "Child material deprivation", "Children's material deprivation", # now source direct from stats.gov (see Child Poverty.R script) as more disaggregated there
                          # "Child Wellbeing and Happiness", #NPF name for young peoples mental wellbeing indicator
                        # Additional CWB indicators available:
                          "Access to green and blue space",
                          "Healthy Start", #perinatal mort
                          "Employees on the Living wage", "Employees on the living wage",
                          "Places to interact",
                          "Satisfaction with housing", "Satisfaction with Housing",
                          "Quality of public services",
                          "Visits to the outdoors", "Visits to the Outdoors",
                          "Work place learning",
                          "Contractually secure work"
                        # Other ScotPHO/CWB indicators that could be read in from this file:
                          # "Educational attainment 7", # school leaver attainment (other educ attainment vars too)
                          # "Child social and physical development",
                          # "Food Insecurity",
                          # "Healthy life expectancy - Female",                         
                          # "Healthy life expectancy - Male",
                          # "Healthy life expectancy: Females",                         
                          # "Healthy life expectancy: Males",
                          # "Healthy weight - Adult",
                          # "Influence over local decisions", "Loneliness", "Mental wellbeing", "Pay gap", 
                          # "Perceptions of local area", "Physical activity", "Premature mortality", "Unmanageable debt", "Young peoples participation"
                      )) %>%

  # Convert indicator names to lower case and add underscore 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_")) %>%

  # Standardise/simplify disaggregation names
  
  # Persistent poverty has splits labelled the wrong way round in 2024 data: reverse these
  mutate(temp_breakdown = ifelse(indicator=="persistent_poverty", disaggregation, breakdown),
         temp_disagg = ifelse(indicator=="persistent_poverty", breakdown, disaggregation)) %>%
  
  select(-c(breakdown, disaggregation)) %>%
  
  rename(split_value = temp_breakdown,
         split_name = temp_disagg) %>%

  # Sort for satisfaction with housing (add note in tech doc that the splits refer to the highest income householder)
  mutate(split_name = gsub(" of the highest income householder", "", split_name)) %>%
  
  # Standardise other splits:
  mutate(split_name = case_when(split_name %in% c("Six fold urban-rural 2020 classification", "Two fold urban-rural 2020 classification",
                                                          "Urban Rural  Classification 6-fold", "Urban Rural classification") ~ "Urban/Rural", TRUE ~ split_name)) %>%
  mutate(split_name = case_when(split_name %in% c("Scottish Index of Multiple Deprivation", "SIMD") ~ "Deprivation (SIMD)", TRUE ~ split_name)) %>%
  mutate(split_name = ifelse(split_name == "Equivalised Income", "Income (equivalised)", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Household tenure", "Tenure", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Self-perception of health", "Self-assessed health", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Declared limiting long term physical or mental health condition", "Limiting long term physical or mental health condition", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Employees on less than the Living wage", "Total", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Local authority", "Local Authority", split_name)) %>% # to be extracted into area column later
  mutate(split_name = ifelse(split_name == "NHS Board", "Health Board", split_name)) %>% # to be extracted into area column later
  
  # Standardise/simplify breakdown names

  # Convert indicator names to lower case and hyphenate 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_")) %>%
         

  # Age breakdowns 
  mutate(split_value = str_replace_all(split_value, "Age ", ""),
         split_value = str_replace_all(split_value, "-", " to "),

  # Add hyphen back in where needed:
         split_value = if_else(split_value == "Non to Limiting Longstanding Illness", "Non-Limiting Longstanding Illness", split_value),
         split_value = if_else(split_value == "Working to age adults", "Working-age adults", split_value)) %>% 
  
  # SIMD breakdowns 
  mutate(split_value = str_replace_all(split_value, "Quintile ", ""),
         split_value = if_else(str_detect(split_value, "^1$|1st|(?i)most"), "1", split_value), # regexp looks for single number, or "1st", or "most" (ignoring case)
         split_value = if_else(str_detect(split_value, "^2$|2nd"), "2", split_value),
         split_value = if_else(str_detect(split_value, "^3$|3rd"), "3", split_value),
         split_value = if_else(str_detect(split_value, "^4$|4th"), "4", split_value),
         split_value = if_else(str_detect(split_value, "^5$|5th|(?i)least"), "5", split_value)) %>%
  
  # Income splits
  mutate(split_value = case_when(split_value=="Bottom Quintile" ~ "1 - lowest income",
                               split_value=="Top Quintile" ~ "5 - highest income",
                               TRUE ~ split_value)) %>%
  # Gender
  mutate(split_value = case_when(split_value %in% c("Men", "Man") ~ "Men",
                                 split_value %in% c("Women", "Woman") ~ "Women",
                                 split_value %in% c("Males", "Male") ~ "Male",
                                 split_value %in% c("Females", "Female") ~ "Female",
                                 TRUE ~ split_value)) %>%
  
  # Disability
  mutate(split_value = case_when(split_value %in% c("Not disabled", "Not Disabled") ~ "Not disabled",
                                 TRUE ~ split_value)) %>%
  
  # Formats
  mutate(split_value = gsub("  ", " ", split_value)) %>% # remove double spaces
  
  # Totals
  mutate(split_value = str_replace_all(split_value, "All", "Total")) %>% # this series used in pop group data file (both are found in there actually)
 
  
  # Add indicator ids
  mutate(ind_id = case_when(#indicator == "persistent_poverty" ~ 99116, # subsequently split out child poverty into ind_id 30155
                            indicator == "health_risk_behaviours" ~ 99121,
                            indicator == "gender_balance_in_organisations" ~ 99123
                            # Uncomment once these CWB indicators have IDs and are added to techdoc (will be NA currently):
                            # indicator == "contractually_secure_work" ~ xxxxx,
                            # indicator == "access_to_green_and_blue_space" ~ xxxxx,
                            # indicator == "healthy_start" ~ xxxxx,
                            # indicator == "employees_on_the_living_wage" ~ xxxxx, 
                            # indicator == "places_to_interact" ~ xxxxx,
                            # indicator == "satisfaction_with_housing" ~ xxxxx, 
                            # indicator == "quality_of_public_services" ~ xxxxx,
                            # indicator == "visits_to_the_outdoors" ~ xxxxx, 
                            # indicator == "work_place_learning" ~ xxxxx
                            )) %>%
         
  # Create date variables (N.B. revisit the logic if more indicators added: available periods may change)
  mutate(trend_axis = case_when(nchar(year)==4 ~ year, # keep as is if single years
                                # expand the ranges to show full years (some currently do but some don't)       
                                nchar(year)>4 ~ paste0(substr(year, 1, 4), "-", as.character(2000+as.numeric(substr(year, nchar(year)-1, nchar(year))))), 
                                TRUE ~ NA)) %>%
  mutate(year = case_when(nchar(year)==4 ~ as.numeric(year),
                          nchar(year)>4 ~ as.numeric(substr(year, 1, 4)) + 2)) %>% # add 2 to the start year of 4- and 5-year ranges as an approximate midpoint for plotting
  mutate(def_period = case_when(indicator == "healthy_start" ~ paste0(year, " calendar year"),
                                nchar(trend_axis)==4 ~ paste0(year, " survey year"),
                                as.numeric(substr(trend_axis, 8, 9)) - as.numeric(substr(trend_axis, 3, 4)) == 3 ~ paste0("4-year aggregate (",trend_axis,")"),
                                as.numeric(substr(trend_axis, 8, 9)) - as.numeric(substr(trend_axis, 3, 4)) == 4 ~ paste0("5-year aggregate (",trend_axis,")"))) %>%

  # Add geography codes (N.B. current 5 indicators are Scotland-only)
  mutate(geographylevel = case_when(split_name %in% c("Local Authority", "Health Board") ~ split_name,
                                    TRUE ~ "Scotland")) %>%
  mutate(areaname = case_when(split_name %in% c("Local Authority", "Health Board") ~ split_value,
                              TRUE ~ "Scotland")) %>%  #3041
  filter(!(split_name=="Local Authority" & split_value=="Scotland")) %>% #Scotland data included in the LA split
  mutate(areaname = gsub("&", "and", areaname)) %>%
  mutate(areaname = ifelse(areaname%in% c("Edinburgh, City of", "Edinburgh,City of"), "City of Edinburgh", areaname)) %>%
  mutate(areaname = ifelse(areaname%in% c("Na h to Eileanan Siar", "Na to h Eileanan Siar"), "Na h-Eileanan Siar", areaname)) %>%
  mutate(areaname = ifelse(areaname== "Aberdeen city", "Aberdeen City", areaname)) %>%
  mutate(areaname = case_when(geographylevel=="Health Board" ~ paste("NHS", areaname),
                              TRUE ~ areaname)) %>%
  left_join(area_lookup, by=c("geographylevel","areaname")) %>%
  mutate(code = ifelse(areaname=="Scotland", "S00000001", code)) %>%
  
  # Create other variables required
  mutate(numerator = NA, 
         lowci = NA, upci = NA) %>%
  mutate(rate = round(figure, 1)) %>% # some irritating duplicates due to inconsistent rounding
  
  # # Select breakdowns of interest
  # filter(split_name %in% c(       )) %>% # Any to be removed?
  
  # Select relevant variables
  select(c(ind_id, indicator, code, split_name, split_value, year, trend_axis, def_period, rate, numerator, lowci, upci)) %>%
  
  # Reorder data frame
  arrange(indicator, code, year) %>%
  distinct() # get rid of duplicates, n=2805 now (still includes those with no ind_id)


  
# Now get the totals:
# Make sure each split has a Total split_value rate:
# This gets more complicated than it should, because of differences in the groupings available for each indicator.
# N.B. If additional indicators are read in from this file make sure that this processing captures all the possible permutations... 

# Get the totals: select a single total for each indicator-trendaxis-code grouping
totals <- data %>% 
  filter((ind_id != 99116 & split_value == "Total" & split_name == "Total") | 
           (ind_id == 99116 & split_value == "Total" & split_name == "Age") ) %>% # persistent poverty is the odd one out: there are only 3 values for split_name=="Total", but more for split_name=="Age"
  select(c(ind_id, indicator, code, split_value, year, trend_axis, def_period, rate, numerator, lowci, upci)) %>%
  distinct() # n=40

# Get the unique splits (by indicator-trendaxis-code) and drop their indicator data
splits_needing_totals <- data %>%
  filter(split_value != "Total") %>%
  select(c(ind_id, indicator, code, split_name, year, trend_axis, def_period)) %>%
  distinct() # n=1018 (includes all indicators, with/without ind_id)

# Get the total indicator data for each of those unique splits (the same total from the totals df is added to each unique split, by indicator-trendaxis-code)
splits_with_totals <- splits_needing_totals %>%
  merge(y=totals, by=c("ind_id", "indicator", "code", "year", "trend_axis", "def_period")) # n=122; just restricted to the 4 indicators with non-NA ind_id

# Get the original split data, and drop their total rows, if present
splits_without_totals <- data %>%
  filter(split_value != "Total") %>%
  filter(split_name != "Total")
  
# Add geography totals (can't even rely on these to be present in the original data, e.g., see persistent poverty)
all_data_with_totals <- totals %>%
  mutate(split_name="Total") %>% # makes the unsplit data for any geographies in the data (split_name=Total and split_value=Total)
  rbind(splits_with_totals) %>%
  rbind(splits_without_totals) %>% # n=2671
  filter(!is.na(ind_id)) %>% # n=598
  arrange(readr::parse_number(split_value)) # sorts the age groups into the right order (throws up warning about parsing failure, because most split_values don't have numbers in them. This is OK)

### Check availability: ----
# (and whether the convoluted processing above has worked)
availability <- all_data_with_totals %>%
  mutate(geog = substr(code, 1, 3)) %>%
  select(ind_id, geog, year, split_name, split_value) %>%
  unique() 
# cross-tabulate to check:
ftable(availability, row.vars = c("geog", "ind_id", "split_name"), col.vars = c("year"))
# shows that each indicator has split_name=total for the relevant years (these are the geography totals, which are only for Scotland currently, as there are no HB/CA data for the current indicators extracted (and kept) here)
# and that for each split_name there is one more grouping than its individual split_values (e.g., deprivation has 5 split_values + 1 total = 6; sex has 2 split_values + 1 total = 3)






### 3 - Prepare final files -----

# Function to prepare final files:
# Creates three data files for each indicator (main, popgroup, and deprivation data)
prepare_final_files <- function(ind, agerange=NULL){
  
  df <- all_data_with_totals %>%
    filter(indicator==ind) %>%
    arrange(code, year) # sort order for plotting purposes

  
     
  # 1 - Main data 
  # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
  
  maindata <- df %>%
    filter(split_name %in% c("Total", "Health Board", "Local Authority")) %>% 
    select(code, ind_id, year, numerator, rate, lowci, upci, def_period, trend_axis)  #select fields required for maindata file (ie summary/trend/rank tab)

  # Save files
  write_rds(maindata, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(maindata, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  maindata_result <<- maindata

  
    
  # 2 - Population group data 
  # (ie data behind population groups tab)

  pop_grp_data <- df %>%
    filter(!split_name %in% c("Total", "Health Board", "Local Authority", "Deprivation (SIMD)")) %>%
    select(code, ind_id, split_name, split_value, year, numerator, rate, lowci, upci, def_period, trend_axis) 

  # Save files
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)

  # Make data created available outside of function so it can be visually inspected if required
  pop_grp_data_result <<- pop_grp_data
  
  
  
  # 3 - Deprivation data
  # National level only 
  if("Deprivation (SIMD)" %in% unique(df$split_name)) {

  # Process SIMD data
  simd_data <- df %>% 
    filter(indicator == ind) %>% 
    filter(split_name=="Deprivation (SIMD)") %>%
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin") %>%
    select(-split_name, -indicator) %>%
    arrange(code, year, quintile)
  
  # get arguments for the add_population_to_quintile_level_data() function: (done because the ind argument to the current function is not the same as the ind argument required by the next function)
  ind_name <- ind # dataset will already be filtered to a single indicator based on the parameter supplied to 'prepare final files' function
  ind_id <- unique(simd_data$ind_id) # identify the indicator number 
  
  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop=paste0("depr_pop_", agerange),ind = ind_id,ind_name = ind_name) |>
    filter(!is.na(rate)) # not all years have data
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  simd_data_result <<- simd_data
  
  }
  
}



#== Create final files and run QA reports ----


# Indicator  99121: Health risk behaviours ----
prepare_final_files(ind = "health_risk_behaviours", agerange="16+")

# Indicator 99123: Gender balance in organisations (for minority ethnic population)
prepare_final_files(ind = "gender_balance_in_organisations", agerange="16+")





###  Run QA reports ----
run_qa(type = "main", filename = "health_risk_behaviours", test_file = FALSE)
run_qa(type = "main", filename = "gender_balance_in_organisations", test_file = FALSE)

run_qa(type = "deprivation", filename = "health_risk_behaviours", test_file = FALSE)
run_qa(type = "deprivation", filename = "gender_balance_in_organisations", test_file = FALSE)
 
run_qa(type = "popgrp", filename = "health_risk_behaviours", test_file = FALSE)
run_qa(type = "popgrp", filename = "gender_balance_in_organisations", test_file = FALSE)


#END

