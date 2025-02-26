#new indicators introduced for care and wellbeing portfolio
# TO DO: enable automated checking at end of the indicator production
#         create a deprivation data file output (for any indicators that may have sutiable data)

###   Update ScotPHO Care and Wellbeing indicators: 
#   99116: Persistent poverty (includes both adults and children)
#   30155: Child persistent poverty (uses the same data as appears 99116 but this includes only child age group as this is specifically presented in CYP mental health indicators)
#   99117: Young peoples mental wellbeing (was known as 'Child wellbeing and happiness' in NPF but naming conventioned expected to change and we are adopting new name)
#   99118: Child material deprivation
#   99121: Health risk behaviours
#   99123: Gender balance in organisations (for minority ethnic population)


# Data source is the National Performance Framework open data on statistics.gov.scot
# 2024 update: https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx
# (N.B. THERE ARE OTHER SCOTPHO INDICATORS IN THIS FILE THAT COULD BE USED TO DE-DUPLICATE OTHER SCRIPTS. NOT READ IN CURRENTLY.)

### functions/packages ----
source("1.indicator_analysis.R") 
source("2.deprivation_analysis.R") 

##TO DO - adapt scripts to run using new functions

### Lookups

# bring in LA dictionary and include LA codes
la_lookup <- readRDS(paste0(lookups, "Geography/CAdictionary.rds"))%>%
  mutate(geographylevel="Local Authority")
hb_lookup <- readRDS(paste0(lookups, "Geography/HBdictionary.rds"))%>%
  mutate(geographylevel="Health Board")

area_lookup <-rbind(la_lookup,hb_lookup)

rm(hb_lookup,la_lookup)


### 1 - Read in data -----

# Specify url of the NPF file to download from stats.gov
url <- "https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx"

# Specify file name and where to save file to
file_name <- "NPF_database_2024.xlsx"
file_path <- paste0(data_folder, "Received Data/")

# Download file
download.file(url = url, destfile = paste(file_path, file_name, sep = ""))

# Read in data file
dat <- read_xlsx(paste0(file_path, file_name))


### 2. Prepare data  -----

data <- dat %>%
  
  # Clean column names
  clean_names() %>% 
  
  # Select relevant indicators 
  filter(indicator %in% c("Persistent poverty", 
                          "Child Wellbeing and Happiness", #NPF name for young peoples mental wellbeing indicator
                          #"Child material deprivation", # now source direct from stats.gov (see Chil(see Child Poverty.R script)d Poverty.R script)
                          #"Children's material deprivation", #now sourced direct from stats.gov
                          #"Child material deprivation", "Children's material deprivation", #indicators will come from stats.gov in future (see Child Poverty.R script)
                          "Contractually secure work",
                          "Health risk behaviours",
                          "Gender balance in organisations",
                          "Access to green and blue space",
                          "Healthy Start", #perinatal mort
                          "Employees on the Living wage", "Employees on the living wage",
                          "Places to interact",
                          "Satisfaction with housing", "Satisfaction with Housing",
                          "Quality of public services",
                          "Visits to the outdoors", "Visits to the Outdoors",
                          "Work place learning"
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
  filter(!(indicator=="Child Wellbeing and Happiness" & substr(disaggregation, 1, 18)!="Total Difficulties")) %>% # remove the SDQ subsection data, keep only Total Diffs
  
  # Convert indicator names to lower case and add underscore 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_")) %>%
         #indicator = str_replace_all(indicator, "children's", "child")) %>% #standardise these two indicators - not needed relates to child material deprivation which has moved

  # Standardise/simplify disaggregation names
  
  # Persistent poverty has splits labelled the wrong way round in 2024 data: reverse these
  mutate(temp_breakdown = ifelse(indicator=="persistent_poverty", disaggregation, breakdown),
         temp_disagg = ifelse(indicator=="persistent_poverty", breakdown, disaggregation)) %>%
  
  select(-c(breakdown, disaggregation)) %>%
  
  rename(split_value = temp_breakdown,
         split_name = temp_disagg) %>%

  # Sort the splits for child wellbeing and happiness
  mutate(split_name = gsub("Total Difficulties Score", "Total", split_name)) %>%
  mutate(split_name = gsub("Total X ", "", split_name)) %>%
  
  # Sort for satisfaction with housing (add note in tech doc that the splits refer to the highest income householder)
  mutate(split_name = gsub(" of the highest income householder", "", split_name)) %>%
  
  # standardise other splits:
  mutate(split_name = case_when(split_name %in% c("Six fold urban-rural 2020 classification", "Two fold urban-rural 2020 classification",
                                                          "Urban Rural  Classification 6-fold", "Urban Rural classification") ~ "Urban/Rural", TRUE ~ split_name)) %>%
  mutate(split_name = case_when(split_name %in% c("Scottish Index of Multiple Deprivation", "SIMD") ~ "Deprivation (SIMD)", TRUE ~ split_name)) %>%
  mutate(split_name = ifelse(split_name == "Equivalised Income", "Income (equivalised)", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Household tenure", "Tenure", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Self-perception of health", "Self-assessed health", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Declared limiting long term physical or mental health condition", "Limiting long term physical or mental health condition", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Local authority", "Local Authority", split_name)) %>%
  mutate(split_name = ifelse(split_name == "Employees on less than the Living wage", "Total", split_name)) %>%
  mutate(split_name = ifelse(split_name == "NHS Board", "Health Board", split_name)) %>%
  
  
  # Standardise/simplify breakdown names

        # Convert indicator names to lower case and hyphenate 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_")) %>%
         
        
         # Ensure age breakdowns are named consistently
        # breakdown = str_replace_all(breakdown, "Age ", ""),
        # breakdown = str_replace_all(breakdown, "-", " to "),

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
  
  # Formats
  mutate(split_value = gsub("  ", " ", split_value)) %>% # remove double spaces
  
  # Totals
  mutate(split_value = str_replace_all(split_value, "All", "Total")) %>% # this series used in pop group data file (both are found in there actually)
 
  
  # Add indicator ids
  mutate(ind_id = case_when(indicator == "persistent_poverty" ~ 99116,
                            indicator == "child_wellbeing_and_happiness" ~ 99117,
                            #indicator == "child_material_deprivation" ~ 99118,
                            indicator == "health_risk_behaviours" ~ 99121,
                            indicator == "gender_balance_in_organisations" ~ 99123,
                            # These indicators need IDs, and adding to techdoc:
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
                                # expand the ranges to show full years (some do but some don't)       
                                nchar(year)>4 ~ paste0(substr(year, 1, 4), "-", as.character(2000+as.numeric(substr(year, nchar(year)-1, nchar(year))))), 
                                TRUE ~ NA)) %>%
  mutate(year = case_when(nchar(year)==4 ~ as.numeric(year),
                          nchar(year)>4 ~ as.numeric(substr(year, 1, 4)) + 2)) %>% # add 2 to the start year of 4- and 5-year ranges as an approximate midpoint for plotting
  mutate(def_period = case_when(indicator == "healthy_start" ~ paste0(year, " calendar year"),
                                nchar(trend_axis)==4 ~ paste0(year, " survey year"),
                                as.numeric(substr(trend_axis, 8, 9)) - as.numeric(substr(trend_axis, 3, 4)) == 3 ~ paste0("4-year aggregate (",trend_axis,")"),
                                as.numeric(substr(trend_axis, 8, 9)) - as.numeric(substr(trend_axis, 3, 4)) == 4 ~ paste0("5-year aggregate (",trend_axis,")"))) %>%

  # Add geography codes
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
  
  #rename indicator to fit new name that NPF will adopt
  mutate(indicator = case_when (indicator=="child_wellbeing_and_happiness" ~ "young_peoples_mental_wellbeing", TRUE ~indicator)) %>%
  
  # Reorder data frame
  arrange(indicator, code, year) %>%
  distinct() # get rid of duplicates n=2979 now
  
# Make sure each non-geographic split-name has a Total split_value rate:

# get the totals 
totals <- data %>% 
  filter(split_value == "Total") %>% 
  filter(!(indicator %in% c("persistent_poverty", "contractually_secure_work") & split_name!="Age")) %>% # keep only Age split for these two, as some of their other 'Total' data differs, bizarrely
  select(c(ind_id, indicator, code, split_value, year, trend_axis, def_period, rate, numerator, lowci, upci)) %>%
  distinct() # n=158

# which rows require totals to be added in?
data_totals <- data %>%
  filter(split_name!="Total") %>% # drop all the totals
  filter(split_value!="Total") %>%
  filter(code=="S00000001") %>% # exclude CA and HB data (to be separated out into main data, with no splits available. Their 'total' is the Scotland-level data)
  select(ind_id, indicator, code, split_name, year, trend_axis, def_period) %>%
  distinct() %>%
  merge(y=totals, by=c("ind_id", "indicator", "code", "year", "trend_axis", "def_period")) # still n=629 #609

# get original rows for splits without totals
data_no_totals <- data %>% 
  filter(split_name!="Total") %>%
  filter(split_value!="Total") %>% 
  distinct() #2559 obs #2509

# get original rows for Scotland
scot_data <- data %>% 
  filter(split_name=="Total") %>% 
  distinct() #162

# combine these:
data_with_totals <- rbind(scot_data, data_no_totals, data_totals) %>% # 3270
  arrange(readr::parse_number(split_value)) # sorts the age groups into the right order (throws up warning about parsing failiure)

  

### 3. Prepare final files -----

# Function to prepare final files:
# Creates three data files for each indicator (main, popgroup, and deprivation data)
prepare_final_files <- function(ind){
  
  df <- data_with_totals %>%
    filter(indicator==ind)

  
     
  # 1 - Main data 
  # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
  
  maindata <- df %>%
    filter(split_name %in% c("Total", "Health Board", "Local Authority")) %>%
    select(code, ind_id, year, numerator, rate, lowci, upci, def_period, trend_axis)  #select fields required for maindata file (ie summary/trend/rank tab)

  # Save files
  write_rds(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  write.csv(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  write_rds(maindata, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
  write.csv(maindata, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  maindata_result <<- maindata

  
    
  # 2 - Population group data 
  # (ie data behind population groups tab)

  pop_grp_data <- df %>%
    filter(!split_name %in% c("Total", "Health Board", "Local Authority", "Deprivation (SIMD)")) %>%
    select(code, ind_id, split_name, split_value, year, numerator, rate, lowci, upci, def_period, trend_axis) 

  # Save files
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)

  #remove saving to test location once all indicators are approved and live in tool.
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  pop_grp_data_result <<- pop_grp_data
  
  
  
  # 3 - Deprivation data
  # National level only 
  if("Deprivation (SIMD)" %in% unique(df$split_name)) {
    simd1 <- df %>%
      filter(split_name == "Deprivation (SIMD)") %>%
      rename(quintile = split_value) %>%
      mutate(quint_type="sc_quin") %>%
      select(-split_name, -indicator)
  
  # Save intermediate SIMD file
  write_rds(simd1, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  write.csv(simd1, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  #get ind_id argument for the analysis function 
  ind_id <- unique(simd1$ind_id)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
                                 pop = "depr_pop_16+", # these are adult (16+) indicators, with no sex split for SIMD
                                 ind_id, 
                                 ind)
  
  }
  
}


# Create final files and run QA reports - QA report won't work until changes made to checking reports - come back to this

# Indicator 99116: Persistent poverty ---- #latest data 2012-2016 (no update available until)
prepare_final_files(ind = "persistent_poverty")


# Indicator 99117: Young peoples mental wellbeing  ----
prepare_final_files(ind = "young_peoples_mental_wellbeing")

  
# Indicator 99118: Child material deprivation ----
# will switch to sourcing data from stas.gov ind_id 30154 - slight name change but this is actually a better description
# see child poverty.R
#prepare_final_files(ind = "child_material_deprivation")


# Indicator  99121: Health risk behaviours ----
prepare_final_files(ind = "health_risk_behaviours")


# Indicator 99123: Gender balance in organisations (for minority ethnic population)
prepare_final_files(ind = "gender_balance_in_organisations")


# # Run QA reports 
 run_qa(filename = "persistent_poverty")
 run_qa(filename = "young_peoples_mental_wellbeing")
 run_qa(filename = "health_risk_behaviours")
 run_qa(filename = "gender_balance_in_organisations")

 #wont work until swtich to new functions
 run_qa(filename = "health_risk_behaviours", type="Deprivation", test_file=FALSE)
 
 

#END

