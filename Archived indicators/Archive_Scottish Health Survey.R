## FEB 2025 - VE
## SCRIPT ARCHIVED AND NEW VERSION CREATED WHICH TAKES DATA FROM STATS.GOV PLATFORM RATHER THAN BESPOKE REQUEST
## NEW SCRIPT CREATED BY LIZ RICHARDSON AND INCLUDED A NUMBER OF ADDITIONAL SCOTPHO PROFILES INDICATORS
## THIS MEANT THAT ONE SCRIPT COULD GENERATE MULITPLE INDICATORS WHERE ULITMATE SOURCE WAS SCOTTISH HEALTH SURVEY

## REPLACEMENT SCRIPT 'Scottish Health Survey.R' added to github repo 


#########################################################
# Scottish Health Survey data import
#########################################################


### Update ScotPHO indicators sourced from Scottish Health Survey: 

### Care and Wellbeing indicators: 
#   99105: Food insecurity
#   99106: Adult Healthy Weight 
#   99107: Summary activity levels (also MEN)
#   99108: Self-assessed health of adults (age 16+) (also MEN)
#   99109: Limiting long-term conditions (age 16+) (also MEN)
### Adult mental health indicators:
#   30013: Fruit & vegetable consumption (guidelines) 
#   30003: General health questionnaire (GHQ-12) 
#   30001: Mental wellbeing (WEMWBS)

# all indicators available as male/female/all splits (Scotland, HB, CA)
# all indicators also available for age, SIMD, income and long-term conditions splits (Scotland only).
# This script runs the deprivation analysis on the SIMD-level data.

# Data source is the Scottish Health Survey - received dashboard files from SHeS team (scottishhealthsurvey@gov.scot)
# data supplied in .sav file format (this is file format used by SPSS - requires haven package to open in R).

### functions/packages -----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R") 

library(haven) # for reading in .sav files supplied by SG


### 1. Read in data ----

# Identify data folder
shes_data_folder <- paste0(data_folder, "Received Data/Scottish Health Survey/")


## Main data  -----
## This is dataset behind summary/trend/rank tabs of profiles tool

# Identify main data file with geographic breakdowns and aggregated Scotland data (create list of data files)
main_data_files <- paste0(shes_data_folder, list.files(path = shes_data_folder, pattern = "rank"))

# Read in main data file
main_data_raw <- read_spss(main_data_files)


## Pop groups data -----
## This is dataset behind population group tab of profiles tool

# Identify data files with population group breakdowns at Scotland level (for single years)
pop_grp_data_files <- paste0(shes_data_folder, list.files(path = shes_data_folder, pattern = "trend"))

# Read in population groups data files
pop_grp_data_list <- lapply(pop_grp_data_files, read_spss)

# Name each file (for later use in the split_name column)
names(pop_grp_data_list) <- c("Age", "Income (equivalised)", "Long-term conditions", 
                              "Sex", "Deprivation (SIMD)")

# Rename columns so they're consistent across files and rows can be combined
pop_gpr_col_names <- c("year", "areaname", "topic", "indicator", "categories", 
                       "split_value", "unweightedbases", "mean", "lowci", "upci", "rate")
pop_grp_data_list <- lapply(pop_grp_data_list, setNames, pop_gpr_col_names)

# Save as single data frame
pop_grp_data_raw <- bind_rows(pop_grp_data_list, .id = "split_name")


## Geography lookup -----

# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))



### 2. Prepare data  -----

## Prepare data sets so they can be combined (to reduce duplicate wrangling) ----

# Rename, add and remove relevant columns for main data
main_data <- main_data_raw %>% 
  clean_names() %>% 
  rename(areaname = location,
         areatype = geographylevel,
         rate = percent,
         lowci = lower_ci,
         upci = upper_ci,
         split_value = sex) %>% 
  mutate(split_name = "Sex") %>% 
  select(!significance_scot)

# Add new area type column to pop groups data
pop_grp_data <- pop_grp_data_raw %>% 
  mutate(areatype = "Scotland")

# Combine to single data frame
all_data <- bind_rows(main_data, pop_grp_data)



## Wrangle single data frame ----

data <- all_data %>% 
  
  # Filter for relevant indicators
  filter(indicator %in% c("Self-assessed general health", # Very good/Good
                          "Long-term conditions", # Limiting long-term conditions
                          "Healthy weight",
                          "Food insecurity (worried would run out of food)",
                          "Fruit & vegetable consumption (guidelines)", # 5 portions or more
                          "General health questionnaire (GHQ-12)", # Score 4+
                          "Mental wellbeing (WEMWBS)", # keep mean score
                          "Summary activity levels"), # Meets recommendations
         
         # Filter for category of interest for each indicator
         categories %in% c("Healthy weight", # Healthy weight
                           "Very good/Good", # Self-assessed health
                           "Limiting long-term conditions",
                           "Yes", # Food insecurity
                           "5 portions or more", # fruit and veg
                           "Score 4+", # GHQ
                           "Meets recommendations", # phys activity
                           "" #WEMWBS
         )) %>% 
  
  # Tidy area type and names
  mutate(areatype = str_to_sentence(areatype),
         areatype = if_else(areatype == "Local authority", "Council area", areatype),
         areaname = str_replace_all(areaname, c(" and " = " & ")),
         areaname = str_replace_all(areaname, c("Edinburgh City" = "City of Edinburgh")),
         areaname = if_else(areatype == "Health board", paste0("NHS ", areaname), areaname),
         
         # Tidy indicator names
         indicator = case_when(indicator == "Self-assessed general health" ~ "self_assessed_health",
                               indicator == "Long-term conditions" ~ "limiting_long_term_condition",
                               indicator == "Healthy weight" ~ "healthy_weight",
                               indicator == "Food insecurity (worried would run out of food)" ~ "food_insecurity",
                               indicator == "Fruit & vegetable consumption (guidelines)" ~ "fruit_veg_consumption", 
                               indicator == "General health questionnaire (GHQ-12)" ~ "common_mh_probs", 
                               indicator == "Mental wellbeing (WEMWBS)" ~ "mental_wellbeing", 
                               indicator == "Summary activity levels" ~ "physical_activity"),
         
         # Round confidence intervals to 0 dp to match main estimate # wemwbs = 1dp
         # lowci = round(lowci),
         # upci = round(upci),
         
         # Copy mean scores (WEMWBS) to the rate column
         rate = ifelse(indicator == "mental_wellbeing", mean, rate),
         
         # Create new indicator id and date columns
         ind_id = case_when(indicator == "self_assessed_health" ~ 99108,
                            indicator == "limiting_long_term_condition" ~ 99109,
                            indicator == "healthy_weight" ~ 99106,
                            indicator == "food_insecurity" ~ 99105,
                            indicator == "fruit_veg_consumption" ~ 30013, 
                            indicator == "common_mh_probs" ~ 30003, 
                            indicator == "mental_wellbeing" ~ 30001, 
                            indicator == "physical_activity" ~ 99107),
         trend_axis = year,
         year = if_else(str_detect(trend_axis, "-"), as.numeric(str_sub(year, start = 1, end = 4))+2, as.numeric(year)),
         def_period = if_else(str_detect(trend_axis, "-"), paste0("4-year aggregate"," (", trend_axis, ")"), paste0(year, " survey year")),
         numerator = NA) %>% 
  
  # Join geography codes
  left_join(dictionary, by = c("areatype", "areaname")) %>%
  
  # Select relevant columns
  select(ind_id, indicator, code, year, trend_axis, def_period, split_name, split_value, rate, lowci, upci, numerator)



### 3. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data (4-year aggregate)
  main_data_final <- data %>% 
    filter(indicator == ind,
           split_value == "All",
           str_detect(def_period, "aggregate")) %>% 
    select(!c(split_name, split_value, indicator)) %>% 
    unique() 
  
  write.csv(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))

  # 2 - population groups data (ie data behind population groups tab)
  # Contains LA/HB data by sex (4-year aggregate) and Scotland data by sex/age/condition/income/simd (single year)
  
  # Get the totals to add to each split:
  # extract the available splits
  split_name <- unique(data$split_name[data$indicator == ind])
  
  # extract the totals (split_value==All)
  totals <- data %>% 
    filter(indicator == ind,
           split_value == "All") %>% 
    select(-split_name) %>% 
    unique() 
  
  # unique combinations of these:
  totals2 <- crossing(totals, split_name) 
  
  # need to restrict to the code x split_name x def_period in the data
  split_totals <- data %>%
    filter(indicator == ind) %>% 
    select(code,split_name,def_period) %>% 
    unique() %>%
    merge(y=totals2, by=c("code", "split_name", "def_period"))

  # now add to the popgrp data
  pop_grp_data <- data %>% 
    filter(indicator == ind) %>% 
    filter(split_value != "All") %>% # remove for those splits that already had "All"
    rbind(split_totals) %>%
    mutate(split_value = ifelse(split_value=="All", "Total", split_value)) %>%
    select(!indicator)
  
  # remove SIMD data for further analysis
  pop_grp_data_final <- pop_grp_data %>%
    filter(split_name!="Deprivation (SIMD")

  # Save
  write.csv(pop_grp_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))

  # Process SIMD data
  simd_data <- pop_grp_data %>%
    filter(split_name=="Deprivation (SIMD)") %>%
    mutate(quintile = case_when(split_value=="1st-Most deprived" ~ "1",
                                split_value=="2nd" ~ "2",
                                split_value=="3rd" ~ "3",
                                split_value=="4th" ~ "4",
                                split_value=="5th-Least deprived" ~ "5",
                                TRUE ~ split_value)) %>%
    mutate(quint_type="sc_quin") %>%
    select(-split_name, -split_value)

  
  # Save intermediate SIMD file
  write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  ind_id <- unique(simd_data$ind_id)
  ind_name <- ind
  
  # Run the deprivation analysis 
  analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
                                 pop = "depr_pop_16+", # these are adult (16+) indicators, with no sex split for SIMD
                                 ind = ind_id, ind_name = ind_name)
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data_final
  pop_grp_data_result <<- pop_grp_data_final
  simd_data_result <<- simd_data
  
}


# Run function to create final files
prepare_final_files(ind = "food_insecurity")
prepare_final_files(ind = "self_assessed_health")
prepare_final_files(ind = "limiting_long_term_condition")
prepare_final_files(ind = "healthy_weight")
prepare_final_files(ind = "fruit_veg_consumption")
prepare_final_files(ind = "common_mh_probs")
prepare_final_files(ind = "mental_wellbeing")
prepare_final_files(ind = "physical_activity") 




# Run QA reports 
# main data
run_qa(filename = "food_insecurity")
run_qa(filename = "self_assessed_health")
run_qa(filename = "limiting_long_term_condition")
run_qa(filename = "healthy_weight")
run_qa(filename = "fruit_veg_consumption")
run_qa(filename = "common_mh_probs")
run_qa(filename = "mental_wellbeing")
run_qa(filename = "physical_activity") 

# ineq data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_ineq_qa(filename = "food_insecurity")
run_ineq_qa(filename = "self_assessed_health")
run_ineq_qa(filename = "limiting_long_term_condition")
run_ineq_qa(filename = "healthy_weight")
run_ineq_qa(filename = "fruit_veg_consumption")
run_ineq_qa(filename = "common_mh_probs")
run_ineq_qa(filename = "mental_wellbeing")
run_ineq_qa(filename = "physical_activity") 


#END

