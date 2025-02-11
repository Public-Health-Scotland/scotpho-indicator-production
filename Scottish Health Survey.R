#########################################################
# Scottish Health Survey data import
#########################################################

# to do - 
# fix ineq_qa to take data without HBs

### Update ScotPHO indicators that can be sourced from published Scottish Health Survey data: 

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
### Alcohol profile indicators:
#   4170: "Drinking over (6/8) units in a day (includes non-drinkers): Over 8 units for men, over 6 units for women" (previous indicator definition excluded non-drinkers from denom)
#   4171: "Alcohol consumption: Hazardous/Harmful drinker" (derived from AUDIT, score 8+) (NB. original ScotPHO indicator excluded non-drinkers from denominator... it's not clear whether they are included here) 
#   4172: "Alcohol consumption (mean weekly units)" (not an indicator previously, but % exceeding guidelines not available from this extract)
### Physical activity profile:
#   88888:  "Whether meets MVPA & muscle strengthening recommendations: Meets MVPA & muscle strengthening recommendations"

# The data are downloaded from statistics.gov.scot:
# https://statistics.gov.scot/data/search?search=scottish+health+survey
# 6 separate csv files are downloaded (for each split type, + Scotland and local areas)

# Availability: 
# all indicators available as male/female/all splits (Scotland, HB, CA)
# all indicators also available for age, SIMD, income and long-term conditions splits (Scotland only).
# This script runs the deprivation analysis on the SIMD-level data.


### functions/packages -----
source("1.indicator_analysis.R") # for packages and QA
source("2.deprivation_analysis.R") # for packages and QA
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

# Run this lines below to install the opendata scotland package
# devtools::install_github("ScotGovAnalysis/opendatascot",upgrade = "never",build_vignettes = TRUE)
library(arrow) # for parquet files
library(opendatascot) # for getting data from stats.gov.scot

### 0. Get new data ---- 
# # Download each of the datasets 
# # (N.B. only do this if reading in new data. Latest downloaded = 2023, published in Nov 2024)
# # (persist if gives HTTP errors such as 302...)
# SHeS_SCOTLAND <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data")
# SHeS_LA <- opendatascot::ods_get_csv("scottish-health-survey-local-area-level-data")
# SHeS_SIMD <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-simd")
# SHeS_AGE <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-age")
# SHeS_INCOME <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-equivalised-income")
# SHeS_CONDITIONS <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-long-term-conditions")
# 
# 
# #Write the datasets to the Received Data folder in .parquet format
# write_parquet(SHeS_SCOTLAND, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_SCOTLAND.parquet"))
# write_parquet(SHeS_LA, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_LA.parquet"))
# write_parquet(SHeS_SIMD, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_SIMD.parquet"))
# write_parquet(SHeS_AGE, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_AGE.parquet"))
# write_parquet(SHeS_INCOME, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_INCOME.parquet"))
# write_parquet(SHeS_CONDITIONS, paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_LONGTERM_CONDITIONS.parquet"))

### 1. Read in the downloaded and saved data ----

SHeS_SCOTLAND <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_SCOTLAND.parquet")) %>% mutate(split_name = "Sex")
SHeS_LA <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_LA.parquet")) %>% mutate(split_name = "Sex")
SHeS_SIMD <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_SIMD.parquet")) %>% mutate(split_name = "Deprivation (SIMD)")
SHeS_AGE <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_AGE.parquet")) %>% mutate(split_name = "Age")
SHeS_INCOME <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_INCOME.parquet")) %>% mutate(split_name = "Income (equivalised)")
SHeS_CONDITIONS <- read_parquet(paste0(data_folder, "Received Data/Scottish Health Survey/SHeS_LONGTERM_CONDITIONS.parquet")) %>% mutate(split_name = "Long-term illness")


### 2. Combine data and get column data and formats right----
shes_df <- mget(ls(pattern="^SHeS_")) %>% # get all the dataframes in the environment starting with "SHeS_"
  bind_rows() %>% # append them together
  mutate(code = ifelse(FeatureCode=="S92000003", "S00000001", FeatureCode)) %>% # recode Scotland
  mutate(stat = case_when(Measurement=="95% Lower Confidence Limit" ~ "lowci", # recode the measures
                          Measurement=="95% Upper Confidence Limit" ~ "upci",
                          Measurement %in% c("Mean", "Percent") ~ "rate")) %>%
  rename(trend_axis = DateCode) %>%
  # create a new split_value column: coalesce combines non-NA values into a single column
  mutate(split_value = coalesce(Age, Sex, `Long-term illness`, `Equivalised Income`, `SIMD quintiles`)) %>% # this works because there is only ever one non-NA cell in these 5 columns
  mutate(split_value = if_else(split_value == "All", "Total", split_value)) %>% # recode All -> Total
  mutate(split_value = case_when(split_value=="1 - most deprived" ~ "1", # format needed for the inequalities analysis
                                 split_value=="5 - least deprived" ~ "5",
                                 split_value=="1st-Top quintile" ~ "1 - highest income", # match ScotPHO format
                                 split_value=="2nd quintile" ~ "2",
                                 split_value=="3rd quintile" ~ "3",
                                 split_value=="4th quintile" ~ "4",
                                 split_value=="5th-Bottom quintile" ~ "5 - lowest income",
                                 TRUE ~ split_value)) %>%
  mutate(split_value = ifelse(split_name=="Age" & split_value!="Total",
                              paste0(split_value, " y"), # add y for years to age groups
                              split_value)) %>%
  
  # keep the required columns
  select(code, ind = `Scottish Health Survey Indicator`, trend_axis, split_name, split_value, stat, value = Value, Units) %>%
  
  # reshape to wide
  pivot_wider(names_from=stat, values_from = value) %>%
  
  # add def_period column
  # first need to calculate the difference (year_diff) between the first and last year in the trend_axis to work out what def_period to add
  # this calc copes with single year (e.g. year_diff for 2008 = 0), two years (year_diff for 2010-2011 = 1), 
  # and the aggregates used for lower geogs (year_diff = 3 if not including 2020, or 4 if 2020 is in the range, because 2020 data are excluded)
  mutate(year_diff = 
           as.numeric(substr(trend_axis, nchar(trend_axis) - 3, nchar(trend_axis))) # the last year in trend_axis
         - as.numeric(substr(trend_axis, 1, 4))) %>% # minus the first year in trend_axis
  mutate(year = case_when(year_diff <= 1 ~ as.numeric(substr(trend_axis, 1, 4)), # year = first/only year in the label
                          year_diff>1 ~ as.numeric(substr(trend_axis, 1, 4))+2)) %>% # year = first year in the label + 2 (=mid point or midpoint rounded up to nearest whole year)
  mutate(def_period = ifelse(year_diff <= 1, 
                             paste0("Survey year (", trend_axis, ")"),
                             paste0("Aggregated survey years (", trend_axis, ")"))) 
  

### 3. Which indicators should be kept? ----

# print out list of all available indicators in the data:
unique(shes_df$ind)
# look through to check which ones we need to keep

# check LLTI data: 
# there are 2 similarly-named indicators for LLTI. 
# they have overlapping temporal coverage, so need to look at the splits, geographies, and year-ranges involved
llti <- shes_df %>% 
  filter(ind %in% c("Long-term conditions: Limiting long-term conditions", 
                    "Long-term illness: Limiting long-term illness")) %>%
  mutate(geog = substr(code, 1, 3)) %>%
  select(ind, split_name, geog, year_diff) %>%
  unique() %>% 
  pivot_wider(names_from = split_name, values_from = year_diff) 
# Conclusion: "Long-term conditions" is the one to keep, "Long-term illness" just available for annual Scotland data, for fewer years than the LT conditions data.

# List all the indicators we want to keep:
keep <- c("Drinking over (6/8) units in a day (includes non-drinkers): Over 8 units for men, over 6 units for women",  # binge drinking: M/F/Total (ind_id 4166, 4167, 4168) (NB. original ScotPHO indicator excluded non-drinkers from denominator)                   
          "Alcohol consumption (mean weekly units)", # units: can't use to derive % exceed weekly guidelines: M/F/Total (ind_id 4163-5)                                                                                     
          "Alcohol consumption: Hazardous/Harmful drinker", # Problem drinker: M/F/Total (ind_id 4169, 12554, 12555) (NB. original ScotPHO indicator excluded non-drinkers from denominator... it's not clear whether they are included here, as for binge drinkers)                                                                              
          "Food insecurity (worried would run out of food): Yes",  # 99105                                                                        
          "Healthy weight: Healthy weight", #99106                                                                                              
          "Summary activity levels: Meets recommendations",  #99107 
          "Whether meets MVPA & muscle strengthening recommendations: Meets MVPA & muscle strengthening recommendations", #88888
          "Self-assessed general health: Very good/Good",    # 99108                                                                             
          "Fruit & vegetable consumption: 5 portions or more",  #30013                                                                          
          "Mental wellbeing", # 30001 (mean score, as in the indicator definition)                                                                                                           
          "General health questionnaire (GHQ-12): Score 4+", # 30003                                                                             
          "Long-term conditions: Limiting long-term conditions" # 99109 
       #   "Long-term illness: Limiting long-term illness",  # 99109                                                                           
       #   "Involved in the local community: A fair amount", # 30021 (check: probably not possible to combine into 'a fair amount or a great deal')
       #   "Involved in the local community: A great deal",  # 30021 (check: probably not possible to combine into 'a fair amount or a great deal')                                                                                              
       #   "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)", # 30002 (better definition than existing: mean score?)                                                             
       #   "Symptoms of anxiety: No anxiety symptoms", # 30005: would need the inverse... would this be valid?                                                                                    
       #   "Symptoms of depression: No depression symptoms", # 30004: would need the inverse... would this be valid?                                                                               
       #   "How stressful you find your job: Extremely stressful", # 30051 (check: probably not possible to combine into 'very/extremely stressful')
       #   "How stressful you find your job: Very stressful" # 30051 (check: probably not possible to combine into 'very/extremely stressful')         
          )                                                                    

# keep the required indicators
shes_df <- shes_df %>%
  filter(ind %in% keep)

# # check their units
# units <- shes_df %>%
#   select(ind, Units) %>%
#   unique()
# # Confirms WEMWBS is mean score (as we want), and alc consumption is units.

### 4. Further processing:  ----

shes_df <- shes_df %>% 
  
  # Add shorter indicator name column (used as filename)
  mutate(indicator = case_when(ind == "Self-assessed general health: Very good/Good" ~ "self_assessed_health",
                               ind == "Long-term conditions: Limiting long-term conditions" ~ "limiting_long_term_condition",
                               ind == "Healthy weight: Healthy weight" ~ "healthy_weight",
                               ind == "Food insecurity (worried would run out of food): Yes" ~ "food_insecurity",
                               ind == "Fruit & vegetable consumption: 5 portions or more" ~ "fruit_veg_consumption", 
                               ind == "General health questionnaire (GHQ-12): Score 4+" ~ "common_mh_probs", 
                               ind == "Mental wellbeing" ~ "mental_wellbeing", 
                               ind == "Summary activity levels: Meets recommendations" ~ "physical_activity",
                               ind == "Whether meets MVPA & muscle strengthening recommendations: Meets MVPA & muscle strengthening recommendations" ~ "meets_mvpa_and_strength_recs",
                               ind == "Drinking over (6/8) units in a day (includes non-drinkers): Over 8 units for men, over 6 units for women" ~ "binge_drinking",
                               ind == "Alcohol consumption: Hazardous/Harmful drinker" ~ "problem_drinker",
                               ind == "Alcohol consumption (mean weekly units)" ~ "weekly_alc_units"),

         # Create new ind_id column
         ind_id = case_when(indicator == "self_assessed_health" ~ 99108,
                            indicator == "limiting_long_term_condition" ~ 99109,
                            indicator == "healthy_weight" ~ 99106,
                            indicator == "food_insecurity" ~ 99105,
                            indicator == "fruit_veg_consumption" ~ 30013, 
                            indicator == "common_mh_probs" ~ 30003, 
                            indicator == "mental_wellbeing" ~ 30001, 
                            indicator == "physical_activity" ~ 99107,
                            indicator == "meets_mvpa_and_strength_recs" ~ 88888,
                            indicator == "binge_drinking" ~ 4170,
                            indicator == "problem_drinker" ~ 4171,
                            indicator == "weekly_alc_units" ~ 4172),
         numerator = NA) %>% 
  
  # Select relevant columns
  select(ind_id, indicator, code, year, trend_axis, def_period, split_name, split_value, rate, lowci, upci, numerator)

### 5. Add totals for the popgroup and SIMD splits ----

# Get split_value = "Total" for the splits without totals.
# This is needed for subsequent calculation of the inequalities metrics, and is nice-to-have for the popgroup data:
splits_w_no_total <- shes_df %>%
  group_by(indicator, split_name) %>%
  mutate(has_total = "Total" %in% split_value) %>%
  ungroup() %>%
  filter(has_total==FALSE) %>%
  select(code, indicator, ind_id, trend_axis, year, def_period, split_name) %>%
  unique()
  
totals_to_add <- shes_df %>%
  filter(split_value=="Total") %>%
  select(code, indicator, trend_axis, split_value, rate, lowci, upci, numerator) %>%
  unique() %>%
  merge(y = splits_w_no_total, by=c("code", "indicator", "trend_axis"), all.y=TRUE)

shes_df <- shes_df %>%
  rbind(totals_to_add)


### 6. Check geographical availability: ----

# which Scotland-wide data to keep to match any lower geographies in the main_data (for trend chart, summary and rank comparisons)?
availability <- shes_df %>%
  mutate(geog = substr(code, 1, 3)) %>%
  filter(split_value=="Total") %>%
  select(ind_id, indicator, geog, trend_axis, split_name) %>%
  unique()
ftable(availability$indicator, availability$geog, availability$split_name, availability$trend_axis)
# shows that there are HB/CA data for all indicators, but these are only for split_name==sex, and always aggregated (3 or 4 years)
# so the main data file needs to keep only Scotland data that are aggregated in the same way.


### 7. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data (4-year aggregate)
  main_data_final <- shes_df %>% 
    filter(indicator == ind,
           split_value == "Total",
           str_detect(def_period, "Aggregated")) %>% 
    select(!c(split_name, split_value, indicator)) %>% 
    unique() %>%
    arrange(code, year)
  
  write.csv(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))

  # 2 - population groups data (ie data behind population groups tab)
  # Contains LA/HB data by sex (3 or 4-year aggregate) and Scotland data by sex/age/condition/income/simd (single year)
  
  # All data can be used in the popgrp tab (as will only be comparing across data with the same time period: single year (all splits x Scotland) or aggregated (sex x lower geogs))
  pop_grp_data <- shes_df %>% 
    filter(indicator == ind) %>% 
    select(!indicator) %>%
    arrange(code, year, split_name, split_value)
  
  # remove SIMD data for further analysis
  pop_grp_data_final <- pop_grp_data %>%
    filter(split_name!="Deprivation (SIMD")

  # Save
  write.csv(pop_grp_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))

  # Process SIMD data
  simd_data <- pop_grp_data %>%
    filter(split_name=="Deprivation (SIMD)") %>%
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin") %>%
    select(-split_name) %>%
    arrange(code, year, quintile)

  
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
prepare_final_files(ind = "self_assessed_health")
prepare_final_files(ind = "limiting_long_term_condition")
prepare_final_files(ind = "healthy_weight")
prepare_final_files(ind = "food_insecurity")
prepare_final_files(ind = "fruit_veg_consumption")
prepare_final_files(ind = "common_mh_probs")
prepare_final_files(ind = "mental_wellbeing") 
prepare_final_files(ind = "physical_activity")
prepare_final_files(ind = "meets_mvpa_and_strength_recs")
prepare_final_files(ind = "binge_drinking")
prepare_final_files(ind = "problem_drinker")
prepare_final_files(ind = "weekly_alc_units")


# Run QA reports 
# main data
run_qa(type = "main", filename = "self_assessed_health", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "limiting_long_term_condition", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "food_insecurity", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "common_mh_probs", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "mental_wellbeing", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "physical_activity", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "fruit_veg_consumption", test_file = FALSE) # no diffs flagged
run_qa(type = "main", filename = "healthy_weight", test_file = FALSE) # no diffs flagged 
run_qa(type = "main", filename = "meets_mvpa_and_strength_recs", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "binge_drinking", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "problem_drinker", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "weekly_alc_units", test_file = FALSE) # no historic file
# Differences for 2018-2022 data: have compared the raw data read in (previous data = SPSS sav files that Calli got from SHeS team) and the differences exist there. 
# Opt to trust the later data? I'm checking this with SHeS team.

# ineq data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_qa(type = "deprivation", filename = "self_assessed_health", test_file=FALSE)
run_qa(type = "deprivation", filename = "limiting_long_term_condition", test_file=FALSE)
run_qa(type = "deprivation", filename = "healthy_weight", test_file=FALSE)
run_qa(type = "deprivation", filename = "food_insecurity", test_file=FALSE)
run_qa(type = "deprivation", filename = "fruit_veg_consumption", test_file=FALSE)
run_qa(type = "deprivation", filename = "common_mh_probs", test_file=FALSE)
run_qa(type = "deprivation", filename = "mental_wellbeing", test_file=FALSE) 
run_qa(type = "deprivation", filename = "physical_activity", test_file=FALSE)
run_qa(type = "deprivation", filename = "meets_mvpa_and_strength_recs", test_file=FALSE)
run_qa(type = "deprivation", filename = "binge_drinking", test_file=FALSE)
run_qa(type = "deprivation", filename = "problem_drinker", test_file=FALSE)
run_qa(type = "deprivation", filename = "weekly_alc_units", test_file=FALSE)

#END
