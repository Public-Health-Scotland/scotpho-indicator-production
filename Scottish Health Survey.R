#########################################################
# Scottish Health Survey data import
#########################################################


### Update ScotPHO indicators that can be sourced from published Scottish Health Survey data: 

### Care and Wellbeing indicators: 
#   99105: Food insecurity
#   99106: Adult Healthy Weight 
#   99107: Summary activity levels (also MEN)*
#   99108: Self-assessed health of adults (age 16+) (also MEN)*
#   99109: Limiting long-term conditions (age 16+) (also MEN)*
### Adult mental health indicators:
#   30013: Fruit & vegetable consumption (guidelines) *
#   30003: General health questionnaire (GHQ-12) *
#   30001: Mental wellbeing (WEMWBS)*
### Alcohol profile indicators:
#   4170: Alcohol consumption: Binge drinking (drinking over (6/8) units in a day (includes non-drinkers): Over 8 units for men, over 6 units for women" (previous indicator definition excluded non-drinkers from denom)
#   4171: Alcohol consumption: Hazardous/Harmful drinker" (derived from AUDIT, score 8+) (NB. original ScotPHO indicator excluded non-drinkers from denominator... it's not clear whether they are included here) 
#   4172: Alcohol consumption (mean weekly units)" (not an indicator previously, but % exceeding guidelines not available from this extract)
### Physical activity profile:
#   88888:  "Whether meets MVPA & muscle strengthening recommendations: Meets MVPA & muscle strengthening recommendations"

### And adding data for a further 14 indicators (12 adult mental health, 2 CYP mental health) that have been processed in the ScotPHO_survey_data repo 
### (raw data processing elsewhere because they use UK Data Service data)
### (data for SIMD x sex are also added for the 6 published variables above that are in the adult MH profile (see *), as these can be derived from the UKDS data.)

# 30002 = life_sat	Mean score on the question "All things considered, how satisfied are you with your life as a whole nowadays?" (variable LifeSat).  N.B. This indicator is also available from the ScotPHO Online Profiles (national and council area level, but not by SIMD). Life satisfaction is measured by asking participants to rate, on a scale of 0 to 10, how satisfied they are with their life in general. On the scale, 0 represented 'extremely dissatisfied' and 10 'extremely satisfied' (the intervening scale points were numbered but not labelled). 
# 30052 = work_bal	Mean score for how satisfied adults are with their work-life balance (paid work). Respondents were asked "How satisfied are you with the balance between the time you spend on your paid work and the time you spend on other aspects of your life?" on a scale between 0 (extremely dissatisfied) and 10 (extremely satisfied). The intervening scale points were numbered but not labelled. The variable was WorkBal. 
# 30004 = depsymp	Percentage of adults who had a symptom score of two or more on the depression section of the Revised Clinical Interview Schedule (CIS-R). A score of two or more indicates symptoms of moderate to high severity experienced in the previous week. The variable used was depsymp (or dvg11 in 2008). 
# 30005 = anxsymp	Percentage of adults who had a symptom score of two or more on the anxiety section of the Revised Clinical Interview Schedule (CIS-R). A score of two or more indicates symptoms of moderate to high severity experienced in the previous week. The variable used was anxsymp (or dvj12 in 2008). 
# 30009 = suicide2	Percentage of adults who made an attempt to take their own life, by taking an overdose of tablets or in some other way, in the past year. The variable used was suicide2. 
# 30010 = dsh5sc	Percentage of adults who deliberately harmed themselves but not with the intention of killing themselves in the past year. The variable used was DSH5 from 2008 to 2011, or DSH5SC from 2013 onwards. 
# 30021 = involve	Percentage of adults who, when asked "How involved do you feel in the local community?", responded "a great deal" or "a fair amount". The four possible options ranged from "a great deal" to "not at all". The variables used were Involve and INVOLV19. 
# 30023 = p_crisis	Percentage of adults with a primary support group of three or more to rely on for comfort and support in a personal crisis. Respondents were asked "If you had a serious personal crisis, how many people, if any, do you feel you could turn to for comfort and support?", and the variables were PCrisis or PCRIS19. 
# 30051 = str_work2	Percentage of adults who find their job "very stressful" or "extremely stressful". Respondents were asked "In general, how do you find your job?" and were given options from "not at all stressful" to "extremely stressful". The variable was StrWork2. 
# 30053 = contrl	Percentage of adults who often or always have a choice in deciding how they do their work, in their current main job. The five possible responses ranged from "always" to "never". The variable was Contrl. 
# 30054 = support1	Percentage of adults who "strongly agree" or "tend to agree" that their line manager encourages them at work. The five options ranged from "strongly agree" to "strongly disagree". The variables used were Support1 and Support1_19. 
# 30026 = rg17a_new	Percentage of adults who provide 20 or more hours of care per week to a member of their household or to someone not living with them, excluding help provided in the course of employment. Participants were asked whether they look after, or give any regular help or support to, family members, friends, neighbours or others because of a long-term physical condition, mental ill-health or disability; or problems related to old age. Caring which is done as part of any paid employment is not asked about. From 2014 onwards, this question explicitly instructed respondents to exclude caring as part of paid employment. The variables used to construc this indicator were RG15aNew (Do you provide any regular help or care for any sick, disabled, or frail people?) and RG17aNew (How many hours do you spend each week providing help or unpaid care for him/her/them?). 
# 30130 = ch_ghq  Percentage of children aged 15 years or under who have a parent/carer who scores 4 or more on the General Health Questionnaire-12 (GHQ-12)
# 30129 = ch_audit  Percentage of children aged 15 years or under with a parent/carer who reports consuming alcohol at hazardous or harmful levels (AUDIT questionnaire score 8+)



# The published data are downloaded from statistics.gov.scot:
# https://statistics.gov.scot/data/search?search=scottish+health+survey
# 6 separate csv files are downloaded (for each split type, + Scotland and local areas)

# Availability: 
# all indicators available as male/female/all splits (Scotland, HB, CA)
# all indicators also available for age, SIMD, income and long-term conditions splits (Scotland only).
# This script runs the deprivation analysis on the SIMD-level data.


### functions/packages -----
#source("1.indicator_analysis.R") # for packages and QA
#source("2.deprivation_analysis.R") # for packages and QA
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

# Published data from statistics.gov.scot:
SHeS_SCOTLAND <- read_parquet(paste0(profiles_data_folder,"/Received Data/Scottish Health Survey/SHeS_SCOTLAND.parquet")) %>% mutate(split_name = "Sex")
SHeS_LA <- read_parquet(paste0(profiles_data_folder, "/Received Data/Scottish Health Survey/SHeS_LA.parquet")) %>% mutate(split_name = "Sex")
SHeS_SIMD <- read_parquet(paste0(profiles_data_folder, "/Received Data/Scottish Health Survey/SHeS_SIMD.parquet")) %>% mutate(split_name = "Deprivation (SIMD)")
SHeS_AGE <- read_parquet(paste0(profiles_data_folder, "/Received Data/Scottish Health Survey/SHeS_AGE.parquet")) %>% mutate(split_name = "Age")
SHeS_INCOME <- read_parquet(paste0(profiles_data_folder, "/Received Data/Scottish Health Survey/SHeS_INCOME.parquet")) %>% mutate(split_name = "Income (equivalised)")
SHeS_CONDITIONS <- read_parquet(paste0(profiles_data_folder, "/Received Data/Scottish Health Survey/SHeS_LONGTERM_CONDITIONS.parquet")) %>% mutate(split_name = "Long-term illness")

# Pre-processed UKDS data
shes_from_ukds <- readRDS(paste0(profiles_data_folder, "/Prepared Data/shes_raw.rds")) %>%
  mutate(code = as.character(code))

###------------------------------------------------------------------------------------------------
### PUBLISHED DATA PROCESSING:
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
  select(code, ind = `Scottish Health Survey Indicator`, trend_axis, split_name, split_value, stat, value = Value) %>%
  
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
                             paste0("Aggregated survey years (", trend_axis, ")"))) %>%
  mutate(numerator = NA, # columns needed to get into same format as the imported UKDS data
         sex = "Total")


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
                            indicator == "weekly_alc_units" ~ 4172)) %>% 
  
  # Select relevant columns
  select(ind_id, indicator, code, year, trend_axis, def_period, sex, split_name, split_value, rate, lowci, upci, numerator)

### 5. Add totals for the popgroup and SIMD splits ----
# (some have totals but some don't...)

# Get split_value = "Total" for the splits without totals.
# This is needed for subsequent calculation of the inequalities metrics, and is nice-to-have for the popgroup data:
splits_w_no_total <- shes_df %>%
  group_by(indicator, split_name) %>%
  mutate(has_total = "Total" %in% split_value) %>%
  ungroup() %>%
  filter(has_total==FALSE) %>%
  select(code, indicator, ind_id, trend_axis, year, def_period, sex, split_name) %>%
  unique()

totals_to_add <- shes_df %>%
  filter(split_value=="Total") %>%
  select(code, indicator, trend_axis, split_value, rate, lowci, upci, numerator) %>%
  unique() %>%
  merge(y = splits_w_no_total, by=c("code", "indicator", "trend_axis"), all.y=TRUE)

shes_df <- shes_df %>%
  rbind(totals_to_add)


###------------------------------------------------------------------------------------------------
### Now add the UKDS data:
shes_df <- shes_from_ukds %>%
  select(-denominator) %>%
  rbind(shes_df)

table(shes_df$ind_id, useNA="always") # 26 in total, no NA
table(shes_df$indicator, useNA="always") # 26 in total, no NA
table(shes_df$code, useNA="always") # Scot, HB, LA, no NA
table(shes_df$trend_axis, useNA="always") # 2008 to 2023, single year and 4-y aggregates, no NA
table(shes_df$def_period, useNA="always") # 2008 to 2023, single year and 4-y aggregates, no NA
table(shes_df$sex, useNA="always") # M/F/total, no NA
table(shes_df$split_name, useNA="always") # 5 different splits, no NA
table(shes_df$split_value, useNA="always") # correct, no NA






### 6. Check geographical availability: ----

# which Scotland-wide data to keep to match any lower geographies in the main_data (for trend chart and rank comparisons)?
availability <- shes_df %>%
  mutate(geog = substr(code, 1, 3),
         years = case_when(nchar(trend_axis)==4 ~ "single",
                           nchar(trend_axis)>4 ~ "aggregated",
                           TRUE ~ as.character(NA))) %>%
  filter(split_value=="Total") %>%
  select(ind_id, indicator, geog, years, split_name) %>%
  unique() %>%
  group_by(ind_id, indicator, geog, split_name) %>%
  summarise(count = n()) %>%
  ungroup()
ftable(availability$indicator, availability$geog, availability$split_name, availability$count)
# main_data needs to select data at the level of aggregation of any lower geographies, if present.

# make a list of the indicators this affects:
indicators_w_lower_geogs <- availability %>%
  filter(!geog=="S00") %>%
  select(indicator) %>%
  unique()
indicators_w_lower_geogs <- as.vector(indicators_w_lower_geogs$indicator)

# which splits are available for single and aggregated years? Want to select single years for these...
splits_w_single_and_aggd_years <- availability %>%
  filter(count==2) %>%
  select(indicator, geog, split_name) %>%
  unique()


### 7. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data (4-year aggregate)
  main_data_final <- shes_df %>% 
    filter(indicator == ind,
           split_name == "Sex",
           split_value == "Total") %>%
    {if (ind %in% indicators_w_lower_geogs) filter(., str_detect(def_period, "Aggregated")) #select the aggregated data
      else filter(., str_detect(def_period, "Survey year "))} %>% # select the un-aggregated data
    select(!c(split_name, split_value, indicator, sex)) %>% 
    unique() %>%
    arrange(code, year)
  
  write.csv(main_data_final, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (i.e. data behind population groups tab)
  # Contains LA/HB data by sex (3 or 4-year aggregate) and Scotland data by sex/age/condition/income/simd (single year)
  # Can use single year and aggregated data in the popgrp tab (as will only be comparing across data with the same time period: single year (all splits x Scotland) or aggregated (sex x lower geogs))
  pop_grp_data <- shes_df %>% 
    filter(indicator == ind) %>% 
    filter(split_name!="Deprivation (SIMD)") %>%
    mutate(geog = substr(code, 1, 3)) %>%
    filter((geog=="S00" & str_detect(def_period, "Survey year ")) |  #select the un-aggregated data for Scotland
             (geog!="S00" & str_detect(def_period, "Aggregated"))) %>%   # select the aggregated data for lower geogs
    select(-indicator, -sex, -geog) %>%
    arrange(code, year, split_name, split_value)
  
  # Save
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  # Process SIMD data
  simd_data <- shes_df %>% 
    filter(indicator == ind) %>% 
    filter(split_name=="Deprivation (SIMD)" & str_detect(def_period, "Survey year ")) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin") %>%
    select(-split_name) %>%
    arrange(code, year, quintile)
  
  # get arguments for the add_population_to_quintile_level_data() function: (done because the ind argument to the current function is not the same as the ind argument required by the next function)
  ind_name <- ind # dataset will already be filtered to a single indicator based on the parameter supplied to 'prepare final files' function
  ind_id <- unique(simd_data$ind_id) # identify the indicator number 

  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop="depr_pop_16+",ind = ind_id,ind_name = ind_name) |>
    filter(!is.na(rate)) # some data biennial so not all years have data
    
  # simd_data$numerator[is.na(simd_data$numerator)] <- 0 # Converting any NAs to 0s # Not necessary for the analysis, and important that NA remain as NA, not 0. 0s get flagged in the QA.
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(indicator,overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
    
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data_final
  pop_grp_data_result <<- pop_grp_data
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
prepare_final_files(ind = "unpaid_caring") 
prepare_final_files(ind = "life_satisfaction")  
prepare_final_files(ind = "cyp_parent_w_ghq4")    
prepare_final_files(ind = "cyp_parent_w_harmful_alc")
prepare_final_files(ind = "involved_locally")  
prepare_final_files(ind = "support_network") 
prepare_final_files(ind = "stress_at_work")
prepare_final_files(ind = "choice_at_work")   
prepare_final_files(ind = "line_manager") 
prepare_final_files(ind = "depression_symptoms")  
prepare_final_files(ind = "anxiety_symptoms")  
prepare_final_files(ind = "deliberate_selfharm")   
prepare_final_files(ind = "attempted_suicide")
prepare_final_files(ind = "work-life_balance")


# Run QA reports 
# main data
run_qa(type = "main", filename = "self_assessed_health", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "limiting_long_term_condition", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "food_insecurity", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "common_mh_probs", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "mental_wellbeing", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
run_qa(type = "main", filename = "physical_activity", test_file = FALSE) # diffs flagged for year==2020 (trend_axis == 2018-2022)
# Differences for 2018-2022 data: have compared the raw data read in (previous data = SPSS sav files that Calli got from SHeS team) and the differences exist there. 
# Opt to trust the later data? I'm checking this with SHeS team.
# SHeS team confirmed the error in the data they sent. Replace with these as soon as poss.
run_qa(type = "main", filename = "fruit_veg_consumption", test_file = FALSE) # no diffs flagged
run_qa(type = "main", filename = "healthy_weight", test_file = FALSE) # no diffs flagged 
run_qa(type = "main", filename = "meets_mvpa_and_strength_recs", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "binge_drinking", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "problem_drinker", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "weekly_alc_units", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "unpaid_caring", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "life_satisfaction", test_file = FALSE)  # no historic file
run_qa(type = "main", filename = "cyp_parent_w_ghq4", test_file = FALSE)    # no historic file
run_qa(type = "main", filename = "cyp_parent_w_harmful_alc", test_file = FALSE)# no historic file
run_qa(type = "main", filename = "involved_locally", test_file = FALSE)  # no historic file
run_qa(type = "main", filename = "support_network", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "stress_at_work", test_file = FALSE)# no historic file
run_qa(type = "main", filename = "choice_at_work", test_file = FALSE)   # no historic file
run_qa(type = "main", filename = "line_manager", test_file = FALSE) # no historic file
run_qa(type = "main", filename = "depression_symptoms", test_file = FALSE)  # no historic file
run_qa(type = "main", filename = "anxiety_symptoms", test_file = FALSE)  # no historic file
run_qa(type = "main", filename = "deliberate_selfharm", test_file = FALSE)   # no historic file
run_qa(type = "main", filename = "attempted_suicide", test_file = FALSE)# no historic file
run_qa(type = "main", filename = "work-life_balance", test_file = FALSE)# no historic file

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
run_qa(type = "deprivation", filename = "unpaid_caring", test_file = FALSE) 
run_qa(type = "deprivation", filename = "life_satisfaction", test_file = FALSE)  
run_qa(type = "deprivation", filename = "cyp_parent_w_ghq4", test_file = FALSE)    
run_qa(type = "deprivation", filename = "cyp_parent_w_harmful_alc", test_file = FALSE)
run_qa(type = "deprivation", filename = "involved_locally", test_file = FALSE)  
run_qa(type = "deprivation", filename = "support_network", test_file = FALSE) 
run_qa(type = "deprivation", filename = "stress_at_work", test_file = FALSE)
run_qa(type = "deprivation", filename = "choice_at_work", test_file = FALSE)   
run_qa(type = "deprivation", filename = "line_manager", test_file = FALSE) 
run_qa(type = "deprivation", filename = "depression_symptoms", test_file = FALSE)  
run_qa(type = "deprivation", filename = "anxiety_symptoms", test_file = FALSE)  
run_qa(type = "deprivation", filename = "deliberate_selfharm", test_file = FALSE)   
run_qa(type = "deprivation", filename = "attempted_suicide", test_file = FALSE)
run_qa(type = "deprivation", filename = "work-life_balance", test_file = FALSE)

#END




