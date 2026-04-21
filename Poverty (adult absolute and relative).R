#########################################################
# SG adult poverty data - data import
#########################################################

### Update ScotPHO indicators on adult poverty
### Author: Liz Richardson, 14 Nov 2024

# 2 indicators:
# Definitions:
# 30035 = absolute poverty = Percentage of adults living in households whose income is below 60% of the inflation-adusted UK median income in 2010/11. (doesn't specify whether after housing costs, but I've selected AHC to match relpov definition)
# 30031 = relative poverty = Percentage of adults living in private households with an equivalised income of less than 60% of the UK median income in the same year, after housing costs

### Notes on the data source:
# statistics.gov.scot 

# Source: Scottish Government analysis of the Family Resources Survey
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpoverty-adults

# POVERTY DATA FROM STATISTICS.GOV.SCOT:

# National Statistics of the number and proportions of people living in private households with an equivalised household income below various poverty thresholds. 
# Relative poverty: Individuals living in households whose equivalised income is below 60% of UK median income in the same year. 
#     This is a measure of whether those in the lowest income households are keeping pace with the growth of incomes in the economy as a whole.
# Absolute poverty: Individuals living in households whose equivalised income is below 60% of inflation adjusted UK median income in 2010/11. 
#     This is a measure of whether those in the lowest income households are seeing their incomes rise in real terms.
# Where estimates are suppressed due to small sample size this is marked with an asterisk ("*"). 
# The income measure used is equivalised net disposable income before and after housing costs. 
# The before housing costs measure is income from all sources (including earnings, benefits, tax credits, pensions, and investments) after deductions for 
# income tax, national insurance contributions, council tax, pension contributions and maintenance payments. 
# The after housing costs measure further deducts housing costs such as rent and/or mortgage payments.
# The data source is the Department for Work and Pensions' Family Resources Survey (Households Below Average Income dataset).
# Adults are defined as all working age and pensionable age adults.

# Coverage:
# 1994/95-1996/97 to 2021/22-2023/24 (some splits don't go back this far though).
# N.B. The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the averaged estimates. 
# This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain data from two financial years each. 
# From 2011 the dataset includes both 3-year and 5-year rolling averages. 5-year aggregations used for religion breakdown.


# # statistics.gov.scot data were downloaded using opendatascot:
# # https://scotgovanalysis.github.io/opendatascot/
# # How to install:
# # download the zipped repository from the opendatascot github page to a UNIX space.
# install.packages("devtools")
# library(devtools)
# devtools::install_local(
# #  "<FILEPATH OF ZIPPED FILE>/opendatascot-main.zip",
#   "//PHI_conf/PHSci/Liz/packages/opendatascot-master",
#   upgrade = "never",
#   build_vignettes = TRUE
# )


### functions/packages -----
source("functions/main_analysis.R") #Normal indicator functions

library(opendatascot)

### 1. Read in data ----

# see structure and variables of this dataset
ods_structure("poverty-adults")

# extract data
adultpov_raw <- opendatascot::ods_dataset("poverty-adults",
                                          measureType = c("ratio", "sample-size"),
                                          housingCosts = "after-housing-costs",
                                          indicatorpoverty = c("relative-poverty", "absolute-poverty"),
                                          familyType = "all",
                                          maritalStatus = "all",
                                          sexualOrientation = "all")  %>%
  select(-c(refArea, housingCosts, familyType, maritalStatus, sexualOrientation))

# prepare data
adultpov <- adultpov_raw %>% 
  
  # clean column names
  clean_names() %>%
  
  # reshape
  pivot_wider(names_from = measure_type, values_from = value) %>%
  rename(samplesize="sample-size") %>%
  
  # confidence intervals
  mutate(ci_wald = 100 * (1.96*sqrt(((ratio/100)*(1-(ratio/100)))/samplesize)), # Wald method. 
         lowci = ratio - ci_wald,
         upci = ratio + ci_wald) %>%
  
  # rename columns
  rename(trend_axis = ref_period,
         rate = ratio,
         indicator = indicatorpoverty) %>% 
  
  # create single split name column
  mutate(split_name = case_when(age != "all" ~ "Age",
                                religion != "all" ~ "Religion",
                                gender != "all" ~ "Gender",
                                gender == "all" & religion == "all" & age=="all" ~ "Total"),
         
  # create single split value column
         split_value = case_when(split_name == "Religion" ~ religion,
                                 split_name == "Gender" ~ gender,
                                 split_name == "Age" ~ age,
                                 split_name == "Total" ~ "Total"),
         
  # tidy split values
         split_value = str_to_sentence(split_value), # capitalises first letter
         split_value = str_replace_all(split_value, c("-years" = " years",
                                                      "years-and-over" = "years+",
                                                      "Church-of-scotland" = "Church of Scotland",
                                                      "No-religion" = "No religion",
                                                      "Other-christian" = "Other Christian",
                                                      "Roman-catholic" = "Roman Catholic")),
  # sort split values correctly
  split_value_sort = case_when(split_value == "16-24 years" ~ "a 16-24 years"  ,
                               split_value == "25-34 years" ~  "b 25-34 years" ,
                               split_value == "35-44 years" ~  "c 35-44 years" ,
                               split_value == "45-54 years" ~ "d 45-54 years"  ,
                               split_value == "55-64 years" ~ "e 55-64 years"  ,
                               split_value == "65-74 years" ~ "f 65-74 years"  ,
                               split_value == "75-84 years" ~  "g 75-84 years" ,
                               split_value == "85 years+" ~ "h 85 years+", 
                               split_value == "Church of Scotland" ~ "a Church of Scotland", 
                               split_value == "Roman Catholic" ~ "b Roman Catholic", 
                               split_value == "Other Christian" ~ "c Other Christian",
                               split_value == "Muslim" ~ "d Muslim", 
                               split_value == "No religion" ~ "e No religion", 
                               split_value == "Other" ~ "f Other", 
                               TRUE ~ split_value),

    # Create new columns
         code = "S00000001", #all are Scotland
         numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
         def_period = paste0(trend_axis, " (", 
                             (as.numeric(substr(trend_axis, 9, 12)) - as.numeric(substr(trend_axis, 1, 4)) + 1), 
                             " year aggregate)"),
         year = as.numeric(substr(trend_axis, 1, 4)) + 1,  # 3 year average, so find mid point
         ind_id = case_when(indicator == "relative-poverty" ~ 30031,
                            indicator == "absolute-poverty" ~ 30035)
  ) %>%
  #ensure pop groups sort in desired order
  arrange(code,year,split_name,split_value_sort) %>%
  # Drop vars not needed
  select(-c(age, religion, gender, samplesize, ci_wald, split_value_sort)) 



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland data, total pop
  main_data <- adultpov %>% 
    filter(indicator == ind,
           split_name == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code,year)
  
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))

  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland data by sex (including total)
  pop_grp_data <- adultpov %>% 
    filter(indicator == ind & !(split_name %in% c("Total"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code,year, split_name)
  
  
  # Save
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))

  # Make data created available outside of function so it can be visually inspected if required
  assign(paste0("main_", ind), main_data, envir=.GlobalEnv)
  assign(paste0("pop_grp_", ind), pop_grp_data, envir=.GlobalEnv)
  

}


# Run function to create final files
prepare_final_files(ind = "absolute-poverty")
prepare_final_files(ind = "relative-poverty")

# # Run QA reports 
# # main data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_qa(type = "main", filename = "absolute-poverty", test_file = FALSE)
run_qa(type = "main", filename = "relative-poverty", test_file = FALSE)
 
# Manual plot checks - which include pop groups

# main data
rbind(`main_absolute-poverty`, `main_relative-poverty`) %>%
  ggplot(aes(year, rate, group = as.factor(ind_id), colour = as.factor(ind_id))) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# popgroup data
`pop_grp_absolute-poverty` %>%
  ggplot(aes(year, rate, group = as.factor(split_value), colour = as.factor(split_value))) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) +
  facet_wrap(~split_name)

`pop_grp_relative-poverty` %>%
  ggplot(aes(year, rate, group = as.factor(split_value), colour = as.factor(split_value))) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) +
  facet_wrap(~split_name)

