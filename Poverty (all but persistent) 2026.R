#########################################################
# SG poverty data - data import
#########################################################

### Update ScotPHO poverty indicators 
### Author: Liz Richardson, August 2026

# Indicators:

# Previously prepared in script "Poverty (child absolute relative and persistent).R" (NB persistent poverty wasn't in that script despite being in title: this is still prepared in the script "Persistent poverty.R")
# 30152 = Percentage of 'dependent children' living in relative poverty (after housing costs). Relative poverty is defined as living in households whose equivalised income is below 60% of UK median income in the same year.
# 30153 = Percentage of 'dependent children' living in absolute poverty (after housing costs). Absolute poverty is defined as living in households whose equivalised income is below 60% of the (inflation adjusted) Great Britain median income in 2010/11. 
# 30154 = Percentage of 'dependent children' in combined material deprivation and low income after housing costs (below 70% of UK median income).

# Previously prepared in "Poverty (in work).R"
# 99147 = In-work poverty: % of working age adults (16-64 years) living in households in relative poverty AHC where someone in the household is in paid work

# Previously prepared in script "Poverty (absolute and relative).R", and data not updated since 2025
# 30035 = LAST PUBLISHED 2025 - absolute poverty = Percentage of adults living in households whose income is below 60% of the inflation-adusted UK median income in 2010/11, AHC. 
# 30031 = LAST PUBLISHED 2025 - relative poverty = Percentage of adults living in private households with an equivalised income of less than 60% of the UK median income in the same year, after housing costs

# New indicators proposed here (/thinking about): (given that adult poverty 30035 and 30031 aren't presented anymore)
# 99990 Relative poverty AHC, overall and by age groups
# 99991 Absolute poverty AHC, overall and by age groups


############################################
# ADULT POVERTY RATES NOT PRODUCED NOW.
# LIZ EMAILED SG ON 25 AUG 2026 TO ASK IF THESE COULD BE MADE AVAILABLE
# ALTERNATIVE: USE WORKING AGE OR ALL PEOPLE AS SPLITS ARE AVAILABLE FOR THESE? 
# SG DATA REFERS TO WORKING AGE AS 'ADULTS' FOR SHORTHAND
# ADULTS ARE 78% OF TOTAL POP IN 2022-25, AND WORKING AGE IS 82% OF ADULT POP
# SO EITHER OVERALL OR WORKING AGE POVERTY ARE PRETTY REASONABLE PROXIES FOR ADULT POVERTY...
# PLOT ADULT POVERTY (TO 2021-24) AGAINST OVERALL AND WORKING AGE TO ASSESS
############################################

### Notes on the data source:
# NEW SOURCE FROM 2026:
# SOURCE: https://www.gov.scot/publications/poverty-and-income-inequality-in-scotland-2022-25/documents/
# National Statistics of the number and proportions of people living in private households with an equivalised household income below various poverty thresholds. 

### Definitions:
# Relative poverty: Individuals living in households whose equivalised income is below 60% of UK median income in the same year. 
#     This is a measure of whether those in the lowest income households are keeping pace with the growth of incomes in the economy as a whole.
# Absolute poverty: Individuals living in households whose equivalised income is below 60% of inflation adjusted UK median income in 2010/11. 
#     This is a measure of whether those in the lowest income households are seeing their incomes rise in real terms.
# Where estimates are suppressed due to small sample size this is marked with an asterisk ("*"). 
# The income measure used is equivalised net disposable income after housing costs. 
# After housing costs = income from all sources (including earnings, benefits, tax credits, pensions, and investments) after deductions for 
# income tax, national insurance contributions, council tax, pension contributions, maintenance payments, and housing costs such as rent and/or mortgage payments.
# The data source is the Department for Work and Pensions' Family Resources Survey (Households Below Average Income dataset).
# Adults are defined as all working age and pensionable age adults.
# Working age adults are defined as all individuals aged 16 and over but below state pension age, 
# except unmarried 16 to 19 year-olds in full-time non-advanced education, who are considered children. 

### Coverage:
# 1994/95-1996/97 to 2022/23-2024/25 (some splits don't go back this far though).
# N.B. The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the averaged estimates. 
# This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain data from two financial years each. 
# From 2011 the dataset includes both 3-year and 5-year rolling averages. 5-year aggregations used for religion breakdown.
# Note: There is a break in series from 2021/22 with the introduction of integrated survey and benefit data.

### Integration of survey and benefit administrative data from 2021/22:
# These tables present the first results of FRS survey data linked to administrative records on benefits.
# The change applies back to 2021/22 and results in revisions for household income, relative poverty, absolute poverty and the child low income & material deprivation measure.  There are no revisions for pensioner material deprivation or food security as income is not factored into these measures.
# In terms of this workbook, revisions apply to 2020-23 and 2021-24 as they include one or more of the revised years. 
# Estimates for 2019-22 have been suppressed using a [b] as they contain one year of linked data and one year of unlinked data.

### Pandemic: 
# The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the three-year- 
# or five-year averaged estimates. This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain 
# data from two financial years each. This means that some real changes that happened to incomes, such as the furlough scheme or 
# the temporary increase of Universal Credit are only partially captured in the time series. 
# In addition, it also reduced the combined sample size, and data can be more volatile in the most recent periods.
# Note that poverty is measured at a household level. Everyone in the same household is considered either in poverty or not in poverty. 
# This makes it difficult to measure the poverty rate by age or gender of an individual person if they share the households with others. 


### functions/packages -----
source("functions/main_analysis.R") #Normal indicator functions
library(readODS) # for reading in ods files (open source spreadsheets)

### 1. Read in data ----

# the downloaded spreadsheets:
data_folder = paste0(profiles_data_folder, "/Received Data/Absolute and relative poverty/")
pov_3y <- paste0(data_folder, "data2026.ods")
pov_CIs <- paste0(data_folder, "2026_Confidence_intervals_3yr.ods")


# REL/ABS POVERTY X AGE GROUP SPLITS
# Function to get headline pov rates and CIs 
get_data_from_pov_CIs_file <- function(tab, names_row, ind_num, ind_name) {
  
  df <- read_ods(pov_CIs, sheet = tab, skip=2) %>% # skip 2 so we read in the first blank line as the column headings, so that we know what the cols will be called whichever tab is read in
    
    # population group headings
    mutate(split_value = ifelse(str_detect(...1, "Rate"), ...1, NA)) %>% # puts the sub-headings into a new column at the end (all contain "Rate")...
    fill(split_value) %>% # ... then apply the heading to all rows in that part (i.e., until the next heading is encountered)
    
    # column names
    row_to_names(row_number = names_row-3) %>% # now can apply the correct col headings...
    rename(split_value = names(.)[ncol(.)] ) %>% # ... but need to fix the last column back to "split_value"
    
    # recode the measures (rate, lowci or upci)
    filter(!str_detect(level, "level|Rate" )) %>% # now can drop any rows with "level" or "rate" in them (these contain no data)
    mutate(level = case_when(str_detect(level, "Central") ~ "rate", 
                             str_detect(level, "Lower") ~ "lowci",
                             str_detect(level, "Upper") ~ "upci",
                                        )) %>%
    rename(measure = level) %>%
    
    # recode the population groups
    mutate(split_value = case_when(str_detect(split_value, "people") ~ "Total", 
                             str_detect(split_value, "children") ~ "Children",
                             str_detect(split_value, "working-age adults") ~ "Working-age adults",
                             str_detect(split_value, "pensioners") ~ "Pension-age adults")) %>%
    
    # fix the data
    mutate(across(everything(), ~str_replace(., "[b]", "NA"))) %>% # [b] is the break in the data (the break will be noted in techdoc), so replace with NA
    mutate(across(-c(measure, split_value), ~100*as.numeric(.))) %>% # convert proportions to percentages
    
    # get years into a column, and measures into columns
    pivot_longer(-c(measure, split_value), names_to = "trend_axis", values_to = "value") %>%
    pivot_wider(names_from=measure, values_from = value) %>%
    
    # create new columns required for the dashboard
    mutate(ind_id = ind_num,
           indicator = ind_name,
           split_name = "Age group",
           code = "S00000001", #all are Scotland
           numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
           def_period = paste0(trend_axis, " (aggregated financial years)"),
           year = as.numeric(substr(trend_axis, 1, 4)) + 1) # data are 3 year average, so find mid point
    
}

# Tab 1 = Relative poverty
rel_pov_ahc <- get_data_from_pov_CIs_file(tab="1", names_row=8, ind_num=99990, ind_name="rel_pov_ahc")
children_relpov_ahc <- rel_pov_ahc %>%
  filter(split_value == "Children") %>%
  mutate(ind_id = 30152, 
         indicator = "child_rel_pov_ahc",
         split_name = "Total",
         split_value = "Total")

# Tab 3 = Absolute poverty
abs_pov_ahc <- get_data_from_pov_CIs_file(tab="3", names_row=9, ind_num=99991, ind_name="abs_pov_ahc")
children_abspov_ahc <- abs_pov_ahc %>%
  filter(split_value == "Children") %>%
  mutate(ind_id = 30153, 
         indicator = "child_abs_pov_ahc",
         split_name = "Total",
         split_value = "Total")


### OTHER RELATIVE POVERTY SPLITS ###
# Function to get the splits from the pov_3y file
# These rates do not have CIs, and are only available for rel pov, not abs pov
get_splits_from_pov_3y_file <- function(tab, names_row, split_name, text_to_keep, ind_num, ind_name) {
  
  df <- read_ods(pov_3y, sheet = tab, skip=2) %>%
    
    # sort the subheadings 
    mutate(measure = ifelse(str_detect(...1, "Scotland"), ...1, NA)) %>% # puts the sub-headings into a new column at the end (all contain "Scotland")...
    fill(measure) %>% # ... then apply the heading to all rows in that part (i.e., until the next heading is encountered)
    
    # sort the column names
    row_to_names(row_number = names_row-3) %>% # puts the years as column names
    rename(split_value = names(.)[1], # rename the 1st column 
           measure = names(.)[ncol(.)] ) %>% # renames the last column back to measure
    
    # keep the rows we want
    filter(!str_detect(split_value, "Group|Scotland" )) %>% #drop non-data rows
    filter(str_detect(split_value, text_to_keep)) %>% #keeps just the rows we want
    filter(str_detect(measure, "ate:")) %>% #keep the rates only
    filter(!str_detect(measure, "Severe")) %>% #drop the severe poverty rates
    select(-measure) %>%
    
    # fix the data
    mutate(across(-c(split_value), ~100*as.numeric(.))) %>% # convert proportions to percentages; break in series replaced with NA
    pivot_longer(-c(split_value), names_to = "trend_axis", values_to = "rate") %>%
    
    # create new columns required for the dashboard
    mutate(ind_id = ind_num,
            indicator = ind_name,
            split_name = split_name,
            code = "S00000001", #all are Scotland
            numerator = as.numeric(NA), # insert NA columns where required
            lowci = as.numeric(NA),
            upci = as.numeric(NA),
            def_period = paste0(trend_axis, " (aggregated financial years)"),
            year = as.numeric(substr(trend_axis, 1, 4)) + 1) %>% # 3 year average, so find mid point
    mutate(split_value = ifelse(split_value=="All", "Total", split_value)) 

  }


### Get splits for relative poverty (AHC)

### overall rel poverty (given new ind_id 99990)
overall_urbrur <- get_splits_from_pov_3y_file(tab="16", names_row=9, split_name="Urban-rural classification", 
                                              text_to_keep="All|Urban|Rural", ind_num=99990, ind_name="rel_pov_ahc")

overall_simd <- get_splits_from_pov_3y_file(tab="17", names_row=7, split_name="SIMD decile", 
                                            text_to_keep="All|[1-9]", ind_num=99990, ind_name="rel_pov_ahc")

overall_tenure <- get_splits_from_pov_3y_file(tab="15", names_row=9, split_name="Housing tenure", 
                                              text_to_keep="All|Own|Buy|Rent", ind_num=99990, ind_name="rel_pov_ahc")

overall_disabled <- get_splits_from_pov_3y_file(tab="11", names_row=11, split_name="Disabled person(s) in household", 
                                                text_to_keep="All|person", ind_num=99990, ind_name="rel_pov_ahc") %>%
  mutate(split_value = case_when(str_detect(split_value, "no") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "with disabled") ~ "Yes",
                                 TRUE ~ split_value))
  
### child rel poverty (existing ind_id 30152)
children_disabled <- get_splits_from_pov_3y_file(tab="27", names_row=11, split_name="Disabled person(s) in household", 
                                                 text_to_keep="All|person", ind_num=30152, ind_name="child_rel_pov_ahc") %>%
  mutate(split_value = case_when(str_detect(split_value, "no") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "with disabled") ~ "Yes",
                                 TRUE ~ split_value))

children_age <- get_splits_from_pov_3y_file(tab="20", names_row=8, split_name="Child age group (years)", 
                                            text_to_keep="All|[1-9]", ind_num=30152, ind_name="child_rel_pov_ahc") 

children_urbrur <- get_splits_from_pov_3y_file(tab="26", names_row=9, split_name="Urban-rural classification", 
                                               text_to_keep="All|Urban|Rural", ind_num=30152, ind_name="child_rel_pov_ahc")

children_inwork <- get_splits_from_pov_3y_file(tab="24", names_row=9, split_name="Someone in paid work", 
                                               text_to_keep="All|work", ind_num=30152, ind_name="child_rel_pov_ahc") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "Someone") ~ "Yes",
                                 TRUE ~ split_value))

children_tenure <- get_splits_from_pov_3y_file(tab="25", names_row=9, split_name="Housing tenure", 
                                               text_to_keep="All|Own|Buy|Rent", ind_num=30152, ind_name="child_rel_pov_ahc")

children_loneparent <- get_splits_from_pov_3y_file(tab="18", names_row=8, 
                                                   split_name="Lone parent household", text_to_keep="All|parent", 
                                                   ind_num=30152, ind_name="child_rel_pov_ahc") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "Single") ~ "Yes",
                                 TRUE ~ split_value))

### in-work rel poverty (ind_id=99147)
in_work_pov_ahc <- get_splits_from_pov_3y_file(tab="33", names_row=8, split_name="Someone in paid work", 
                                              text_to_keep="All|work", ind_num=99147, ind_name="in_work_pov_ahc") %>%
  filter(str_detect(split_value, "Someone")) %>% # keep those where someone in the household is in work
  mutate(split_value = "Total", # no splits in this file
         split_name = "Total") 

# Children's combined low income and material deprivation:
# The definition of child material deprivation changed in 2010/11 and in 2023/24, creating breaks in the time series. 
# Please consult the single year workbook and methodological notes for the one year estimates.
# IS THERE A CASE FOR USING THE 1 YEAR DATA INSTEAD?
# LOOK AT 1 YEAR DATA FOR THE HEADLINE POVERTY MEASURES - ABS AND REL BY POP GROUP - THOUGH THE SPLITS AREN'T AVAILABLE. 

children_lowinc_matdep <- get_splits_from_pov_3y_file(tab="7", names_row=9, split_name="Total", 
                                               text_to_keep="After|after", ind_num=30154, ind_name="children_lowinc_matdep")

children_lowinc_matdep <- children_lowinc_matdep %>%
  group_by(trend_axis) %>%
  arrange(rate, .by_group=TRUE) %>% # there's only a single rate for each trend_axis: this brings the single rate to the top of the group...
  filter(row_number()==1) %>%  # then just keeps that row. Will keep NA if there's no data for that trend_axis
  ungroup() %>%
  mutate(split_value = "Total", # no splits in this file
         split_name = "Total") 


# combine the data:
pov_file <- mget(ls(pattern = "children|work|overall|_ahc"), .GlobalEnv) %>% # finds all the dataframes from the CI files
  bind_rows(.) %>%
  mutate(sex=NA) 
rm(list=ls(pattern="children|work|overall|_ahc"))

# get sort order right for split_values:
pov_file <- pov_file %>%
  mutate(split_value = factor(split_value,
                              levels = c("Total", "Children", "Working-age adults", "Pension-age adults",
                                         "0-4", "5-12", "13-19",
                                         "No", "Yes",
                                         "Owned outright", "Buying with a mortgage", "Rented from council or housing association", "Rented privately",
                                         "Urban", "Rural",
                                         "1 - Most deprived", "2", "3","4", "5", "6","7","8","9","10 - Least deprived"),
                              labels = c("Total", "Children", "Working-age adults", "Pension-age adults",
                                         "0-4", "5-12", "13-19",
                                         "No", "Yes",
                                         "Owned outright", "Buying with a mortgage", "Rented from council or housing association", "Rented privately",
                                         "Urban", "Rural",
                                         "1 - Most deprived", "2", "3","4", "5", "6","7","8","9","10 - Least deprived")))

# # save intermediate df:
# arrow::write_parquet(pov_file, paste0(data_folder, "pov_file.parquet"))
# pov_file <- arrow::read_parquet(paste0(data_folder, "pov_file.parquet")) 



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind) {
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- pov_file %>% 
    filter(indicator == ind,
           split_value == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    group_by(code, ind_id, year, # need to drop some duplicate rows: unique() won't work because those from reading in the second file don't have CI data
             numerator, def_period, trend_axis) %>%
    arrange(upci, .by_group = TRUE) %>% # keeps those with the CI data at the top of the group (if any CI data)
    filter(row_number()==1) %>%  # then just keeps the top row. Will keep NA if there's no data for that group
    ungroup() %>%
    arrange(code, year)

  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  assign(paste0("main_", ind), main_data, envir=.GlobalEnv)

  # 2 - population groups data (ie data behind population groups tab)
  # NB only applies to these 3 indicators:
  if(ind %in% c("abs_pov_ahc", "rel_pov_ahc", "child_rel_pov_ahc")) {
      
      pop_grp_data <- pov_file %>% 
        filter(indicator == ind & !(split_name %in% c("Total"))) %>% 
        select(code, ind_id, year, numerator, rate, upci, 
               lowci, def_period, trend_axis, split_name, split_value) %>%
        arrange(code, year, split_name, split_value)
      
      # Save
      write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
      write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
      
      # Make data created available outside of function so it can be visually inspected if required
      assign(paste0("pop_grp_", ind), pop_grp_data, envir=.GlobalEnv)
  
}
}


# Run function to create final files
prepare_final_files(ind = "abs_pov_ahc")
prepare_final_files(ind = "rel_pov_ahc")
prepare_final_files(ind = "child_rel_pov_ahc")
prepare_final_files(ind = "child_abs_pov_ahc")
prepare_final_files(ind = "children_lowinc_matdep")
prepare_final_files(ind = "in_work_pov_ahc")

                                 
# # Run QA reports 
run_qa(type = "main", filename = "abs_pov_ahc", test_file = FALSE)
run_qa(type = "main", filename = "rel_pov_ahc", test_file = FALSE)
run_qa(type = "main", filename = "child_rel_pov_ahc", test_file = FALSE)
run_qa(type = "main", filename = "child_abs_pov_ahc", test_file = FALSE)
run_qa(type = "main", filename = "children_lowinc_matdep", test_file = FALSE)
run_qa(type = "main", filename = "in_work_pov_ahc", test_file = FALSE)

run_qa(type = "popgrp", filename = "abs_pov_ahc", test_file = FALSE)
run_qa(type = "popgrp", filename = "rel_pov_ahc", test_file = FALSE)
run_qa(type = "popgrp", filename = "child_rel_pov_ahc", test_file = FALSE)

