#########################################################
# SG poverty data - data import
#########################################################

### Update ScotPHO poverty indicators 
### Author: Liz Richardson, Sept 2026
### Script to read in 2026 ods data published by SG (plus additional xlsx they provided us with). 
### From 2027 we'll use data extracted from data.gov.scot.



# Indicators:

# Previously prepared in script "Poverty (child absolute relative and persistent).R" (NB persistent poverty wasn't in that script despite being in title: this is still prepared in the script "Poverty (persistent).R")
# 30152 = Percentage of 'dependent children' living in relative poverty (after housing costs). Relative poverty is defined as living in households whose equivalised income is below 60% of UK median income in the same year.
# 30153 = Percentage of 'dependent children' living in absolute poverty (after housing costs). Absolute poverty is defined as living in households whose equivalised income is below 60% of the (inflation adjusted) Great Britain median income in 2010/11. 
# 30154 = Percentage of 'dependent children' in combined material deprivation and low income after housing costs (below 70% of UK median income).

# Previously prepared in "Poverty (in work).R"
# 99147 = In-work poverty: % of working age adults (16-64 years) living in households in relative poverty AHC where someone in the household is in paid work

# Previously prepared in script "Poverty (adult absolute and relative).R"
# 30035 = absolute adult poverty = Percentage of adults living in households whose income is below 60% of the inflation-adusted UK median income in 2010/11, AHC. 
# 30031 = relative adult poverty = Percentage of adults living in private households with an equivalised income of less than 60% of the UK median income in the same year, after housing costs


### Notes on the data source:
# NEW SOURCE FOR 2026:
# SOURCE: https://www.gov.scot/publications/poverty-and-income-inequality-in-scotland-2022-25/documents/
# National Statistics of the number and proportions of people living in private households with an equivalised household income below various poverty thresholds. 
# FROM 2027 WE SHOULD BE ABEL TO DOWNLOAD DIRECT FROM DATA.GOV.SCOT SO THIS SCRIPT WILL CHANGE

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
library(readxl) # to read in excel spreadsheets

### 1. Read in data ----

# the downloaded spreadsheets:
data_folder = paste0(profiles_data_folder, "/Received Data/Poverty - absolute and relative/")
pov_CIs <- paste0(data_folder, "2026_Confidence_intervals_3yr.ods")
pov_adult <- paste0(data_folder, "Copy of alladults_analysis2.xlsx") # from Gillian Diggins in SG; Gillian.Diggins@gov.scot and social-justice-analysis@gov.scot
pov_3y <- paste0(data_folder, "data2026.ods")


# Function to get pov rates when provided with CIs: 
get_rows_from_CIs_file <- function(filename, filetype, tab, range ) {
  
  if(filetype=="ods") {
    df <- read_ods(path=filename, sheet = tab, range = range) 
  } else {
    df <- read_xlsx(path=filename, sheet = tab, range = range)
  }
  
  names(df) <- c("measure", names(df)[2:length(names(df))]) # ensures first column has standard name
  
  df <- df %>%
    
    # rename the measures (rate, lowci or upci)
    mutate(measure = case_when(str_detect(measure, "Central|All") ~ "rate", 
                             str_detect(measure, "Lower") ~ "lowci",
                             str_detect(measure, "Upper") ~ "upci",
                                        )) %>%

    # fix the data
    mutate(across(everything(), ~str_replace(., "^[b]", "NA"))) %>% # [b] is the break in the data (the break will be noted in techdoc), so replace with NA
    mutate(across(-measure, ~100*as.numeric(.))) %>% # convert proportions to percentages
    
    # get years into a column, and measures into columns
    pivot_longer(-measure, names_to = "trend_axis", values_to = "value") %>%
    pivot_wider(names_from=measure, values_from = value) %>%
    
    # create new columns required for the dashboard
    mutate(code = "S00000001", #all are Scotland
           numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
           year = as.numeric(substr(trend_axis, 1, 4)) + 1, # data are 3 year average, so find mid point
           split_name = "Total",
           split_value = "Total") 
}


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
    filter(str_detect(measure, c("ate:"))) %>% #keep the rates only
    filter(!str_detect(measure, "Severe")) %>% #drop the severe poverty rates
    mutate(measure = case_when(str_detect(measure, "ate:") ~ "rate",
                               str_detect(measure, "Sample") ~ "denominator")) %>%

    # fix the data
    mutate(across(-c(split_value), ~100*as.numeric(.))) %>% # convert proportions to percentages; break in series replaced with NA
    pivot_longer(-c(split_value), names_to = "trend_axis", values_to = "rate") %>%
    # opted not to back-calculate numerators and CIs from the denominator as the numerator would be numerator x weights, and the CIs wouldn't account for complex survey design (would be too narrow)
    
    # create new columns required for the dashboard
    mutate(ind_id = ind_num,
           indicator = ind_name,
           split_name = split_name,
           code = "S00000001", #all are Scotland
           numerator = as.numeric(NA), # insert NA columns where required
           lowci = as.numeric(NA),
           upci = as.numeric(NA),
           year = as.numeric(substr(trend_axis, 1, 4)) + 1) %>% # 3 year average, so find mid point
    mutate(split_value = ifelse(split_value=="All", "Total", split_value)) 

  }

# Child relative poverty
cyp_relpov <- get_rows_from_CIs_file(filename=pov_CIs, filetype="ods", tab="1", range="A13:AD16") %>%
  mutate(ind_id = 30152, 
         indicator = "cyp-relative-poverty")

# Child absolute poverty
cyp_abspov <- get_rows_from_CIs_file(filename=pov_CIs, filetype="ods", tab="3", range="A14:AD17") %>%
  mutate(ind_id = 30153, 
         indicator = "cyp-absolute-poverty")

# Adult relative poverty
adult_relpov <- get_rows_from_CIs_file(filename=pov_adult, filetype="xlsx", tab="After housing costs", range="A7:AD10")  %>% 
  mutate(ind_id=30031, 
         indicator="adult-relative-poverty")

# Adult absolute poverty
adult_abspov <- get_rows_from_CIs_file(filename=pov_adult, filetype="xlsx", tab="After housing costs", range="A15:AD18")  %>% 
  mutate(ind_id=30035, 
         indicator="adult-absolute-poverty")

# Other splits for child relative poverty:
cyp_disabled <- get_splits_from_pov_3y_file(tab="27", names_row=11, split_name="Disabled person(s) in household", 
                                                 text_to_keep="All|person", ind_num=30152, ind_name="cyp-relative-poverty") %>%
  mutate(split_value = case_when(str_detect(split_value, "no") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "with disabled") ~ "Yes",
                                 TRUE ~ split_value))

cyp_age <- get_splits_from_pov_3y_file(tab="20", names_row=8, split_name="Child age group (years)", 
                                            text_to_keep="All|[1-9]", ind_num=30152, ind_name="cyp-relative-poverty") 

cyp_urbrur <- get_splits_from_pov_3y_file(tab="26", names_row=9, split_name="Urban-rural classification", 
                                               text_to_keep="All|Urban|Rural", ind_num=30152, ind_name="cyp-relative-poverty")

cyp_inwork <- get_splits_from_pov_3y_file(tab="24", names_row=9, split_name="Someone in paid work", 
                                               text_to_keep="All|work", ind_num=30152, ind_name="cyp-relative-poverty") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "Someone") ~ "Yes",
                                 TRUE ~ split_value))

cyp_tenure <- get_splits_from_pov_3y_file(tab="25", names_row=9, split_name="Housing tenure", 
                                               text_to_keep="All|Own|Buy|Rent", ind_num=30152, ind_name="cyp-relative-poverty")

cyp_loneparent <- get_splits_from_pov_3y_file(tab="18", names_row=8, 
                                                   split_name="Lone parent household", text_to_keep="All|parent", 
                                                   ind_num=30152, ind_name="cyp-relative-poverty") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No", # recode the splits to Yes, No or keep as Total
                                 str_detect(split_value, "Single") ~ "Yes",
                                 TRUE ~ split_value))

### in-work rel poverty (ind_id=99147)
in_work_poverty <- get_splits_from_pov_3y_file(tab="33", names_row=8, split_name="Someone in paid work", 
                                              text_to_keep="All|work", ind_num=99147, ind_name="in-work-poverty") %>%
  filter(str_detect(split_value, "Someone")) %>% # keep those where someone in the household is in work
  mutate(split_value = "Total", # no splits in this file
         split_name = "Total") 

# Children's combined low income and material deprivation:
# The definition of child material deprivation changed in 2010/11 and in 2023/24, creating breaks in the time series. 
# Please consult the single year workbook and methodological notes for the one year estimates.
cyp_lowincome_matdep <- get_splits_from_pov_3y_file(tab="7", names_row=9, split_name="Total", 
                                               text_to_keep="After|after", ind_num=30154, ind_name="cyp-combined-low-income-and-material-deprivation")

cyp_lowincome_matdep <- cyp_lowincome_matdep %>%
  group_by(trend_axis) %>%
  arrange(rate, .by_group=TRUE) %>% # there's only a single rate for each trend_axis: this brings the single rate to the top of the group...
  filter(row_number()==1) %>%  # then just keeps that row. Will keep NA if there's no data for that trend_axis
  ungroup() %>%
  mutate(split_value = "Total", # no splits in this file
         split_name = "Total") 


# combine the data:
pov_file <- mget(ls(pattern = "^adult|cyp|work"), .GlobalEnv) %>% # finds all the dataframes processed above
  bind_rows() %>%
  mutate(sex=NA) 
rm(list=ls(pattern="^adult|cyp|work"))


# get sort order right for split_values:
pov_file <- pov_file %>%
  mutate(split_value = factor(split_value,
                              levels = c("Total", 
                                         "0-4", "5-12", "13-19",
                                         "No", "Yes",
                                         "Owned outright", "Buying with a mortgage", "Rented from council or housing association", "Rented privately",
                                         "Urban", "Rural"),
                              labels = c("Total", 
                                         "0-4", "5-12", "13-19",
                                         "No", "Yes",
                                         "Owned outright", "Buying with a mortgage", "Rented from council or housing association", "Rented privately",
                                         "Urban", "Rural")))

# get trend_axis labels right:
# current format 2019-22 but these are aggregated financial years, so need to be 2019/20-2021/22
pov_file <- pov_file %>%
  mutate(start_year = as.numeric(str_sub(trend_axis, 1, 4)),
         trend_axis = paste0(start_year, "/", 
                             str_sub(as.character(start_year+1), 3, 4), "-",
                             start_year+2, "/",
                             str_sub(as.character(start_year+3), 3, 4)),
         def_period = paste0(trend_axis, " (aggregated financial years)")) %>%
  select(-start_year)



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
  # NB only applies to child rel pov:
  if(ind %in% c("cyp-relative-poverty")) {
      
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
prepare_final_files(ind = "adult-absolute-poverty")
prepare_final_files(ind = "adult-relative-poverty")
prepare_final_files(ind = "cyp-relative-poverty")
prepare_final_files(ind = "cyp-absolute-poverty")
prepare_final_files(ind = "cyp-combined-low-income-and-material-deprivation")
prepare_final_files(ind = "in-work-poverty")

# # Run QA reports 
run_qa(type = "main", filename = "adult-absolute-poverty", test_file = FALSE)
run_qa(type = "main", filename = "adult-relative-poverty", test_file = FALSE)
run_qa(type = "main", filename = "cyp-relative-poverty", test_file = FALSE)
run_qa(type = "main", filename = "cyp-absolute-poverty", test_file = FALSE)
run_qa(type = "main", filename = "cyp-combined-low-income-and-material-deprivation", test_file = FALSE) # no CIs
run_qa(type = "main", filename = "in-work-poverty", test_file = FALSE) # no CIs

run_qa(type = "popgrp", filename = "cyp-relative-poverty", test_file = FALSE) #no CIs

