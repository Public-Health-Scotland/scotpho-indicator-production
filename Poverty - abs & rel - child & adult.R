#########################################################
# SG poverty data - data import
#########################################################

### Update ScotPHO poverty indicators 
### Author: Liz Richardson, August 2026

# Definitions:
# 30152 = Percentage of 'dependent children' living in relative poverty (after housing costs). Relative poverty is defined as living in households whose equivalised income is below 60% of UK median income in the same year.
# 30153 = Percentage of 'dependent children' living in absolute poverty (after housing costs). Absolute poverty is defined as living in households whose equivalised income is below 60% of the (inflation adjusted) Great Britain median income in 2010/11. 
# 30154 = Percentage of 'dependent children' in combined material deprivation and low income after housing costs (below 70% of UK median income).
# 99147 = Adults (16-64 years) in working households in relative poverty AHC
# 30035 = LAST PUBLISHED 2025 - absolute poverty = Percentage of adults living in households whose income is below 60% of the inflation-adusted UK median income in 2010/11, AHC. 
# 30031 = LAST PUBLISHED 2025 - relative poverty = Percentage of adults living in private households with an equivalised income of less than 60% of the UK median income in the same year, after housing costs

# new indicators proposed here (/thinking about): (given that adult poverty isn't presented anymore)
# 99990 Overall relative poverty AHC
# 99991 Overall absolute poverty AHC
# 99992 Working-age relative poverty AHC
# 99993 Working-age absolute poverty AHC


############################################
# ADULT POVERTY RATES NOT PRODUCED NOW.
# USE WORKING AGE OR ALL PEOPLE AS SPLITS ARE AVAILABLE FOR THESE? 
# SG DATA REFERS TO WORKING AGE AS 'ADULTS' FOR SHORTHAND
# ADULTS ARE 78% OF TOTAL POP IN 2022-25, AND WORKING AGE IS 82% OF ADULT POP
# SO EITHER OVERALL OR WORKING AGE POVERTY ARE REASONABLE PROXIES FOR ADULT POVERTY...
# PLOT ADULT POVERTY (TO 2021-24) AGAINST OVERALL AND WORKING AGE TO ASSESS
############################################

### Notes on the data source:
# NEW SOURCE FROM 2026:
# SOURCE: https://www.gov.scot/publications/poverty-and-income-inequality-in-scotland-2022-25/documents/

# National Statistics of the number and proportions of people living in private households with an equivalised household income below various poverty thresholds. 
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

# Coverage:
# 1994/95-1996/97 to 2022/23-2024/25 (some splits don't go back this far though).
# N.B. The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the averaged estimates. 
# This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain data from two financial years each. 
# From 2011 the dataset includes both 3-year and 5-year rolling averages. 5-year aggregations used for religion breakdown.
# Note: There is a break in series from 2021/22 with the introduction of integrated survey and benefit data.

# Integration of survey and benefit administrative data from 2021/22
# These tables present the first results of FRS survey data linked to administrative records on benefits.
# The change applies back to 2021/22 and results in revisions for household income, relative poverty, absolute poverty and the child low income & material deprivation measure.  There are no revisions for pensioner material deprivation or food security as income is not factored into these measures.
# In terms of this workbook, revisions apply to 2020-23 and 2021-24 as they include one or more of the revised years. Estimates for 2019-22 have been suppressed using a [b] as they contain one year of linked data and one year of unlinked data.


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


# Function to get headline pov rates and CIs 
get_data_from_pov_CIs_file <- function(tab, names_row, group_name, ind_num, ind_name) {
  
  df <- read_ods(pov_CIs, sheet = tab, skip=2) %>%
    mutate(group = ifelse(str_detect(...1, "Rate"), ...1, NA)) %>%
    fill(group) %>%
    row_to_names(row_number = names_row-3) %>%
    filter(!str_detect(level, "level|Rate" )) %>%
    mutate(level = case_when(str_detect(level, "Central") ~ "rate",
                             str_detect(level, "Lower") ~ "lowci",
                             str_detect(level, "Upper") ~ "upci",
                                        )) %>%
    rename(measure = level) %>%
    rename(group = names(.)[ncol(.)] ) %>%
    mutate(group = case_when(str_detect(group, "people") ~ "all", 
                             str_detect(group, "children") ~ "children",
                             str_detect(group, "working-age adults") ~ "working-age adults",
                             str_detect(group, "pensioners") ~ "pensioners")) %>%
    mutate(across(everything(), ~str_replace(., "[b]", "NA"))) %>% 
    # all values are proportions, so x100 converts them to percentages
    mutate(across(-c(measure, group), ~100*as.numeric(.))) %>% # break in series ([b]) replaced with NA.
    pivot_longer(-c(measure, group), names_to = "trend_axis", values_to = "value") %>%
    pivot_wider(names_from=measure, values_from = value) %>%
    filter(group==group_name) %>%
    # Create new columns
    mutate(ind_id = ind_num,
           indicator = ind_name,
           code = "S00000001", #all are Scotland
           numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
           def_period = paste0(trend_axis, " (aggregated financial years)"),
           year = as.numeric(substr(trend_axis, 1, 4)) + 1) %>% # 3 year average, so find mid point
    select(-group)
}

# Tab 1 = Relative poverty
children_relpov_ahc <- get_data_from_pov_CIs_file(tab="1", names_row=8, group_name="children", ind_num=30152, ind_name="child relative poverty AHC")
overall_relpov_ahc <- get_data_from_pov_CIs_file(tab="1", names_row=8, group_name="all", ind_num=99990, ind_name="overall relative poverty AHC")
working_relpov_ahc <- get_data_from_pov_CIs_file(tab="1", names_row=8, group_name="working-age adults", ind_num=99992, ind_name="working age relative poverty AHC")

# Tab 3 = Absolute poverty
children_abspov_ahc <- get_data_from_pov_CIs_file(tab="3", names_row=9, group_name="children", ind_num=30153, ind_name="child absolute poverty AHC")
overall_abspov_ahc <- get_data_from_pov_CIs_file(tab="3", names_row=9, group_name="all", ind_num=99991, ind_name="overall absolute poverty AHC")
working_abspov_ahc <- get_data_from_pov_CIs_file(tab="3", names_row=9, group_name="working-age adults", ind_num=99993, ind_name="working age absolute poverty AHC")


# Function to get the splits from the pov_3y file
# These rates do not have CIs
get_splits_from_pov_3y_file <- function(tab, names_row, split_name, text_to_keep, ind_num, ind_name) {
  
  df <- read_ods(pov_3y, sheet = tab, skip=2) %>%
    mutate(measure = ifelse(str_detect(...1, "Scotland"), ...1, NA)) %>% # puts the sub-headings into a column
    fill(measure) %>% # fills in the blanks in that column
    row_to_names(row_number = names_row-3) %>% # puts the years as column names, but messes up the last (i.e., measure) column...
    rename(Group = names(.)[1], # 1st col is 'Group' already in all but one tab... 
           measure = names(.)[ncol(.)] ) %>% # renames the last column back to measure
    filter(!str_detect(Group, "Group|Scotland" )) %>% #drop non-data rows
    filter(str_detect(Group, text_to_keep)) %>% #keeps just the rows we want
    filter(str_detect(measure, "ate:")) %>% #keep the rates only
    filter(!str_detect(measure, "Severe")) %>% #drop the severe poverty rates
    select(-measure) %>%
    mutate(across(-c(Group), ~100*as.numeric(.))) %>% # convert proportions to percentages; break in series replaced with NA
    pivot_longer(-c(Group), names_to = "trend_axis", values_to = "rate") %>%
    # Create new columns
    mutate(ind_id = ind_num,
            indicator = ind_name,
            split_name = split_name,
            code = "S00000001", #all are Scotland
            numerator = as.numeric(NA), # insert NA columns where required
            lowci = as.numeric(NA),
            upci = as.numeric(NA),
            def_period = paste0(trend_axis, " (aggregated financial years)"),
            year = as.numeric(substr(trend_axis, 1, 4)) + 1) %>% # 3 year average, so find mid point
    rename(split_value = Group) %>%
    mutate(split_value = ifelse(split_value=="All", "Total", split_value)) 

  }


# Get splits for relative poverty (AHC)

# overall rel poverty (given new ind_id 99990)
overall_urbrur <- get_splits_from_pov_3y_file(tab="16", names_row=9, split_name="Urban-rural classification", 
                                              text_to_keep="All|Urban|Rural", ind_num=99990, ind_name="overall relative poverty AHC")

overall_simd <- get_splits_from_pov_3y_file(tab="17", names_row=7, split_name="SIMD decile", 
                                            text_to_keep="All|[1-9]", ind_num=99990, ind_name="overall relative poverty AHC")

overall_tenure <- get_splits_from_pov_3y_file(tab="15", names_row=9, split_name="Housing tenure", 
                                              text_to_keep="All|Own|Buy|Rent", ind_num=99990, ind_name="overall relative poverty AHC")

overall_disabled <- get_splits_from_pov_3y_file(tab="11", names_row=11, split_name="Disabled person(s) in household", 
                                                text_to_keep="All|person", ind_num=99990, ind_name="overall relative poverty AHC") %>%
  mutate(split_value = case_when(str_detect(split_value, "no") ~ "No",
                                 str_detect(split_value, "with disabled") ~ "Yes",
                                 TRUE ~ split_value))
  
# child rel poverty (existing ind_id 30152)
children_disabled <- get_splits_from_pov_3y_file(tab="27", names_row=11, split_name="Disabled person(s) in household", 
                                                 text_to_keep="All|person", ind_num=30152, ind_name="child relative poverty AHC") %>%
  mutate(split_value = case_when(str_detect(split_value, "no") ~ "No",
                                 str_detect(split_value, "with disabled") ~ "Yes",
                                 TRUE ~ split_value))

children_age <- get_splits_from_pov_3y_file(tab="20", names_row=8, split_name="Child age (years)", 
                                            text_to_keep="All|[1-9]", ind_num=30152, ind_name="child relative poverty AHC") 

children_urbrur <- get_splits_from_pov_3y_file(tab="26", names_row=9, split_name="Urban-rural classification", 
                                               text_to_keep="All|Urban|Rural", ind_num=30152, ind_name="child relative poverty AHC")

children_inwork <- get_splits_from_pov_3y_file(tab="24", names_row=9, split_name="Someone in paid work", 
                                               text_to_keep="All|work", ind_num=30152, ind_name="child relative poverty AHC") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No",
                                 str_detect(split_value, "Someone") ~ "Yes",
                                 TRUE ~ split_value))

children_tenure <- get_splits_from_pov_3y_file(tab="25", names_row=9, split_name="Housing tenure", 
                                               text_to_keep="All|Own|Buy|Rent", ind_num=30152, ind_name="child relative poverty AHC")

children_loneparent <- get_splits_from_pov_3y_file(tab="18", names_row=8, 
                                                   split_name="Lone parent household", text_to_keep="All|parent", 
                                                   ind_num=30152, ind_name="child relative poverty AHC") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No",
                                 str_detect(split_value, "Single") ~ "Yes",
                                 TRUE ~ split_value))

# working-age rel poverty (given new ind_id 99992)
working_gender <- get_splits_from_pov_3y_file(tab="35", names_row=8, split_name="Gender", 
                                              text_to_keep="All|ale", ind_num=99992, ind_name="working-age adult relative poverty AHC")

working_inwork <- get_splits_from_pov_3y_file(tab="33", names_row=8, split_name="Someone in paid work", 
                                              text_to_keep="All|work", ind_num=99992, ind_name="working-age adult relative poverty AHC") %>%
  mutate(split_value = case_when(str_detect(split_value, "No") ~ "No",
                                 str_detect(split_value, "Someone") ~ "Yes",
                                 TRUE ~ split_value))

# children's combined low income and material deprivation:
# The definition of child material deprivation changed in 2010/11 and in 2023/24, creating breaks in the time series. 
# Please consult the single year workbook and methodological notes for the one year estimates.
# IS THERE A CASE FOR USING THE 1 YEAR DATA INSTEAD?
# LOOK AT 1 YEAR DATA FOR THE HEADLINE POVERTY MEASURES - ABS AND REL BY POP GROUP - THOUGH THE SPLITS AREN'T AVAILABLE. 

children_lowinc_matdep <- get_splits_from_pov_3y_file(tab="7", names_row=9, split_name="Total", 
                                               text_to_keep="After|after", ind_num=30154, ind_name="children's combined low income and material deprivation")

children_lowinc_matdep <- children_lowinc_matdep %>%
  group_by(trend_axis) %>%
  arrange(rate, .by_group=TRUE) %>% # there's only a single rate for each trend_axis: this brings the single rate to the top of the group...
  filter(row_number()==1) %>%  # then just keeps that row. Will keep NA if there's no data for that trend_axis
  ungroup()

# combine the data:
pov_file1 <- mget(ls(pattern = "_ahc$"), .GlobalEnv) %>% # finds all the dataframes from the CI files
  bind_rows(.) %>%
  mutate(sex=NA,
         split_name="Total",
         split_value="Total")
rm(list=ls(pattern="_ahc$"))

pov_file2 <- mget(ls(pattern = "children|working|overall"), .GlobalEnv) %>% # finds all the dataframes from the CI files
  bind_rows(.) %>%
  mutate(sex=NA) 
rm(list=ls(pattern="children|working|overall"))

inwork_poverty <- pov_file2 %>%
  filter(str_detect(indicator, "work") & str_detect(split_name, "work") & split_value=="Yes") %>%
  mutate(indicator="working-age in-work rel poverty AHC",
         split_name="Total",
         split_value="Total",
         ind_id=99147)

pov_file3 <- rbind(pov_file1, pov_file2, inwork_poverty)

# save intermediate df:
arrow::write_parquet(pov_file3, paste0(data_folder, "pov_file3.parquet"))
pov_file3 <- arrow::read_parquet(paste0(data_folder, "pov_file3.parquet")) 





#children's combined low income and material dep tab=7
# 30154 = Percentage of 'dependent children' in combined material deprivation and low income after housing costs (below 70% of UK median income).







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




