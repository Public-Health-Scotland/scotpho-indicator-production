#########################################################
# SG child poverty data - data import
#########################################################

# TO DO: 
# WORK OUT HOW TO USE SIMD DECILE DATA (WHEN MORE THAN ONE DATA POINT, AS NOW)
# Use the child material deprivation data processed here for the CWB indicator (see note below): check this is ok to do.


### Update ScotPHO indicators on child poverty
### Author: Liz Richardson, 13 Nov 2024

# Child poverty indicators (part of the CYP mental health indicators)

# 30152 = Percentage of 'dependent children' living in relative poverty (before housing costs). Relative poverty is defined as living in households whose equivalised income is below 60% of UK median income in the same year.
# 30153 = Percentage of 'dependent children' living in absolute poverty (before housing costs). Absolute poverty is defined as living in households whose equivalised income is below 60% of the (inflation adjusted) Great Britain median income in 2010/11. 
# 30154 = Percentage of 'dependent children' in combined material deprivation and low income after housing costs (below 70% of UK median income).
# N.B. CWB takes data for 30154 (numbered 99118 there) from NPF xlsx, but 4-y aggregations (3y here) and fewer breakdowns. Slightly different figures. Assess which to use.

### Notes on the data source:
# statistics.gov.scot 

# Data notes:
# Figures are presented as three-year rolling averages to ensure robust time series analyses. 
# Ethnicity and religion breakdowns are presented as five-year averages due to small sample sizes. 
# Note that ethnicity and religion estimates are particularly volatile. 
# The reason for this is that ethnic and religious composition of the population are not accounted for in the survey weighting process, and therefore the ethnic and religious composition of the population is not accurately captured.
# Where estimates are suppressed due to small sample size this is marked with an asterisk ("*"). 
# Where estimates are between 0 and 0.5% this is marked as "[low]". 
# Where estimates are missing for other reasons this is marked with a hyphen ("-"). 
# The income measure used is equivalised net disposable income before and after housing costs. 
# The before housing costs measure is income from all sources (including earnings, benefits, tax credits, pensions, and investments) after deductions for income tax, national insurance contributions, council tax, pension contributions and maintenance payments.
# Relative poverty: Individuals living in households whose equivalised income is below 60% of UK median income in the same year. 
# This is a measure of whether those in the lowest income households are keeping pace with the growth of incomes in the economy as a whole.
# Absolute poverty: Individuals living in households whose equivalised income is below 60% of inflation adjusted UK median income in 2010/11. 
# This is a measure of whether those in the lowest income households are seeing their incomes rise in real terms.
# Combined low income and material deprivation: measures living standards of children, and refers to the inability of households to afford basic goods and activities that are seen as necessities in society.
# The data source is the Department for Work and Pensions' Family Resources Survey (Households Below Average Income dataset).
# Working age adults are defined as all individuals aged 16 and over but below state pension age, 
# except unmarried 16 to 19 year-olds in full-time non-advanced education, who are considered children. 
# The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the three-year- 
# or five-year averaged estimates. This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain 
# data from two financial years each. This means that some real changes that happened to incomes, such as the furlough scheme or 
# the temporary increase of Universal Credit are only partially captured in the time series. 
# In addition, it also reduced the combined sample size, and data can be more volatile in the most recent periods.
# Note that poverty is measured at a household level. Everyone in the same household is considered either in poverty or not in poverty. 
# This makes it difficult to measure the poverty rate by age or gender of an individual person if they share the households with others. 

# SIMD data only available for 2019/20 to 2021/22 in this dataset, and only for deciles. 
# Possible to use count data (grossed up to population) in future to calculate a population-weighted average at quintile level?


# statistics.gov.scot data were downloaded using opendatascot: 
# https://scotgovanalysis.github.io/opendatascot/
# How to install:
# download the zipped repository from the opendatascot github page to a UNIX space.
# install.packages("devtools")
# library(devtools)
# devtools::install_local(
#   "<FILEPATH OF ZIPPED FILE>/opendatascot-main.zip",
#   upgrade = "never",
#   build_vignettes = TRUE
# )


### functions/packages -----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R")
library(opendatascot)

### 1. Read in data ----

# see structure and variables of this dataset
ods_structure("poverty-children")

# extract data
childpov_raw <- opendatascot::ods_dataset("poverty-children",
                          measureType = c("ratio", "sample-size"),
                          typeOfTenure = "all",
                          workStatus = "all",
                          numberOfChildrenInHousehold = "all",
                          ageOfYoungestChild = "all",
                          singleParenthood = "all",
                          ageOfMother = "all",
                          familyEmploymentStatus = "all",
                          indicatorpoverty = c("relative-poverty", "absolute-poverty", "combined-low-income-and-material-deprivation") 
                          )  %>%
  select(-c(refArea, typeOfTenure, workStatus, numberOfChildrenInHousehold, ageOfYoungestChild, singleParenthood, 
            ageOfMother, familyEmploymentStatus))

# prepare data
childpov <- childpov_raw %>% 
  
  # clean column names
  clean_names() %>%
  
  # convert string to numeric after replacing suppressed values with NA
  mutate(value = case_when(value == "*" ~ as.numeric(NA),
                           value == "[low]" ~ as.numeric(NA),
                           TRUE ~ as.numeric(value))) %>%
  
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
  mutate(split_name = case_when(urban_rural_classification != "all" ~ "Urban/Rural",
                                simd_deciles != "all" ~ "Deprivation (SIMD)",
                                ethnicity != "all" ~ "Ethnicity",
                                household_disability_status != "all" ~ "Household disability",
                                age != "all" ~ "Age",
                                urban_rural_classification == "all" & simd_deciles == "all" & household_disability_status=="all" &
                                  age=="all" & ethnicity=="all" ~ "Total"),
         
  # create single split value column
         split_value = case_when(split_name == "Urban/Rural" ~ urban_rural_classification,
                                 split_name == "Deprivation (SIMD)" ~ simd_deciles,
                                 split_name == "Ethnicity" ~ ethnicity,
                                 split_name == "Household disability" ~ household_disability_status,
                                 split_name == "Age" ~ age,
                                 split_name == "Total" ~ "Total"),
         
  # tidy split values
         split_value = str_to_sentence(split_value), # capitalises first letter
         split_value = str_replace_all(split_value, c("-" = " ",
                                                      "1 most deprived" = "1 - most deprived",
                                                      "10 least deprived" = "10 - least deprived",
                                                      "person s" = "persons",
                                                      "child ren" = "children",
                                                      "adult s" = "adults",
                                                      "0 4" = "0-4",
                                                      "5 12" = "5-12",
                                                      "asian" = "Asian",
                                                      "british" = "British",
                                                      "black" = "Black"
                                                      )),

  # Create new columns
         code = "S00000001", #all are Scotland
         ind_id = case_when(indicator == "relative-poverty" ~ 30152,
                              indicator == "absolute-poverty" ~ 30153, 
                              indicator == "combined-low-income-and-material-deprivation" ~ 30154),
         numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
         def_period = paste0(trend_axis, " (", 
                             (as.numeric(substr(trend_axis, 9, 12)) - as.numeric(substr(trend_axis, 1, 4)) + 1), 
                             " year aggregate)"),
         year = as.numeric(substr(trend_axis, 1, 4)) + 0.5*(as.numeric(substr(trend_axis, 9, 12)) - as.numeric(substr(trend_axis, 1, 4)))  # 3 or 5 year average, so find mid point
  ) %>%
  
  # select before/after housing costs based on indicator definition
  filter((ind_id %in% c(30152, 30153) & housing_costs=="before-housing-costs") |
           (ind_id == 30154 & housing_costs=="after-housing-costs")) %>%
  
  # select right variables
  select(-c(housing_costs, age, ethnicity, household_disability_status, urban_rural_classification, simd_deciles))
  


##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland data, total pop
  main_data <- childpov %>% 
    filter(indicator == gsub("cyp-","",ind),
           split_name == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() 
  
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland data by sex (including total)
  pop_grp_data <- childpov %>% 
    filter(indicator == gsub("cyp-","",ind) & !(split_name %in% c("Total"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value,) 
  
  # Save
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  
  # # 3 - SIMD data (ie data behind deprivation tab) # RUN THIS WHEN DEP CALCS CAN HANDLE DECILES (OR WE APPROXIMATE QUINTILES FROM DECILES)
  # # Contains Scotland data by SIMD quintile (single year or 2y aggregate)
  # 
  # # Process SIMD data
  # simd_data <- childpov %>% 
  #   filter(indicator == gsub("cyp-","",ind) & split_name == "Deprivation (SIMD)") %>% 
  #   unique() %>%
  #   select(-indicator, -split_name) %>%
  #   rename(quintile = split_value) %>%
  #   mutate(quint_type = "sc_quin")
  # 
  # # Save intermediate SIMD file
  # write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  # write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  # 
  # #get ind_id argument for the analysis function 
  # ind_id <- unique(simd_data$ind_id)
  # 
  # # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  # analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
  #                                pop = "xxxxx", # need under 16 pop for the calc
  #                                ind_id, 
  #                                ind
  # )
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
#  simd_data_result <<- simd_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "cyp-absolute-poverty")
prepare_final_files(ind = "cyp-relative-poverty")
prepare_final_files(ind = "cyp-combined-low-income-and-material-deprivation")

