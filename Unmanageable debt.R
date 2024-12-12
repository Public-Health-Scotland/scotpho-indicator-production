#########################################################
# Unmanageable debt indicator from Wealth and Assets Survey - data import
#########################################################

### Update ScotPHO indicator on unmanageable debt sourced from SG Scotland's Labour Market data: 
### Author: Liz Richardson, 15 Nov 2024

# Indicator: 
# 30044 = Percentage of households where the household is falling behind with bills or credit commitments and either making 
# excessive debt repayments or is in arrears on monthly commitments (liquidity problems); or where the household is 
# burdened by high debt levels relative to annual income (solvency problems)
# Problem debt is defined as having liquidity problems, solvency problems or both. 
# Liquidity problems is defined as "falling behind with bills or credit commitments and (in two or more consecutive 
# months arrears on bills or credit commitments or household debt repayment to net monthly income ratio >25%)". 
# Solvency problems is defined as "feel debt is a heavy burden and debt to net annual income ratio >20%".  

# Source: Wealth and Assets Survey, ONS
# SG have extracted Scottish data for 5 waves into:
# https://data.gov.scot/wealth/
# WAS does not cover the regions north of the Caledonian Canal and the Scottish islands. 
# It is therefore not representative of the whole of the Scottish population, and especially leaves out 
# some of the most remote areas in Scotland.
# The household head is the person with the highest income, or, for equal incomes, the older person.

# Availability: Scot, M/F/Total, 2010-12 to 2018-20
#	CIs derived using Wald method, % and unweighted base (sample size). 

### functions/packages -----
source("1.indicator_analysis.R")

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
was_data_folder <- paste0(data_folder, "Received Data/Unmanageable debt/sg_wealth_in_scotland_report/")
file <- "tables2022.xlsx"


## Read in data

import_wealth_data <- function(sheet, rows, groups, split_name) {
  
  df <- read.xlsx(paste0(was_data_folder, file),
                        sheet = sheet,
                        colNames = TRUE,
                        rows = rows) %>%
    filter(Group %in% groups) %>%
    cbind(statistic = c(rep("rate", length(groups)), # adding a column giving the statistic type for the imported data. Only % and sample size are required. 
                        rep("NA", 2*length(groups)), 
                        rep("samplesize", length(groups)))) %>%  
    filter(statistic != "NA") %>%
    pivot_longer(-c(statistic, Group), names_to = "trend_axis", values_to = "value") %>%
    mutate(value = case_when(statistic=="rate" ~ round(as.numeric(value)*100, 1),
                             TRUE ~ as.numeric(value))) %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    mutate(ci_wald = 100 * (1.96*sqrt(((rate/100)*(1-(rate/100)))/samplesize)), # Wald method. 
           lowci = rate - ci_wald,
           upci = rate + ci_wald) %>%
    mutate(split_value = case_when(Group == "All" ~ "Total",
                                   TRUE ~ Group),
           split_name = split_name) %>%
    select(-c(Group, ci_wald, samplesize))
}

data_sex <- import_wealth_data(sheet="53", rows=c(6:24), groups=c("Female", "Male", "All"), split_name="Sex of household head")
data_ur <- import_wealth_data(sheet="54", rows=c(6:24), groups=c("Rural", "Urban", "All"), split_name="Urban/Rural status")
data_disab <- import_wealth_data(sheet="51", rows=c(6:24), groups=c("Someone disabled", "No-one disabled", "All"), split_name="Disability of household member(s)")
data_ethnicity <- import_wealth_data(sheet="46", rows=c(7:25), groups=c("White British", "Minority ethnic", "All"), split_name="Ethnicity of household head")
data_income <- import_wealth_data(sheet="49", rows=c(6:36), 
                                  groups=c("1st (lowest) household income quintile", "2nd", "3rd", "4th", "5th (highest) household income quintile", "All"), 
                                  split_name="Household income (equivalised)") %>%
  mutate(split_value = case_when(split_value == "1st (lowest) household income quintile" ~ "1 - lowest income", 
                                 split_value == "2nd" ~ "2", 
                                 split_value == "3rd" ~ "3", 
                                 split_value == "4th" ~ "4", 
                                 split_value == "5th (highest) household income quintile" ~ "5 - highest income",
                                 split_value == "All" ~ "Total"))
data_age <- import_wealth_data(sheet="47", rows=c(6:40), 
                               groups=c("16-34","35-44","45-54","55-64","65-74","75+","All"), split_name="Age of household head") %>%
  mutate(split_value = paste0(split_value, "y"),
         split_value = case_when(split_value == "Totaly" ~ "Total",
                                 split_value == "75+y" ~ "75y+",
                                 TRUE ~ split_value))

all_data <- rbind(data_sex,
                     data_ur,
                     data_disab,
                     data_ethnicity,
                     data_income,
                     data_age) %>%
  mutate(code = "S00000001", #all are Scotland
         ind_id = 30044,
         numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
         def_period = paste0(trend_axis, " (3 survey year aggregate)"),
         year = as.numeric(substr(trend_axis, 1, 4)) + 1)  # 3  year average, so find mid point
        

##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland  data, total pop
  main_data <- all_data %>% 
    filter(split_name == "Sex of household head" & split_value=="Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland data by population groups (including total)
  pop_grp_data <- all_data %>% 
    filter(!(split_name %in% c("None", "Deprivation (SIMD)"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year, split_name)
  
  # Save
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "unmanageable_debt")

# # Run QA reports 
run_qa(filename = "unmanageable_debt")

# Manual plot checks as pop group automated checks not written yet

# Plot the indicator(s)
# =================================================================================================================
# Let's now see what the series and CIs look like:

# Sex of household head
all_data %>%
  filter(split_name=="Sex of household head") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Age of household head
all_data %>%
  filter(split_name=="Age of household head") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Urban/rural status
all_data %>%
  filter(split_name=="Urban/Rural status") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Disability of household member(s)
all_data %>%
  filter(split_name=="Disability of household member(s)") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Household income
all_data %>%
  filter(split_name=="Household income (equivalised)") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Ethnicity of household head
all_data %>%
  filter(split_name=="Ethnicity of household head") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

