#########################################################
# SG Persistent Poverty data import
#########################################################

### Update ScotPHO persistent poverty indicators 
### Author: Liz Richardson, August 2026

#   99116: Persistent poverty (includes both adults and children)
#   30155: Child persistent poverty (uses the same data as 99116 but this includes only child age group as this is specifically presented in CYP mental health indicators)
#   NB. Poverty defined as 60% of median income for both

### Notes on the data source:
# https://data.gov.scot/poverty/persistent.html
# Latest = March 2026
# Downloaded to Persistent poverty folder


### functions/packages -----
source("functions/main_analysis.R") # for packages and QA function 

# Load additional packages
library(readODS) # for reading in ods files (open source spreadsheets)

### 1. Paths ----

# Identify data folder
data_folder <- paste0(profiles_data_folder, "/Received Data/Poverty - persistent/")
data_file <- "Persistent+Poverty+in+Scotland+2010-2024.ods"

### 2. Read in data ----

get_pers_pov_data <- function(tab) {

  rate <- read_ods(paste0(data_folder, data_file),
                           sheet = tab,
                           skip = 6,
                           col_names = TRUE) %>%
    select(trend_axis = Period, rate = "After housing costs") %>%
    mutate(rate = as.numeric(rate)) %>%
    filter(!is.na(rate)) 
  
  count <- read_ods(paste0(data_folder, data_file),
                            sheet = tab,
                            skip = 20,
                            col_names = TRUE) %>%
    select(trend_axis = Period, denominator = Sample)
  
  rate_and_count <- rate %>%
    merge(y = count, by = "trend_axis") %>%
    mutate(rate = rate * 100, # spreaddsheet's % formatting saves it as rate/100
           numerator = round(denominator * rate / 100),
           # confidence intervals
           ci_wald = 100 * (1.96*sqrt(((rate/100)*(1-(rate/100)))/denominator)), # Wald method.
           lowci = rate - ci_wald,
           upci = rate + ci_wald,
           indicator = "people_persistent_poverty",
           ind_id = 99116)
  
  }
  
  
# All people
people_df <- get_pers_pov_data(tab="1") %>%
  mutate(split_name = "Age group", 
         split_value = "Total")

# Children
children_df <- get_pers_pov_data(tab="2") %>%
  mutate(split_name = "Age group", 
         split_value = "Children")

# Working age adults
wkadults_df <- get_pers_pov_data(tab="3") %>%
  mutate(split_name = "Age group", 
         split_value = "Working age")

# Pensioners
pensioners_df <- get_pers_pov_data(tab="4") %>%
  mutate(split_name = "Age group", 
         split_value = "Pensioners")



# Combine
pers_pov <- rbind(people_df, children_df, wkadults_df, pensioners_df) %>%
  mutate(sex = "Total",
         year = as.numeric(substr(trend_axis, 1, 4)) + 2, # mid-point of range
         code = "S00000001",
         def_period = "Aggregated 4 x 2-wave periods",
         measure_type = "percent") 

# add in CYP data again with different ind_id:

pers_pov2 <- pers_pov %>%
  filter(split_value == "Children") %>%
  mutate(ind_id = 30155,
         indicator = "children_persistent_poverty",
         split_name = "Total",
         split_value = "Total") %>%
  rbind(pers_pov)

##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- pers_pov2 %>% 
    filter(indicator == ind & split_value == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year) 
  
  # Save 
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  assign(paste0("main_", ind), main_data, envir=.GlobalEnv)
  
  if(ind == "people_persistent_poverty") { # don't do popgroups for CYP indicator, as there aren't any
    
  # 2 - population groups data (ie data behind population groups tab)
  pop_grp_data <- pers_pov2 %>% 
    filter(indicator == ind) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year)
  
  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  assign(paste0("popgrp_", ind), pop_grp_data, envir=.GlobalEnv)
  
  }
  
  
}


# Run function to create final files
####################
prepare_final_files(ind = "people_persistent_poverty")
prepare_final_files(ind = "children_persistent_poverty")



# Run QA reports
####################

run_qa(type = "main", filename = "people_persistent_poverty", test_file = FALSE) 
run_qa(type = "main", filename = "children_persistent_poverty", test_file = FALSE) 

run_qa(type = "popgrp", filename = "people_persistent_poverty", test_file = FALSE)


#END

