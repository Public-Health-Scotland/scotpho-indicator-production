#########################################################
# Underemployment indicator from Annual Population Survey - data import
#########################################################

### Update ScotPHO indicators sourced from SG Scotland's Labour Market data: 
### Author: Liz Richardson, 14 Nov 2024

# 30033 = Percentage of adults (16+ years) in work who would prefer to work more hours for the same 
# rate of pay. Underemployment estimates include those looking for additional hours in their existing 
# role at the same rate of pay, an additional job, and/or a different job with more hours. 

### Notes on the data source:
# denom = 16+ year olds
# Source: APS via SG Regional Employment Patterns xlsx
# Downloaded from https://www.gov.scot/publications/scotlands-labour-market-people-places-and-regions-background-tables-and-charts/
# N here are just the population level if percent is grossed up, not the survey base, so don't use

### functions/packages -----
source("1.indicator_analysis.R")

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
aps_data_folder <- paste0(data_folder, "Received Data/Underemployment/")
file <- "Scotland%27s+Labour+Market+-+People+Places+and+Regions+-+Jan-Dec+Tables.xlsx"

## Geography lookup -----

# Read in geography lookup
geo_lookup <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


## Read in data

underemp_geog_pc <- read.xlsx(paste0(aps_data_folder, file),
                      sheet = "Table 1.15",
                      startRow = 5,
                      rows = c(6:40),
                      cols = c(1:35),
                      colNames = TRUE) 
names(underemp_geog_pc) <- c("spatial.unit", # correct the column names
                             paste0(c("rate_", "ci_"), rep(2004:2020, each=2)))
underemp_geog <- underemp_geog_pc %>%
  pivot_longer(-spatial.unit, names_to = c("statistic", "year"), names_sep="_", 
               values_to = "value" ) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(lowci=rate-ci,
         upci=rate+ci,
         split_name = "None",
         split_value = "None",
         year = as.integer(year),
         spatial.scale = ifelse(spatial.unit=="Scotland", "Scotland", "Council area")) %>% 
  filter(!is.na(rate))


# by sex
underemp_sex_pc <- read.xlsx(paste0(aps_data_folder, file),
                             sheet = "Table 1.16B",
                             colNames = TRUE,
                             rows = c(6:23),
                             cols = c(1, 8, 9, 17, 18)) 
names(underemp_sex_pc) <- c("year", 
                            "rate_Male", "ci_Male",
                            "rate_Female", "ci_Female") 
underemp_sex <- underemp_sex_pc %>%
  pivot_longer(-year, names_to = c("statistic", "split_value"), names_sep="_", values_to = "value") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(lowci=rate-ci,
         upci=rate+ci,
         spatial.unit = "Scotland",
         spatial.scale = "Scotland",
         split_name = "Sex") %>%
filter(!is.na(rate))
underemp_sex_with_totals <- underemp_geog %>%
  filter(spatial.unit=="Scotland") %>%
  mutate(split_name = "Sex",
         split_value = "Total") %>%
  rbind(underemp_sex)
  

# by age
underemp_age_pc <- read.xlsx(paste0(aps_data_folder, file),
                             sheet = "Table 1.16A",
                             colNames = TRUE,
                             rows = c(5:22),
                             cols = c(1:16)) 
names(underemp_age_pc) <- c("year", # correct the column names
                             paste0(c("rate_", "ci_"), 
                                    rep(c("16 to 24 y", "25 to 34 y", "35 to 49 y", 
                                          "50 to 64 y", "50+ y", "65+ y", "Total"), each=2)))
underemp_age_with_totals <- underemp_age_pc %>%
  pivot_longer(-year, names_to = c("statistic", "split_value"), names_sep="_", values_to = "value") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(lowci=rate-ci,
         upci=rate+ci,
         spatial.unit = "Scotland",
         spatial.scale = "Scotland",
         split_name = "Age") %>%
  filter(!is.na(rate))

# combine
all_data <- rbind(underemp_geog,
                  underemp_sex_with_totals,
                  underemp_age_with_totals) %>%
  select(-ci) %>%
  mutate(ind_id = 30033,
         numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
         trend_axis = as.character(year),
         def_period = paste0("Survey year (", trend_axis, ")"),
         spatial.unit = gsub(" and ", " & ", spatial.unit),
         spatial.unit = gsub("Edinburgh, City of", "City of Edinburgh", spatial.unit)) %>%
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype"), all.x=T)  
  



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland and council area data, total pop
  main_data <- all_data %>% 
    filter(split_name == "None") %>% 
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
  # Contains Scotland data 
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
prepare_final_files(ind = "underemployment")

# # Run QA reports 
run_qa(filename = "underemployment")




# Plot the indicator(s)
# =================================================================================================================
# Let's now see what the series and CIs look like:

# Scot, by sex
all_data %>%
  filter(split_name=="Sex") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# Scot, by age
all_data %>%
  filter(split_name=="Age") %>% 
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) 

# LAs vs. Scotland
all_data %>%
  filter(split_name=="None") %>% 
  ggplot(aes(year, rate, group = spatial.unit, colour = spatial.unit, shape = spatial.unit)) + 
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lowci, ymax = upci), alpha = 0.1) +
  facet_wrap(~spatial.scale)







