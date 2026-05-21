#########################################################
# Underemployment indicator from Annual Population Survey - data import
#########################################################

# To do:
# look at deriving own estimates from APS (in SRS) as none published since 2022 (get nums and denoms too, so can calc CIs for all geogs)
# look at possibility of SIMD data from SRS

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
# We use N (grossed up numerator) to derive numerators and to aggregate the data up to higher geogs, but can't then use these to calculate CIs. CIs left blank for aggregated geogs. 

### functions/packages -----
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA


# Load additional packages
library(openxlsx)
library(hablar) # sum_ function from hablar keeps NA when there should be NA

### 1. Read in data ----

# Identify data folder
aps_data_folder <- paste0(profiles_data_folder, "/Received Data/Underemployment/")
file <- "Scotland%27s+Labour+Market+-+People+Places+and+Regions+-+Jan-Dec+Tables.xlsx"

## Geography lookup -----

# Read in lookup for getting area codes
code_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# Create lookup from CAs to higher geogs
geog_lookup <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019, hscp2019, adp, pd) %>%
  distinct(.)

## Read in data

# rates for council areas x year
underemp_ca_pc <- read.xlsx(paste0(aps_data_folder, file),
                            sheet = "Table 1.15",
                            startRow = 5,
                            rows = c(6:40),
                            cols = c(1:35),
                            colNames = TRUE) 
names(underemp_ca_pc) <- c("spatial.unit", # correct the column names
                           paste0(c("rate_", "ci_"), rep(2004:2020, each=2)))
underemp_ca <- underemp_ca_pc %>%
  pivot_longer(-spatial.unit, names_to = c("statistic", "year"), names_sep="_", 
               values_to = "value" ) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(lowci=rate-ci,
         upci=rate+ci,
         split_name = "None",
         split_value = "None")  %>%
  select(-ci)

# counts for council areas x year
underemp_ca_count <- read.xlsx(paste0(aps_data_folder, file),
                               sheet = "Table 1.15",
                               startRow = 5,
                               rows = c(45:79),
                               cols = c(1:35),
                               colNames = TRUE) 
names(underemp_ca_count) <- c("spatial.unit", # correct the column names
                              paste0("numerator_", c(2004:2020)))
underemp_ca_n <- underemp_ca_count %>%
  pivot_longer(-spatial.unit, names_to = c("statistic", "year"), names_sep="_", 
               values_to = "value" ) %>%
  pivot_wider(names_from = statistic, values_from = value) 
underemp_ca <- underemp_ca %>%
  merge(y=underemp_ca_n, by=c("spatial.unit", "year")) %>%
  mutate(denominator = 100 * numerator/rate,
         spatial.scale = ifelse(spatial.unit=="Scotland", "Scotland", "Council area"),
         spatial.unit = gsub(" and ", " & ", spatial.unit),
         spatial.unit = gsub("Edinburgh, City of", "City of Edinburgh", spatial.unit)) %>%
  # add the geog codes, 
  merge(y=code_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype"), all.x=T)  %>% 
  filter(!is.na(rate)) %>%
  select(-spatial.unit, -spatial.scale)

# Aggregate the CA data up to higher geogs
agg_to_higher <- function(df, geog) {
  
  df <- df %>%
    filter(substr(code, 1, 3) == "S12") %>% # just the council areas
    merge(y=geog_lookup, by.x="code", by.y= "ca2019") %>%
    select(-code, -rate, -lowci, -upci) %>%
    rename(code = geog) %>%
    group_by(across(any_of(c("code", "split_name", "split_value", "year")))) |>
    summarise(numerator = sum_(numerator), # sum_ function from hablar keeps NA when there should be NA, and doesn't replace with 0 (as occurs if summed with na.rm=T). This helps to avoid Inf and NaN values from incomplete rate calcs. 
              denominator = sum_(denominator)) %>%
    ungroup() %>%
    # use helper function to calculate the % and the confidence intervals (Byars method)
    calculate_percent() %>%
    # but drop the CIs as these have been calculated from grossed up counts, so are artificially small
    mutate(lowci = as.numeric(NA),
           upci = as.numeric(NA))
}

underemp_hb <- agg_to_higher(underemp_ca, "hb2019")
underemp_pd <- agg_to_higher(underemp_ca, "pd")
#underemp_adp <- agg_to_higher(underemp_ca, "adp") #exclude adp for now as this indicator less relevant for alcohol and drug partnerships?
underemp_hscp <- agg_to_higher(underemp_ca, "hscp2019")


# Combine
underemp_all <- underemp_ca %>%
  rbind(underemp_hb, 
        underemp_pd, 
        underemp_hscp) %>%
  select(-denominator, -numerator) 


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
         spatial.scale = "Scotland",
         spatial.unit = "Scotland",
         split_name = "Sex") %>%
  # add the geog codes, 
  merge(y=code_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype"))  %>% 
  filter(!is.na(rate)) %>%
  select(-ci, -spatial.scale, -spatial.unit)
underemp_sex_with_totals <- underemp_all %>%
  filter(code=="S00000001") %>%
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
  # add the geog codes, 
  merge(y=code_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype"))  %>% 
  filter(!is.na(rate)) %>%
  select(-ci, -spatial.scale, -spatial.unit)

# combine
all_data <- rbind(underemp_all,
                  underemp_sex_with_totals,
                  underemp_age_with_totals) %>%
  mutate(ind_id = 30033,
         numerator = as.numeric(NA), # insert column where numerator would ordinarily be 
         trend_axis = as.character(year),
         year = as.numeric(year),
         def_period = paste0("Survey year (", trend_axis, ")"))







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
  
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland data 
  pop_grp_data <- all_data %>% 
    filter(!(split_name %in% c("None", "Deprivation (SIMD)"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year, split_name)
  
  # Save
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
}


# Run function to create final files
prepare_final_files(ind = "underemployment")

# # Run QA reports 
run_qa(type ="main",filename="underemployment", test_file=FALSE)

# # Run Pop group reports 
run_qa(type ="popgrp",filename="underemployment", test_file=FALSE)

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







