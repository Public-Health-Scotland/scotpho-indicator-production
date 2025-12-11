
##################### Section 1: Set up #########################

# load 
library(readr)
library(janitor)
library(tidyverse)
library(readxl)
library(scales)
library(openxlsx)
library(ggplot2)
library(phsstyles)
library(tidylog)
library(stringr)
library(purrr)


# Set file paths for folders
fp_cpp <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/"

# All areas lookup
lookup <- read_rds("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DataZone11_All_Geographies_Lookup.rds")


# Long lookup with all codes of interest
long_lookup <- lookup %>% 
  select(datazone2011,intzone2011, ca2019,hscp2019,hb2019,hscp_locality) 


# Set rounding function
rounding <- function(x){
  case_when(x < 1 ~ round_half_up(x, 2),
            between(x, 1, 1000) ~ round_half_up(x, 1),
            x > 1000 ~ round_half_up(x, 0))
}

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")


#################### Section 2: Data imports and cleaning ######################

# Read in populations data 
# popdat <- readRDS(find_latest_pop_file(pattern_type = "IntZone2011"))

allages_pop <- readRDS(paste0("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DZ11_pop_allages_SR.rds")) |>  
  rename(numerator = denominator) 

##total values at geographical levels
total_df <- allages_pop %>%
  group_by(year, code, sex_grp) %>%
  summarise(total_numerator = sum(numerator, na.rm = TRUE), .groups = "drop")

#Pivot to get sex=1 and sex=2 totals in separate columns
second_result <- total_df %>%
  pivot_wider(names_from = sex_grp, values_from = total_numerator, values_fill = 0) %>%
  rename(male = `1`, female = `2`)

##add other columns
final_agg <- second_result |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed

# Gender ratio female to male
st_pop_sex <- final_agg %>% 
  mutate(ratio = round_half_up(`male`/female, 2)) %>% 
  mutate(ratio = paste0("1:", as.character(ratio)))

# Get totals across Scotland
pop_scot <- allages_pop %>% 
  group_by(year, sex_grp, age_grp) %>% 
  summarise(pop = sum(numerator)) %>% 
  ungroup() %>% 
  mutate(area_code = "S92000003") %>% select(year,area_code,everything())


#scotland values---------------------
total_pop_scot <- pop_scot %>%
  group_by(year, "code"=area_code, sex_grp) %>%
  summarise(total_numerator = sum(pop, na.rm = TRUE), .groups = "drop")

#Pivot to get sex=1 and sex=2 totals in separate columns
result_scot <- total_pop_scot %>%
  pivot_wider(names_from = sex_grp, values_from = total_numerator, values_fill = 0) %>%
  rename(male = `1`, female = `2`)


#Scotland values aggregation
scot_agg <- final_agg %>%
  group_by(year) %>%
  summarise(
    male = sum(male, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(code = "S92000003")

##add other columns
final_scot_agg <- scot_agg |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed


# Gender ratio female to male for Scotland
scot_pop_sex <- final_scot_agg %>% 
  mutate(ratio = round_half_up(`male`/female, 2)) %>% 
  mutate(ratio = paste0("1:", as.character(ratio)))


##combine scotland values with with other values
combined_df <- bind_rows(st_pop_sex, scot_pop_sex) %>% 
  select(trend_axis,numerator="male",ratio,lowci,upci,ind_id,code,year,def_period)

