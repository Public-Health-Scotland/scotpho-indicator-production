##### Section 1: Notes -----------------------------------------------------------------
# This script extracts the total number of male and female of all ages from the population to calculate male:female ratio.

#Indicator tibbles generated in this script are 
# * st_pop_sex # male:female ratio

##### 2. Packages/Dependancies -----------------------------------------------
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

##### 3.  Functions ------------------------------------------------------------

# Set rounding function
rounding <- function(x){
  case_when(x < 1 ~ round_half_up(x, 2),
            between(x, 1, 1000) ~ round_half_up(x, 1),
            x > 1000 ~ round_half_up(x, 0))
}

##### Section 4 : Data imports and cleaning ######################

# Set file paths for folders (***need to change path***)
fp_cpp <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/"

# All areas lookup (***need to change path***)
lookup <- read_rds("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DataZone11_All_Geographies_Lookup.rds")

# Long lookup with all codes of interest
long_lookup <- lookup %>% 
  select(datazone2011,intzone2011, ca2019,hscp2019,hb2019,hscp_locality) 

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")

##### 5.  male:female indicator----------------------------------------

# Read in populations data (***need to change path***)
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
  mutate(ratio = round_half_up(male/female, 2)) %>% 
  mutate(ratio = paste0("1:", as.character(ratio))) %>% 
  select(trend_axis,numerator="male",ratio,lowci,upci,ind_id,code,year,def_period)

###########################  save data to suggested .rds and .csv files ##################
#Including both rds and csv file for now
saveRDS(st_pop_sex, file = paste0(data_folder, "Data to be checked/st_pop_sex_shiny.rds"))
write_csv(st_pop_sex, file = paste0(data_folder, "Data to be checked/st_pop_sex_shiny.csv"))

