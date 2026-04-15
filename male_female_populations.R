##### Section 1: Notes -----------------------------------------------------------------
# This script extracts the total number of male and female of all ages from the population to calculate male:female ratio.

#Indicator tibbles generated in this script are 
# * pop_male # percentage of male population
# * pop_female # percentage of female population

##### 2. Packages/Dependancies -----------------------------------------------

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")

##### 3.  Functions ------------------------------------------------------------

source("functions/main_analysis.R")

##### Section 4 : Data imports and cleaning ######################

##### 5.  male:female indicator----------------------------------------

# Read in populations data
allages_pop <- readRDS(paste0(profiles_data_folder, "/Lookups/Population/DZ11_pop_allages_SR.rds")) |>  
  rename(numerator = denominator) 
denom <- readRDS(paste0(profiles_data_folder, "/Lookups/Population/DZ11_pop_allages.rds"))

##total values at geographical levels
total_df <- allages_pop |> 
  group_by(year, code, sex_grp) |> 
  summarise(total_numerator = sum(numerator, na.rm = TRUE), .groups = "drop")

#Pivot to get sex=1 and sex=2 totals in separate columns
second_result <- total_df |> 
  pivot_wider(names_from = sex_grp, values_from = total_numerator, values_fill = 0) |> 
  rename(male = `1`, female = `2`)

# add denominator
pop <- left_join(second_result, denom, by = c("year", "code"))

# save prepared raw data file per indicator
# male pop 
pop |>
  select(year, code, "numerator" = male, denominator) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/male_pop_raw.rds'))

# female pop 
pop |>
  select(year, code, "numerator" = female, denominator) |> 
  saveRDS(file=paste0(profiles_data_folder, '/Prepared Data/female_pop_raw.rds'))

#call main analysis function - male pop
main_analysis(filename = "male_pop",  measure = "percent",
              geography = "multiple",  year_type = "calendar",  ind_id = 40006, 
              time_agg = 1,  yearstart = 2002,   yearend = 2024,
              test_file = TRUE, QA = TRUE)

#call main analysis function - female pop
main_analysis(filename = "female_pop",  measure = "percent",
              geography = "multiple",  year_type = "calendar",  ind_id = 40007, 
              time_agg = 1,  yearstart = 2002,   yearend = 2024,
              test_file = TRUE, QA = TRUE)
