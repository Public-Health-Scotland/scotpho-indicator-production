# INEQUALITIES DATA : HEALTHY LIFE EXPECTANCY (USING REVISED 2025 ONS/NRS METHODOLOGY)
# Generates SIMD Quintile Split (could be updated to include decile data in future release)

#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE SIMD deprivation splits can only be generated at Scotland level since estimates of healthy life by ca/nhs board split by sex and deprivation are not robust enough to produce reliable estimates for all areas.

# HLE data published annually by NRS/ONS
# Data not yet published on SG open data platform so excel tables sourced from 
# https://www.nrscotland.gov.uk/publications/healthy-life-expectancy-2022-2024/
# excel tables saved in scotpho network drive and read in below

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
source("functions/deprivation_analysis.R") # source functions & libraries to run script
library(readxl) # additional package since LE data being sourced from Excel file until SG open data platform updated.


# Extracts for Healthy Life Expectancy data saved in left expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"

###############################################.
## Read in & format HLE by SIMD quintile data ----
###############################################.

nrs_hle_simd <- read_excel((paste0(source_network,"healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 5",skip = 3) |>
  clean_names() |>
  mutate(quintile=as.character(area_code))

# Read Scotland in and filter the scotland overall HLE figures which will  insert rows which include
#scotland data
nrs_hle_scot <- read_excel((paste0(source_network,"healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 1",skip = 3) |>
  clean_names() |>
  filter(area_name =="Scotland" & period != "2013 to 2015") |>
  mutate(quintile="Total")
  
#add the quintile specific and total HLE files together
hle_depr_data <-rbind(nrs_hle_simd,nrs_hle_scot) |>
  mutate(quint_type = "sc_quin") |>
  filter(age_group =="<1") |>
  mutate(code =  "S00000001",
         trend_axis =paste0(substr(period,1,4),"-",substr(period,9,12)),
         def_period = paste(trend_axis, "(3 year aggregate)"),
         year = as.numeric(substr(trend_axis,1,4))+1, # year for LE is mid-point of aggregate years (this helps line up data series when comparing le & hle which aren't always same periods))
         split_value=sex, # required for pop group file
         numerator = NA,
         ind_id = case_when(sex == "Females" ~ 99101,
                                sex == "Males" ~ 99102),
         sex_grp=case_when(sex == "Females" ~ 2,
                            sex == "Males" ~ 1)) |>  #required by shiny app but is null for HLE
  rename("rate" = "healthy_life_expectancy",
         "lowci" = "lower_95_percent_confidence_interval",
         "upci" = "upper_95_percent_confidence_interval") |>
  select(-age_group,-area_type, -area_code, -area_name, -period, -sex)

rm(nrs_hle_scot, nrs_hle_simd)  
  
###############################################.
## Read in & format population denominator data ----
## required for SII/RII calculation
## not straightforward to use scotpho functions to add population denominators for this specific indicator (which is split by sex but not age)
## also more appropriate to use 3 year population sums than 3 year averages.
###############################################.

# read in the simd population lookup, filter by age group and summarise to get annual populations by sex, simd quintile & decile
population_lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Population", "basefile_deprivation.rds")) |> #new file with PD, HSCP in it (revert to simd_population_lookup once that file has these geogs in)
  filter(year >= 2014, #filter for year 2009 onwards as this is earliest time year used
         quint_type == "sc_quin") |> #we only have data for quintile
  group_by(year, quint_type, quintile,sex_grp) |>
  summarise(denominator=sum(denominator)) |>
  ungroup() 

# Because SAPE 2024 are not available apply 2023 population as if it was 2024 (2023 is in fact 2022 population reapplied) 
population_lookup2024 <- population_lookup |>
  filter(year==2023) |>
  mutate(year=2024)
# bind assumed 2024 population to main lookup 
population_lookup<- bind_rows(population_lookup,population_lookup2024) 

# convert populations to rolling 3 year totals (2013-2015 onwards) roll_sumr (will right align ie output on last year in series)
var_order <- c("quintile","quint_type","sex_grp")

population_lookup  <-population_lookup |>
  filter(year >= 2014)|> #make sure that time series starts with complete 3 years 
  arrange(quint_type,quintile, sex_grp,year)|>
  group_by(across(all_of(setdiff(var_order, "year")))) |>
  mutate(across(any_of(c("denominator")), ~ RcppRoll::roll_meanr(., 3, na.rm=TRUE))) |>  #calculate average population over 3 years since LE already calculated/averaged
  ungroup() |>
  mutate(trend_axis=paste0(year-2,"-",year))|>  #3 year time period (required for matching to le data)
  filter(!is.na(denominator)) |>
  select(trend_axis,quint_type,quintile,sex_grp,denominator)
  
rm(population_lookup2024)  

###########################################################################.
## Join population and LE data & generate inequalities data ----
###########################################################################.

hle_data <-left_join(hle_depr_data, population_lookup, by = c("trend_axis","quintile","sex_grp","quint_type"))|>
  rename(sex=sex_grp) |>
  arrange("code", "year", "quint_type", "sex") |>
  select(code,year,trend_axis,quint_type,quintile,sex,rate,lowci,upci, denominator) |>
  calculate_inequality_measures() |>
mutate(
  numerator = "", #add a numerator column even though its blank, this column expected by app
  def_period = paste0(trend_axis," (3 year rolling average)"), #full time period description for metadata
  year = as.numeric(substr(year,1,4)) + 1)|> # adjusts year column needs to be a single year value which represents midpoint of 3 year average
  select(-c(overall_rate, total_pop, proportion_pop,par_rr, count,most_rate,least_rate))


###########################################################################.
## save distinct male & female indicator files ----
###########################################################################.

hle_data_male <-hle_data |>
  filter(sex==1)|>
  select(-sex)|>
  mutate(ind_id=99101)

hle_data_female <-hle_data |>
  filter(sex==2)|>
  select(-sex)|>
  mutate(ind_id=99102)


# Save Shiny data files

# male indicator
write_rds(hle_data_male, file = paste0(profiles_data_folder, "/Data to be checked/healthy_life_expectancy_male_ineq.rds"))
write.csv(hle_data_male, file = paste0(profiles_data_folder, "/Data to be checked/healthy_life_expectancy_male_ineq.csv"), row.names = FALSE)

# female indicator
write_rds(hle_data_female, file = paste0(profiles_data_folder, "/Data to be checked/healthy_life_expectancy_female_ineq.rds"))
write.csv(hle_data_female, file = paste0(profiles_data_folder, "/Data to be checked/healthy_life_expectancy_female_ineq.csv"), row.names = FALSE)


  
# This indicator script doesn't use analysis functions but indicator checking report can still be called:
run_qa(filename="healthy_life_expectancy_female", type="deprivation", test_file = FALSE)
run_qa(filename="healthy_life_expectancy_male", type="deprivation", test_file = FALSE)