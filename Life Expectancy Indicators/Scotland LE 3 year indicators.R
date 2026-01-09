#install.packages("PHEindicatormethods")


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
source("functions/deprivation_analysis.R") # source functions & libraries to run script
library(readxl) # additional package since LE data being sourced from Excel file until SG open data platform updated.

###############################################.
## Read in & format LE data ----
###############################################.

# Extracts for Life Expectancy data saved in life expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"

# Scotland level simd decile LE
scotland_decile <- read_excel(sheet="decile",paste0(source_network,"Scotland Life Expectancy by SIMD 2009 to 2024.xlsx")) |>
  mutate(quint_type="sc_decile") |>
  rename(quintile=decile)

# Scotland level simd quintile LE
scotland_quin <- read_excel(sheet="quintile",paste0(source_network,"Scotland Life Expectancy by SIMD 2009 to 2024.xlsx")) |>
  mutate(quint_type="sc_quin") 

#bind decile & quintile LE data

le_data_depr <- bind_rows(scotland_decile,scotland_quin) |>
  mutate(code="S00000001", #add a scotland geography code
         sex=case_when(sex=="Males" ~ 1, sex=="Females" ~ 2, TRUE ~3)) |> 
  rename(rate=le,
         lowci=lci) #rename life expectancy column to rate to work with scotpho functions

# clean up dataframes
rm(scotland_decile,scotland_quin)


###############################################.
## Read in & format population denominator data ----
## required for SII/RII calculation
## not straightforward to use scotpho functions to add population denominators for this specific indicator (which is split by sex but not age)
## also more appropriate to use 3 year population sums than 3 year averages.
###############################################.

# read in the simd population lookup, filter by age group and summarise to get annual populations by sex, simd quintile & decile
population_lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Population", "basefile_deprivation.rds")) |> #new file with PD, HSCP in it (revert to simd_population_lookup once that file has these geogs in)
  filter(year >= 2009, #filter for year 2009 onwards as this is earliest time year used
         quint_type %in% c("sc_decile","sc_quin")) |> #we have data for both decile and quintile
  group_by(year, quint_type, quintile,sex_grp) |>
  summarise(denominator=sum(denominator)) |>
  ungroup() 

# Because SAPE 2024 are not available apply 2023 population as if it was 2024 (2023 is in fact 2022 population reapplied) 
population_lookup2024 <- population_lookup |>
  filter(year==2023) |>
  mutate(year=2024)
# bind assumed 2024 population to main lookup 
population_lookup<- bind_rows(population_lookup,population_lookup2024) 

# convert populations to rolling 3 year totals (2009-2011 onwards) roll_sumr (will right align ie output on last year in series)
var_order <- c("quintile","quint_type","sex_grp")

population_lookup  <-population_lookup |>
  arrange(quint_type, quintile,sex_grp,year)|>
  group_by(across(all_of(setdiff(var_order, "year")))) |>
  mutate(rolling=across(any_of(c("denominator")), ~ RcppRoll::roll_sumr(., 3))) |>
  ungroup()|>
  mutate(year2=paste0(year-2,"-",year)) |>  #label full 3 year time period (required for matching to le data)
  filter(!is.na(denominator)) |>
  select(year2,quint_type,quintile,sex_grp,denominator) |>
  rename(year=year2,
         sex=sex_grp)

###############################################.
## Join population and LE data & generate inequalities data ----
###############################################.


le_data <-left_join(le_data_depr, population_lookup, by = c("year","quintile","sex","quint_type")) |>
  select(code,year,quint_type,quintile,sex,rate,lowci,upci, denominator) |>
  calculate_inequality_measures()



###############################################.
## Finalise file for Profiles tool app & QA report ----
###############################################.

# Note this step is technically not required and has no impact on the calculation of rolling averages or rates
# but is being kept in due to having been used in legacy analysis functions
# It will ensure that any automatated QA's that compares old data file (prepared via old functions) and new data file
# (prepared by this new function) are comparing the correct years against eachother.



# select final columns
le_data <- le_data |>
  mutate(
    numerator = "", #add a numerator column even though its blank, this column expected by app
    trend_axis = year, #simple description of years covered for use in chart labels
    def_period = paste0(year," (3 year rolling average)"), #full time period description for metadata
    year = as.numeric(substr(year,1,4)) + 1)|> # adjusts year column needs to be a single year value which represents midpoint of 3 year average
  select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
            least_rate, par_rr, count))

# IF THIS GOES INTO SHINY APP THEN MIGHT NEED TO SPLIT INTO MALE/FEMALE INDICATOR SEPARATE FILES?


# save the data as both an RDS and CSV file
saveRDS(le_data, paste0(profiles_data_folder, "/Data to be checked/LTMHI_LE_by_depr_scot3yr_ineq.rds"))
write.csv(le_data, paste0(profiles_data_folder, "/Data to be checked/LTMHI_LE_by_depr_scot3yr_ineq.csv"), row.names = FALSE)


run_qa(type = "deprivation", filename="LTMHI_LE_by_depr_scot3yr",test_file=FALSE)
