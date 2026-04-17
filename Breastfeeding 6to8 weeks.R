# ScotPHO indicators: 
#Babies exclusively breastfed at 6-8 weeks (21004)
#Babies exclusively or partially breastfed at 6-8 weeks (Overall breastfeeding) (21007)

# Originally ScotPHO had single indicator of exclusive breastfeeding but overall breastfeeding indicator added April 2026 
# Note the two indicators are not mutually exclusive, overall breast feeding indicator includes exclusive breastfed babies plus those
# who are mixed breast and formula fed. 

#Data are requested from phs.childhealthstats@phs.scot

## Part 1 - Formatting raw data ready for analysis functions 
## Part 2 - Calling the analysis functions 
## Part 3 - Applying suppression

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefiles ----
###############################################.
breastfed <- read_csv(paste0(profiles_data_folder, "/Received Data/Babies exclusively breastfed/IR2026-00223.csv")) |> 
  clean_names() |> 
  mutate(year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                        TRUE ~ paste0("20",substr(fin_year,1,2)))) |>   #convert financial year to correct format
  pivot_longer(cols = c("excbf_6to8wk", "overall_6to8wk"), names_to = "indicator", values_to = "numerator") |> #pivoting both indicators into 1 col to avoid repeating code
  group_by(datazone2011, year, indicator) |> #aggregate up males and females into 1 count per dz per year
  summarise(numerator = sum(numerator), denominator = sum(tot_6to8wk), .groups = "drop") |> 
  filter(year >= 2010) #starting time series at 2010 when data are complete

#split the data into 2 indicators
excl_breastfed <- breastfed |> 
  filter(indicator == "excbf_6to8wk") |> 
  select(-indicator)

overall_breastfed <- breastfed |> 
  filter(indicator == "overall_6to8wk") |> 
  select(-indicator)

saveRDS(excl_breastfed, file.path(profiles_data_folder, '/Prepared Data/excl_breastfed_raw.rds')) 
saveRDS(overall_breastfed, file.path(profiles_data_folder, "/Prepared Data/overall_breastfed_raw.rds"))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Babies exclusively breastfed
main_analysis(filename = "excl_breastfed", geography = "datazone11", measure = "percent", 
              yearstart = 2010, yearend = 2024, time_agg = 3, ind_id = 21004, 
              year_type = "financial")

#Babies overall breastfed (ie totally or partially breastfed)
main_analysis(filename = "overall_breastfed", geography = "datazone11", measure = "percent",
              yearstart = 2010, yearend = 2024, time_agg = 3, ind_id = 21007,
              year_type = "financial")


###############################################.
## Part 3 - Suppressions and Exclusions ----
###############################################.

#Read the data back in
excl_breastfed <- readRDS(file.path(profiles_data_folder, "/Data to be checked/excl_breastfed_shiny.rds"))
overall_breastfed <- readRDS(file.path(profiles_data_folder, "/Data to be checked/overall_breastfed_shiny.rds"))

#Exclude any rows where denominator 5 or under for an area
#Previously this was done between first and second analysis functions but because these are now combined
#there is no intermediate file where datazones have been aggregated to higher geographies but the denominator column is still present
#The code below
# - recalculates the denominator using rate and numerator
# - excludes denominators of 5 or under
# - drops the denominator column and re-saves

excl_breastfed <- excl_breastfed |> 
  mutate(denominator = numerator/(rate/100)) |> #calc denom. rate is divided by 100 to get proportion e.g. 33% -> 0.33
  filter(denominator >= 6) |> #exclude all denominators 5 or under
  select(-denominator)

overall_breastfed <- overall_breastfed |> 
  mutate(denominator = numerator/(rate/100)) |> 
  filter(denominator >= 6) |> 
  select(-denominator)

# Resave both rds and csv files with exclusions
saveRDS(excl_breastfed , file.path(profiles_data_folder, "Data to be checked/excl_breastfed_shiny.rds"))
write_csv(excl_breastfed , file.path(profiles_data_folder, "Data to be checked/excl_breastfed_shiny.csv"))

saveRDS(overall_breastfed , file.path(profiles_data_folder, "Data to be checked/overall_breastfed_shiny.rds"))
write_csv(overall_breastfed , file.path(profiles_data_folder, "Data to be checked/overall_breastfed_shiny.csv"))

##END



