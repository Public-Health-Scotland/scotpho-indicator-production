
# ScotPHO indicators: 
#Babies exclusively breastfed at 6-8 weeks (21004)
#Babies exclusively or partially breastfed at 6-8 weeks (21007)

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 
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
  pivot_longer(cols = c("excbf_6to8wk", "overall_6to8wk"), names_to = "indicator", values_to = "numerator") |> 
  group_by(datazone2011, year, indicator) |> 
  summarise(numerator = sum(numerator), denominator = sum(tot_6to8wk), .groups = "drop") |> 
  filter(year >= 2010) #starting time series at 2010 when data are complete - data are partial for earlier years

excl_breastfed <- breastfed |> 
  filter(indicator == "excbf_6to8wk") |> 
  select(-indicator)

total_breastfed <- breastfed |> 
  filter(indicator == "overall_6to8wk") |> 
  select(-indicator)

saveRDS(excl_breastfed, file.path(profiles_data_folder, '/Prepared Data/excl_breastfed_raw.rds')) 
saveRDS(total_breastfed, file.path(profiles_data_folder, "/Prepared Data/total_breastfed_raw.rds"))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Babies exclusively breastfed
main_analysis(filename = "excl_breastfed", geography = "datazone11", measure = "percent", 
              yearstart = 2010, yearend = 2024, time_agg = 3, ind_id = 21004, 
              year_type = "financial")

#Babies totally or partially breastfed
main_analysis(filename = "total_breastfed", geography = "datazone11", measure = "percent",
              yearstart = 2010, yearend = 2024, time_agg = 3, ind_id = 21007,
              year_type = "financial")


###############################################.
## Part 3 - Suppressions and Exclusions ----
###############################################.

#Read the data back in
excl_breastfed <- readRDS(file.path(profiles_data_folder, "/Data to be checked/excl_breastfed_shiny.rds"))
total_breastfed <- readRDS(file.path(profiles_data_folder, "/Data to be checked/total_breastfed_shiny.rds"))

#Exclude any rows where denominator <= 5 for an area
excl_breastfed <- filter(excl_breastfed, denominator > 5)
total_breastfed <- filter(total_breastfed, denominator > 5)


## Exclusions at this point for geographies where denominator <=5 for an area  
data_indicator <- readRDS(file=paste0(data_folder, "Temporary/breastfed_formatted.rds"))  |> 
  subset(denominator>5)

saveRDS(data_indicator, file=paste0(data_folder, "Temporary/breastfed_formatted.rds"))

analyze_second(filename = "breastfed", measure = "percent", time_agg = 3, 
               ind_id = 21004, year_type = "financial")

#These exclusions need to be applied to exlcude areas with incomplete data submissions.
#If NHS boards & council have incomplete data then all sub geographies should also be excluded.


# Resave both rds and csv files with exclusions
saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/breastfed_shiny.rds"))
write_csv(data_shiny, path = paste0(data_folder, "Data to be checked/breastfed_shiny.csv"))

#qa_function(filename = "breastfed", iz=TRUE)

##END



