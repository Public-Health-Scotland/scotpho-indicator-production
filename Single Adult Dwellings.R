# ScotPHO indicators: Single Adult Dwellings

#   Part 1 - Create basefile
#   Part 2 - Call analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") #Deprivation function

library(opendatascot) 

#Install opendatascot - uncomment if not already installed
# devtools::install_github(
#   "ScotGovAnalysis/opendatascot",
#   upgrade = "never",
#   build_vignettes = TRUE
# )

###############################################.
## Part 1 - Create basefile ----
###############################################.

#datazones adding up to have numerators a little lower than LAs when extracted from statistics.gov.scot
#Keep all appropriate geography levels and select "multiple" in geog filters? 

single_adult_dwellings <- opendatascot::ods_get_csv("household-estimates")

sad_2 <- opendatascot::ods_dataset("household-estimates", 
                                   measureType = "count",
                                   geography = "dz",
                                   indicatordwellings = c("with-single-adult-discounts", "total-dwellings"),
                                   refPeriod = as.character(2023:2024))

sad <- sad_2|> 
  filter(str_detect(refArea, "S01"), #keep datazones only
         measureType == "count", #get rid of ratio
         indicatordwellings %in% c("with-single-adult-discounts",  "total-dwellings")) |> #remove other household makeups
  tidyr::pivot_wider(id_cols = c("refArea", "refPeriod"), names_from = indicatordwellings, values_from = value) |> #pivot wider to get numerator and denominator to sep cols
  clean_names() |> #tidy col names 
  rename(datazone = ref_area, #rename for analysis functions
         year = ref_period,
         numerator = with_single_adult_discounts, 
         denominator = total_dwellings)

saveRDS(sad, file.path(profiles_data_folder, "/Prepared Data/single_adult_dwellings_raw.rds"))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

main_analysis(filename = "single_adult_dwellings", geography = "datazone11", measure = "percent",
             yearstart = 2023, yearend = 2024, time_agg = 1, ind_id = 20504, year_type = "calendar")

deprivation_analysis(filename = "single_adult_dwellings", measure = "percent", time_agg = 1, 
                     yearstart = 2024, yearend = 2024, year_type = "calendar", ind_id = 20504)

##END
