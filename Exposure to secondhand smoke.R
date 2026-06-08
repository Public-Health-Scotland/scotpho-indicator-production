# ScotPHO indicator:Exposure to secondhand smoke 13037

#   Part 1 - Prepare basefiles
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/data cleaning functions/fix_fin_year.R") #for converting to financial year
source("./functions/data cleaning functions/exclude_geog_codes.R") #for removing intermediate zone data and areas w/ DQ issues
library(readxl) #for reading in xlsx filetype

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
#data from child health team
exposure_smoking <- read_excel(file.path(profiles_data_folder, "Received Data/Exposure to secondhand smoke/IR2026-00350_secondhandsmoke_valid.xlsx"), col_types = "text") |> 
  janitor::clean_names() |>  #set names to lower case
  fix_fin_year("fin_year", "2") #fix fin year

#removes all other geographies apart from datazone(needed if received data contains Scotland and hb data)
exposure_smoking <- exposure_smoking |> 
  filter(!(is.na(datazone2011))) |> 
  select(-ca2019, -hb_residence_desc) |> 
  rename(numerator = passive_smoke_yes, denominator = totvalid_6to8wk) |> 
  mutate(across(c("numerator", "denominator"), as.numeric))
         
saveRDS(exposure_smoking, file.path(profiles_data_folder, "Prepared Data/exposure_smoking_raw.rds"))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "exposure_smoking", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2024, time_agg = 3, ind_id = 13037, year_type = "financial")

#Exclude intermediate zones for all years due to small numbers. Remove Highland Council for years prior to 2007 as
#system was not properly in place yet and data could cause confusion
exclude_geog_codes(filename = "exposure_smoking", iz = TRUE, codes = c("S12000017"), codes_years = 2002:2006)
  
##END