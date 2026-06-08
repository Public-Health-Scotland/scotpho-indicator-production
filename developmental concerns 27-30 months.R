# ScotPHO indicators: DEVELOPMENTAL CONCERNS AT 27-30 MONTHS

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function
source("./functions/data cleaning functions/fix_fin_year.R") #fix financial year function
library(readxl) #for reading in xlsx files

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

# read in the data
dev_concerns <- read_xlsx(file.path(profiles_data_folder, "Received Data/Developmental concerns/IR2026-00349.xlsx")) 

# tidy up col names
dev_concerns <- dev_concerns |> 
  janitor::clean_names() |> #sets all column names to lowercase
  rename(datazone = datazone2011,
         fin_year = finyr_eligible) #renaming variables to datazone and fin_year

#removes all other geographies apart from datazone (needed if received data contains Scotland and hb data)
dev_concerns <- dev_concerns |> 
  filter(!(is.na(datazone))|hb_residence_desc=="Unknown")
  
dev_concerns <- dev_concerns |> 
  fix_fin_year("fin_year", "4") |> #fix fin year
  group_by(year, datazone) |> 
  summarise(numerator = sum(concerns), denominator = sum(reviews), .groups = "drop")

saveRDS(dev_concerns, file.path(profiles_data_folder, 'Prepared Data/dev_concerns_raw.rds')) 

###############################################.
## Part 2 - calling analysis functions ----
###############################################.

main_analysis(filename = "dev_concerns", geography = "datazone11", measure = "percent", 
              yearstart = 2013, yearend = 2024, time_agg = 3, year_type = "financial",
              ind_id = 13048)

deprivation_analysis(filename = "dev_concerns", yearstart = 2013, yearend = 2024, 
                     time_agg = 3, year_type = "financial", measure = "percent", 
                     ind_id = 13048)

#End
