# ScotPHO indicators: DEVELOPMENTAL CONCERNS AT 27-30 MONTHS

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

# read in the data
dev_concerns <- read_excel(paste0(data_folder, "Received Data/Developmental concerns/IR2024-00253_development27months.xlsx")) 

# tidy up col names
dev_concerns <- dev_concerns |> 
  setNames(tolower(names(dev_concerns))) |> #sets all column names to lowercase
  clean_names() |> 
  rename(datazone = datazone2011,
         fin_year = finyr_eligible) #renaming variables to datazone and fin_year


#removes all other geographies apart from datazone (needed if received data contains Scotland and hb data)
dev_concerns <- dev_concerns |> 
  filter(!(is.na(datazone))|hb_residence_desc=="Unknown")
  
dev_concerns <- dev_concerns |> 
  mutate( #creates year field based on length of fin_year
    year=substr(fin_year, start=1, stop=4)) |> 
  group_by(year, datazone) |> 
  summarise(numerator = sum(concerns), denominator = sum(reviews)) |> 
  ungroup()

saveRDS(dev_concerns, file=paste0(data_folder, 'Prepared Data/dev_concerns_raw.rds')) 

###############################################.
## Part 2 - calling analysis functions ----
###############################################.

analyze_first(filename = "dev_concerns", geography = "datazone11", hscp = T, measure = "percent", 
              yearstart = 2013, yearend = 2022, time_agg = 3)

analyze_second(filename = "dev_concerns", measure = "percent", time_agg = 3, 
               ind_id = 13048, year_type = "financial")
