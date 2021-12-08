# ScotPHO indicators: DEVELOPMENTAL CONVERNS AT 27-30 MONTHS

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

library(janitor)

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
dev_concerns <- read_csv(paste0(data_folder, "Received Data/IR2021-00903_development27months.csv")) %>%
  setNames(tolower(names(.))) %>%
  clean_names() %>%
  mutate(datazone = as.factor(datazone2011))
#removes all other geographies apart from datazone(needed if received data contains Scotland and hb data)
dev_concerns <- dev_concerns %>%
  filter(!(is.na(datazone))|hb_residence_desc=="Unknown")
  

dev_concerns <- dev_concerns %>%
  mutate( #creates year field based on lenght of fin_year
    year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                   TRUE ~ paste0("20",substr(fin_year,1,2)))) %>%
  group_by(year, datazone) %>%
  summarise(numerator = sum(no_with_concern), denominator = sum(no_reviews)) %>%
  ungroup()



saveRDS(dev_concerns, file=paste0(data_folder, 'Prepared Data/dev_concerns_raw.rds')) 

###############################################.
## Part 2 - calling analysis functions ----
###############################################.

analyze_first(filename = "dev_concerns", geography = "datazone11", hscp = T, measure = "percent", 
              yearstart = 2013, yearend = 2020, time_agg = 3)

analyze_second(filename = "dev_concerns", measure = "percent", time_agg = 3, 
               ind_id = 13048, year_type = "financial")
