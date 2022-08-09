###  notes
# ScotPHO indicators
# 20604: Working age adults with low or no educational qualifications
# data queried directly from statistics.gov


# install the opendata scotland r package which communicates with the statistics.gov wesbite api
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")

source("1.indicator_analysis.R") 
library(opendatascot)

ods_all_datasets() # to see available datasets on statistic.gov.scot
ods_structure("adults-16-64-years-with-low-or-no-qualifications") # see structure and variables of this dataset
# see dates available so that the last year can be included by adjusting the date range below

# A small number with unknown qualifications are excluded from this calculation of this indicator. 
# denominator therefore isnt the total population(16-64 yr old) but total population minus this excluded numbers and therefore cant be retrieved from population lookup files
# the rates and numerator are published and so the denominator can be calculated
# this calcluation is neccessary so the council area figures can be aggregated up to health board level


date_range = (as.character(2004:2019)) # adjust to include most recent year



working_age_adults = ods_dataset("adults-16-64-years-with-low-or-no-qualifications", 
                                 geography = "la",age = "16-64",gender="all",refPeriod = date_range) %>% 
  setNames(tolower(names(.))) %>%
  rename("ca"= "refarea","year"="refperiod") %>%
  select(c("ca","year","value","measuretype"))  %>% 
  
  # counts and percentage rates for council areas exist as different rows hence the pivot wider below
  pivot_wider(names_from = measuretype, values_from = value) %>% 
  
  rename("numerator"="count","rate"="ratio") %>% 
  mutate(across(c("numerator","rate","year"), ~ as.numeric(.))) %>%  
  
  mutate("denominator"= numerator/(rate/100)) %>%
  select(c(ca,year,numerator,denominator))

saveRDS(working_age_adults, file=paste0(data_folder, 'Prepared Data/working_age_adults_raw.rds')) 

# runninng analysis functions 
analyze_first(filename = "working_age_adults", geography = "council", measure = "percent", yearstart = 2004,
              yearend = 2019, time_agg = 1)

analyze_second(filename = "working_age_adults", measure = "percent", time_agg = 1,
               ind_id = 20604, year_type = "calendar") 



################################################################################
# retrieve old shiny file and run QA to compare to old file to new file 

# temp_working_age_adults_shiny <- read_csv(paste0(data_folder, 'Shiny Data/All Data for Shiny.csv')) %>%
#   filter(INDICATOR_ID == 20604) %>% replace(is.na(.), 0)%>% # some columns have na values 
#   mutate("denominator"=0) %>% # denominator column is not in file
#   select(c("GEOGRAPHY_CODE","YEAR","NUMERATOR","denominator","MEASURE","LOWCI","UPCI","INDICATOR_ID","trend_axis","DEF_PERIOD" )) %>% 
#   rename("code"="GEOGRAPHY_CODE","year"="YEAR","numerator"="NUMERATOR","rate"="MEASURE","lowci"="LOWCI","upci"="UPCI","ind_id"="INDICATOR_ID","def_period"="DEF_PERIOD")
# 
# # saving temporary file to the shiny folder (delete from folder later)
# write_csv(temp_working_age_adults_shiny, file=paste0(data_folder, 'Shiny Data/temp_working_age_adults_shiny.csv'))
# 
# old_shiny_file = read_csv(paste0(data_folder, "Shiny Data/temp_working_age_adults_shiny.csv")) # just making sure temp shiny file is created correctly 
# 
# # run quality analysis function
# run_qa(filename = "working_age_adults", old_file = "temp_working_age_adults") 

