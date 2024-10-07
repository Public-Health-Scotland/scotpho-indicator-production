#ScotPHO Indicators: Recorded Crime rate

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

#Note - add link to FOI page on Police Scotland website if the indicator continues to be udpated this way.

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #normal indicator functions
source("2.deprivation_analysis.R") #deprivation function

filepath <- paste0(data_folder, "Received Data/Crime data/data/") #general crime data folder

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#read in historic data
crime_historic <- readRDS(paste0(filepath, "crime_data_historic_DO_NOT_DELETE.rds"))


recorded_crime <- read_excel(filepath, sheet = 2) |>
  select(-c(2:3,6:8)) |>  #drop unnecessary variables e.g. crime type
  rename(year = Calendar_Year, #rename for analysis functions
         datazone = dzone_code) |> 
  filter(datazone != "NULL") |> #drop NULL datazones - mostly driving offences
  group_by(year, datazone) |> #aggregate the months to get whole year totals
  summarise(numerator = sum(`Number of Recorded Crimes`))

saveRDS(crime, file=paste0(data_folder, 'Prepared Data/recorded_crime_raw.rds'))

saveRDS(crime, file=paste0(data_folder, 'Prepared Data/recorded_crime_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "recorded_crime", geography = "datazone11", adp = TRUE, hscp = TRUE, measure = "crude",
              yearstart = 2020, yearend = 2020, pop = 'DZ11_pop_allages', time_agg = 1)

analyze_second(filename = "recorded_crime", measure = "crude", time_agg = 1, 
               crude_rate = 1000, ind_id = 99104, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="recorded_crime", measure="crude",  crude_rate = 1000,
                    time_agg=1, pop = "depr_pop_allages", 
                    yearstart= 2020, yearend=2020, 
                    year_type = "calendar", ind_id = 99104)
