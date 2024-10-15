#ScotPHO Indicators: Recorded Crime rate

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

#Note - add link to FOI page on Police Scotland website if the indicator continues to be udpated this way.

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #normal indicator functions
source("2.deprivation_analysis.R") #deprivation function

library(lubridate) #convert to date format
library(phsmethods) #for extracting financial year from calendar year

filepath <- paste0(data_folder, "Received Data/Crime data/data/") #general crime data folder


##############################################.
## Part 1 - Prepare Basefile
###############################################.

#Read in and tidy up data for most recent calendar year 
rec_crime_2018 <- read_excel(paste0(filepath, "recorded-2018.xlsx"), sheet = 2) |>
  clean_names() |> #simplify col names
  select(-c(2:3,6:8)) |>  #drop unnecessary variables e.g. crime type
  rename(datazone = dzone_code) |> #rename for analysis functions
  filter(datazone != "NULL") |>  #drop NULL datazones - mostly driving offences
  mutate(rec_date = my(paste(calendar_month, calendar_year)), #convert month and year columns to date format
         fin_year = extract_fin_year(rec_date)) #extract financial year from date
  
#Extract Jan-Mar data to use
crime_janmar_18 <- rec_crime_2018 |> 
  filter(rec_date <= '2018-03-31')

#Read in current fy data from previous calendar year (Apr-Dec)
crime_aprdec_17 <- readRDS(paste0(filepath, 'recorded_crime_next_fy_DO_NOT_DELETE.rds'))

#Combine both calendar years to get current financial year
crime_17_18 <- rbind(crime_janmar_18, crime_aprdec_17)

#Tidy up current financial year
crime_17_18 <- crime_17_18 |> 
  group_by(datazone, fin_year) |> #aggregate the months to get whole year totals by dz
  summarise(numerator = sum(number_of_recorded_crimes)) |> 
  rename(year = fin_year) #rename for analysis functions


#Read in historic data and combine with new data
crime_historic <- readRDS(paste0(filepath, "recorded_crime_historic_data_DO_NOT_DELETE.rds"))

recorded_crime <- rbind(crime_historic, crime_16_17)


#Save new historic data file
saveRDS(recorded_crime, file = paste0(filepath, 'recorded_crime_historic_data_DO_NOT_DELETE.rds'))

#Save prepared data for analysis functions
saveRDS(crime_11_12, file=paste0(data_folder, 'Prepared Data/recorded_crime_raw.rds'))

saveRDS(crime_11_12, file=paste0(data_folder, 'Prepared Data/recorded_crime_depr_raw.rds'))


#Extract Apr-Dec data and save for next year 
crime_aprdec_18 <- rec_crime_2018 |> 
  filter(rec_date > '2018-03-31')


saveRDS(crime_aprdec_18, file=paste0(filepath, 'recorded_crime_next_fy_DO_NOT_DELETE.rds'))



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
