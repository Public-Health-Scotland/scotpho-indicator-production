#ScotPHO Indicators: Recorded Crime rate

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

#Note - add link to FOI page on Police Scotland website if the indicator continues to be updated this way.

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
rec_crime_newest_cal_year <- read_excel(paste0(filepath, "recorded-2012.xlsx"), sheet = 2) |>
  clean_names() |> #simplify col names
  select(-c(2:3,6:8)) |>  #drop unnecessary variables e.g. crime type
  rename(datazone = dzone_code) |> #rename for analysis functions
  mutate(rec_date = my(paste(calendar_month, calendar_year)), #convert month and year columns to date format
         fin_year = extract_fin_year(rec_date)) #extract financial year from date
  
#Extract Jan-Mar data to use
crime_jan_mar<- rec_crime_newest_cal_year |> 
  filter(month(rec_date) <= 3) #filtering on first 3 months of the year


#Read in current fy data from previous calendar year (Apr-Dec)
crime_apr_dec<- readRDS(paste0(filepath, 'recorded_crime_next_fin_year_DO_NOT_DELETE.rds')) 

#Combine both calendar years to get current financial year
crime_fin_year <- rbind(crime_jan_mar, crime_apr_dec)

#Tidy up current financial year
crime_current_fin_year <- crime_fin_year |> 
  group_by(datazone, fin_year) |> #aggregate the months to get whole year totals by dz
  summarise(numerator = sum(number_of_recorded_crimes)) |> 
  rename(year = fin_year) |>  #rename for analysis functions
  mutate(year = substr(year, 1, 4)) |> 
  mutate(year = as.numeric(year))


#Read in historic data and combine with new data
#Final fix to year - remove 2nd year - possibly move to another section
crime_historic <- readRDS(paste0(filepath, "recorded_crime_historic_data_DO_NOT_DELETE.rds"))

recorded_crime <- rbind(crime_historic, crime_current_fin_year) |> 
  mutate(year = substr(year, 1, 4),
         year = as.numeric(year)) 


#Save new historic data file
saveRDS(recorded_crime, file = paste0(filepath, 'recorded_crime_historic_data_DO_NOT_DELETE.rds'))

#Save prepared data for analysis functions
saveRDS(recorded_crime, file=paste0(data_folder, 'Prepared Data/recorded_crime_raw.rds'))

saveRDS(recorded_crime, file=paste0(data_folder, 'Prepared Data/recorded_crime_depr_raw.rds'))


#Extract Apr-Dec data and save for next year 
crime_apr_dec <- rec_crime_newest_cal_year |> 
  filter(month(rec_date) > 3)

saveRDS(crime_apr_dec, file=paste0(filepath, 'recorded_crime_next_fin_year_DO_NOT_DELETE.rds'))


#Save new historic data file
saveRDS(recorded_crime, file = paste0(filepath, 'recorded_crime_historic_data_DO_NOT_DELETE.rds'))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "recorded_crime", geography = "datazone11", adp = TRUE, hscp = TRUE, measure = "crude",
              yearstart = 2011, yearend = 2022, pop = 'DZ11_pop_16to64', time_agg = 1)

analyze_second(filename = "recorded_crime", measure = "crude", time_agg = 1, 
               crude_rate = 10000, ind_id = 99999, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="recorded_crime", measure="crude",  crude_rate = 10000,
                    time_agg=1, pop = "depr_pop_16to64", 
                    yearstart= 2011, yearend=2022, 
                    year_type = "financial", ind_id = 99999)
