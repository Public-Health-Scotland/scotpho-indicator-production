### notes --------------------------------------------------------------

# 4165: Alcohol consumption exceeding weekly limits, females
# 4163: Alcohol consumption exceeding weekly limits, males
# 4164: Alcohol consumption exceeding weekly limits, all

# source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data 

# data is automatically extacted from statistics.gov.scot when this script is run 
# just update the date period below to ensure the latest date period is extracted: -

### 1. load packages/dependencies ----------------------------------------------

source("1.indicator_analysis.R") # sourcing custom indicator functions

library(reactable)
library(janitor) # for cleaning column names


### 2. read in downloaded data -------------------------------------------------

# Set filepath and read in csv file
raw_data_filepath <- paste0(data_folder, "Received Data/Alcohol exceeding weekly limits/Alcohol_exceeding_limits.csv")
raw_data <- read.csv(raw_data_filepath)


# clean the column names 
data <- raw_data |> 
  clean_names() 


### 3. tidy up the data --------------------------------------------------------

#select columns
data <- data |> 
  select(year, geographylevel, location, categories, sex, percent, lower_ci, upper_ci)

  
#filter out both combined sexes to leave male and female
data <- data |> 
  filter(sex == "Male" | sex == "Female") |>
  filter(categories == "Hazardous/Harmful drinker")


#rename some of the existing columns to match final output expected by profiles tool
data <- data |>
  rename(lowci = lower_ci, upci = upper_ci, rate = percent)


### 4. use geography lookup to recode geography names --------------------------

#read in geography lookup named 'codedictionary' from the scotpho lookups folder
geography_lookup <- readRDS(paste0(data_folder, "Lookups/Geography/codedictionary.rds"))


#filter geography lookup for healthboards, council areas and intermediate zones respectively
geography_lookup <- geography_lookup |>
  filter(str_detect(code, "S08|S12|S00"))


#add "NHS" to the beginning of the HB names to match lookup
data_correct <- data |>
  mutate(location_new = case_when(geographylevel == "Health Board" ~ paste("NHS", location), TRUE ~ location))

#recode City of Edinburgh to Edinburgh city to match lookup 
data_correct <- data_correct |>
  mutate(location_new = case_when(
    location_new == "Edinburgh City" ~ "City of Edinburgh",
    TRUE ~ location_new
  ))

#join the new data to the lookup 
data_correct <- data_correct |>
  left_join(geography_lookup, by = c("location_new" = "areaname"))


# Remove unnecessary columns from joining the lookup to the data
data_correct <- data_correct |>
  select(-c(geographylevel, location, categories))

### 5. format dates ------------------------------------------------------------

#create a def_period column 
data_correct <- data_correct |>
  mutate(def_period= paste0("4-year aggregate"," (", year, ")")) %>%
  mutate(numerator = '') %>%
  mutate(trend_axis = year)

#arrange the columns
data_correct <- data_correct %>% 
  select(code, year, sex, numerator, def_period, trend_axis, upci, lowci, rate)

#get a single year from the given range
data_correct <- data_correct |>
  mutate(year = str_sub(year, start = -4))

#make the year variable numeric rather than character
data_correct$year <- as.numeric(data_correct$year)

#change to required year by subtracting 1
data_correct$year = data_correct$year - 1

rows_to_modify <- data_correct$trend_axis == '2017-2021'
data_correct$year[rows_to_modify] <- data_correct$year[rows_to_modify] - 1

### 6. separate male and female data -------------------------------------------

#separate into males and females and create indicator ID col based on tech doc

males <- data_correct |>
  filter(sex == "Male") |>
  mutate(ind_id="4163") |>
  select(-sex)

females <- data_correct |>
  filter(sex == "Female")|>
  mutate(ind_id = "4165") |>
  select(-sex)


### 7. save output -------------------------------------------------------------

# saving the male indicator data
write.csv(males, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-male_shiny.csv"), row.names = FALSE)
saveRDS(males, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-male_shiny.rds"))

# saving the female indicator data
write.csv(females, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-female_shiny.csv"), row.names = FALSE)
saveRDS(females, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-female_shiny.rds"))

### 8. run QA functions --------------------------------------------------------

run_qa(filename = "alcohol-exceeding-weekly-limits-female", check_extras=c())
run_qa(filename = "alcohol-exceeding-weekly-limits-male", check_extras=c())

### END