### notes --------------------------------------------------------------

# 4165: Alcohol consumption exceeding weekly limits, females
# 4163: Alcohol consumption exceeding weekly limits, males
# 4164: Alcohol consumption exceeding weekly limits, all

# source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data 

# data is automatically extacted from statistics.gov.scot when this script is run 
# just update the date period below to ensure the latest date period is extracted: -

### 1. load packages/dependencies ----------------------------------------------

install.packages("reactable")
library(reactable)

## 1.a load R script from this project which contains :-
# custom functions
# filepaths to scotpho folders
# packages that are commonly used
source("1.indicator_analysis.R") # sourcing custom indicator functions



## 1.b. load any other packages not included in the file above that you intend to use functions from 
library(janitor) # for cleaning column names



### 2. read in downloaded data -------------------------------------------------

# In the quotation marks, add the rest of the file-path to where you saved the data
raw_data_filepath <- paste0(data_folder, "Received Data/Alcohol exceeding weekly limits/Alcohol_exceeding_limits.csv")

# Then run this line of code to read the csv file in
# You will now see it saved in your environment on the right hand side - if you click on it you can view the data
raw_data <- read.csv(raw_data_filepath)


# clean the column names 
data <- raw_data |> 
  clean_names() 


### 3. select columns  ---------------------------------------------------------
data <- data |>
  select(year, geographylevel, location, categories, sex, percent, lower_ci, upper_ci)


### 3. filter the sex column --------------------------------------------------
# we only want males and females 
data <- data |>
  filter(sex == "Male" | sex == "Female") |>
  filter(categories == "Hazardous/Harmful drinker")


       
### 5. rename some of the existing columns to match what is required in the final output 
data <- data |>
  rename(lowci = lower_ci, upci = upper_ci, rate = percent)


### 6. read in geography lookup named 'codedictionary' from the scotpho lookups folder ------------
geography_lookup <- readRDS(paste0(data_folder, "Lookups/Geography/codedictionary.rds"))

  
  # this lookup contains geography codes for multiple different geography levels
  # since our data only includes health boards, local authorities and scotland level, we can filter out a lot from here
  # this is required because there are some geographies that have the same name but refer to a different geography level
  # i.e. 'Glasgow City' is the name of a local authority (LA), a health and social care partnership (HSCP) and an alcohol and drug partnershop (ADP)
  # each of these will have a different geography code, so we need to ensure we are matching to the correct one
  # HINT: we can usually tell which geography code refers to which geography level by it's first few digits which will come useful for this step
  # ANOTHER HINT: Read up about the grepl() function.... ;)
  geography_lookup <- geography_lookup |>
  filter(str_detect(code, "S08|S12|S00"))



# looking back at our indicator data, there's an issue with the health boards - they don't have NHS on front of their names
# this means if we try get the code using the name, it's not going to work properly. Look at what happens here:
data_wrong <- data |>
  left_join(geography_lookup, by = c("location" = "areaname"))

# solution: paste NHS on the beginning health boards...
data_correct <- data |>
  mutate(location_new = case_when(geographylevel == "Health Board" ~ paste("NHS", location), TRUE ~ location))

data_correct <- data_correct |>
  mutate(location_new = case_when(
    location_new == "Edinburgh City" ~ "City of Edinburgh",
    TRUE ~ location_new
  ))

# now try join the codes again 
data_correct <- data_correct |>
  left_join(geography_lookup, by = c("location_new" = "areaname"))


# we can now get rid of the columns geographylevel and location as the final file should only have the geography code.
data_correct <- data_correct |>
  select(-c(geographylevel, location, categories))


# create a def_period column 
# hint: look a the old data file for your indicator that is in the 'Shiny data' folder to see what this needs to look like 
# code you have used elsewhere already in this script should help you achieve this...?

data_correct <- data_correct |>
  mutate(def_period= paste0("4-year aggregate"," (", year, ")")) %>%
mutate(numerator = '') %>%
mutate(trend_axis = year)

#arrange
data_correct <- data_correct %>% 
  select(code, year, sex, numerator, def_period, trend_axis, upci, lowci, rate)

# finally, we need to create a 'year' column. Unlike some of our other year columns (trend axis, def_period) which are strings
# we want this to be in a numeric format. This column won't be displayed on the profiles tool dashboard itself, but will be used for filtering in the background 
# for instance, in the summary tab of the tool, we use this column to filter on the latest data for each indicator
# this would be hard to do with the other columns, since there wouldn't be an easy way to order the strings


#Get a single year from the given range
data_correct <- data_correct |>
  mutate(year = str_sub(year, start = -4))

#Make the year variable numeric rather than character
data_correct$year <- as.numeric(data_correct$year)

#Change to required year by subtracting 1
data_correct$year = data_correct$year - 1

rows_to_modify <- data_correct$trend_axis == '2017-2021'
data_correct$year[rows_to_modify] <- data_correct$year[rows_to_modify] - 1


# split the data into 2 seperate indicator - one for males and one for females
# then create an ind_id column which is populate with that indicators unique id
# look at the technical document (the excel file containing the metdata for each indicator) to see what the indicator IDs are
males <- data_correct |>
  filter(sex == "Male") |>
  mutate(ind_id="4163") |>
  select(-sex)
  
females <- data_correct |>
  filter(sex == "Female")|>
mutate(ind_id = "4165") |>
  select(-sex)
  
  
  
  # finally, we want to save these files to the 'data to be checked' folder
  # complete the filepaths below to save the data
  # note: the filess NEED to be named exactly the same as they previously were otherwise the data quality checks in the step below might not work properly
  # If you cannot work out what this was, look at the repository on github and find the old R script which will contain the old filename
  # OR try and find the file in the 'Shiny data' folder
  
  # saving the male indicator data
  write.csv(males, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-male_shiny.csv"), row.names = FALSE)
saveRDS(males, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-male_shiny.rds"))

# saving the female indicator data
write.csv(females, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-female_shiny.csv"), row.names = FALSE)
saveRDS(females, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-female_shiny.rds"))


# finally, we can quality assure the data, by checking how it compares to the previous data (i.e. what is currently in the tool)
# and sense checking the confidence intervals, trends etc.
# add part of the filename to the filename argument of this function
# note this should only be the name of the file without _shiny.csv on the end
# for instance, if the filename is mental-wellbeing-female_shiny.csv, the filename you would include below would be menal-wellbeing-female
# run these lines seperately, when you are done QAing the first one you can hit the 'stop' button in the console and run the next line 
run_qa(filename = "alcohol-exceeding-weekly-limits-female", check_extras=c())


run_qa(filename = "alcohol-exceeding-weekly-limits-male", check_extras=c())


# Once you are happy with your changes, commit them and push them to github. 
# Then create a 'pull request' for your branch and request Monica as a reviewer.
# Once your branch has been reviewed Monica will approve your request so you can merge your changes into the master version of the repository on github


### END