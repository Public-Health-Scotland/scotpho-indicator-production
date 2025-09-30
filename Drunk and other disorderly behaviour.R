### Analyst notes ----

# this script prepares data for the indicator:
# drunk and other disorderly behaviour per 10,000 (4157)

# data is extracted using the opendatascot R package
# To install this package, un comment and run the following 2 lines:

# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")

# part 1: extract and prepare data
# part 2: run analysis functions

### load dependencies ----
source("./functions/main_analysis.R")
source("./functions/data cleaning functions/fix_fin_year.R")
library(opendatascot)

### 1. Read and prepare data -----
data <- opendatascot::ods_dataset("recorded-crime",
                                  geography = "la",
                                  measureType="count",
                                  crimeOrOffence = c("offences-group-6-drunkenness-and-other-disorderly-conduct"))

# prepare data to be used in functions 
data <- data |> 
  fix_fin_year("refPeriod", first_year_digits = "4") |>  # extract year from financial year
  rename(numerator = value, ca = refArea) |>  # rename cols
  select(year, ca, numerator) #|> 
  #mutate(numerator = case_when(numerator == "-" ~ NA_character_,
                               #TRUE ~ numerator)) #24/25 data not published by SG due to DQ issue so convert - to NA to break trend for next year

# find max year in dataset to add to analyze_first() argument
max_year = max(data$year)

# save files to be used in analysis functions
saveRDS(data, file.path(profiles_data_folder, '/Prepared Data/drunkeness_and_dis_behaviour_raw.rds'))

### 2. run analysis functions ----


main_analysis(filename = "drunkeness_and_dis_behaviour", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4157, time_agg = 1, yearstart = 2012, 
              yearend = max_year, pop = "CA_pop_allages", crude_rate = 10000)


### END

