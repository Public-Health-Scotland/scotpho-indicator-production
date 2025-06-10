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
library(opendatascot)

### 1. Read and prepare data -----
data <- opendatascot::ods_dataset("recorded-crime",
                                  geography = "la",
                                  measureType="count",
                                  crimeOrOffence = c("offences-group-6-drunkenness-and-other-disorderly-conduct"))

# prepare data to be used in functions 
data <- data |> 
  mutate(year = as.numeric(str_sub(refPeriod, start= 1, end = 4))) |>  # extract year from financial year
  rename(numerator = value, ca = refArea) |>  # rename cols
  select(year, ca, numerator)

# find max year in dataset to add to analyze_first() argument
max_year = max(data$year)

# save files to be used in analysis functions
saveRDS(drunkeness_and_dis, file=paste0(profiles_data_folder, '/Prepared Data/drunkeness_and_dis_behaviour_raw.rds'))

### 2. run analysis functions ----


main_analysis(filename = "drunkeness_and_dis_behaviour", geography = "council", measure = "crude",
              year_type = "financial", ind_id = 4157, time_agg = 1, yearstart = 2012, 
              yearend = max_year, pop = "CA_pop_allages", crude_rate = 10000)


### END

