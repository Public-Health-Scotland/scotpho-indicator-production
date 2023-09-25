### Analysts notes ----

# this script prepares data for the following indicators:- 

# 1: driving under the influence offences per 10,000
# 2: drunk and other disorderly behaviour per 10,000

# data is extracted using the opendatascot R package
# To install this package, un comment and run the following 2 lines:

#install.packages("devtools")
#devtools::install_github("datasciencescotland/opendatascot")

# part 1: extract and prepare data
# part 2: run analysis functions

### load dependencies ----
source("1.indicator_analysis.R")
library(opendatascot)


### 1. Read and prepare data -----
data <- opendatascot::ods_dataset("recorded-crime",
                                  geography = "la",
                                  measureType="count",
                                  crimeOrOffence = c("offences-group-6-drunkenness-and-other-disorderly-conduct",
                                                     "offences-group-8-driving-under-the-influence"))

# prepare data to be used in functions 
data <- data %>%
  mutate(year = as.numeric(str_sub(refPeriod, start= 1, end = 4))) %>% # extract year from financial year
  rename(numerator = value, ca = refArea) # rename cols


# create driving under the influence dataframe 
driving_under_inf <- data %>%
  filter(grepl("driving", crimeOrOffence)) %>%
  select(year, ca, numerator)

# create drunkeness and other disorderly behaviour dataframe
drunkeness_and_dis <- data %>%
  filter(grepl("disorderly", crimeOrOffence)) %>%
  select(year, ca, numerator)


# find max year in dataset to add to analyze_first() argument
max_year = max(data$year)

# save files to be used in analysis functions
saveRDS(driving_under_inf, file=paste0(data_folder, 'Prepared Data/driving_under_influence_raw.rds'))
saveRDS(drunkeness_and_dis, file=paste0(data_folder, 'Prepared Data/drunkeness_and_dis_behaviour_raw.rds'))



### 2. run analysis functions ----

# driving under influence
analyze_first(filename = "driving_under_influence", measure = "crude", 
              time_agg = 1, yearstart = 2012, yearend = max_year, 
              geography = "council", pop = "CA_pop_allages", adp = TRUE, hscp = TRUE)


analyze_second(filename = "driving_under_influence", measure = "crude", 
               time_agg = 1, ind_id = "4158", year_type = "financial", 
               pop = "CA_pop_allages", crude_rate = 10000)


# drunk and other disorderly behaviour 
analyze_first(filename = "drunkeness_and_dis_behaviour", measure = "crude", 
              time_agg = 1, yearstart = 2012, yearend = max_year, 
              geography = "council", pop = "CA_pop_allages", adp = TRUE, hscp = TRUE)


analyze_second(filename = "drunkeness_and_dis_behaviour", measure = "crude", 
               time_agg = 1, ind_id = "4157", year_type = "financial", 
               pop = "CA_pop_allages", crude_rate = 10000)


### END