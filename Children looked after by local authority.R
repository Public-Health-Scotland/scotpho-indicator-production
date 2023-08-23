### Indicator name and definition -----------------------------------------------

# This script prepares data for the following indicator:-
# 20503 - Children looked after by local authority

# Full definition:
# Children looked after by the local authority; number and rate per 1,000 children aged 0-17 years.
# based on children looked after as at 31 July when snapshot taken

#   Part 1 - extract data and format to be used in analysis functions
#   Part 2 - Run analysis functions 



### Analyst notes ---------------------------------------------------------------

# Data is extracted using the 'opendatascot' package

# uncomment the 2 lines below to install package if required:-
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")


### Part 1 - extract and prepare data ------------------------------------------

# 1.a load dependencies/functions ----
source("1.indicator_analysis.R") 
library(opendatascot)


#1.b extract data ----
looked_after_children <- opendatascot::ods_dataset("looked-after-children",
                                                   measureType="count",
                                                   residentialStatus = "all",
                                                   geography = "la") 

#1.c format data ----
looked_after_children  %<>%
  select(ca = refArea, year = refPeriod, numerator = value) %>%
  mutate(across(c("year", "numerator"), as.numeric))
  
  
  
#1.d save file to be used in analysis functions ------
saveRDS(looked_after_children, file=paste0(data_folder, 'Prepared Data/looked_after_raw.rds'))



### Part 2 - Run analysis functions  ---------------------------------
analyze_first(filename = "looked_after", geography = "council", pop = "CA_pop_under18",
              measure = "crude", yearstart = 2009, yearend = 2022, time_agg = 1)


analyze_second(filename = "looked_after", measure = "crude", time_agg = 1,
               ind_id = 20503, year_type = "July snapshot", crude_rate = 1000)

