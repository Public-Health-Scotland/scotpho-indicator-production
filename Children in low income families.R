################################################################################
################################################################################
#########                                                              #########
#####           Children in low income families indicator prep             #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses HMRC data on the Children in Low Income Families measure

## The latest data (August 2016) is available here:
#    https://www.gov.uk/government/statistics/personal-tax-credits-children-in-low-income-families-local-measure-2016-snapshot-as-at-31-august-2016

## Once the datazone level data for Scotland is downloaded, there is some work 
#  to be done to unround the estimates.  This is done before importing to R.


################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
#install.packages("tidyverse")
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/"

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
child_low <- read.csv(paste0(data_folder, "Received Data/children_low_income_raw.csv"))

saveRDS(child_low, file=paste0(data_folder, "Prepared Data/children_low_income_raw.rds"))
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "children_low_income", geography = "datazone11", 
              measure = "percent", yearstart = 2013, yearend = 2016, time_agg = 1)

# before the second phase of analysis, the data needs to be rounded to be in line with the HMRC 
# published data. Open the intermediate file and export to csv - update in the csv - then import 
# and save as rds for next phase.  I also changed the name of the formatted file so as not to 
# overwrite it with the new one.  
child_low_temp <- readRDS("X:/ScotPHO Profiles/Data/Temporary/children_low_income_formatted.rds")

write.csv (child_low_temp, "X:/ScotPHO Profiles/Data/Temporary/children_low_income_formatted_temp.csv")

child_low_formatted <- read.csv(paste0(data_folder, "Temporary/children_low_income_formatted.csv"))

saveRDS(child_low_formatted, file=paste0(data_folder, "Temporary/children_low_income_formatted.rds"))

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "children_low_income", measure = "percent", time_agg = 1,
              ind_id = 13027, year_type = "calendar", profile = "cy", min_opt = 1471290)



