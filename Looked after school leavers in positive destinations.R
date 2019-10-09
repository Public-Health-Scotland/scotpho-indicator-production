################################################################################
################################################################################
#########                                                              #########
#####       Looked after school leavers in positive destination            #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses Scottish Government data on the number of looked after 
## school leavers achieving 1 qualification at SCQF level 4 or better

## The data at local authority level is not published routinely; this needs to be
## requested from the Children and Families Directorate (email: 
## childrens.statistics@gov.scot). 

################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
#install.packages("tidyverse")
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings

organisation <- "HS" 

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/"

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

## HOW TO USE THESE FUNCTIONS
# FUNCTION ONE: ANALYZE_FIRST
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# geography - what is the base geography of the raw file: council or datazone2011
# adp - To calculate the data for ADP level as well change it to TRUE, default is false.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent),
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.

# FUNCTION TWO: ANALYZE_SECOND
# filename -  Name of the formatted file the function reads without the "_formatted.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
#           percentage with finite population correction factor (perc_pcf)
# time_agg - Aggregation period used expressed in year, e.g. 3 
# ind_id - indicator code/number
# year_type - calendar, financial, school or annual snapshot. This last one should
#           be used like "Month snapshot" e.g. "August snapshot"
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# pop - Only for crude rate cases that need finite population correction factor. Reference population.

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
looked_after_pos_dest<- read.csv(paste0(data_folder, "Received Data/looked_after_positive_dest_raw.csv"))

saveRDS(looked_after_pos_dest, file=paste0(data_folder, "Prepared Data/looked_after_positive_dest_raw.rds"))
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "looked_after_positive_dest", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2017, time_agg = 1)

analyze_second(filename = "looked_after_positive_dest", measure = "percent", time_agg = 1,
              ind_id = 13011, year_type = "school")



