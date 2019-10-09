################################################################################
################################################################################
#########                                                              #########
#####      Children looked after by local authority indicator prep         #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses Scottish Government data on the number of children looked
## after by a local authority.

## The data at local authority level is not published routinely; this needs to be
## requested from the Children and Families Directorate (email: 
## childrens.statistics@gov.scot)

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
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
looked_after<- read.csv(paste0(data_folder, "Received Data/looked_after_raw.csv"))

saveRDS(looked_after, file=paste0(data_folder, "Prepared Data/looked_after_raw.rds"))
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "looked_after", geography = "council", 
              measure = "crude", yearstart = 2011, yearend = 2018, time_agg = 1)

looked_after_temp <- readRDS("X:/ScotPHO Profiles/Data/Temporary/looked_after_formatted.rds")

write.csv (looked_after_temp, "X:/ScotPHO Profiles/Data/Temporary/looked_after_formatted_temp.csv")

looked_after_formatted <- read.csv(paste0(data_folder, "Temporary/looked_after_formatted_temp.csv"))

saveRDS(looked_after_formatted, file=paste0(data_folder, "Temporary/looked_after_formatted.rds"))

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "looked_after", measure = "crude", time_agg = 1,
              ind_id = 13038, year_type = "July snapshot", crude_rate = 1000)



