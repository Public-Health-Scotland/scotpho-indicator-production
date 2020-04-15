################################################################################
################################################################################
#########                                                              #########
#####            Population within 500 metres of a derelict site           #####
#########                                                              #########
################################################################################
################################################################################


####### in development

## This script prepares SG Population within 500 metres of a derelict site indicator.
## Data formost recent update requested from SG SDVLS team.  
## Data are now published on statistics.scot.gov.uk:
## https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fproximity-to-derelict-site
## NOTE: does not currently include pop numbers to allow profile indicator production.

################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
library(tidyverse) # all kinds of stuff 
#library(stringr) # for strings

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/" 


################################################################################
#####  Part 1)  format prepared data --------------------------------
################################################################################

# open received data
df_received <- read.csv(paste0(data_folder,"/Received Data/20901 - 500m derelict site.csv")) %>% 
  as_tibble()

names(df_received) <- tolower(names(df_received)) # make col names lower case

df_received$datazone <- as.character(df_received$datazone)

saveRDS(df_received, paste0(data_folder,"Prepared Data/derelict_site_raw.rds"))

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("./1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "derelict_site", geography = "datazone2011",
              measure = "percent", yearstart = 2016, yearend = 2017, adp = FALSE,
              time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "derelict_site", measure = "percent", 
               time_agg = 1, ind_id = "20901", year_type = "calendar")


