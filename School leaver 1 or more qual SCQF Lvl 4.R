################################################################################
################################################################################
#########                                                              #########
#####     School leavers with 1 or more qualification at SCQF Level 4      #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses SG education data on the School leaver with 1 or more qualification at SCQF Level 4

## The latest data (Feb 2019) is available here:
#    https://www2.gov.scot/Topics/Statistics/Browse/School-Education/leavedestla

## Data in statistics.scot.gov but not currently in useable state
 # request for additional data sent to SG - keep eye out for future years.  
## Need to add in geog codes for LAs as SG Ed Stats use their own ones.

################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
#install.packages("tidyverse")
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings

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

source("./1.Analysts_space/Vicki PH/ScotPHO/ScotPHO Profile/1.indicator_analysis.R") #Normal indicator functions
source("./1.Analysts_space/Vicki PH/ScotPHO/ScotPHO Profile/2.deprivation_analysis.R") # deprivation function

getwd()
setwd("/PHI_conf/ScotPHO/")
################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
school_leaver <- read.csv(paste0(data_folder, "Received Data/school_leaver_SCQFL4_raw.csv")) %>% 
  as_tibble()


###############################################.
## Part 1 - data prep
###############################################.

head(school_leaver)

# rename vars
names(school_leaver) <- tolower(names(school_leaver)) # make lower case
school_leaver <- school_leaver %>% rename(areaname = la.name,
                                          denominator = number.of.leavers, 
                                          numerator = positive.destination) %>% 
  filter(areaname != "Scotland") # drop Scotland rows

# geog codes 
geog <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) # load file
school_leaver$areaname <- gsub("&", "and", school_leaver$areaname) # change & to match lookup
school_leaver$areaname[school_leaver$areaname == "Edinburgh, City of"] <- "City of Edinburgh"
school_leaver <- full_join(x = school_leaver, y = geog, by = "areaname") %>% 
  rename(ca = code)


# save rds raw file for use in analysis funtions
saveRDS(school_leaver, file=paste0(data_folder, "Prepared Data/school_leaver_destinations_raw.rds"))

################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in data
school_leaver_destinations <- read_rds(paste0(data_folder, "Prepared Data/school_leaver_SCQFL4_raw.rds"))

saveRDS(school_leaver_destinations, file=paste0(data_folder, "Prepared Data/school_leaver_SCQFL4_raw.rds"))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "school_leaver_SCQFL4", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2019, 
              time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "school_leaver_SCQFL4", measure = "percent", 
               time_agg = 1, ind_id = "13010",year_type = "school", qa=FALSE)


# re-check test chart
ggplot(data = final_result %>% filter((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                        & year== max(year)), aes(code, rate) ) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)



#for QA 

saveRDS(final_result, file = paste0(data_folder, "Data to be checked/school_leaver_SCQFL4_shiny.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/school_leaver_SCQFL4_shiny.csv"))



