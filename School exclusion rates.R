################################################################################
################################################################################
#########                                                              #########
#####                  School exclusion rates indicator prep               #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses SG education data on the school exclusion rates 

## The latest data (Feb 2019) is available here:
#    https://www.gov.scot/publications/school-exclusion-statistics/

## Data in statistics.scot.gov but not currently in useable state
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


source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function



################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
school_exclusion <- read.csv(paste0(data_folder, "Received Data/school_exclusion_raw.csv")) %>% 
  as_tibble()

school_exclusion<- read_excel("/PHI_conf/ScotPHO/1.Analysts_space/Christina/ScotPHO/ScotPHO files/New data/school_exclusion_orig.xlsx")
View(school_exclusion_orig)

###############################################.
## Part 1 - data prep
###############################################.



# geog codes 
geog <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) # load file
school_exclusion$areaname <- gsub("&", "and", school_exclusion$areaname) # change & to match lookup
school_exclusion$areaname[school_exclusion$areaname == "Edinburgh, City of"] <- "City of Edinburgh"
school_exclusion<- full_join(x = school_exclusion, y = geog, by = "areaname") %>% 
  rename(ca = code)

school_exclusion$year <- substr(school_exclusion$year, 1, 4) # truncate year in usual way

school_exclusion <- select(school_exclusion, c(1,5,4,3))

###append
school_exclusion


# save rds raw file for use in analysis funtions
saveRDS(school_exclusion, file=paste0(data_folder, "Prepared Data/school_exclusion_raw.rds"))


###############################################.
## Packages/Filepaths/Functions ----
###############################################.


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "school_exclusion", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2018, 
              time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "school_exclusion", measure = "crude", 
               time_agg = 1, ind_id = "13016",year_type = "school")

# convert zeroes back to NA for supressed data
final_result[final_result == 0] <- NA

# convert numertaor into integar
final_result$numerator <- as.integer(final_result$numerator)

# re-check test chart
ggplot(data = final_result %>% filter((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                      & year== max(year)), aes(code, rate) ) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)


#resave both rds and csv files
final_result <- final_result %>% select(c(code, ind_id, year, numerator, rate, lowci,
                                          upci, def_period, trend_axis))
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/school_leaver_destinations_shiny.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/school_leaver_destinations_shiny.csv"))

