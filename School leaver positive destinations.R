################################################################################
################################################################################
#########                                                              #########
#####         School leaver positive destinations indicator prep           #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses SG education data on the School leaver positive destinations measure

## The latest data (Feb 2019) is available here:
#    https://www2.gov.scot/Topics/Statistics/Browse/School-Education/leavedestla

## Numerators not provided so have to back calculate
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

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/"


################################################################################
#####                          read in prepared data                       #####
################################################################################
# read in csv
school_leaver <- read.csv(paste0(data_folder, "Received Data/school_leaver_destinations_raw.csv")) %>% 
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

school_leaver$year <- substr(school_leaver$year, 1, 4) # truncate year in usual way

school_leaver <- select(school_leaver, c(1,5,4,3))

# save rds raw file for use in analysis funtions
saveRDS(school_leaver, file=paste0(data_folder, "Prepared Data/school_leaver_destinations_raw.rds"))

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("X:/ScotPHO Profiles/indicator-production-master/1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "school_leaver_destinations", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2017, 
              time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "school_leaver_destinations", measure = "percent", 
               time_agg = 1, ind_id = "13010",year_type = "school")

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
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/school_leaver_destinations_shiny.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/school_leaver_destinations_shiny.csv"))




