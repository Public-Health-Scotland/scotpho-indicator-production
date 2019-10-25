################################################################################
################################################################################
#########                                                              #########
#####                    School meals indicator prep                       #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses SG education data on the free school meals measure

## The latest data (Sept 2019) is available here:
#    https://www2.gov.scot/Topics/Statistics/Browse/School-Education/SchoolMealsDatasets

## NOTE: in development: Part 1 updated, changed parameters in Part 2
## error in analyse first call

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
## read in csv files
# new data
school_meals_new <- read.csv(paste0(data_folder, "Received Data/school_meals_2018_2019.csv")) %>% 
  as_tibble()
# old data for merging
school_meals_old <- read.csv(paste0(data_folder, "Received Data/school_meals_2015_2017.csv")) %>% 
  as_tibble() %>% 
  select(c(year, LA, numerator, denominator)) %>% 
  rename(ca = LA)

###############################################.
## Part 1 - data prep
###############################################.

head(school_meals_new)

# rename vars
names(school_meals_new) <- tolower(names(school_meals_new))# make lower case
school_meals_new <- school_meals_new %>%   rename(areaname = la)



# geog codes 
geog <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) # load file
school_meals_new$areaname <- gsub("&", "and", school_meals_new$areaname) # change & to match lookup
school_meals_new$areaname[school_meals_new$areaname == "Edinburgh City"] <- "City of Edinburgh"
school_meals_new <- full_join(x = school_meals_new, y = geog, by = "areaname") %>% 
  rename(ca = code)

# reorder columns
school_meals_new <- select(school_meals_new, c(1,5,3,4))

# check col names are same across dataframes
names(school_meals_new)== names(school_meals_old)

# merge old and new dataframes
school_meals <- school_meals_old %>% 
  rbind(school_meals_new)

# save rds raw file for use in analysis funtions
saveRDS(school_meals, file=paste0(data_folder, "Prepared Data/school_meals_raw.rds"))

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("X:/ScotPHO Profiles/indicator-production-master/1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "school_meals", geography = "council", 
              measure = "percent", yearstart = 2015, yearend = 2019, 
              time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "school_meals", measure = "percent", 
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
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/school_meals_shiny.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/school_meals_shiny.csv"))




