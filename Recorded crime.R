################################################################################
################################################################################
#########                                                              #########
#####               Recorded crime profile indicators prep                 #####
#########                                                              #########
################################################################################
################################################################################

############## IN DEVELOPMENT ############## 

### need to add pop look up into function call

## This script prepares SG recorded crime profile indicators:
##      Breach of the Peace
##      Common assault
##      Drug crimes recorded
##      Vandalism
##      Violent crime
##      Attempted murder & serious assault 

## Data are bulk downloaded from statistics.scot.gov.uk:
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Frecorded-crime

## D/l entire dataset as a CSV file and save as:
#     .\Data\Received Data\rec_crime_received

## Function arguments:
#   id = indicator ID 
#   topic = ""


################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/" 


################################################################################
#####  Part 1)  format prepared data --------------------------------
################################################################################

# open received data
df_received <- read.csv(paste0(data_folder,"Received Data/rec_crime_received.csv")) %>% 
  as_tibble()

df_received <- df_received %>% 
  # select only LA data
  filter(substr(FeatureCode, 1, 3) == "S12") %>% 
  # select only counts
  filter(Measurement == "Count") %>% 
  # remove vars not needed
  select(-c(Units, Measurement)) %>% 
  rename(ca = FeatureCode, year = DateCode, 
         numerator = Value, crime_offence = Crime.or.Offence) 

# change Scotland geog code
df_received$ca <- as.character(df_received$ca)
df_received$ca[df_received$ca == "S92000003"] <- "S00000001"

df_received <- df_received %>% 
  arrange(year, ca, numerator)

# extract crime/offence category
df_received$crime_offence <- sub(".*: ", "", df_received$crime_offence)
df_received$crime_offence <- as.factor(df_received$crime_offence)

# filter only crime/offence categories for indicators
df_received <- df_received %>%  
  filter(crime_offence =="Breach of the peace etc."|
         crime_offence =="Common assault"|
         crime_offence =="Drugs"|
         crime_offence =="Vandalism etc."|
         crime_offence =="Non-sexual crimes of violence"|
         crime_offence =="Att. murder & serious assault")

#drop unused levels in factors
df_received <- droplevels(df_received)

# change year format for analysis functions
df_received$year <- substr(df_received$year, 1, 4)
df_received$year <- as.numeric(df_received$year)

# select only years to be uploaded
df_received <- df_received %>% 
  filter(year >= 2004)

## create separate df for each indicator
# split df into list of 6 objects then assign ad df
df_list <- split(df_received,df_received$crime_offence)
att_murder <- as_tibble(df_list[[1]])
breach <- as_tibble(df_list[[2]])
comm_assault <- as_tibble(df_list[[3]])
drugs <- as_tibble(df_list[[4]])
violence <- as_tibble(df_list[[5]])
vandalism <- as_tibble(df_list[[6]])

# drop crime_offcence var and save rds raw files for use in analysis funtions
att_murder <- att_murder %>% select(-crime_offence) 
saveRDS(att_murder, paste0(data_folder,"Prepared Data/att_murder_raw.rds"))

breach <- breach %>% select(-crime_offence) 
saveRDS(breach, paste0(data_folder,"Prepared Data/breach_raw.rds"))

comm_assault <- comm_assault %>% select(-crime_offence)
saveRDS(comm_assault, paste0(data_folder,"Prepared Data/comm_assault_raw.rds"))

drugs <- drugs %>% select(-crime_offence)
saveRDS(drugs, paste0(data_folder,"Prepared Data/drugs_raw.rds"))

violence <- violence %>% select(-crime_offence)
saveRDS(violence, paste0(data_folder,"Prepared Data/violence_raw.rds"))

vandalism <- vandalism %>% select(-crime_offence)
saveRDS(vandalism, paste0(data_folder,"Prepared Data/vandalism_raw.rds"))


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("X:/ScotPHO Profiles/indicator-production-master/1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

###### attempted murder --------

analyze_first(filename = "att_murder", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "att_murder", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4111", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/att_murder.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/att_murder.csv"))

###### breach of the peace --------

analyze_first(filename = "breach", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "breach", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4156", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/breach.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/breach.csv"))

###### common assault --------
analyze_first(filename = "comm_assault", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "comm_assault", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4154", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/comm_assault.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/comm_assault.csv"))


###### drugs --------

analyze_first(filename = "drugs", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "drugs", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "20806", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/drugs.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/drugs.csv"))


###### vandalism --------

analyze_first(filename = "vandalism", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "vandalism", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4155", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/vandalism.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/vandalism.csv"))

###### violent crime --------

analyze_first(filename = "violence", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2004, yearend = 2018, 
              pop = "CA_pop_allages", time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "violence", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "20805", year_type = "financial", pop = "CA_pop_allages")


#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/violence.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/violence.csv"))
