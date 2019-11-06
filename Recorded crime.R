################################################################################
################################################################################
#########                                                              #########
#####               Recorded crime profile indicators prep                 #####
#########                                                              #########
################################################################################
################################################################################

############## IN DEVELOPMENT ############## 
<<<<<<< HEAD
### need to add pop look up into function call
=======
>>>>>>> 2e8b0a7e8f15470059b430c454a7635b316741b9

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
  # select only Scotland, HB, LA
  filter(!substr(FeatureCode, 1, 3) == c("S13", "S14", "S16")) %>% 
  # select only counts
  filter(Measurement == "Count") %>% 
  # remove vars not needed
  select(-c(Units, Measurement)) %>% 
  rename(code = FeatureCode, year = DateCode, 
         numerator = Value, crime_offence = Crime.or.Offence) 

# change Scotland geog code
df_received$code <- as.character(df_received$code)
df_received$code[df_received$code == "S92000003"] <- "S00000001"

df_received <- df_received %>% 
  arrange(year, code, numerator)

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

# save rds raw files for use in analysis funtions
saveRDS(att_murder, paste0(data_folder,"Prepared Data/att_murder_raw.rds"))
saveRDS(breach, paste0(data_folder,"Prepared Data/breach_raw.rds"))
saveRDS(comm_assault, paste0(data_folder,"Prepared Data/comm_assault_raw.rds"))
saveRDS(drugs, paste0(data_folder,"Prepared Data/drugs_raw.rds"))
saveRDS(violence, paste0(data_folder,"Prepared Data/violence_raw.rds"))
saveRDS(vandalism, paste0(data_folder,"Prepared Data/vandalism_raw.rds"))

<<<<<<< HEAD
###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("X:/ScotPHO Profiles/indicator-production-master/1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

## attempted murder

analyze_first(filename = "att_murder", geography = "council", 
              measure = "crude", yearstart = 2004, yearend = 2018, 
              time_agg = 1)


# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "att_murder", measure = "crude", 
               time_agg = 1, ind_id = "4111", year_type = "financial", pop = "######################################")


## Check correct number of HB and LA per year
final_result %>%
  mutate(geo_type=substr(code,1,3)) %>%
  group_by(geo_type, trend_axis) %>%
  summarise(count=n()) 

#resave both rds and csv files
saveRDS(final_result, file = paste0(data_folder, "Data to be checked/########################.rds"))
write_csv(final_result, path = paste0(data_folder, "Data to be checked/#######################.csv"))
=======
###### up to here completed



################################################################################
#####                          Analysis function                           #####
################################################################################

# This function filters the spcific age-sex groupings required for the 
# smoking prevalence indictaors, formats for saving to final datafile and saves
# datafiles for upload to the profiles platform
analyze <- function(id, profile = "tx", topic = "Smoking", age_range = "All", 
                    sex = "All", min_opt){
  df_indicator <- df_wide %>% 
    # filter specific age-sex groups
    filter(age_grp == age_range & sex_grp == sex) %>% 
    # add ind_id
    mutate(ind_id = id) %>% 
    # reorder columns and deselect unneeded variables
    select(c(code, ind_id, year, numerator, rate, lowci,
      upci, def_period, trend_axis)) %>% 
    arrange(year, code) %>% 
    # save shiny file
    write_csv(paste0(data_folder, "Shiny Data/",id, "_smoking_prev_", age_range, "_", sex, "_shiny.csv"))  
    
  df_oldopt <- df_indicator %>% 
    mutate(uni_id = paste0(profile, (seq_len(nrow(.)) + min_opt - 1))) %>% #OPT number
    # reorder 
    select(c("uni_id", "code", "ind_id", "year", "numerator", "rate", "lowci" ,
             "upci", "def_period", "trend_axis")) %>% 
  # save opt file
  write_csv(paste0(data_folder, "OPT Data/",id, "_smoking_prev_", age_range, "_", sex, "_OPT.csv"),
            col_names = FALSE, na="")  

  } # end of function


################################################################################
#####                             Function calls                           #####
################################################################################

# these call the above function to create indictaor data files

##  all ages, both sexes - H&W
analyze(id = 20202, profile = "hh", min_opt = 1041710)

##  all ages, both sexes - Tobacco
analyze(id = 1563, min_opt = 1003713
)

## all ages, male
analyze(id = 1568, sex = "Male", min_opt = 1003995)

## all ages, female
analyze(id = 1569, sex = "Female", min_opt = 1004277)

## 16-34, both sexes
analyze(id = 1564, age_range = "16-34", min_opt = 1004559)   

## 16-64, both sexes
analyze(id = 1565, age_range = "16-64", min_opt = 1004841)   

## 35-64, both sexes
analyze(id = 1566, age_range = "35-64", min_opt = 1005123)   

## 65+, both sexes
analyze(id = 1567, age_range = "65 And Over", min_opt = 1003008)   
>>>>>>> 2e8b0a7e8f15470059b430c454a7635b316741b9

