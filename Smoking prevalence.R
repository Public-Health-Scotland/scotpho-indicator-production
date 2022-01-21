################################################################################
################################################################################
#########                                                              #########
#####              Smoking prevalence profile indicators prep              #####
#########                                                              #########
################################################################################
################################################################################

## This script prepares SSCQ smoking cessation data bulk d/l from: 
#     statistics.scot.gov.uk

## Initially go to:
#     https://www2.gov.scot/Topics/Statistics/About/Surveys/SSCQ

## Select Smoking - Local Authority/Health Board

## D/l entire dataset as a CSV file and save as:
#     X:\ScotPHO Profiles\Data\Received Data\smoking_prev_received

## Function arguments:
#   id = indicator ID 
#   profile = defaults to tobacco (tx), can also select H&W (hh)
#             will not be required once old OPT is retired
#   topic = "Smoking"
#   age_range = defaults to "All", 
#               can also select "16-34", "16-64", "35-64" or "65 And Over" 
#   sex = "All", can also select "Female" or "Male"
#   min_opt = manually enter from OPT numbers file at:
#               X:\ScotPHO Profiles\Rolling Updates\Documentation\Planning Docs
#             will not be required once old OPT is retired

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
lookups <- "X:/ScotPHO Profiles/Data/Lookups/" # not required?

################################################################################
#####                          create prepared data                        #####
################################################################################
# open received data
df_received <- read.csv(paste0(data_folder,"Received Data/smoking_prev_received.csv"))

# save as rdf in prepared data
saveRDS(df_received, paste0(data_folder,"Prepared Data/smoking_prev_raw.rds"))

# remove received data from environment
remove(df_received)

################################################################################
#####                          format prepared data                        #####
################################################################################

# open prepared data
df_prepared <- readRDS(paste0(data_folder,"Prepared Data/smoking_prev_raw.rds")) %>% 
  as_tibble() %>% 
  # select only Scotland, HB, LA
  filter(!substr(FeatureCode, 1, 3)== c("S13", "S14", "S16")) %>% 
  # remove pooled years
  filter(!str_detect(DateCode, "-")) %>% 
  # remove non-smokers
  filter(Currently.Smokes.Cigarettes == "Yes") %>%  
  # remove specific tenures
  filter(Type.Of.Tenure == "All") %>%  
  # remove specific h/hold types
  filter(Household.Type == "All") %>% 
  # remove l-t conditions
  filter(Limiting.Long.term.Physical.or.Mental.Health.Condition == "All") %>% 
  # remove vars not needed
  select(c(-Units, -Currently.Smokes.Cigarettes, -Type.Of.Tenure, 
           -Household.Type, 
           -Limiting.Long.term.Physical.or.Mental.Health.Condition)) %>% 
  rename(code = FeatureCode, year = DateCode, measure = Measurement,
         rate = Value,age_grp = Age, sex_grp = Gender) 

#drop unused levels in factors
df_prepared <- droplevels(df_prepared)

# change Scotland geog code
df_prepared$code <- as.character(df_prepared$code)
df_prepared$code[df_prepared$code == "S92000003"] <- "S00000001"

df_prepared <- df_prepared %>% 
  arrange(year, code, sex_grp, age_grp, measure)

# long to wide
df_wide <- df_prepared %>% 
  spread(measure, rate) %>% 
  rename(rate = "Percent",
         lowci = "95% Lower Confidence Limit, Percent",
         upci = "95% Upper Confidence Limit, Percent")

# reorder columns so that % befoe CIs
df_wide <- df_wide[,c(1:4, 7, 5:6)]

# define variables for numerator, def_period	and trend_axis
df_wide$numerator <- "" # creates blank col as not reported in profiles
df_wide$def_period <- paste(df_wide$year, "survey year")
df_wide$trend_axis <- df_wide$year

# confidence intervenal check 
# (>0 indictaes number of cases where rate sits outside CI range)
df_wide %>% mutate(ci_check = as.logical(rate<lowci | rate>upci)) %>% 
  summarise(sum(ci_check))

################################################################################
#####                          Analysis function                           #####
################################################################################

# This function filters the spcific age-sex groupings required for the 
# smoking prevalence indictaors, formats for saving to final datafile and saves
# datafiles for upload to the profiles platform
analyze <- function(id, profile = "tx", topic = "Smoking", age_range = "All", 
                    sex = "All"){
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
    write_csv(paste0(data_folder, "Data to be checked/",id, "_smoking_prev_", age_range, "_", sex, "_shiny.csv"))  
    

  } # end of function


################################################################################
#####                             Function calls                           #####
################################################################################

# these call the above function to create indictaor data files

##  all ages, both sexes - H&W
analyze(id = 20202)

##  all ages, both sexes - Tobacco
analyze(id = 1563)

## all ages, male
analyze(id = 1568, sex = "Male")

## all ages, female
analyze(id = 1569, sex = "Female")

## 16-34, both sexes
analyze(id = 1564, age_range = "16-34")   

## 16-64, both sexes
analyze(id = 1565, age_range = "16-64")   

## 35-64, both sexes
analyze(id = 1566, age_range = "35-64")   

## 65+, both sexes
analyze(id = 1567, age_range = "65 And Over")   

