################################################################################
################################################################################
#########                                                              #########
#####     People aged 65+ with high levels of care needs who are cared     #####
#####                     for at home - indicator prep                     #####
#########                                                              #########
################################################################################
################################################################################

############## IN DEVELOPMENT ############## 


## This script prepares SG recorded crime profile indicators:
##      People aged 65+ with high levels of care needs who are cared for at home

## Data taken from additional Excel table from (pg. 45 or search for "high level):
# https://www.isdscotland.org/Health-Topics/Health-and-Social-Community-Care/Publications/2019-06-11/2019-06-11-Social-Care-Report.pdf?
## Requested from Source Team as link to workbook is broken

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
library(openxlsx) # for reading excel file and accessing hidden data sheet

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/" 


################################################################################
#####  Part 1)  format prepared data --------------------------------
################################################################################

## open received data
df_received <- readWorkbook(xlsxFile = "X:/ScotPHO Profiles/Data/Received Data/high_care_needs_received.xlsm", 
                   sheet = "T2 Data") %>% as_tibble(df) %>% 
  select(-c(X1, X3)) %>% # drop unnecessary vars
  rename(ca = X4) %>% # rename vars
  filter(X2 != "Percentage",
         ca != "Scotland") %>% # filter out %s and Scotland
  gather(key = year, value = cases, `2009`:`2018`) # gather years

## filter only numberatord
df_numerator <- df_received %>% filter(X2 == "Home care") %>% 
  rename(numerator = cases) %>% select(-X2)

## calculate denominator
df_denom  <-  df_received %>% 
  select(-X2) %>% 
  group_by(ca, year) %>% 
  summarise(denominator = sum(cases)) %>% ungroup()

## join back together
df_prepared <- df_numerator %>% left_join(df_denom)
df_prepared$denominator <-  round(df_prepared$denominator, 0) # round denominator to whole n

## add in geog codes
# crate dataframe for lookup
ca_lookup <- data.frame(
  ca2019 = c("S12000005", "S12000006", "S12000006", 
             "S12000008", "S12000010", "S12000011", 
             "S12000013", "S12000013", "S12000014", 
             "S12000047", "S12000017", "S12000018", 
             "S12000019", "S12000020", "S12000021", 
             "S12000023", "S12000048", "S12000048", 
             "S12000026", "S12000027", "S12000028", 
             "S12000029", "S12000030", "S12000033", 
             "S12000034", "S12000035", "S12000036", 
             "S12000036", "S12000038", "S12000039", 
             "S12000040", "S12000041", "S12000042", 
             "S12000050", "S12000045", "S12000049"),
  ca = c("Clackmannanshire","Dumfries & Galloway", "Dumfries and Galloway",
              "East Ayrshire", "East Lothian", "East Renfrewshire", 
              "Na h-Eileanan Siar", "Eilean Siar","Falkirk", 
              "Fife", "Highland", "Inverclyde",
              "Midlothian", "Moray", "North Ayrshire", 
              "Orkney Islands", "Perth & Kinross", "Perth and Kinross", 
              "Scottish Borders", "Shetland Islands", "South Ayrshire", 
              "South Lanarkshire", "Stirling", "Aberdeen City", 
              "Aberdeenshire", "Argyll & Bute", "Edinburgh, City of", 
              "City of Edinburgh", "Renfrewshire", "West Dunbartonshire", 
              "West Lothian", "Angus", "Dundee City", 
              "North Lanarkshire", "East Dunbartonshire", "Glasgow City"))

# match to raw data (change df name as required)
df_prepared <- df_prepared %>% left_join(ca_lookup, by = "ca") 
# check for unmatched cases
sum(is.na(df_prepared$ca2019))

df_prepared <- df_prepared %>% 
  select(c(year, ca2019, numerator, denominator)) %>% 
  rename(ca = ca2019)


## add in earlier years?
df_old <- readWorkbook(xlsxFile = "N:/All/ScotPHO Profiles/Rolling Updates/Social Care/Raw Data/Prepared Data/12205 - 65+ high needs care at home.xlsx", 
                       sheet = "EXPORT") %>% 
  as_tibble() %>% 
  rename(ca2019 = LA) %>% 
  filter(Year < 2009) # select only years not in current data

df_old$ca2019 <- as_factor(df_old$ca2019) # change ca2019 to factor

names(df_old) <- tolower(names(df_old)) # lowercase var names

# old data has earlier ca codes
df_old <- df_old %>% left_join(ca_lookup, by = "ca2019") # attach ca2019 names
unique(df_old$ca2019[is.na(df_old$ca)]) # print unmatched codes 

## sort out these ca2011 codes
#  S12000015	"Fife" - "S12000047"
df_old$ca2019[df_old$ca2019 == "S12000015"] <- "S12000047"
#  S12000046	"Glasgow City"  - "S12000049"
df_old$ca2019[df_old$ca2019 == "S12000046"] <- "S12000049"
#  S12000044	"North Lanarkshire"  - "S12000050"
df_old$ca2019[df_old$ca2019 == "S12000044"] <- "S12000050"
#  S12000024	"Perth and Kinross"  - "S12000048"
df_old$ca2019[df_old$ca2019 == "S12000024"] <- "S12000048"

# recheck ca codes
df_old <- df_old %>% select(-ca) %>% left_join(ca_lookup, by = "ca2019") # attach ca2019 names
unique(df_old$ca2019[is.na(df_old$ca)]) # print unmatched codes 

# append to prepared data
df_old <- df_old %>% select(-ca) %>% rename(ca = ca2019)
df_old$year <- as.character(df_old$year)
df_prepared <- df_prepared %>%  
  bind_rows(df_old) %>% arrange(year, ca)

## save raw file
saveRDS(df_prepared, paste0(data_folder,"Prepared Data/high_care_needs_raw.rds"))


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("./1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

###### high level care needs at home --------

analyze_first(filename = "high_care_needs", geography = "council", 
              measure = "percent", yearstart = 2006, yearend = 2018, 
              time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "high_care_needs", measure = "percent", 
               time_agg = 1, ind_id = "20502", year_type = "financial")




