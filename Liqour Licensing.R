################################################################################
################################################################################
#########                                                              #########
#####       Liquor Licensing in Scotland profile indicators prep           #####
#########                                                              #########
################################################################################
################################################################################

# AP

## This script prepares SG Liquor Licensing in Scotland profile indicators:
##      Personal licences in force
##      Premise licences in force - off-sale 
##      Premise licences in force - on-sale
##      Premise licences in force - total


## Data are downloaded from SG website:
# https://www2.gov.scot/Topics/Statistics/Browse/Crime-Justice/PubLiquor

## Previous years are saved here:
# N:/All/ScotPHO Profiles/Rolling Updates/Alcohol/Raw Data/Received Data/LLiS 2011-12 to 2017-18 combined.xlsx

## Manlually added 2018/19 data into:
# X:/ScotPHO Profiles/Data/Received Data/llis_2011-12_to_ 2018-19_combined.xlsx


################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
library(tidyverse) # all kinds of stuff 
library(stringr) # for strings
library(readxl) # for reading excel file and all data sheets

## set file pathways
# NHS HS PHO Team Large File repository file pathways
data_folder <- "X:/ScotPHO Profiles/Data/" 
lookups <- "X:/ScotPHO Profiles/Data/Lookups/" 


################################################################################
#####  Part 1)  format prepared data --------------------------------
################################################################################

### open received data
## this function will read all excel sheets as a list
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

## call function with received data
mysheets <- read_excel_allsheets(filename = "X:/ScotPHO Profiles/Data/Received Data/llis_2011-12_to_ 2018-19_combined.xlsx")

## bind list items together into dataframe
df_received <- bind_rows(mysheets) %>% as_tibble() %>% 
  filter(la != "SCOTLAND") # filter out Scotland total

## add la geog codes
# create dataframe for lookup
ca_lookup <- data.frame(
  ca2019 = c("S12000005", "S12000006", "S12000006", 
             "S12000008", "S12000010", "S12000011", 
             "S12000013", "S12000013", "S12000013", "S12000014", 
             "S12000047", "S12000017", "S12000018", 
             "S12000019", "S12000020", "S12000021", 
             "S12000023", "S12000048", "S12000048", 
             "S12000026", "S12000027", "S12000028", 
             "S12000029", "S12000030", "S12000033", 
             "S12000034", "S12000035", "S12000036", 
             "S12000036", "S12000038", "S12000039", 
             "S12000040", "S12000041", "S12000042", 
             "S12000050", "S12000045", "S12000049"),
  la = c("Clackmannanshire","Dumfries & Galloway", "Dumfries and Galloway",
         "East Ayrshire", "East Lothian", "East Renfrewshire", 
         "Na h-Eileanan Siar", "Na h-Eilanan Siar", "Eilean Siar","Falkirk", 
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
df_prepared <- df_received %>% left_join(ca_lookup, by = "la") 
# check for unmatched cases
sum(is.na(df_prepared$ca2019))

df_prepared <- df_prepared %>% select(year, ca2019, premises_total, premises_on,
                                      premises_off, personal) %>% 
  rename(ca = ca2019)

## create separate df for each indicator
premises_total <- df_prepared %>% select(year, ca, premises_total) %>% 
  rename(numerator = premises_total)
premises_on <- df_prepared %>% select(year, ca, premises_on) %>% 
  rename(numerator = premises_on)
premises_off <- df_prepared %>% select(year, ca, premises_off) %>% 
  rename(numerator = premises_off)
personal <- df_prepared %>% select(year, ca, personal) %>% 
  rename(numerator = personal)


# save rds raw files for use in analysis funtions

saveRDS(premises_total, paste0(data_folder,"Prepared Data/premises_total_raw.rds"))

saveRDS(premises_on, paste0(data_folder,"Prepared Data/premises_on_raw.rds"))

saveRDS(premises_off, paste0(data_folder,"Prepared Data/premises_off_raw.rds"))

saveRDS(personal, paste0(data_folder,"Prepared Data/personal_raw.rds"))


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
organisation  <-  "HS"
source("./1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

###### premises licenses total --------

analyze_first(filename = "premises_total", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2018, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_total", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4144", year_type = "financial", pop = "CA_pop_18+")

###### premises licenses on trade --------
analyze_first(filename = "premises_on", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2018, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_on", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4114", year_type = "financial", pop = "CA_pop_18+")


###### premises licenses off trade --------
analyze_first(filename = "premises_off", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2018, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_off", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4139", year_type = "financial", pop = "CA_pop_18+")

###### personal licenses --------
analyze_first(filename = "personal", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2018, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "personal", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4140", year_type = "financial", pop = "CA_pop_18+")


#### ----------------

