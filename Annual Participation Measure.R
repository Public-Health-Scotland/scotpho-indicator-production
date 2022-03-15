### notes ----

# this script produces data for the following indicator:- 13053- Annual participation (in education, training or employment) measure for 16 – 19 year olds


# data source: https://www.skillsdevelopmentscotland.co.uk/publications-statistics/statistics/annual-participation-measure/


###############################################.
## Part 1 - Prepare data ----
###############################################.


###1.a load dependencies/functions ----

source("1.indicator_analysis.R") 

library("janitor") #for row_to_names() function 
library("stringr")#for string_replace() function


###1.b read in data ----

dat <- read_xlsx(paste0(data_folder, "Received Data/annual-participation-measure-2021-supplementary-tables.xlsx"), sheet = "Table 1.7") #aps data

ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) #council area lookup



###1.c clean data ----

#select required columns
dat <- dat %>%
  tail(-6) %>% #remove metadata from top of spreadsheet
  row_to_names(row_number = 1) %>% #set 1st row as column names
  setNames(tolower(names(.))) %>%
  select(year, `local authority`, `total cohort (16-19)`, `participating (16-19)`)


#rename council areas and match with ca lookup
dat <- dat %>%
  mutate(`local authority` = str_replace(`local authority`, "&","and"),
         `local authority` = str_replace(`local authority`, "Edinburgh City","City of Edinburgh"),
         `local authority` = str_replace(`local authority`, "Eilean Siar","Na h-Eileanan Siar")) %>%
  filter(`local authority` != "Scotland") %>%
  left_join(ca, by = c("local authority" = "areaname"), all.x = TRUE)


#final formatting of data to ensure works with analyse functions 
  dat <- dat %>%
    mutate(across(contains(c("16-19")), as.numeric)) %>%
    rename("ca" = "code",
         "denominator" = "total cohort (16-19)",
         "numerator" = "participating (16-19)") %>%
  select(year, ca, numerator, denominator) 



#1.d. Save file to be used in part 2 ----
  saveRDS(dat, file=paste0(data_folder, 'Prepared Data/participation_raw.rds'))



###############################################.
## Part 2 - Run analysis functions ----
###############################################.
  
  analyze_first(filename = "participation", geography = "council", measure = "percent",  
                yearstart = 2015, yearend = 2020, time_agg = 1)
  
  
  analyze_second(filename = "participation", measure = "percent", 
                 time_agg = 1, ind_id = "13053", year_type = "calendar")
  

