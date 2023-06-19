### notes ----

# this script produces data for indicator 13030 - school leavers in positive destinations
# source of data: https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-5-2023-edition/documents/


###############################################.
## Part 1 - Prepare data ----
###############################################.


###1.a load functions/dependencies ----

source("1.indicator_analysis.R") 

library("janitor") #for row_to_names() function 
library("stringr")#for string_replace() function


###1.b read in data ----

positive_dest <- read_xlsx(paste0(data_folder, "Received Data/summary-statistics-attainment-initial-leaver-destinations-no-5-2023-edition-supplementary-tables.xlsx"), sheet = "L2.2") #positive destinations data

ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) #council area lookup

###1.c clean data ----

positive_dest <- tail(positive_dest, -3) %>% # remove metadata from top of speadsheet
  row_to_names(row_number = 1) %>% #convert first row to headers
  setNames(tolower(names(.))) %>%
  mutate(`year` = str_sub(year,1,nchar(year)-3),#convert from FY YY/YY to YYYY
         `la name` = str_replace(`la name`, "Edinburgh, City of","City of Edinburgh"),
         `la name` = str_replace(`la name`, "&","and"),
         across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         across(contains(c("positive", "leaver", "year")), as.numeric)) %>%
  left_join(ca, by = c("la name" = "areaname")) %>% # join with council area lookup
  mutate(ca = ifelse(`la name` == "Scotland", "S00000001", code)) %>% 
  select("year", "ca", "positive destination", "number of leavers") %>%
  rename("numerator" = "positive destination",
         "denominator" = "number of leavers")


#1.d. Save file - do some QA checks at this point ----
saveRDS(positive_dest, file=paste0(data_folder, 'Prepared Data/school_leaver_destinations_raw.rds'))



###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "school_leaver_destinations", measure = "percent", 
              time_agg = 1, source_suppressed = TRUE, yearstart = 2009, yearend = 2022, geography = "council")

analyze_second(filename = "school_leaver_destinations", measure = "percent", 
               time_agg = 1, ind_id = "13010",year_type = "school")









