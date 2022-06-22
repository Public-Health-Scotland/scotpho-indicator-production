### notes ----

# this script produces data for the following 3 indicators:-

# 13009 - school leavers living in most deprived quantile with 1 or more qualification at SCQF L6
# 13006 - school leavers with 1 or more qualification at SCQF L4
# 20601 - scool leavers with 1 more qualification at SCQF L6

#data source: https://www.gov.scot/publications/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition/ 


###############################################.
## Part 1 - Prepare data ----
###############################################.


###1.a load dependencies/functions ----

source("1.indicator_analysis.R") 

library("janitor") #for row_to_names() function 
library("stringr")#for string_replace() function


###1.b read in data ----

scqf_level <- read_xlsx(paste0(data_folder, "Received Data/summary-statistics-attainment-initial-leaver-destinations-no-4-2022-edition.xlsx"), sheet = "A2.3a and A2.3b")

ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) #council area lookup


###1.c clean data ----

scqf_level <- scqf_level %>%
  select(c(1, 2, 3, 4, 5, 7, 9)) %>%
  tail(-4) %>% #remove metadata from top of spreadsheet
  row_to_names(row_number = 1) %>% #set 1st row as column names
  setNames(tolower(names(.))) %>%
  mutate(`year` = str_sub(year,1,nchar(year)-3), #convert year from FY yy/yy to yyyy
         `local authority` = str_replace(`local authority`, "Edinburgh, City of","City of Edinburgh"),
         `local authority` = str_replace(`local authority`, "&","and"),
         across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), 0)), # replace suppressed figures symbols with 0
         across(contains(c("scqf", "leaver", "year")), as.numeric)) %>% 
  rename("denominator" = "number of leavers") %>%
  left_join(ca, by = c("local authority" = "areaname"), all.x = TRUE) %>% # join with council area lookup
  mutate(code = ifelse(`local authority` == "Scotland", "S00000001", code)) # assign scotland totals a geography code


# school leavers with 1 or more qualification at scqf level 4 ----
scqf_4 <- scqf_level %>%
  filter(`simd quintile [note 5]` == "Total") %>%
  rename("numerator" = "1+ at scqf\r\nlevel 4 or better") %>%
  select(year, code, denominator, numerator)


# school leavers with 1 or more qualification at scqf level 6 ----
scqf_6 <- scqf_level %>%
  filter(`simd quintile [note 5]` == "Total") %>%
  rename("numerator" = "1+ at scqf\r\nlevel 6 or better") %>%
  select(year, code, denominator, numerator) 


# school leavers in most deprived quantile with 1 or more qualification at level 6
scqf_6_depr <- scqf_level %>%#
  filter(`simd quintile [note 5]` == "0-20% (Most Deprived)") %>%
  rename("numerator" = "1+ at scqf\r\nlevel 6 or better") %>%
  select(year, code, denominator, numerator) 


#1.d. Save files - do some QA checks at this point ----
saveRDS(scqf_4, file=paste0(data_folder, 'Prepared Data/school_leavers_scqf_4_raw.rds'))
saveRDS(scqf_6, file=paste0(data_folder, 'Prepared Data/school_leavers_scqf_6_raw.rds'))
saveRDS(scqf_6_depr, file=paste0(data_folder, 'Prepared Data/school_leavers_scqf_6_depr_raw.rds'))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.


#(note: analyze_first() can't be used because some geographies figures were suppressed when published)
# workaround below formats data to be used in analyze_second() function 
#it ensures Scotland totals remain accurate and are not just the sum of all council area figures

yearstart <- 2009
yearend <- 2021 # note: change each time updating indicator

#read data back in (containing council area and scotland figures)
scqf_4 <- readRDS(paste0(data_folder, "Prepared Data/school_leavers_scqf_4_raw.rds"))
scqf_6 <- readRDS(paste0(data_folder, "Prepared Data/school_leavers_scqf_6_raw.rds"))
scqf_6_depr <- readRDS(paste0(data_folder, "Prepared Data/school_leavers_scqf_6_depr_raw.rds"))


# read in geography lookup to get health boards
geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)


# function to combine health board figures with council area and scotland figures
include_hb_figures <- function(df) {
  
  df %>%
    filter(year >= yearstart & year <= yearend,
           code != "S00000001") %>% 
    left_join(geo_lookup, by = c("code" = "ca2019")) %>%
    select(-code) %>%
    rename("code" = "hb2019") %>%
    group_by(year, code) %>%
    summarise_all(sum) %>% ungroup() %>%
    rbind(df)
  
}

scqf_4_final <- include_hb_figures(scqf_4)
scqf_6_final <- include_hb_figures(scqf_6)
scqf_6_depr_final <- include_hb_figures(scqf_6_depr)


#save files to be used in analyze_second() function
saveRDS(scqf_4_final, file=paste0(data_folder, "Temporary/school_leavers_scqf_4_formatted.rds"))
saveRDS(scqf_6_final, file=paste0(data_folder, "Temporary/school_leavers_scqf_6_formatted.rds"))
saveRDS(scqf_6_depr_final, file=paste0(data_folder, "Temporary/school_leavers_scqf_6_depr_formatted.rds"))



analyze_second(filename = "school_leavers_scqf_4", measure = "percent", 
               time_agg = 1, ind_id = "13009",year_type = "school")


analyze_second(filename = "school_leavers_scqf_6", measure = "percent", 
               time_agg = 1, ind_id = "13006",year_type = "school")


analyze_second(filename = "school_leavers_scqf_6_depr", measure = "percent", 
               time_agg = 1, ind_id = "20601",year_type = "school")


