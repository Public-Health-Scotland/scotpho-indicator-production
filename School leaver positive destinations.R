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

positive_dest <- read_xlsx(paste0(data_folder, "Received Data/School leaver positive destinations/summary-statistics-attainment-initial-leaver-destinations-no-6-2024.xlsx"), sheet = "L2.1a") #positive destinations data

ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) #council area lookup

###1.c clean data ----

positive_dest <- tail(positive_dest, -4) %>% # remove metadata from top of speadsheet
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


########


#end of original 

##altertnative script by liz richardson but this doesn't import numerator and denominator which would enable NHS board level data calculation
##revisit this script and find combination of both options
##could test monicas revamp of analysis functions


### notes ----

# this script produces data for indicator 13030 - school leavers from publicly funded secondary schools in positive destinations, 9 months after leaving school
# Summary Statistics for Attainment and Initial Leaver Destinations, No. 6: 2024 Edition, released on 27th February 2024
# https://www.gov.scot/publications/summary-statistics-follow-up-leaver-destinations-no-6-2024-edition/
# Script updated in Nov 2024 (by ER) to import more population group splits from the data file (previously only Scotland and CA data were imported)


###############################################.
## Part 1 - Setup ----
###############################################.


### Load functions/dependencies ----

source("1.indicator_analysis.R") 
source("2.deprivation_analysis.R")

library(openxlsx) 


### Paths ----

# Identify data folder
schdest_data_folder <- paste0(data_folder, "Received Data/School leaver positive destinations/")
file <- "summary-statistics-attainment-initial-leaver-destinations-no-6-2024.xlsx"


### Lookups ----

# council area lookup
ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) 

# geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) |>
#   select(ca2019, hb2019) 
# geo_lookup %<>% distinct %>% rename(ca = ca2019, hb = hb2019)

###############################################.
## Part 2 - Import data ----
###############################################.


# National:
###############

# Scotland data, totals
scot_series <- read.xlsx(paste0(schdest_data_folder, file),
                         sheet = "L1.1",
                         colNames = TRUE,
                         rows = c(6:37)) %>%
  rename(trend_axis = "Year.[note.19][note.20]") %>%
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         across(contains(c("Voluntary", "Activity", "Personal")), as.numeric),
         rate = rowSums(pick("Higher.Education":"Personal.Skills.Development.[note.13]"), na.rm = T),
         denominator = `Number.of.Leavers`,
         numerator = (rate * `Number.of.Leavers`)/100) 
%>%
  select(trend_axis, numerator, rate) %>%
  mutate(split_name = "None",
         split_value = "None")

# Scotland data, by population groups

## Function to read in population group data
import_schdest_split_data <- function(sheet, rows, split_name) {
  
  df <- read.xlsx(paste0(schdest_data_folder, file),
                  sheet = sheet,
                  colNames = TRUE,
                  rows = rows) %>%
    mutate(Initial.Destination = str_trim(Initial.Destination)) %>%
    filter(Initial.Destination %in% c("All Positive Destinations", "Number of Leavers")) %>%
    pivot_longer(-c(Year, Initial.Destination), names_to = "split_value", values_to = "value") %>%
    mutate(across(value, ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
           across(value, as.numeric),
           split_value = str_replace_all(split_value, "\\.", " "),
           split_value = str_to_sentence(split_value)) %>%
    pivot_wider(names_from = Initial.Destination, values_from = value) %>%
    mutate(numerator = round(`Number of Leavers` * `All Positive Destinations`/100),
           split_name = split_name) %>%
    rename(rate = `All Positive Destinations`,
           trend_axis = Year) %>%
    select(-`Number of Leavers`)
  
}

# Run the function to import the data for the various splits available:
schdest_sex <- import_schdest_split_data(sheet = "L.1.2", rows = c(6:188), split_name = "Sex")
schdest_urbrur <- import_schdest_split_data(sheet = "L1.3", rows = c(6:188), split_name = "Urban/Rural status") 
schdest_ethnicity <- import_schdest_split_data(sheet = "L1.4", rows = c(6:188), split_name = "Ethnicity") %>%
  mutate(split_value = str_replace_all(split_value, "/ ", "/"), # extra tidying of values needed
         split_value = str_replace(split_value, " \\[.*\\]", ""),
         split_value = str_to_title(split_value))
schdest_disabled <- import_schdest_split_data(sheet = "L1.6", rows = c(6:136), split_name = "Disabled (declared or assessed)") %>%
  mutate(split_value = str_replace(split_value, "Declared or assessed disabled: ", ""),
         split_value = str_to_title(split_value))
schdest_simd <- import_schdest_split_data(sheet = "Table_2", rows = c(6:188), split_name = "Deprivation (SIMD)") %>%
  mutate(split_value = case_when(split_value=="Quintile 0-20% (most deprived)"  ~ "1", # recode the splits
                                 split_value=="Quintile 20-40%" ~ "2",
                                 split_value=="Quintile 40-60%" ~ "3",
                                 split_value=="Quintile 60-80%" ~ "4",
                                 split_value=="Quintile 80-100% (least deprived)" ~ "5",
                                 split_value=="Total" ~ "Total")) %>%
  filter(!is.na(split_value))

# combine Scotland data
scot_all <- rbind(scot_series, 
                  schdest_disabled,
                  schdest_ethnicity,
                  schdest_sex,
                  schdest_urbrur,
                  schdest_simd) %>%
  mutate(code = "S00000001") # add code for Scotland


# make population file for school leavers by year and SIMD quintile, for inequalities metrics:
depr_pop_schoolleavers <- read.xlsx(paste0(schdest_data_folder, file),
                                    sheet = "Table_2", 
                                    rows = c(6:188),
                                    colNames = TRUE) %>%
  mutate(Initial.Destination = str_trim(Initial.Destination),
         year = as.numeric(substr(Year, 1, 4))) %>%
  filter(Initial.Destination == "Number of Leavers") %>%
  select(-Year, -Initial.Destination) %>%
  pivot_longer(-year, names_to = "quintile", values_to = "denominator") %>%
  select(year, quintile, denominator) %>%
  mutate(quintile = str_replace_all(quintile, "\\.", " "),
         quintile = str_to_sentence(quintile)) %>%
  mutate(quintile = case_when(quintile=="Quintile 0-20% (most deprived)"  ~ "1", # recode the splits
                              quintile=="Quintile 20-40%" ~ "2",
                              quintile=="Quintile 40-60%" ~ "3",
                              quintile=="Quintile 60-80%" ~ "4",
                              quintile=="Quintile 80-100% (least deprived)" ~ "5",
                              quintile=="Total" ~ "Total")) %>%
  filter(!is.na(quintile)) %>%
  mutate(code = "S00000001",
         quint_type = "sc_quin") %>%
  mutate(denominator = as.numeric(denominator))

saveRDS(depr_pop_schoolleavers, paste0(lookups, "Population/depr_pop_schoolleavers.rds"))

# Council area:
##################

# Council area data, totals
ca_series <- read.xlsx(paste0(schdest_data_folder, file),
                       sheet = "L2.1",
                       colNames = TRUE,
                       rows = c(6:468)) %>%
  rename(trend_axis = Year) %>%
  filter(LA.Name!="Scotland") %>% # longer time series in the scot_series data
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         rate = as.numeric(`Positive.Destination`),
         numerator = (rate * `Number.of.leavers`)/100,
         areaname = str_replace(LA.Name, "Edinburgh, City of","City of Edinburgh"),
         areaname = str_replace(areaname, "&","and")) %>%
  merge(ca, by = "areaname") %>% # join with council area lookup
  select(trend_axis, code, numerator, rate) %>%
  mutate(split_name = "None",
         split_value = "None")


# Combine all:
###################

all_data <- rbind(scot_all, ca_series) %>%
  mutate(ind_id = 13010,
         year = substr(trend_axis, 1, 4),
         def_period = paste0("School year (", trend_axis, ")"),
         upci = NA,
         lowci = NA)


##########################################################
### Part 3 - Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- all_data %>% 
    filter(split_name == "None") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code,year)
  
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE) #remove when data live and scripts not being developed
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds")) #remove when data live and scripts not being developed
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)
  pop_grp_data <- all_data %>% 
    filter(!(split_name %in% c("None", "Deprivation (SIMD)"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value,) 
  
  # Save
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE) #remove when data live and scripts not being developed
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds")) #remove when data live and scripts not being developed
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  # 3 - SIMD data (ie data behind deprivation tab)
  
  # Process SIMD data
  simd_data <- all_data %>% 
    filter(split_name == "Deprivation (SIMD)") %>% 
    unique() %>%
    select(-split_name) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type = "sc_quin")
  
  # Save intermediate SIMD file
  write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
                                 pop = "depr_pop_schoolleavers", # the population file created above
                                 ind_id = 13010, 
                                 ind_name = ind
  )
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "pos_school_leaver_destinations")


# Run QA reports
####################

# main data: 
run_qa(filename = "pos_school_leaver_destinations")

# ineq data: # NOT RUNNING DUE TO MISSING HBS???
run_ineq_qa(filename = "pos_school_leaver_destinations")


#END


















