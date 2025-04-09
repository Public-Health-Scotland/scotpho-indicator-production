
# TO DO:
# # too many different ethnic groupings over time to work out how to standardise right now: do in future

### 1. notes -----

# this script updates the following indicator: 30140 - School attendance rate: Percentage school attendance by primary and secondary pupils in the past year
# NB. Primary and secondary school attendance were 2 separate indicators previously (ind_id 11201 and 20603 respectively) 
# but these haven't been updated since 2018.

# data source: https://www.gov.scot/publications/school-attendance-and-absence-statistics/
# The files for individual years are downloaded to "data received" folder. 
# Update file names and ranges below when new data are saved in "data received" folder.
# Latest data (2023/24) published March 2025

# The data spreadsheets are in various formats, so importing the data is convoluted...



#################################################################################
### 1. Required packages/functions -----
#################################################################################

source("functions/main_analysis.R") # needed for the QA
source("functions/deprivation_analysis.R") # needed for the QA
library(readxl) # handles xls and xlsx in same read function, which is needed here
library(hablar) # sum_ function from hablar keeps NA when there should be NA

# Read in geography lookup
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# # aggregating LA to HB level is not possible currently due to most years lacking denominators
# # health board lookup
# hb <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
#   select(ca2019, hb2019) %>%
#   distinct(.)

#################################################################################
### 2. Read in data -----
#################################################################################

# File paths:
#############################

# the folder where the data are saved
attendance_folder <- paste0(profiles_data_folder, "/Received Data/School attendance/")

# get list of files 
files <- list.files(attendance_folder, pattern='*.xls*')
# 12 files as of March 2025

data_2006 <- "attendance-absence-2006-7.xls"                          
data_2007 <- "attendance-absence-2007-08.xls"                        
data_2008 <- "attendance-absence-2008-09.xls"                         
data_2009 <- "attendance-absence-2009-10.xls"                        
data_2010 <- "attendance-absence-2010-11.xls"                         
data_2012 <- "attendance-absence-2012-13.xls"                        
data_2014 <- "attendance-absence-2014-15.xls"                         
data_2016 <- "attendance-absence-2016-17.xlsx"                       
data_2018 <- "Attendance+and+Absence+201819+-+Excel+web+version.xlsx" 
data_2020 <- "Attendance+and+absence+statistics+202021+V4.xlsx"       
data_2022 <- "Attendance+and+absence+statistics+202223+V3.xlsx"
data_2023 <- "Attendance+and+absence+2023-24.xlsx"                   


## Define helper functions 
###############################

# Wide format data 

# Function to get Scotland data from a wide format tab
import_scot_wide_data <- function(filename, sheetnum, rowrange, split_type, value_type) {
  df <- read_excel(paste0(attendance_folder, filename),
                  sheet = paste0("Table ", sheetnum),
                  range = cell_rows(rowrange)) %>%
    mutate(across(-1, ~str_replace(., "z", "0"))) %>%
    mutate(across(-1, ~str_replace(., "c", "0"))) %>%
    pivot_longer(-1, names_to = split_type, values_to = value_type) %>%
    mutate(code = "S00000001")
}  

# Function to import and process half-days data (counts) for Scotland
import_scot_halfdays <- function(filename, sheetnum, rowrange, split_type, year) {
  
  df <- import_scot_wide_data(filename, sheetnum, rowrange, split_type, "count") %>%
    mutate(trend_axis = year,
           count = as.numeric(count)) %>%
    clean_names() 
  
  if (year!="2020/21") { # non-COVID year data
    
    df <- df %>%
      mutate(attendance_absence_reason = substr(attendance_absence_reason, 1, 20)) %>%
      pivot_wider(names_from = attendance_absence_reason, values_from = count) %>%
      clean_names() %>%
      rename(denominator = possible_attendance) %>%
      mutate(numerator = attendance_in_scho + attendance_late + attendance_work_ex + attendance_sicknes) %>%   
      mutate(rate = 100 * numerator / denominator) %>%
      # NB. we're only deriving rates from the numerator and denominator data for Scotland, as there are no suppression issues and the % produced match the published data exactly (while we get additional numerator data for the app)
      select(-contains("attendance"))
      
  } else { # if COVID year data
    
    df <- df %>%
      mutate(attendance_absence_reason = substr(attendance_absence_reason, 1, 41)) %>% #longer reasons in covid data
      pivot_wider(names_from = attendance_absence_reason, values_from = count) %>%
      clean_names() %>%
      rename(denominator = possible_attendance_schools_open) %>%
      mutate(numerator = attendance_schools_open_in_school + attendance_schools_open_late + attendance_schools_open_work_experien +
               attendance_schools_open_sickness_with + attendance_schools_open_covid_19_sick + attendance_schools_open_covid_19_self) %>%
      mutate(rate = 100 * numerator / denominator) %>%
      select(-contains("attendance"))
    
  }
}

# Function to import LA data from a wide format tab
import_la_wide_data <- function(filename, sheetnum, range, split_type, value_type, year) {
  
  if (is.character(range)) { # i.e., format "R4C1:R38C9"
    df <- read_excel(paste0(attendance_folder, filename),
                  sheet = paste0("Table ", sheetnum),
                  range = range
                  )
  } else { # i.e., only rows specified like range = c(4, 37)
      df <- read_excel(paste0(attendance_folder, filename),
                       sheet = paste0("Table ", sheetnum),
                       range = cell_rows(range)
                       ) 
    }
      
    df <- df  %>%
      mutate(across(-1, ~str_replace(., "z", "0"))) %>% # added to another column, so a true zero
      mutate(across(-1, ~str_replace(., "c", "NA"))) %>% # suppressed
      mutate(across(-1, ~as.numeric(.))) %>%
      pivot_longer(-1, names_to = split_type, values_to = value_type) %>%
      clean_names() 
    
    if (split_type!="trend_axis")  {
    
      df <- df %>%  mutate(trend_axis = year)
  
    }
    
    df

    }  

# Get data from 2009/10 to 2018/19 (files with similar format)

# Function to import old format Scottish data

get_old_file_attainment_data_scotland <- function(filename, trend_axis, range_scot, range_rural, range_ethnic) {
  
  # Just one sheet with counts: Stage (1ry, 2ry, Special, Total) and Sex
  scot_counts <- read_excel(paste0(attendance_folder, filename),
                            sheet = "Table 1.4",
                            range = cell_rows(range_scot)) %>%
    mutate(code = "S00000001") %>%
    mutate(trend_axis = trend_axis) %>%
    clean_names() %>%
    rename(denominator = possible_attendance,
           split_value = x1) %>%
    setNames(str_remove(names(.), "_1")) %>% # cuts the notes off the end of the column names, if present, and makes col names consistent across tabs
    rename(numerator = attendance) %>%
    mutate(rate = 100 * numerator / denominator) %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ "Sex")) %>%
    mutate(split_value = case_when(split_value %in% c("Females", "Girls") ~ "Female",
                                   split_value %in% c("Males", "Boys") ~ "Male",
                                   TRUE ~ split_value)) %>%
    select(-contains(c("attendance", "absence")))
  
  # # repeat the Total row for school type:
  # scot_counts <- scot_counts %>%
  #   filter(split_value == "Total") %>%
  #   mutate(split_name = "School type") %>% 
  #   rbind(scot_counts)
  
  # Urban/Rural 
  scot_rural <- read_excel(paste0(attendance_folder, filename), sheet = "Table 1.6", range = cell_rows(range_rural)) %>%
    rename(rate = Attendance, 
           split_value = ...1) %>%
    mutate(code = "S00000001",
           split_name = "Urban/Rural classification",
           trend_axis = trend_axis) %>%
    select(-contains(c("Attendance", "Absence")))
  
  # Ethnicity 
  scot_ethnic <- read_excel(paste0(attendance_folder, filename), sheet = "Table 1.11", range = cell_rows(range_ethnic)) %>%
    rename(rate = Attendance, 
           split_value = ...1) %>%
    mutate(code = "S00000001",
           split_name = "Ethnicity",
           trend_axis = trend_axis) %>%
    select(-contains(c("Attendance", "Absence", "pupils")))
  
  # SIMD (only vigintile chart provided, no data)
  
  # Combine the counts for all splits
  scot_splits <- 
    bind_rows(scot_rural, scot_ethnic, scot_counts) %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ split_name))
  
  
}


# Function to import old format LA data

get_old_file_attainment_data_la <- function(filename, trend_axis, range_la) {
  
  # 2010/11 LA data (percents from wide) (NB. ranges and post-processing differ here from later files)
  la_1ry <- import_la_wide_data(filename, sheetnum = "2.1", range = range_la, split_type = "na", value_type = "rate", year = trend_axis) %>%  
    filter(na %in% c("Attendance", "Attendance...6")) %>% select(-na) %>% mutate(split_value = "Primary", split_name = "School type")  
  la_2ry <- import_la_wide_data(filename, sheetnum = "2.2", range = range_la, split_type = "na", value_type = "rate", year = trend_axis) %>%  
    filter(na %in% c("Attendance", "Attendance...6")) %>% select(-na) %>% mutate(split_value = "Secondary", split_name = "School type") 
  # no LA-level stage, sex and SIMD data
  
  # combine the LA splits percent data
  la_splits <- rbind(la_1ry, la_2ry) %>%
    rename(local_authority = x1) %>%
    filter(!local_authority %in% c("All local authorities", "Grant Aided")) %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ split_name))
  
}






# Get trend data (all years obtainable from latest spreadsheet:
##################################

# time series from 2023/24 data (percents)
scot_all_trend <- import_scot_wide_data(filename = data_2023, sheetnum = "1.1", rowrange = c(4:5), 
                                        split_type = "trend_axis", value_type = "rate") %>% 
  select(-1) %>% #drop 1st column
  mutate(trend_axis = substr(trend_axis, 1, 7)) %>% #drop the note
  mutate(rate = as.numeric(rate)) %>%
  mutate(split_value = "Total", split_name = "Total") 

# get time series from 2023/24 data (percents) (starting 2010/11: look at older sheets in case there is earlier data there)
la_trend_2010to2023 <- import_la_wide_data(filename = data_2023, sheetnum = "2.4", range = "R4C1:R38C9", split_type = "trend_axis", value_type = "rate", year = "2023/24") %>% 
  mutate(trend_axis = substr(trend_axis, 1, 7)) %>% #drop the note
  mutate(split_value = "Total", split_name = "Total") 

# get time series 2007/08 to 2009/10:
la_trend_2007to2009 <- read_excel(paste0(attendance_folder, data_2009),
                 sheet = "Table2.4",
                 range = "A5:D43")  %>%
  pivot_longer(-1, names_to = "trend_axis", values_to = "rate") %>%
  rename(local_authority = ...1) %>% 
  mutate(split_value = "Total", split_name = "Total") 
  
# Combine the LA trend data
la_all_trend <- rbind(la_trend_2010to2023, la_trend_2007to2009)
rm(la_trend_2010to2023, la_trend_2007to2009)

# Get population splits data:
##################################

# Requires going through the spreadsheets to extract year-specific data.
# 2023/24, 2022/23 and 2020/21 are similar format, although slight differences meant I haven't been able to make a global import function for these files. 
# 2009/10 to 2018/19 use a different format, but it is more standard, so the functions above enable more automated data import.

##############
# 2023 data
##############

# Import Scottish data

# Stage (P years, S years, 1ry, 2ry, Special, Total)
scot_stages_2023_all <- import_scot_halfdays(filename = data_2023, sheetnum = "1.2", rowrange = c(5:10), 
                                             split_type = "split_value", year = "2023/24") %>% mutate(split_name = "School stage")
# Sex
scot_2023_F <- 
  import_scot_halfdays(filename = data_2023, sheetnum = "1.3", rowrange = c(5:10), split_type = "split_value", year = "2023/24") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Female", split_name = "Sex")
scot_2023_M <- 
  import_scot_halfdays(filename = data_2023, sheetnum = "1.3", rowrange = c(22:27), split_type = "split_value", year = "2023/24") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Male", split_name = "Sex")

# Urban/Rural (includes NK and Total)
scot_rural_2023 <- 
  import_scot_halfdays(filename = data_2023, sheetnum = "1.4", rowrange = c(5:10), split_type = "split_value", year = "2023/24") %>% mutate(split_name = "Urban/Rural classification")

# Ethnicity (includes NK, Not disclosed, and Total)
scot_ethnic_2023 <- 
  import_scot_halfdays(filename = data_2023, sheetnum = "1.11", rowrange = c(5:10), split_type = "split_value", year = "2023/24") %>% mutate(split_name = "Ethnicity")

# SIMD (includes Not available and Total)
scot_simd_2023 <- 
  import_scot_halfdays(filename = data_2023, sheetnum = "1.14", rowrange = c(5:10), split_type = "split_value", year = "2023/24") %>% mutate(split_name = "Deprivation (SIMD)")

# Combine Scottish splits data
scot_2023 <- rbind(scot_rural_2023, scot_ethnic_2023, scot_simd_2023, scot_stages_2023_all, scot_2023_F, scot_2023_M) 

# Import LA data 

la_1ry_2023 <- import_la_wide_data(filename = data_2023, sheetnum = "2.1", range = "R2C1:R37C2", split_type = "NA", value_type = "rate", year = "2023/24") %>%  mutate(split_value = "Primary", split_name = "School type") %>% select(-na) 
la_2ry_2023 <- import_la_wide_data(filename = data_2023, sheetnum = "2.2", range = "R2C1:R37C2", split_type = "NA", value_type = "rate", year = "2023/24") %>%  mutate(split_value = "Secondary", split_name = "School type") %>% select(-na) 
la_2023_stage <- import_la_wide_data(filename = data_2023, sheetnum = "2.9", range = c(2:37), split_type = "split_value", value_type = "rate", year = "2023/24") %>% mutate(split_name = "School stage") 
la_2023_sex_all <- import_la_wide_data(filename = data_2023, sheetnum = "2.10", range = c(4:39), split_type = "split_value", value_type = "rate", year = "2023/24") %>% mutate(split_name = "Sex") 
la_2023_simd_all <- import_la_wide_data(filename = data_2023, sheetnum = "2.14", range = c(4:39), split_type = "split_value", value_type = "rate", year = "2023/24") %>% mutate(split_name = "Deprivation (SIMD)") 
# no count data for the LA x SIMD data, so unsure about inequals calcs.

# Combine LA splits data
la_2023 <- rbind(la_2023_stage, la_2023_sex_all, la_2023_simd_all, la_1ry_2023, la_2ry_2023) 

# Drop intermediate files
rm(la_2023_stage, la_2023_sex_all, la_2023_simd_all, la_1ry_2023, la_2ry_2023)
rm(scot_rural_2023, scot_ethnic_2023, scot_simd_2023, scot_stages_2023_all,  scot_2023_F, scot_2023_M)



##############
# 2022 data
##############

# Import Scottish data

# Stage (P years, S years, 1ry, 2ry, Special, Total)
scot_stages_2022_all <- import_scot_halfdays(filename = data_2022, sheetnum = "1.2", rowrange = c(5:10),split_type = "split_value", year = "2022/23") %>% mutate(split_name = "School stage")
# Sex
scot_2022_F <- 
  import_scot_halfdays(filename = data_2022, sheetnum = "1.3", rowrange = c(5:10), split_type = "split_value", year = "2022/23") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Female", split_name = "Sex")
scot_2022_M <- 
  import_scot_halfdays(filename = data_2022, sheetnum = "1.3", rowrange = c(22:27), split_type = "split_value", year = "2022/23") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Male", split_name = "Sex")
# Urban/Rural (includes NK and Total)
scot_rural_2022 <- import_scot_halfdays(filename = data_2022, sheetnum = "1.4", rowrange = c(5:10), split_type = "split_value", year = "2022/23") %>% mutate(split_name = "Urban/Rural classification")

# Ethnicity (includes NK, Not disclosed, and Total)
scot_ethnic_2022 <- import_scot_halfdays(filename = data_2022, sheetnum = "1.11", rowrange = c(5:10), split_type = "split_value", year = "2022/23") %>% mutate(split_name = "Ethnicity")

# SIMD (includes Not available and Total)
scot_simd_2022 <- import_scot_halfdays(filename = data_2022, sheetnum = "1.14", rowrange = c(5:10), split_type = "split_value", year = "2022/23") %>% mutate(split_name = "Deprivation (SIMD)")

# Combine the counts for all splits
scot_2022 <- rbind(scot_rural_2022, scot_ethnic_2022, scot_simd_2022, scot_stages_2022_all, scot_2022_F, scot_2022_M) 

# Import LA data 

# 2022/23 LA data (percents from wide)
la_1ry_2022 <- import_la_wide_data(filename = data_2022, sheetnum = "2.1", range = "R2C1:R37C2", split_type = "NA", value_type = "rate", year = "2022/23") %>%  mutate(split_value = "Primary", split_name = "School type") %>% select(-na) 
la_2ry_2022 <- import_la_wide_data(filename = data_2022, sheetnum = "2.2", range = "R2C1:R37C2", split_type = "NA", value_type = "rate", year = "2022/23") %>%  mutate(split_value = "Secondary", split_name = "School type") %>% select(-na) 
la_2022_stage <- import_la_wide_data(filename = data_2022, sheetnum = "2.9", range = c(2:36), split_type = "split_value", value_type = "rate", year = "2022/23") %>% mutate(split_name = "School stage") 
la_2022_sex_all <- import_la_wide_data(filename = data_2022, sheetnum = "2.10", range = c(4:38), split_type = "split_value", value_type = "rate", year = "2022/23") %>% mutate(split_name = "Sex") 
la_2022_simd_all <- import_la_wide_data(filename = data_2022, sheetnum = "2.14", range = c(4:38), split_type = "split_value", value_type = "rate", year = "2022/23") %>% mutate(split_name = "Deprivation (SIMD)") 
# no count data for the LA x SIMD data, so unsure about inequals calcs.

# combine the LA splits percent data
la_2022 <- rbind(la_2022_stage, la_2022_sex_all, la_2022_simd_all, la_1ry_2022, la_2ry_2022) 

# Drop intermediate dfs
rm(la_2022_stage, la_2022_sex_all, la_2022_simd_all, la_1ry_2022, la_2ry_2022)
rm(scot_rural_2022, scot_ethnic_2022, scot_simd_2022, scot_stages_2022_all, scot_2022_F, scot_2022_M)



##############
# 2020 data
##############

# Import Scottish data

# Stage (P years, S years, 1ry, 2ry, Special, Total)
scot_stages_2020_all <- import_scot_halfdays(filename = data_2020, sheetnum = "1.2", rowrange = c(7:15), split_type = "split_value", year = "2020/21") %>% mutate(split_name = "School stage")
# Sex
scot_2020_F <- 
  import_scot_halfdays(filename = data_2020, sheetnum = "1.3", rowrange = c(7:15), split_type = "split_value", year = "2020/21") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Female", split_name = "Sex")
scot_2020_M <- 
  import_scot_halfdays(filename = data_2020, sheetnum = "1.3", rowrange = c(31:39), split_type = "split_value", year = "2020/21") %>% 
  filter(split_value == "Total") %>% mutate(split_value = "Male", split_name = "Sex")

# Urban/Rural (includes NK and Total)
scot_rural_2020 <- import_scot_halfdays(filename = data_2020, sheetnum = "1.4", rowrange = c(7:15), split_type = "split_value", year = "2020/21") %>% mutate(split_name = "Urban/Rural classification")

# Ethnicity (includes NK, Not disclosed, and Total)
scot_ethnic_2020 <- import_scot_halfdays(filename = data_2020, sheetnum = "1.11", rowrange = c(7:15), split_type = "split_value", year = "2020/21") %>% mutate(split_name = "Ethnicity")

# SIMD (includes Not available and Total)
scot_simd_2020 <- import_scot_halfdays(filename = data_2020, sheetnum = "1.14", rowrange = c(7:15), split_type = "split_value", year = "2020/21") %>% mutate(split_name = "Deprivation (SIMD)")

# Combine the counts for all splits
scot_2020 <- rbind(scot_rural_2020, scot_ethnic_2020, scot_simd_2020, scot_stages_2020_all, scot_2020_F, scot_2020_M) 

# Import LA data 

# 2020/21 LA data (percents from wide)
la_1ry_2020 <- import_la_wide_data(filename = data_2020, sheetnum = "2.1", range = "R4C1:R37C2", split_type = "NA", value_type = "rate", year = "2020/21") %>%  mutate(split_value = "Primary", split_name = "School type") %>% select(-na) 
la_2ry_2020 <- import_la_wide_data(filename = data_2020, sheetnum = "2.2", range = "R4C1:R37C2", split_type = "NA", value_type = "rate", year = "2020/21") %>%  mutate(split_value = "Secondary", split_name = "School type") %>% select(-na) 
la_2020_stage <- import_la_wide_data(filename = data_2020, sheetnum = "2.9", range = c(4:37), split_type = "split_value", value_type = "rate", year = "2020/21") %>% mutate(split_name = "School stage") 
la_2020_sex_all <- import_la_wide_data(filename = data_2020, sheetnum = "2.10", range = c(4:37), split_type = "split_value", value_type = "rate", year = "2020/21") %>% mutate(split_name = "Sex") 
la_2020_simd_all <- import_la_wide_data(filename = data_2020, sheetnum = "2.14", range = c(4:37), split_type = "split_value", value_type = "rate", year = "2020/21") %>% mutate(split_name = "Deprivation (SIMD)") 
# no count data for the LA x SIMD data, so unsure about inequals calcs.

# Combine the LA splits percent data
la_2020 <- rbind(la_2020_stage, la_2020_sex_all, la_2020_simd_all, la_1ry_2020, la_2ry_2020) 

# Drop intermediate dfs
rm(scot_rural_2020, scot_ethnic_2020, scot_simd_2020, scot_stages_2020_all, scot_2020_F, scot_2020_M)
rm(la_2020_stage, la_2020_sex_all, la_2020_simd_all, la_1ry_2020, la_2ry_2020)



#####################
# 2009 to 2018 data
#####################

# Import Scottish data

scot_2018 <- get_old_file_attainment_data_scotland("Attendance+and+Absence+201819+-+Excel+web+version.xlsx", "2018/19", range_scot = c(4:12), range_rural = c(4:10), range_ethnic = c(4:20))
scot_2016 <- get_old_file_attainment_data_scotland("attendance-absence-2016-17.xlsx", "2016/17", range_scot = c(4:12), range_rural = c(4:10), range_ethnic = c(4:20))
scot_2014 <- get_old_file_attainment_data_scotland("attendance-absence-2014-15.xls", "2014/15", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:20))
scot_2012 <- get_old_file_attainment_data_scotland("attendance-absence-2012-13.xls", "2012/13", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:20))
scot_2010 <- get_old_file_attainment_data_scotland("attendance-absence-2010-11.xls", "2010/11", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:27))
scot_2009 <- get_old_file_attainment_data_scotland("attendance-absence-2009-10.xls", "2009/10", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:27))

# Import LA data 

la_2018 <- get_old_file_attainment_data_la("Attendance+and+Absence+201819+-+Excel+web+version.xlsx", "2018/19", range_la="R4C1:R42C2")
la_2016 <- get_old_file_attainment_data_la("attendance-absence-2016-17.xlsx", "2016/17", range_la="R4C1:R42C2")
la_2014 <- get_old_file_attainment_data_la("attendance-absence-2014-15.xls", "2014/15", range_la="R5C1:R43C6")
la_2012 <- get_old_file_attainment_data_la("attendance-absence-2012-13.xls", "2012/13", range_la="R5C1:R43C6")
la_2010 <- get_old_file_attainment_data_la("attendance-absence-2010-11.xls", "2010/11", range_la="R5C1:R43C6")
la_2009 <- get_old_file_attainment_data_la("attendance-absence-2009-10.xls", "2009/10", range_la="R5C1:R43C6")


#####################
# 2006 to 2008 data
#####################

# Provided at school level, so can produce LA and Scotland rates, by school type, but no other splits 

la_1ry_2008 <- read_excel(paste0(attendance_folder, data_2008), sheet = "Primary", skip=1) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2008/09")
la_2ry_2008 <- read_excel(paste0(attendance_folder, data_2008), sheet = "Secondary", skip=1) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2008/09")
la_special_2008 <- read_excel(paste0(attendance_folder, data_2008), sheet = "Special", skip=1) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2008/09")

la_1ry_2007 <- read_excel(paste0(attendance_folder, data_2007), sheet = "Primary", skip=3) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2007/08")
la_2ry_2007 <- read_excel(paste0(attendance_folder, data_2007), sheet = "Secondary", skip=3) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2007/08")
la_special_2007 <- read_excel(paste0(attendance_folder, data_2007), sheet = "Special", skip=3) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2007/08")

la_1ry_2006 <- read_excel(paste0(attendance_folder, data_2006), sheet = "Primary", skip=2) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2006/07")
la_2ry_2006 <- read_excel(paste0(attendance_folder, data_2006), sheet = "Secondary", skip=2) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2006/07")
la_special_2006 <- read_excel(paste0(attendance_folder, data_2006), sheet = "Special", skip=2) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2006/07")

# Combine the school data and aggregate to LAs
la_2006to2008 <- bind_rows(la_1ry_2006, la_1ry_2007, la_1ry_2008, 
                           la_2ry_2006, la_2ry_2007, la_2ry_2008,
                           la_special_2006, la_special_2007, la_special_2008) %>%
  mutate(local_authority = coalesce(`Local authority`, LAName)) %>%
  select(local_authority, split_name, split_value, trend_axis,
         `Possible Attendance`, `In school` , `Late`, `Work experience`, `Sick with educational provision`) %>%
  # Replace suppressed code (*) with NA. 
  mutate(across(-c(local_authority, split_name, split_value, trend_axis), ~gsub("\\*", "NA", .))) %>% 
  mutate(across(-c(local_authority, split_name, split_value, trend_axis), ~as.numeric(.))) %>%
  rename(denominator = `Possible Attendance`) %>%
  mutate(numerator = `In school` + `Late` + `Work experience` + `Sick with educational provision`) %>%
  select(-c(`In school` , `Late`, `Work experience`, `Sick with educational provision`)) %>%
  group_by(local_authority, split_name, split_value, trend_axis) %>%
  summarise(numerator = sum_(numerator),
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  mutate(rate = 100 * numerator / denominator) 

# LA totals (needed for 2006, as we don't have trend data for LAs this year)
la_2006to2008_with_totals <- la_2006to2008 %>%
  group_by(local_authority, trend_axis) %>%
  summarise(numerator = sum_(numerator),
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  mutate(rate = 100 * numerator / denominator,
         split_value = "Total", 
         split_name = "Total") %>%
  rbind(la_2006to2008)


# Aggregate to Scotland (Scotland totals needed for 2006 and 2007 as we don't have these in the trend data)
scot_2006to2008_with_totals <- la_2006to2008_with_totals %>%
  group_by(split_name, split_value, trend_axis) %>%
  summarise(numerator = sum(numerator),
            denominator = sum(denominator)) %>%
  ungroup() %>%
  mutate(rate = 100 * numerator / denominator,
         code = "S00000001") 


rm(la_1ry_2006, la_1ry_2007, la_1ry_2008, 
   la_2ry_2006, la_2ry_2007, la_2ry_2008,
   la_special_2006, la_special_2007, la_special_2008)


# Combine the data from all years

# First the LA data
all_la_attendance <- do.call("bind_rows", mget(ls(pattern="^la_"))) %>%
  filter(!local_authority %in% c("xGrant maintained", "Grant Aided", "Grant-aided", "Scotland", "All local authorities")) %>%
  mutate(local_authority = gsub(" and ", " & ", local_authority),
         local_authority = case_when(local_authority == "Clackmannanshir" ~ "Clackmannanshire",      
                                      local_authority == "Dumfries & Gall"~ "Dumfries & Galloway",       
                                      local_authority == "East Dunbartons" ~ "East Dunbartonshire", 
                                      local_authority == "East Renfrewshi" ~ "East Renfrewshire",
                                      local_authority %in% c("Edinburgh City","Edinburgh, City of") ~ "City of Edinburgh",   
                                      local_authority == "Eilean Siar" ~ "Na h-Eileanan Siar", 
                                      local_authority == "North Lanarkshi" ~ "North Lanarkshire",       
                                      local_authority == "Scottish Border" ~ "Scottish Borders",
                                      local_authority == "Shetland Island" ~ "Shetland Islands",     
                                      local_authority == "South Lanarkshi" ~ "South Lanarkshire",     
                                      local_authority == "West Dunbartons" ~ "West Dunbartonshire",
                                      TRUE ~ local_authority)) %>%
  mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                TRUE ~ split_name)) %>%
  filter(!split_value == "SIMD not available [note 2]") %>%
  filter(!is.na(local_authority)) %>%
  filter(!is.na(split_value)) %>%
  filter(!is.na(split_name)) %>%
  filter(!rate==0) %>% # zeroes occur where there are no data (suppressed or non-existent, such as some SIMD quintiles on Orkney/Shetland)
  # add in area codes
  mutate(areatype = "Council area") %>%
  merge(y = geo_lookup, by.x=c("local_authority", "areatype"), by.y = c("areaname", "areatype")) %>%
  select(-areatype, -local_authority)

# Then the Scotland data 
all_scot_attendance <- do.call("bind_rows", mget(ls(pattern="^scot_"))) %>%
  mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                TRUE ~ split_name)) %>%
  filter(!is.na(split_value)) %>%
  filter(!is.na(split_name)) %>%
  mutate(split_value = case_when(split_value == "Accessible rural" ~ "Accessible rural areas", # update to the more recent wording used for the 6-fold classification
                                 split_value == "Accessible towns" ~ "Accessible small towns",
                                 split_value == "Large cities" ~ "Large urban areas",
                                 split_value == "Other urban" ~ "Other urban areas",
                                 split_value == "Remote rural" ~ "Remote rural areas",
                                 split_value == "Remote towns" ~ "Remote small towns",
                                 TRUE ~ split_value)) %>%
  filter(!split_name == "Ethnicity") %>% # too many different ethnic groupings over time to work out how to standardise right now: do in future
  filter(!split_value %in% c("Not known", "SIMD not available", "Not disclosed",
                             "Not disclosed/ not known", "SIMD not available [Note 2]", "SIMD not available [note 2]"   )) 

# Combine LA and Scotland
all_attendance <- rbind(all_la_attendance, all_scot_attendance) %>%
  mutate(split_value = case_when(split_value=="SIMD Quintile 1" ~ "1",
                                 split_value=="SIMD Quintile 2" ~ "2",
                                 split_value=="SIMD Quintile 3" ~ "3",
                                 split_value=="SIMD Quintile 4" ~ "4",
                                 split_value=="SIMD Quintile 5" ~ "5",
                                 TRUE ~ split_value)) %>%
  ## Filling in the denominator and numerator data if known for that grouping (there will be some duplicates, due to how the data have been imported, but these will be dropped later): 
  group_by(split_value, trend_axis, code) %>%
  mutate(numerator = max_(numerator), # filling in any count data if known for this grouping (most will be NA). 
          denominator = max_(denominator)) %>%
  ungroup() %>%
  mutate(rate = round(rate, digits = 1)) %>% # rounding so unique() can identify more identical records
  unique() %>% 
  # any duplicates remaining?
  group_by(split_name, split_value, trend_axis, code) %>%
  mutate(count = n()) %>%
  ungroup()  # no duplicates
# The numerator and denom data are still really patchy, which is not presentable on the app, so will drop later


# Totals are sometimes available, sometimes not. So here we want to add total rates more consistently:
# Every split_name x code x trend_axis grouping should get the same split_value==Total 
# Start by removing any totals that exist
all_attendance2 <- all_attendance %>%
   filter(!(split_value=="Total" & split_name!="Total")) %>%
   select(-count)

# get the groupings that need totals adding
splits_needing_totals <- all_attendance2 %>%
  select(code, trend_axis, split_name) %>%
  unique() 

# get the totals (by code and trend_axis)
totals <- all_attendance %>%
  filter(split_value=="Total" & split_name=="Total") %>%
  select(-split_name) %>%
  unique() %>% # should be one Total for each code - trend_axis grouping: let's check
  group_by(code, trend_axis, split_value) %>%
  mutate(count = n()) %>%
  ungroup() %>% # correct: no duplicates 
  select(-count)

# merge in the totals to the code-trend_axis-split_name groups 
splits_with_totals <- splits_needing_totals %>%
  merge(y = totals, by = c("code", "trend_axis"))

# add back into the attendance data
all_attendance3 <- all_attendance2 %>%
  rbind(splits_with_totals) %>%
  mutate(ind_id = 30140,
         trend_axis = gsub("-", "/", trend_axis), # standardise trend_axis labels
         def_period = paste0("School year (", trend_axis, ")"),
         year = as.numeric(substr(trend_axis, 1, 4)),
         upci = as.numeric(NA),
         lowci = as.numeric(NA),
         numerator = as.numeric(NA))  # remove any existing numerator data: too patchy to present
  
#################################################################################
# make a denominator file by year and SIMD quintile, for inequalities metrics:
#################################################################################

# Needs a bespoke file because attendance is measured in half days.
# requires quintiles as 1 to 5 and Total
# Only available for Scotland from 2020 onwards

depr_attendance_Scot_2020to2023 <- all_attendance3 %>%
  filter(split_name == "Deprivation (SIMD)") %>%
  select(year=trend_axis, code, quintile=split_value, denominator) %>%
  mutate(quint_type = "sc_quin",
         year = as.numeric(substr(year, 1, 4))) %>%
  filter(!is.na(denominator)) # leaves 18: 5 quintiles + a total for 3 years

saveRDS(depr_attendance_Scot_2020to2023, paste0(profiles_lookups, "/Population/depr_attendance_Scot_2020to2023.rds"))




##########################################################
### 3. Prepare final files -----
##########################################################

# 1 - main data (ie data behind summary/trend/rank tab)
main_data <- all_attendance3 %>% 
  filter(split_value == "Total" & split_name == "Total") %>%
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  unique() %>%
  arrange(code, year)

# Save
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny.csv"), row.names = FALSE) 


# 2 - population groups data (ie data behind population groups tab)
# Most deprivation data doesn't have denom data, so can't calc inequalities metrics, so will present all on popgroups tab for now
# Inequalities metrics can be calculated for 3 years, and these will be presented on the deprivation tab

pop_grp_data <- all_attendance3 %>% 
  filter(!split_name == "Total") %>%
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value) %>%
  mutate(split_value = case_when(split_name == "Deprivation (SIMD)" & split_value == 1 ~ "1 - most deprived",
                                 split_name == "Deprivation (SIMD)" & split_value == 5 ~ "5 - least deprived",
                                 TRUE ~ split_value)) %>%
  arrange(code, year)

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny_popgrp.csv"), row.names = FALSE)



# 3 - SIMD data (ie data behind deprivation tab)

# add population data (quintile level) so that inequalities can be calculated
simd_data <- all_attendance3 %>%
  filter(split_name == "Deprivation (SIMD)" & !is.na(denominator)) %>%
  mutate(quint_type = "sc_quin") %>%
  rename(quintile = split_value) %>%
  select(-denominator, -split_name) %>%
  arrange(code, year, quintile) %>%
  add_population_to_quintile_level_data(pop="depr_attendance_Scot_2020to2023",
                                        ind = 30140, ind_name = "school_attendance") %>%
  filter(!is.na(rate)) # not all years have data

# calculate the inequality measures
simd_data <- simd_data |>
  calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
  select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields

# save the data as RDS file
saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_ineq.rds"))


##########################################################
### 4. Run QA reports -----
##########################################################

# # Run QA reports 

run_qa(type ="main",filename="school_attendance", test_file=FALSE)

run_qa(type = "deprivation", filename="school_attendance", test_file=FALSE)





# Extra bits and bobs that might be useful in the future:

# Not used currently (not enough data prior to 2023)  

# # Function to import and process LA half-days data (counts) 
# import_LA_halfdays <- function(filename, sheetnum, range, year) {
#   
#   if (is.character(range)) { # i.e., format "R4C1:R38C9"
#     df <- read_excel(paste0(attendance_folder, filename),
#                      sheet = paste0("Table ", sheetnum),
#                      range = range
#     )
#   } else { # i.e., only rows specified like range = c(4, 37)
#     df <- read_excel(paste0(attendance_folder, filename),
#                      sheet = paste0("Table ", sheetnum),
#                      range = cell_rows(range)
#     ) 
#   }
#   
#   df <- df %>%
#     mutate(across(-1, ~str_replace(., "z", "0"))) %>% # added to another column, so a true zero
#     mutate(across(-1, ~str_replace(., "c", "NA"))) %>% # suppressed
#     mutate(across(-1, ~as.numeric(.))) %>%
#     mutate(trend_axis = year) %>%
#     clean_names() %>%
#     setNames(substr(colnames(.), 1, 20)) %>% # cuts the notes off the end of the column names, if present, and makes col names consistent across tabs
#     rename(denominator = possible_attendance) %>%
#     mutate(numerator = attendance_in_school + attendance_late + attendance_work_expe + attendance_sickness_) %>% # gives NA if any missings (i.e., suppressed)
#     mutate(rate = 100 * numerator / denominator) %>%
#     select(-starts_with("attendance"))
# }

# # get denoms and numerators to calc rates (2023/24)
# some suppression means can't be done for all LAs. And not provided in earlier years, so of limited use.
# la_2023_all_counts <- import_LA_halfdays(filename = data_2023, sheetnum = "2.5", rowrange = c(5:40), colrange = c(1:6), year = "2023/24") %>% mutate(split_value = "Total")
# la_2023_1ry_counts <- import_LA_halfdays(filename = data_2023, sheetnum = "2.6", rowrange = c(5:40), colrange = c(1:6), year = "2023/24") %>% mutate(split_value = "Primary")
# la_2023_2ry_counts <- import_LA_halfdays(filename = data_2023, sheetnum = "2.7", rowrange = c(5:40), colrange = c(1:6), year = "2023/24") %>% mutate(split_value = "Secondary")
# la_2023_schtype_counts <- rbind(la_2023_1ry_counts, la_2023_2ry_counts, la_2023_all_counts) %>%
#   mutate(split_name = "School type") %>%
#   filter(!local_authority %in% c("All local authorities", "Grant Aided")) 
# 
# rm(la_2023_1ry_counts, la_2023_2ry_counts, la_2023_all_counts)




# # merge percents and counts: can then derive accurate numerators (bypassing the suppression on some columns (attendance reasons with small numbers) as not disclosive when aggregated) and calculate CIs
# la_2023_schtype <- la_2023_schtype_percents %>% # percents are all complete and accurate here, based on un-suppressed counts
#   merge(y = la_2023_schtype_counts, # denominator counts are complete here, but numerator counts were derived from some columns with suppression, so should be back_calculated from the percentages
#         by = c("local_authority", "trend_axis", "split_value")) %>% # keeps only 1ry and 2ry (no totals)
#   select(-rate.y, -numerator) %>%
#   mutate(numerator = round(denominator * rate.x/100)) %>%
#   select(-rate.x)
# la_2023_schtype <- la_2023_schtype %>%
#   select(-split_value) %>%
#   group_by(local_authority, trend_axis) %>%
#   summarise(across(everything(), sum)) %>%
#   ungroup() %>%
#   mutate(split_value = "Total") %>%
#   rbind(la_2023_schtype) %>%
#   mutate(rate_derived = 100 * numerator/denominator)
# #check these rates against the published rates 
# check <- la_2023_schtype %>%
#   merge(y = la_2023_schtype_percents, by = c("local_authority", "trend_axis", "split_value"), all.x=TRUE) %>%
#   merge(y = la_trend_all, by = c("local_authority", "trend_axis", "split_value"), all.x=TRUE) %>%
#   mutate(rate_published = coalesce(rate.x, rate.y),
#          rate_diff = round(rate_derived, 1) - round(rate_published, 1))
# # Conclusion: valid approach to use for 1ry and 2ry attendance counts (gives precise match), but not for the overall totals (1ry plus 2ry) 
# # The overall totals can be slightly inaccurate for some LAs (+/- 0.1 % point) due to our non-inclusion of the counts for special schools (~1% of all possible attendances)
# # Do not use derived numerators for LA totals, only use for 1ry and 2ry school figures. 






##END
