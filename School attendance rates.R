####################################
# WAITING ON THE SIMD DATA FROM SG
####################################

# TO DO:
# # too many different ethnic groupings over time to work out how to standardise right now: do in future
# Possibility that this data could be sourced from Stephen.Snedker@phs.scot who manages PHAL dashboard, this team exploring
# obtaining an extract from SG which would be pupil level and could save data manipulation? possibly not worth the effort now the code has been written
# but worth being aware that another PHS dashboard contains the same indicator but sourced differently and may not match exactly with our methodology
# PHAL team plan on matching SG attendance data to CHSP data so mismatches/non-matches might result in slightly different figures.

### 1. notes -----

# this script updates the following indicator: 30140 - School attendance rate: Percentage school attendance by primary and secondary pupils in the past year
# NB. Primary and secondary school attendance were 2 separate indicators previously (ind_id 11201 and 20603 respectively) 
# but these haven't been updated since 2018.

# data source: https://www.gov.scot/publications/school-attendance-and-absence-statistics/
# The files for individual years are downloaded to "data received" folder. 
# Update file names and ranges below when new data are saved in "data received" folder.
# Latest data (2023/24) published March 2025
# Time series data by SIMD (for LAs) provided by SG in August 2025

# The data spreadsheets are in various formats, so importing the data is convoluted...


#################################################################################
### 1. Packages and lookups -----
#################################################################################

library(readxl) # handles xls and xlsx in same read function, which is needed here
library(hablar) # sum_ function from hablar keeps NA when there should be NA
library(here) # for filepaths

source(here("functions", "main_analysis.R")) # needed for the QA
source(here("functions", "deprivation_analysis.R")) # needed for the QA

# Read in geography lookup
geography_lookups <- here(profiles_data_folder, "Lookups", "Geography")
geo_lookup <- readRDS(here(geography_lookups, "opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# create lookup for higher geogs: get simd lookup, and aggregate (lowest geog is CA)
higher_geog_lookup <- readRDS(here(geography_lookups, "simd_datazone_lookup.rds")) %>%
  select(year, code = ca, hb, hscp, pd, scotland) %>%
  unique()


#################################################################################
### 2. Paths to files -----
#################################################################################

# the folder where the data are saved
attendance_folder <- here(profiles_data_folder, "Received Data", "School attendance")

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
data_2024 <- "Attendance+and+absence+2024-25.xlsx"                   
data_simd2023 <- "AAE014_attendance_by_simd_final.xlsx" 
data_simd2024 <- "AAE022_attendance_by_simd_for_phs_final.xlsx"


#################################################################################
### 3. Helper functions -----
#################################################################################

# A. Importing and processing wide format data ----

# Function to get Scotland data from a wide format tab
import_scot_wide_data <- function(filename, sheetnum, rowrange, split_type, value_type) {
  df <- read_excel(here(attendance_folder, filename),
                  sheet = paste0("Table ", sheetnum),
                  range = cell_rows(rowrange)) %>%
    mutate(across(-1, ~str_replace(., "z", "NA"))) %>% # z = not applicable (changed this: used to be recoded to 0)
    mutate(across(-1, ~str_replace(., "c", "NA"))) %>% # c = suppressed
    # Any cells with c or z in them are summed with others, using na.rm=TRUE, so the sums will undercount by the amount being suppressed, but the overall totals are large so perecntages will not be affected
    pivot_longer(-1, names_to = split_type, values_to = value_type) %>%
    mutate(code = "S00000001")
}


# Function to import LA data (percents) from a wide format tab
import_la_wide_data <- function(filename, sheetnum, range, split_type, value_type, year) {
  
  if (is.character(range)) { # i.e., format "R4C1:R38C9"
    df <- read_excel(here(attendance_folder, filename),
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
    mutate(across(-1, ~str_replace(., "z", "NA"))) %>% # z = not applicable (changed this: used to be recoded to 0, but some LA don't have special schools, so more correct to list their attendance as NA than 0)
    mutate(across(-1, ~str_replace(., "c", "NA"))) %>% # c = suppressed
    mutate(across(-1, ~as.numeric(.))) %>%
    pivot_longer(-1, names_to = split_type, values_to = value_type) %>%
    clean_names() 
  
  if (split_type!="trend_axis")  {
    
    df <- df %>%  mutate(trend_axis = year)
    
  }
  
  df
  
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
      rowwise() %>%
      mutate(numerator = sum(across(attendance_in_scho:attendance_sicknes), na.rm=TRUE)) %>%  
      ungroup() %>%
      calculate_percent() %>%
      # NB. we're only deriving rates from the numerator and denominator data for Scotland, as there are no suppression issues and the % produced match the published data exactly (while we get additional numerator data for the app)
      select(-contains("attendance"))
      
  } else { # if COVID year data
    
    df <- df %>%
      mutate(attendance_absence_reason = substr(attendance_absence_reason, 1, 41)) %>% #longer reasons in covid data
      pivot_wider(names_from = attendance_absence_reason, values_from = count) %>%
      clean_names() %>%
      rename(denominator = possible_attendance_schools_open) %>%
      rowwise() %>%
      mutate(numerator = sum(across(attendance_schools_open_in_school:attendance_schools_open_covid_19_self), na.rm=TRUE)) %>%  
      ungroup() %>%
      calculate_percent() %>%
      select(-contains("attendance"))
    
  }
}

# Grouping these functions together to read in recent data with a similar format (from 2020)

# Import recent Scottish data (since 2020)
import_scot_recent_data <- function(filename, year, 
                                    range_stages, range_F, range_M, range_rural, range_ethnic) {
  
  # Stage (P years, S years, 1ry, 2ry, Special, Total)
  stages <- import_scot_halfdays(filename = filename, sheetnum = "1.2", rowrange = range_stages, 
                                 split_type = "split_value", year = year) %>% mutate(split_name = "School stage")
  
  # Sex female
  female <- import_scot_halfdays(filename = filename, sheetnum = "1.3", rowrange = range_F, 
                                 split_type = "split_value", year = year) %>% 
    filter(split_value == "Total") %>% mutate(split_value = "Female", split_name = "Sex")
  
  # Sex male
  male <- import_scot_halfdays(filename = filename, sheetnum = "1.3", rowrange = range_M, 
                               split_type = "split_value", year = year) %>% 
    filter(split_value == "Total") %>% mutate(split_value = "Male", split_name = "Sex")
  
  # Urban/Rural (includes NK and Total)
  rural <- import_scot_halfdays(filename = filename, sheetnum = "1.4", rowrange = range_rural, 
                                split_type = "split_value", year = year) %>% mutate(split_name = "Urban/Rural classification")
  
  # Ethnicity (includes NK, Not disclosed, and Total)
  ethnic <- import_scot_halfdays(filename = filename, sheetnum = "1.11", rowrange = range_ethnic, 
                                 split_type = "split_value", year = year) %>% mutate(split_name = "Ethnicity")
  
  # Combine Scottish splits data
  scot_splits <- rbind(stages, female, male, rural, ethnic)
  
}


# Import recent LA data (since 2020)
import_la_recent_data <- function(filename, year, 
                                  range_1ry, range_2ry, range_stages, range_sex) {
  
  primary <- import_la_wide_data(filename = filename, sheetnum = "2.1", range = range_1ry, split_type = "NA", value_type = "rate", year = year) %>%  
    mutate(split_value = "Primary", split_name = "School type") %>% select(-na) 
  
  secondary <- import_la_wide_data(filename = filename, sheetnum = "2.2", range = range_2ry, split_type = "NA", value_type = "rate", year = year) %>%  
    mutate(split_value = "Secondary", split_name = "School type") %>% select(-na) 
  
  stage <- import_la_wide_data(filename = filename, sheetnum = "2.9", range = range_stages, split_type = "split_value", value_type = "rate", year = year) %>% 
    mutate(split_name = "School stage") 
  
  sex <- import_la_wide_data(filename = filename, sheetnum = "2.10", range = range_sex, split_type = "split_value", value_type = "rate", year = year) %>% 
    mutate(split_name = "Sex") 
  
  # Combine LA splits data
  la_splits <- rbind(primary, secondary, stage, sex) 
  
}



# B. Importing and processing older format data (2009/10 to 2018/19) ----

# Function to import old format Scottish data (counts)
get_old_file_attendance_data_scotland <- function(filename, trend_axis, range_scot, range_rural, range_ethnic) {
  
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
    calculate_percent() %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ "Sex")) %>%
    mutate(split_value = case_when(split_value %in% c("Females", "Girls") ~ "Female",
                                   split_value %in% c("Males", "Boys") ~ "Male",
                                   TRUE ~ split_value)) %>%
    select(-contains(c("attendance", "absence", "denominator")))
  
  # Urban/Rural (percents)
  scot_rural <- read_excel(paste0(attendance_folder, filename), sheet = "Table 1.6", range = cell_rows(range_rural)) %>%
    rename(rate = Attendance, 
           split_value = ...1) %>%
    mutate(code = "S00000001",
           split_name = "Urban/Rural classification",
           trend_axis = trend_axis) %>%
    select(-contains(c("Attendance", "Absence")))
  
  # Ethnicity (percents)
  scot_ethnic <- read_excel(paste0(attendance_folder, filename), sheet = "Table 1.11", range = cell_rows(range_ethnic)) %>%
    rename(rate = Attendance, 
           split_value = ...1) %>%
    mutate(code = "S00000001",
           split_name = "Ethnicity",
           trend_axis = trend_axis) %>%
    select(-contains(c("Attendance", "Absence", "pupils")))
  
  # Combine the counts for all splits
  scot_splits <- 
    bind_rows(scot_rural, scot_ethnic) %>%
    mutate(lowci = as.numeric(NA),
           upci = as.numeric(NA),
           numerator = as.numeric(NA)) %>%
    rbind(scot_counts) %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ split_name))
  
  
}


# Function to import old format LA data
get_old_file_attendance_data_la <- function(filename, trend_axis, range_la) {
  
  # 2010/11 LA data (percents from wide) (NB. ranges and post-processing differ here from later files)
  la_1ry <- import_la_wide_data(filename, sheetnum = "2.1", range = range_la, split_type = "na", value_type = "rate", year = trend_axis) %>%  
    filter(na %in% c("Attendance", "Attendance...6")) %>% select(-na) %>% mutate(split_value = "Primary", split_name = "School type")  
  la_2ry <- import_la_wide_data(filename, sheetnum = "2.2", range = range_la, split_type = "na", value_type = "rate", year = trend_axis) %>%  
    filter(na %in% c("Attendance", "Attendance...6")) %>% select(-na) %>% mutate(split_value = "Secondary", split_name = "School type") 

  # combine the LA splits percent data
  la_splits <- rbind(la_1ry, la_2ry) %>%
    rename(local_authority = x1) %>%
    filter(!local_authority %in% c("All local authorities", "Grant Aided")) %>%
    mutate(split_name = case_when(split_value %in% c("Primary", "Secondary", "Special") ~ "School type",
                                  TRUE ~ split_name))
  
}


# C. Importing and processing the standalone SIMD data file, provided in Aug 2025: 

# Function to import the SIMD data (Scotland and LA)
get_simd_data <- function(tab_name, simd_file, colnames) {
  
  df <- read_excel(here(attendance_folder, simd_file), sheet = tab_name) %>%
    mutate(year = as.numeric(substr(tab_name, nchar(tab_name)-3, nchar(tab_name))) - 1) # years in the tab are the end of the sch year, not the start
  names(df) <- colnames
  if(!"NA" %in% colnames) {
    df <- df %>%
      mutate("NA" = 0)
  }
  df_name <- paste0("tab_", tab_name)
  assign(df_name, df, envir=.GlobalEnv)
  
}


#################################################################################
### 4. Import the data -----
#################################################################################


#################################
## Import SIMD data (and use for area totals too)
#################################

# Counts from all stages combined and all school types combined (Primary, Secondary and Special)

# run the function
sheets2023 <- readxl::excel_sheets(here(attendance_folder, data_simd2023))
#sheets2023 <- sheets2023[2:length(sheets2023)] # drop the cover sheet, keep remaining tabs
for (tab in sheets2023) {
  get_simd_data(tab, simd_file = data_simd2023, colnames = c("areaname", "1", "2", "3", "4", "5", "NA", "year"))
}

sheets2024 <- readxl::excel_sheets(here(attendance_folder, data_simd2024))
for (tab in sheets2024) {
  get_simd_data(tab, simd_file = data_simd2024, colnames = c("areaname", "1", "2", "3", "4", "5", "NA", "year"))
}

# CAN'T COMBINE CURRENTLY: c USED IN MOST RECENT DATA
# combine the numerator tabs and the denominator tabs
numerator_data <- mget(ls(pattern = "tab_att_"), .GlobalEnv) %>% # gets the dataframes starting with tab_att_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  pivot_longer(-c(areaname, year), names_to="quintile", values_to = "numerator")

denominator_data <- mget(ls(pattern = "tab_poss_"), .GlobalEnv) %>% # gets the dataframes starting with tab_poss_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  pivot_longer(-c(areaname, year), names_to="quintile", values_to = "denominator")

# remove unwanted df
rm(list=ls(pattern="tab_"))

simd_scot_and_ca <- numerator_data %>%
  merge(y = denominator_data, by = c("areaname", "year", "quintile"), all = TRUE) %>% # checked: no extra rows added, perfect match
  mutate(areatype = ifelse(areaname=="Scotland", "Scotland", "Council area"),
         areaname = gsub(" and ", " & ", areaname)) %>%
  filter(areaname != "Grant Aided") %>% # these are included in the Scotland totals (this is the default in the published data)
  merge(y = geo_lookup, by = c("areaname", "areatype"), all.x=TRUE) %>%
  select(-areatype, -areaname) 

# make totals 
# N.B. Small boards without every SIMD quintile (e.g., Shetland, Orkney) still can have children attending school from quintiles not represented on the island: 
# counts are sometimes 0 and sometimes not, so 0 is more appropriate than NA here
simd_scot_and_ca <- simd_scot_and_ca %>%
  group_by(year, code) %>%
  summarise(numerator = sum(numerator, na.rm=T), # No NAs in the provided data, but could be in future? 
            denominator = sum(denominator, na.rm = T)) %>%
  ungroup() %>%
  mutate(quintile = "Total") %>% # includes pupils where quintile is not known (implications for inequalities calc?)
  rbind(simd_scot_and_ca) %>%
  filter(quintile != "NA") 

# add higher geogs
simd_higher <- simd_scot_and_ca %>%
  filter(code!="S00000001") %>%
  # join data with lookup
  left_join(higher_geog_lookup, by = c("code", "year"))

# Function to aggregate CA data to higher geogs
aggregate_higher <- function(df, geog) {
  
  df <- df %>%
    select(year, quintile, code=geog, numerator, denominator) %>%
    group_by(year, quintile, code) %>%
    summarise(numerator = sum(numerator, na.rm=TRUE),
              denominator = sum(denominator, na.rm=TRUE)) %>%
    ungroup()
  
}

# Run the function
simd_hb <- aggregate_higher(simd_higher, "hb")
simd_hscp <- aggregate_higher(simd_higher, "hscp")
simd_pd <- aggregate_higher(simd_higher, "pd")

# combine all simd data
simd_all <- rbind(simd_scot_and_ca,
                  simd_hb,
                  simd_pd,
                  simd_hscp) %>%
  mutate(quint_type = "sc_quin",
         trend_axis = paste0(year, "/", as.character(substr(year+1, 3, 4))),
         def_period = paste0("School year (", trend_axis, ")"),
         ind_id = 30140) %>%
  calculate_percent() %>%
  mutate(across(everything(), ~replace(., is.nan(.), NA))) #replace rate and CIs with NA for cases with 0 denominator

# calculate the inequality measures
simd_all <- simd_all |>
  calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
  select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields

# save the data as RDS file
saveRDS(simd_all, here(profiles_data_folder, "Data to be checked", "school_attendance_ineq.rds"))

# Total counts (which include the pupils where SIMD is not known) match the published totals for Scotland and LAs,
# so can be used for these instead of reading in other data: 

# Prepare main data (ie data behind summary/trend/rank tab)
main_data <- simd_all %>% 
  filter(quintile=="Total") %>%
  mutate(def_period = paste0("School year (", trend_axis, ")")) %>%
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  arrange(code, year)

# Save
write_rds(main_data, here(profiles_data_folder, "Data to be checked", "school_attendance_shiny.rds"))
write.csv(main_data, here(profiles_data_folder, "Data to be checked", "school_attendance_shiny.csv"), row.names = FALSE) 


# # Run QA reports 
run_qa(type ="main",filename="school_attendance", test_file=FALSE)
run_qa(type = "deprivation", filename="school_attendance", test_file=FALSE)





# Get population splits data:
##################################

# Requires going through the spreadsheets to extract year-specific data.
# Since 2020/21: similar format, so can use the global import functions defined above for these recent files. 
# 2009/10 to 2018/19 use a different format, so these use different global import functions.
# When adding new data: check what ranges need to be read in, and include these in the appropriate function argument.

##########################
# 2020/21 to 2024/25 data
##########################

# Import Scottish data

scot_2024 <- import_scot_recent_data(data_2024, year="2024/25", 
                                     range_stages=c(5:10), range_F=c(5:10), range_M=c(22:27), range_rural=c(5:10), range_ethnic=c(5:10))
scot_2023 <- import_scot_recent_data(data_2023, year="2023/24", 
                                     range_stages=c(5:10), range_F=c(5:10), range_M=c(22:27), range_rural=c(5:10), range_ethnic=c(5:10))
scot_2022 <- import_scot_recent_data(data_2022, year="2022/23", 
                                     range_stages=c(5:10), range_F=c(5:10), range_M=c(22:27), range_rural=c(5:10), range_ethnic=c(5:10))
scot_2020 <- import_scot_recent_data(data_2020, year="2020/21", 
                                     range_stages=c(7:15), range_F=c(7:15), range_M=c(31:39), range_rural=c(7:15), range_ethnic=c(7:15))



# Import LA data 

la_2024 <- import_la_recent_data(filename = data_2024, year = "2024/25", 
                                 range_1ry = "R2C1:R36C2", range_2ry = "R2C1:R36C2", range_stages = c(2:36), range_sex = c(4:38))
la_2023 <- import_la_recent_data(filename = data_2023, year = "2023/24", 
                                 range_1ry = "R2C1:R37C2", range_2ry = "R2C1:R37C2", range_stages = c(2:37), range_sex = c(4:39))
la_2022 <- import_la_recent_data(filename = data_2022, year = "2022/23", 
                                 range_1ry = "R2C1:R37C2", range_2ry = "R2C1:R37C2", range_stages = c(2:36), range_sex = c(4:38))
la_2020 <- import_la_recent_data(filename = data_2020, year = "2020/21", 
                                 range_1ry = "R4C1:R37C2", range_2ry = "R4C1:R37C2", range_stages = c(4:37), range_sex = c(4:37))


#####################
# 2009/10 to 2018/19 data
#####################

# Import Scottish data

scot_2018 <- get_old_file_attendance_data_scotland(filename = "Attendance+and+Absence+201819+-+Excel+web+version.xlsx", trend_axis = "2018/19", range_scot = c(4:12), range_rural = c(4:10), range_ethnic = c(4:20))
scot_2016 <- get_old_file_attendance_data_scotland("attendance-absence-2016-17.xlsx", "2016/17", range_scot = c(4:12), range_rural = c(4:10), range_ethnic = c(4:20))
scot_2014 <- get_old_file_attendance_data_scotland("attendance-absence-2014-15.xls", "2014/15", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:20))
scot_2012 <- get_old_file_attendance_data_scotland("attendance-absence-2012-13.xls", "2012/13", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:20))
scot_2010 <- get_old_file_attendance_data_scotland("attendance-absence-2010-11.xls", "2010/11", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:27))
scot_2009 <- get_old_file_attendance_data_scotland("attendance-absence-2009-10.xls", "2009/10", range_scot = c(4:12), range_rural = c(4:11), range_ethnic = c(4:27))

# Import LA data 

la_2018 <- get_old_file_attendance_data_la("Attendance+and+Absence+201819+-+Excel+web+version.xlsx", "2018/19", range_la="R4C1:R42C2")
la_2016 <- get_old_file_attendance_data_la("attendance-absence-2016-17.xlsx", "2016/17", range_la="R4C1:R42C2")
la_2014 <- get_old_file_attendance_data_la("attendance-absence-2014-15.xls", "2014/15", range_la="R5C1:R43C6")
la_2012 <- get_old_file_attendance_data_la("attendance-absence-2012-13.xls", "2012/13", range_la="R5C1:R43C6")
la_2010 <- get_old_file_attendance_data_la("attendance-absence-2010-11.xls", "2010/11", range_la="R5C1:R43C6")
la_2009 <- get_old_file_attendance_data_la("attendance-absence-2009-10.xls", "2009/10", range_la="R5C1:R43C6")


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
  ungroup() 

# LA totals (needed for 2006, as we don't have trend data for LAs this year)
la_2006to2008_with_totals <- la_2006to2008 %>%
  group_by(local_authority, trend_axis) %>%
  summarise(numerator = sum_(numerator),
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  mutate(split_value = "Total", 
         split_name = "Total") %>%
  rbind(la_2006to2008)  %>%
  calculate_percent()


# Aggregate to Scotland (Scotland totals needed for 2006 and 2007 as we don't have these in the trend data)
scot_2006to2008_with_totals <- la_2006to2008_with_totals %>%
  group_by(split_name, split_value, trend_axis) %>%
  summarise(numerator = sum_(numerator),
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  calculate_percent() %>%
  mutate(code = "S00000001") 


rm(la_1ry_2006, la_1ry_2007, la_1ry_2008, 
   la_2ry_2006, la_2ry_2007, la_2ry_2008,
   la_special_2006, la_special_2007, la_special_2008,
   la_2006to2008)





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
  filter(!is.na(local_authority)) %>%
  filter(!is.na(split_value)) %>%
  filter(!is.na(split_name)) %>%
  filter(!rate==0) %>% # zeroes occur where there are no data (suppressed or non-existent)
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
  filter(!split_value %in% c("Not known", "Not disclosed", "Not disclosed/ not known")) 

# Combine LA and Scotland
all_attendance <- rbind(all_la_attendance, all_scot_attendance) %>%
  ## Filling in the denominator and numerator data if known for that grouping (there will be some duplicates, due to how the data have been imported, but these will be dropped later): 
  group_by(split_value, trend_axis, code) %>%
  mutate(numerator = max_(numerator), # filling in any count data if known for this grouping (most will be NA). 
          denominator = max_(denominator)) %>%
  ungroup() %>%
  group_by(split_name, split_value, trend_axis, code) %>% # check for duplicates
  mutate(count = n()) %>%
  ungroup()  # no duplicates


# Totals are sometimes available, sometimes not. So here we want to add total rates more consistently:
# Every split_name x code x trend_axis grouping should get the same split_value==Total 
# Start by removing any totals that exist
all_attendance2 <- all_attendance %>%
   filter(split_value!="Total") %>%
   select(-count)

# get the groupings that need totals adding
splits_needing_totals <- all_attendance2 %>%
  select(code, trend_axis, split_name) %>%
  unique() 

# get the totals (by code and trend_axis)
totals <- bind_rows(all_attendance, simd_all) %>%
  filter((split_value=="Total" & split_name == "Total") |
           (quintile=="Total")) %>%
  mutate(split_value = "Total") %>%
  unique() %>% # should be one Total for each code - trend_axis grouping: let's check
  group_by(code, trend_axis, split_value) %>%
  mutate(count = n()) %>%
  ungroup() %>% # correct: no duplicates 
  select(code, trend_axis, split_value, rate, lowci, upci, numerator, denominator)


# merge in the totals to the code-trend_axis-split_name groups 
splits_with_totals <- splits_needing_totals %>%
  merge(y = totals, by = c("code", "trend_axis"))

# add back into the attendance data
all_attendance3 <- all_attendance2 %>%
  rbind(splits_with_totals) %>%
  mutate(ind_id = 30140,
         trend_axis = gsub("-", "/", trend_axis), # standardise trend_axis labels
         def_period = paste0("School year (", trend_axis, ")"),
         year = as.numeric(substr(trend_axis, 1, 4)))  
  

##########################################################
### 3. Prepare final popgrp file -----
##########################################################


# Population groups data (ie data behind population groups tab)

pop_grp_data <- all_attendance3 %>% 
  filter(!split_name == "Total") %>%
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value) %>%
  arrange(code, year)

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_attendance_shiny_popgrp.csv"), row.names = FALSE)

## Run QA report 

run_qa(type ="popgrp", filename="school_attendance", test_file=FALSE)





# Extra bits and bobs that might be useful in the future:
# (Currently just possible for data since 2023, so would introduce inconsistency into the time series)

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
