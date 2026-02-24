## SCHOOL ATTENDANCE: THIS SCRIPT READS IN OLD DATA AND SAVES IT, SO CAN BE READ BACK IN WHEN UPDATING. NO NEED TO RUN AGAIN.
## Saves going through the convoluted processing each time.

# data source: https://www.gov.scot/publications/school-attendance-and-absence-statistics/
# The files for individual years are downloaded to "data received" folder. 


#################################################################################
### Packages and lookups -----
#################################################################################

library(readxl) # handles xls and xlsx in same read function, which is needed here
library(hablar) # sum_ function from hablar keeps NA when there should be NA
library(here) # for filepaths

source(here("functions", "main_analysis.R")) # needed for the lookup

# Read in geography lookup
geography_lookups <- here(profiles_data_folder, "Lookups", "Geography")
geo_lookup <- readRDS(here(geography_lookups, "opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


#################################################################################
### Paths to files -----
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



#################################################################################
### Helper functions -----
#################################################################################

# A. Importing and processing wide format data ----


# Function to import LA data (percents) from a wide format tab
import_la_wide_data <- function(filename, sheetnum, range, split_type, value_type, year) {
  
  if (is.character(range)) { # i.e., format "R4C1:R38C9"
    df <- read_excel(here(attendance_folder, filename),
                     sheet = paste0("Table ", sheetnum),
                     range = range
    )
  } else { # i.e., only rows specified like range = c(4, 37)
    df <- read_excel(here(attendance_folder, filename),
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


# B. Importing and processing older format data (2009/10 to 2018/19) ----

# Function to import old format Scottish data (counts)
get_old_file_attendance_data_scotland <- function(filename, trend_axis, range_scot, range_rural, range_ethnic) {
  
  # Just one sheet with counts: Stage (1ry, 2ry, Special, Total) and Sex
  scot_counts <- read_excel(here(attendance_folder, filename),
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
  scot_rural <- read_excel(here(attendance_folder, filename), sheet = "Table 1.6", range = cell_rows(range_rural)) %>%
    rename(rate = Attendance, 
           split_value = ...1) %>%
    mutate(code = "S00000001",
           split_name = "Urban/Rural classification",
           trend_axis = trend_axis) %>%
    select(-contains(c("Attendance", "Absence")))
  
  # Ethnicity (percents)
  scot_ethnic <- read_excel(here(attendance_folder, filename), sheet = "Table 1.11", range = cell_rows(range_ethnic)) %>%
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


#################################################################################
### Import the data -----
#################################################################################


# Get population splits data:
##################################

# Requires going through the spreadsheets to extract year-specific data.
# 2009/10 to 2018/19 use a different format, so these use different global import functions.
# LA data don't have numerators and denominators, so can't aggregate to HB, HSCP or PD.


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

la_1ry_2008 <- read_excel(here(attendance_folder, data_2008), sheet = "Primary", skip=1) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2008/09")
la_2ry_2008 <- read_excel(here(attendance_folder, data_2008), sheet = "Secondary", skip=1) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2008/09")
la_special_2008 <- read_excel(here(attendance_folder, data_2008), sheet = "Special", skip=1) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2008/09")

la_1ry_2007 <- read_excel(here(attendance_folder, data_2007), sheet = "Primary", skip=3) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2007/08")
la_2ry_2007 <- read_excel(here(attendance_folder, data_2007), sheet = "Secondary", skip=3) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2007/08")
la_special_2007 <- read_excel(here(attendance_folder, data_2007), sheet = "Special", skip=3) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2007/08")

la_1ry_2006 <- read_excel(here(attendance_folder, data_2006), sheet = "Primary", skip=2) %>% mutate(split_name = "School type", split_value = "Primary", trend_axis = "2006/07")
la_2ry_2006 <- read_excel(here(attendance_folder, data_2006), sheet = "Secondary", skip=2) %>% mutate(split_name = "School type", split_value = "Secondary", trend_axis = "2006/07")
la_special_2006 <- read_excel(here(attendance_folder, data_2006), sheet = "Special", skip=2) %>% mutate(split_name = "School type", split_value = "Special", trend_axis = "2006/07")

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

##########################################################
### Combine and save the data -----
##########################################################

# First the LA data
la_attendance_2006to2018 <- do.call("bind_rows", mget(ls(pattern="^la_"))) 
write_rds(la_attendance_2006to2018, here(attendance_folder, "la_attendance_2006to2018.rds"))
  
# Then the Scotland data 
scot_attendance_2006to2018 <- do.call("bind_rows", mget(ls(pattern="^scot_"))) 
write_rds(scot_attendance_2006to2018, here(attendance_folder, "scot_attendance_2006to2018.rds"))




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
