#########################################################
# Scottish Crime and Justice Survey data import
#########################################################

### Update ScotPHO indicators sourced from Scottish Crime and Justice Survey: 
### Author: Liz Richardson, 6 Nov 2024

### Adult mental health indicators:
#   30030: Adults perceiving local crime to be common (Percentage of adults who perceive crime to be very common or fairly common in their local area)
#   30029: Adult victims of non-violent crime (survey-based) (Percentage of adults who have been a victim of non-violent crime in the last 12 months in their local area)
#   30057: Adult victims of violent crime (survey-based) (Percentage of adults who experienced a violent crime in the past year)
#   30056: Adult victims of domestic violence (survey-based) (Percentage of adults who experienced abuse by a partner or ex-partner in the past year)

### CYP mental health indicators:
#   30168: Children exposed to domestic abuse in household
#          (Percentage of adults experiencing physical or psychological abuse in last 12 months who both 
#          have children living in household and report that children were present (in or around the house or close by) 
#          during the most recent incident)

### Notes on the data source:
# Spreadsheets containing the data 2008 to 2021 provided in 2024 (and one revised in 2025) by Stuart Napier (Stuart.Napier@gov.scot).
# PHS - data request - march 2024 - mental health indicators - updated March 2025.xlsx
# PHS - data request - march 2024 - mental health indicators - only partner abuse tables.xlsx
# PHS - data request - September 2024 - mental health indicators - partner abuse with children present.xlsx

# Stuart Napier's emails accompanying the data are in the received data folder. 
# Partner abuse data not updated as part of self-completion module: due to data collection differences and small numbers.
# I have included a variable called ‘qaco_any’ which represents the proportion of people who answered that at least one type of crime or 
# behaviour was common in their local area. You will see that I have included all 16 ‘qaco’ variables, the bank of questions has changed slightly over time, 
# so I included all in order to make the ‘any’ variable representative over the whole time series. 
# This also explains why some variables are missing for entire years. 
# However, in the police division tables there might be some individual results which show blank cells, 
# this is because 0 people in that particular division will have answered in that way.
#  
# Note that the police division tables feature some combined years, these are 2016/18 labelled 2016 in the tables, and 2018/20 which is labelled 2018.
# (N.B. I found this was inconsistently applied, so the code identifies when 2017 and 2019 data are missing and then adjusts the trend_axis variable accordingly)
# Where too few people were asked a question we have suppressed the result in the table and noted that the base size is ‘< 50’, 
# which is common practice when providing data. This has mainly affected police division estimates, particularly for 2021/22 where this 
# is not part of a combined year like some of the prior years.

# Notes on the previous spreadsheet from Stuart Napier (2022):
# ‘Non-violent crime’ is labelled as property crime, which is all valid crimes that weren’t classed as a violent crime
# Sexual abuse by a partner is not currently asked in the partner abuse self-completion module, so the data only relates to psychological or physical abuse by a partner. Also this is not available for geographic breakdown
# Self-completion (partner abuse) and geographical data have had two survey years combined recently, so 2016-18 and 2018-20 are presented, this is so that we have enough data to present figures for all areas
# We present geographical data at Police Division level and not local authority or health board level. There are 13 Police Divisions and these line up with combinations of local authority areas.


### functions/packages -----
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
scjs_data_folder <- paste0(profiles_data_folder, "/Received Data/Scottish Crime and Justice Survey/")


## Geography lookup -----

# Read in geography lookup
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


## Read in data from SCJS data request

################################
# 2024 SCJS data
################################
#   30030: Adults perceiving local crime to be common (Percentage of adults who perceive crime to be very common or fairly common in their local area)
#   30029: Adult victims of non-violent crime (survey-based) (Percentage of adults who have been a victim of non-violent crime in the last 12 months in their local area)
#   30057: Adult victims of violent crime (survey-based) (Percentage of adults who experienced a violent crime in the past year)
#   30056: Adult victims of domestic violence (survey-based) (Percentage of adults who experienced abuse by a partner or ex-partner in the past year)
#   30168: Children exposed to domestic abuse in household

# Function to read in the data from the xlsx files received
# Each spreadsheet gives the %, unweighted base, and CI

get_crime_data <- function(tab, file) {
  
  ind <- substr(tab, 1, 4) #Either = Prop (i.e., non-violent), Viol (violent crime), QACO (i.e. perceptions of crime), Part (partner abuse), or PA_c (partner abuse with child present)

  # read in the right tab
  df <- read.xlsx(paste0(scjs_data_folder, file),
                                   sheet = tab,
                                   startRow = 3,
                                   colNames = TRUE) %>%
    
    # reshape the data and drop 'base'
    pivot_longer(cols = starts_with(c("pct", "ci", "base")), names_to = c("statistic", "year"), 
                 names_pattern = "(.*)_(.*)", values_transform = as.numeric, values_to = "rate") %>%
    filter(statistic!="base") %>% # drop the unweighted base
    
    # add sex column 
    mutate(sex = if(exists("sex_label", where = .)) str_to_title(sex_label) else "Total") %>%

    # add spatial.scale column
    mutate(spatial.scale = if(exists("police_division_label", where = .)) "Police division" else "Scotland") %>%
    
    # add spatial.unit column
    mutate(spatial.unit = if(exists("police_division_label", where = .)) police_division_label else "Scotland") %>%
    
    # add ind_id column
    mutate(ind_id = case_when(ind == "Prop" ~ "30029",
                                 ind == "Viol" ~ "30057",
                                 ind == "QACO" ~ "30030",
                                 ind == "Part" ~ "30056",
                                 ind == "PA_c" ~ "30168")) %>%
    
    # filter for the required responses
    mutate(question = if(exists("question", where = .)) question else "NA") %>%
    filter(question %in% c("NA", "qaco_any")) %>% # gets rid of the individual QACO questions, just keep the combined one
    mutate(cyp_var = if(exists("da_children_present_label", where = .)) da_children_present_label else "NA") %>%
    filter(cyp_var %in% c("NA", "yes")) %>% # gets rid of the non-yes answers to the children_present question
    
    # select required variables
    select(ind_id, year, spatial.unit, spatial.scale, sex, statistic, rate) %>%
    
    # add indicator short name 
    mutate(indicator = case_when(ind == "Prop" ~ "adult_non_violent_crime",
                              ind == "Viol" ~ "adult_violent_crime",
                              ind == "QACO" ~ "adult_crime_perception",
                              ind == "Part" ~ "svy_dom_abuse",
                              ind == "PA_c" ~ "svy_dom_abuse_cyp_present")) 
    
  
  # name the dataframe and make available outside the function
  df_name <- paste0("tab_", tab)
  assign(df_name, df, envir=.GlobalEnv)
  
}


# get the tab names
file1 <- "PHS - data request - march 2024 - mental health indicators - updated March 2025.xlsx"
sheets1 <- readxl::excel_sheets(paste0(scjs_data_folder, file1))[2:13] # drop the cover sheet, keep remaining tabs

# run the function
for (tabname in sheets1) {
 get_crime_data(tabname, file1)
 
}
# Some 'NAs introduced by coercion' warnings: this is OK

file2 <- "PHS - data request - march 2024 - mental health indicators - only partner abuse tables.xlsx"
sheets2 <- readxl::excel_sheets(paste0(scjs_data_folder, file2))[2:5] # drop the cover sheet, keep remaining tabs

# run the function
for (tabname in sheets2) {
  get_crime_data(tabname, file2)
  
}

file3 <- "PHS - data request - September 2024 - mental health indicators - partner abuse with children present.xlsx"
# only sheet to be imported = PA_children_present
# run the function
get_crime_data("PA_children_present", file3)



##################################################.
##  Combine and process the data further ----
##################################################.

# get all the resulting dataframes (those in the global environment with tab_ prefix) and rbind them
crime_data <- mget(ls(pattern = "tab_"), .GlobalEnv) %>% # gets the dataframes starting with tab_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  
  # get the police division names formatted right (so they can be merged with LUT to get the area codes)
  mutate(spatial.unit = str_to_title(spatial.unit),
         spatial.unit = gsub(" \\(\\D Division\\)", "", spatial.unit), # get rid of the division label from end
         spatial.unit = gsub(" And ", " and ", spatial.unit), # 'and' to lower case
         spatial.unit = gsub(" City", "", spatial.unit)) %>% # drop city from Edinburgh
  
  # add trend_axis variable (the string label for the year)
  # convert years (the dates provided in the 2024 xlsx) to financial years (the actual reporting period of the SCJS)
  # find series with missing 2017 and 2019 data (if missing one they're missing both): these have had the 2016+2017 and 2018+2019 data combined, and need different labelling
  group_by(ind_id, spatial.scale, sex) %>%
  mutate(has2017 = as.integer(any(year=="2017"))) %>% # 1 if 2017 data exists in the group, 0 if not
  ungroup() %>%
  
  mutate(trend_axis = paste0(year, "/", substr(as.character(as.integer(year) + 1), 3, 4)),
  # adjust these labels where necessary (i.e., when missing 2017 and 2019)
         trend_axis = case_when(has2017==0 & trend_axis=="2016/17" ~ "2016/17-2017/18",
                                has2017==0 & trend_axis=="2018/19" ~ "2018/19-2019/20",
                                TRUE ~ trend_axis)) %>%
  # convert to wider format
  pivot_wider(names_from = statistic, values_from = rate) %>% # 20+ cells with Nuw<50 have been suppressed, so have no data in them. 
  mutate(lowci = pct - ci,
         upci = pct + ci) %>%
  # drop suppressed here
  filter(!is.na(pct)) %>% #n=1380
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% # still n=1380
  
  # add def_period
  mutate(def_period = ifelse(nchar(trend_axis)==7, 
                             paste0("Survey year (", trend_axis, ")"),
                             paste0("Aggregated survey years (", trend_axis, ")"))) %>%
  
  # format year
  mutate(year = ifelse(nchar(trend_axis)==7, 
                       as.numeric(year),
                       as.numeric(year)+1)) %>% # aggregated years get their mid-point (so e.g., trend_axis 2016/17-2017/18 gets year==2017)
  
  # add numerator column
  mutate(numerator = as.numeric(NA)) %>% 
  
  # add measure_type
  mutate(measure_type = "percent") 
  
### Gap filling: required because the time series is inconsistent: sometimes annual, sometimes not
# This expands the data to the full time series (from min to max year) and adds NA data where there's no data.
# This ensures that points either side can be joined when plotted
data_w_gaps <- crime_data %>%
  select(ind_id, indicator, year, trend_axis, def_period, sex, rate=pct, lowci, upci, code, numerator, spatial.unit, spatial.scale)
crime_data2 <- data_w_gaps %>%
  group_by(ind_id, code, sex) %>%
  expand(full_seq(year,1)) %>% # expands to the full year sequence (from the min to the max for each ind_id x area code x sex combo)
  ungroup() %>%
  rename(year = 'full_seq(year, 1)') %>%
  merge(data_w_gaps, by = c("ind_id", "year", "code", "sex"), all=TRUE) %>% #gets all the original data back in, leaving NA gaps where there wasn't data. This is the intention.
  mutate(trend_axis = case_when(is.na(trend_axis) ~ paste0(as.character(year), "/", substr(year+1, 3, 4)), # every year without data gets converted to a financial year for the trend_axis column
                                TRUE ~ trend_axis))



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){

  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland and Police Division data, total pop
  main_data <- crime_data2 %>% 
    filter(indicator == ind,
           sex=="Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  # Save 
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland and Police Division data by sex (including total)
  pop_grp_data <- crime_data2 %>% 
    filter(indicator == ind) %>% 
    mutate(split_name = "Sex") %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value = sex) %>%
    arrange(code, year)

  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)

  # 3 - Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data

}


# Run function to create final files
prepare_final_files(ind = "adult_non_violent_crime")
prepare_final_files(ind = "adult_violent_crime")
prepare_final_files(ind = "adult_crime_perception")
prepare_final_files(ind = "svy_dom_abuse")
prepare_final_files(ind = "svy_dom_abuse_cyp_present") 

# Run QA reports 
run_qa(type = "main", filename = "adult_non_violent_crime", test_file = FALSE)
run_qa(type = "main", filename = "adult_violent_crime", test_file = FALSE)
run_qa(type = "main", filename = "adult_crime_perception", test_file = FALSE)
run_qa(type = "main", filename = "svy_dom_abuse", test_file = FALSE)
run_qa(type = "main", filename = "svy_dom_abuse_cyp_present", test_file = FALSE) 


#END

