#########################################################
# Scottish Crime and Justice Survey data import
#########################################################

### Update ScotPHO indicators sourced from Scottish Crime and Justice Survey: 
### Author: Liz Richardson, 
### Update Sept 2025 (new data for the 3 non domestic violence indicators)

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
# Spreadsheets containing the data provided by Stuart Napier (Stuart.Napier@gov.scot).
# Latest versions:
# Domestic abuse: (currently to 2021/22)
# "PHS - data request - march 2024 - mental health indicators - only partner abuse tables.xlsx"
# "PHS - data request - September 2024 - mental health indicators - partner abuse with children present.xlsx"
# Perception of crime data x police division level: (currently to 2021/22)
# "PHS - data request - march 2024 - mental health indicators - updated March 2025.xlsx"
# Other indicators (including perception of crime at Scotland level) (currently to 2023/24)
# "PHS - data request - august 2025 - mental health indicators - updated 2023-24 data.xlsx"

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
source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA

# Load additional packages
library(openxlsx)

################################
# Paths and lookups
################################


# Identify data folder
scjs_data_folder <- paste0(profiles_data_folder, "/Received Data/Scottish Crime and Justice Survey/")

# latest data files

# violent crime (Scot + PD), non-violent crime (Scot + PD), and perceptions of crime (Scot only):
file1 <- "PHS - data request - august 2025 - mental health indicators - updated 2023-24 data.xlsx"

# previous version (includes perceptions of crime at police division level, so extract that data)
file1b <- "PHS - data request - march 2024 - mental health indicators - updated March 2025.xlsx"

# domestic/partner abuse data:
file2 <- "PHS - data request - march 2024 - mental health indicators - only partner abuse tables.xlsx"
file3 <- "PHS - data request - September 2024 - mental health indicators - partner abuse with children present.xlsx"

## Geography lookup -----

geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


################################
# Functions to read in and process data
################################

# Function to read in the data from the xlsx files received
# Each spreadsheet gives the %, unweighted base, and CI

read_in_crime_data <- function(tab, file, start_row) {
  
  df <- read.xlsx(paste0(scjs_data_folder, file),
                  sheet = tab,
                  startRow = start_row,
                  colNames = TRUE)
}


# Processing for data provided prior to 2025:

process_old_format_data <- function(df) {
  
  df <- df  %>%
    
    # reshape the data and drop 'base'
    pivot_longer(cols = starts_with(c("pct", "ci", "base")), 
                 names_to = c("statistic", "year"), 
                 names_pattern = "(.*)_(.*)", values_transform = as.numeric) %>%
    pivot_wider(values_from = value, names_from = statistic)  %>% 
    
    # add sex column 
    mutate(split_value = if(exists("sex_label", where = .)) str_to_title(sex_label) else "Total") %>%
    mutate(split_name = if(exists("sex_label", where = .)) "Sex" else "Total") %>%
    
    # add areatype column
    mutate(areatype = if(exists("police_division_label", where = .)) "Police division" else "Scotland") %>%
    
    # add areaname column
    mutate(areaname = if(exists("police_division_label", where = .)) police_division_label else "Scotland") %>%
    
    # filter for the required responses (based on the indicator being read in)
    
    # keeps only the combined crime perception variable (qaco_any) if this tab is about this indicator. 
    mutate(question = if(exists("question", where = .)) question else "NA") %>%
    filter(question %in% c("NA", "qaco_any")) %>% 
    
    # keeps only the yes answers to the children_present question (if this tab is about this indicator)
    mutate(cyp_var = if(exists("da_children_present_label", where = .)) da_children_present_label else "NA") %>%
    filter(cyp_var %in% c("NA", "yes")) %>%
    
    # keep required vars
    select(year, areaname, areatype, split_name, split_value, rate=pct, ci)
  
}


# Processing for data provided in 2025 (and future?):

process_new_format_data <- function(df) {
  
  # Check what crossbreaks are included: next step depends on whether police.division is a crossbreak or not
  crossbreaks <- unique(df$Crossbreak.variable)
  
  if("police_division" %in% crossbreaks) {
    
    df <- df %>%
      
      # areanames are stored in Subgroup currently, so rename column
      rename(areaname = Subgroup) %>%
      mutate(areatype = "Police division") %>%
      
      # no splits in this data
      mutate(split_name = "Total",
             split_value = "Total") %>%
      
      # drop unnecessary variable
      select(-Crossbreak.variable)
    
  }
  
  if(!("police_division" %in% crossbreaks)) {
    
    df <- df %>%
      
      # add areaname column
      mutate(areaname = if(exists("Police.division", where = .)) Police.division else "Scotland") %>%
      
      # add areatype column
      mutate(areatype = if(exists("Police.division", where = .)) "Police division" else "Scotland") %>%
      
      # rename the split columns
      rename(split_name = Crossbreak.variable,
             split_value = Subgroup)
  }
  
  # Check what variables are included: next step run if this tab is about the QACO question
  variables <- unique(df$Variable.description)
  
  if("Answered that any type of crime was common in their area" %in% variables) {
    
    df <- df %>%
      filter(Variable.description == "Answered that any type of crime was common in their area")
    
  }
  
  df <- df %>%
    
    # keep required vars
    select(year=Year, areaname, areatype, split_name, split_value, rate=Percentage, ci=`95%.confidence.interval`)
  
}


# Function to read in, process, and add ind_id

get_crime_data <- function(tab, file, format = c("old", "new")) {
  
  # read in the right tab
  df <- read_in_crime_data(tab, file, start_row = 3) 
  
  # processing depends on format
  if (format=="old") {
    
    df <- process_old_format_data(df)
    
    } else if (format=="new") {
      
      df <- process_new_format_data(df)
      
    } else {
        
      print("ERROR")
      
      }
  
  # Add indicator id: based on first 4 characters of the tab name
  
  ind <- substr(tab, 1, 4) #Either = Prop (i.e., non-violent), Viol (violent crime), QACO (i.e. perceptions of crime), Part (partner abuse), or PA_c (partner abuse with child present)
  ind_num <- ifelse(ind == "Prop", 30029,
                    ifelse(ind == "Viol", 30057,
                           ifelse(ind == "QACO", 30030,
                                  ifelse(ind == "Part", 30056,
                                         ifelse(ind == "PA_c", 30168, 
                                                "ERROR")))))
  
  df <- df %>%
    mutate(ind_id = ind_num) 
  
  # name the dataframe and make available outside the function
  df_name <- paste0("tab_", tab)
  assign(df_name, df, envir=.GlobalEnv)
  
}

# function to get tab names and extract and process the required data from a spreadsheet

get_crime_data_from_whole_spreadsheet <- function(file, sheets=c("allbutfirst", "bespoke"), range = NULL, old_or_new = c("old", "new")) {
  
  # list all the tabs in the file
  tab_list <- readxl::excel_sheets(paste0(scjs_data_folder, file))
  
  # keep the tab names that have the data we want
  if (sheets == "allbutfirst") {
    
    tab_list <- tab_list[2:length(tab_list)] # drop the cover sheet, keep remaining tabs
    
  } else if (sheets == "bespoke") {
    
    tab_list <- tab_list[range]
    
  } else {
    
    print("ERROR")
    
  }
  
  # run the function to read in the data
  for (tab_name in tab_list) {
    get_crime_data(tab_name, file, format=old_or_new)
  }
  
    
}

##################################################.
##  Run functions to extract data ----
##################################################.

get_crime_data_from_whole_spreadsheet(file1, sheets = "allbutfirst", old_or_new = "new")
get_crime_data_from_whole_spreadsheet(file1b, sheets = "bespoke", range = c(12:13), old_or_new = "old")
get_crime_data_from_whole_spreadsheet(file2, sheets = "allbutfirst", old_or_new = "old")
get_crime_data_from_whole_spreadsheet(file3, sheets = "bespoke", range = 2, old_or_new = "old")



##################################################.
##  Combine and process the data further ----
##################################################.

# get all the resulting dataframes (those in the global environment with tab_ prefix) and rbind them
crime_data <- mget(ls(pattern = "tab_"), .GlobalEnv) %>% # gets the dataframes starting with tab_
  do.call(rbind.data.frame, .) %>% # rbinds them all together
  
  # get the police division names formatted right (so they can be merged with LUT to get the area codes)
  mutate(areaname = str_to_title(areaname),
         areaname = gsub(" \\(\\D Division\\)", "", areaname), # get rid of the division label from end
         areaname = gsub(" And | and ", " & ", areaname), # 'and' to '&'
         areaname = gsub(" City", "", areaname)) %>% # drop city from Edinburgh
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("areaname", "areatype"), by.y=c("areaname", "areatype"), all.x=TRUE) %>% 
  
  # Format the split_name column
  mutate(split_name = str_to_title(split_name),
         split_name = ifelse(split_name=="Simd_quintile", "Deprivation (SIMD)", split_name)) %>%
  
  # Format the split_value column
  mutate(split_value = str_to_title(split_value),
         split_value = case_when(split_value == "All Respondents" ~ "Total",
                                 split_value == "Quintile 1 (High Deprivation)" ~ "1",
                                 split_value == "Quintile 2" ~ "2",
                                 split_value == "Quintile 3" ~ "3",
                                 split_value == "Quintile 4" ~ "4",
                                 split_value == "Quintile 5 (Low Deprivation)" ~ "5",
                                 TRUE ~ split_value)) %>%
  
  # calculate the confidence intervals
  mutate(lowci = rate - ci,
         upci = rate + ci) %>%
  mutate(lowci = ifelse(lowci < 0, 0, lowci)) %>% #constrain the lowci to be zero if it's negative
  select(-ci) %>%
  
  # add trend_axis variable (the string label for the year)
  # convert years (the dates provided in the 2024 xlsx) to financial years (the actual reporting period of the SCJS)
  # find series with missing 2017 and 2019 data (if missing one they're missing both): these have had the 2016+2017 and 2018+2019 data combined, and need different labelling
  group_by(ind_id, areatype) %>%
  mutate(has2017 = as.integer(any(substr(year, 1, 4)=="2017"))) %>% # 1 if 2017 data exists in the group, 0 if not
  ungroup() %>%
  
  mutate(trend_axis = paste0(substr(year, 1, 4), "/", substr(as.character(as.integer(substr(year, 1, 4)) + 1), 3, 4)), #format as financial years
         # adjust these labels where necessary (i.e., when missing 2017 and 2019)
         trend_axis = case_when(has2017==0 & trend_axis=="2016/17" ~ "2016/17-2017/18",
                                has2017==0 & trend_axis=="2018/19" ~ "2018/19-2019/20",
                                TRUE ~ trend_axis)) %>%
  
  # format year
  mutate(year = ifelse(nchar(trend_axis)==7, #single FY
                       as.numeric(substr(year, 1, 4)),
                       as.numeric(substr(year, 1, 4))+1)) %>% # aggregated years get their mid-point (so e.g., trend_axis 2016/17-2017/18 gets year==2017)
  
  # add numerator column
  mutate(numerator = as.numeric(NA)) %>% 
  
  # add measure_type
  mutate(measure_type = "percent") %>%
  
  # add def_period
  mutate(def_period = ifelse(nchar(trend_axis)==7, 
                             paste0("Survey year (", trend_axis, ")"),
                             paste0("Aggregated survey years (", trend_axis, ")"))) %>%
  
  # keep required vars
  select(ind_id, year, code, trend_axis, def_period, split_name, split_value, measure_type, rate, lowci, upci, numerator)


## OLD CODE FOR EXPANDING THE SERIES DUE TO INCONSISTENT TIME SERIES: ADDING IN NA YEARS MAKES SURE THE X-AXIS SHOWS TIME CORRECTLY
## UNSURE IF NEEDED?
# ### Gap filling: required because the time series is inconsistent: sometimes annual, sometimes not
# # This expands the data to the full time series (from min to max year) and adds NA data where there's no data.
# # This ensures that points either side can be joined when plotted
# crime_data <- crime_data %>%
#   group_by(ind_id, code, split_name, split_value) %>%
#   expand(full_seq(year, 1)) %>% # expands to the full year sequence (from the min to the max for each ind_id x area code x split combo)
#   ungroup() %>%
#   rename(year = 'full_seq(year, 1)') %>%
#   merge(crime_data, by = c("ind_id", "year", "code", "split_name", "split_value"), all=TRUE) %>% #gets all the original data back in, leaving NA gaps where there wasn't data. This is the intention.
#   mutate(trend_axis = case_when(is.na(trend_axis) ~ paste0(as.character(year), "/", substr(year+1, 3, 4)), # every year without data gets converted to a financial year for the trend_axis column
#                                 TRUE ~ trend_axis))
# 
# # CHECK THIS IS PRODUCING THE PLOTS WE WANT:
# # E.G., HAS ADDED YEARS THERE WERE NO SURVEYS, OR WHEN DOM ABUSE Q NOT ASKED... IS THIS OK?
# # DON'T USE IN SEPT 2025 UPDATE: CHECK IF THE PLOTS ARE OK


##########################################################
### Prepare final files -----
##########################################################


# Function to prepare final files: 
prepare_final_files <- function(ind_num){
  
  indicator <- ifelse(ind_num==30029, "adult_non_violent_crime",
                    ifelse(ind_num==30057, "adult_violent_crime",
                           ifelse(ind_num==30030, "adult_crime_perception",
                                  ifelse(ind_num==30056, "svy_dom_abuse",
                                         ifelse(ind_num==30168, "svy_dom_abuse_cyp_present",
                                                "ERROR")))))
  
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland and Police Division data, total pop
  main_data <- crime_data %>% 
    filter(ind_id == ind_num,
           split_value == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year) 

  # Save 
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  
  
  
  if(ind_num!=30168) { # CYP present during partner abuse only available for total population, no splits
  
    # 2 - population groups data (ie data behind population groups tab)
    # Contains Scotland and Police Division data by sex (including total)
      pop_grp_data <- crime_data %>% 
      filter(ind_id == ind_num & split_name!="Deprivation (SIMD)") %>% 
      mutate(split_name = "Sex") %>% # recodes split_name=Total (split_value=Total) to split_name=Sex
      select(code, ind_id, year, numerator, rate, upci, 
             lowci, def_period, trend_axis, split_name, split_value) %>%
      arrange(code, year)
    
    # Save
    write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny_popgrp.rds"))
    write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", indicator, "_shiny_popgrp.csv"), row.names = FALSE)
  
    # Make data created available outside of function so it can be visually inspected if required
    pop_grp_data_result <<- pop_grp_data
  
    if(ind_num!=30056) { # partner abuse only available by sex, not SIMD
      
    # 3 - SIMD data (ie data behind deprivation tab)
    # add population data (quintile level) so that inequalities can be calculated
    simd_data <- crime_data %>%
      filter(ind_id == ind_num & split_name %in% c("Deprivation (SIMD)", "Total")) %>%
      group_by(year, code) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      filter(count>1) %>% #drops groups where there's only a Total and no SIMD data (i.e., police division data for crime perception indicator)
      mutate(quint_type = "sc_quin") %>%
      rename(quintile = split_value) %>%
      select(-split_name) %>%
      arrange(code, year, quintile) %>%
      add_population_to_quintile_level_data(pop="depr_pop_16+", 
                                            ind = ind_num, ind_name = indicator) %>%
      filter(!is.na(rate)) # not all years have data
    
    # calculate the inequality measures
    simd_data <- simd_data |>
      calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
      select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
    
    # save the data as RDS file
    saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", indicator, "_ineq.rds"))
    
    # Make data created available outside of function so it can be visually inspected if required
    simd_data_result <<- simd_data
    }

  }
  
}


# Run function to create final files
prepare_final_files(ind_num = 30029) # "adult_non_violent_crime"
prepare_final_files(ind_num = 30057) # "adult_violent_crime"
prepare_final_files(ind_num = 30030) # "adult_crime_perception"
prepare_final_files(ind_num = 30056) # "svy_dom_abuse"
prepare_final_files(ind_num = 30168) # "svy_dom_abuse_cyp_present"


# Run QA reports
####################

# main data:
run_qa(type = "main", filename = "adult_non_violent_crime", test_file = FALSE) 
run_qa(type = "main", filename = "adult_violent_crime", test_file = FALSE) 
run_qa(type = "main", filename = "adult_crime_perception", test_file = FALSE) 
run_qa(type = "main", filename = "svy_dom_abuse", test_file = FALSE)
run_qa(type = "main", filename = "svy_dom_abuse_cyp_present", test_file = FALSE) 

# popgrp data:
run_qa(type = "popgrp", filename = "adult_non_violent_crime", test_file = FALSE) 
run_qa(type = "popgrp", filename = "adult_violent_crime", test_file = FALSE) 
run_qa(type = "popgrp", filename = "adult_crime_perception", test_file = FALSE) 
run_qa(type = "popgrp", filename = "svy_dom_abuse", test_file = FALSE)

# ineq data: 
run_qa(type = "deprivation", filename = "adult_non_violent_crime", test_file = FALSE) #some suppression in some PDs
run_qa(type = "deprivation", filename = "adult_violent_crime", test_file = FALSE) #some suppression in some PDs
run_qa(type = "deprivation", filename = "adult_crime_perception", test_file = FALSE) 



#END

