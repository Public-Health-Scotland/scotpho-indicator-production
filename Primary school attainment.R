#########################################################
# SG Curriculum for Excellence attainment: P1, P4 and P7 - data import
#########################################################

### ScotPHO indicators sourced from SG CfE attainment data: 
### Author: Liz Richardson, 8 Aug 2025

### CYP mental health indicators:
# 30157 = Primary educational attainment (literacy)	=	Percentage of P1, P4 and P7 pupils achieving expected Curriculum for Excellence levels in literacy (reading, writing, listening and talking) 
# 30158 = Primary educational attainment (numeracy)	=	Percentage of P1, P4 and P7 pupils achieving expected Curriculum for Excellence levels in numeracy

### Notes on the data source:
# Spreadsheet containing the data 2016/17 to 2023/24 downloaded from SG: 
# Supplementary tables from https://www.gov.scot/publications/achievement-of-curriculum-for-excellence-cfe-levels-2023-24/documents/.
# (statistics.gov.scot has Scotland-wide and SIMD quintile data, but not for councils or population splits)
# SIMD is based on pupil's home post code: 
# SIMD 2016 for 2016/17, 2017/18, 2018/19 
# SIMD 2020 for 2020/21, 2021/22 and 2022/23. 
# No data collected in 2019/20 (collection and publication was cancelled in 2019-2020 due to the difficulties in collecting data whilst schools were closed due to COVID-19)

# For inequalities calcs: 
# Published pupil census data don't provide the denominator granularity we need (quintile x stage x council area). 
# Denominator data obtained directly from Keith Hoy (school.stats@gov.scot). 


##########################################################
### Source functions and additional packages ----
##########################################################

### functions/packages -----
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

# Load additional packages
library(openxlsx)

##########################################################
### 1. Paths and lookups ----
##########################################################

# Identify data folder
cfe_profiles_data_folder <- paste0(profiles_data_folder, "/Received Data/Curriculum for Excellence/")
file <- "ACEL+2324+-+Publication+-+Supplementary+tables+-+final.xlsx"
cohort <- "ACEL 23-24 Table 11 with cohort for Elizabeth Richardson.xlsx"
geography_lookups <- file.path(profiles_data_folder, "Lookups", "Geography")

## Geography lookups -----
#########################################################

# Read in geog code lookup
code_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# get lookup for CA to higher geogs
geography_lookup <- readRDS(file.path(geography_lookups, "DataZone11_All_Geographies_Lookup.rds")) |>
  mutate(scotland = "S00000001") %>% # create scotland column
  select(code = ca2019, hscp=hscp2019, hb=hb2019, adp, pd, scotland) |>
  unique() 


## Population lookup -----
#########################################################

## Make a denominator population lookup -----
# (by CA, SIMD, school stage, year)

# These pop denominators are used for 
# (a) back-calculating the numerators so LA data can be aggregated to higher geogs, and 
# (b) calculating inequalities metrics
# The denominator data had to be requested from SG.

# Process the denominator data (requested from SG)
denoms <- read.xlsx(paste0(cfe_profiles_data_folder, cohort),
                     sheet = "ACEL Table_11",
                     startRow = 5,
                     colNames = TRUE) %>%
  clean_names() %>%
  select(year = year_note_9, stage, areaname=local_authority_note_7, quintile = simd_note_3, 
         denominator = cohort) %>%
  
  # Add split columns (by stage)
  filter(grepl("^P", stage)) %>%
  mutate(split_value = case_when(stage == "P1 (Early Level)" ~ "P1",
                           stage == "P4 (First Level)" ~ "P4",
                           stage == "P7 (Second Level)" ~ "P7",
                           stage == "P1, P4 and P7 combined" ~ "Total")) %>%
  mutate(split_name = "Stage") %>%
  
  # numeric year column (used to match to the indicator data)
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  
  # SIMD column
  mutate(quintile = case_when(quintile %in% c("SIMD Quintile 1 - most deprived", "SIMD Quintile 1") ~ "1",
                              quintile=="SIMD Quintile 2" ~ "2",
                              quintile=="SIMD Quintile 3" ~ "3",
                              quintile=="SIMD Quintile 4" ~ "4",
                              quintile=="SIMD Quintile 5 - least deprived" ~ "5",
                              quintile=="Local Authority Total" ~ "Total")) %>%
  filter(!is.na(quintile)) %>%

  # add areatype column
  mutate(areatype = "Council area") %>%
  
  # adjust the LA names so they match with geo lookup
  mutate(areaname = ifelse(areaname=="Edinburgh City", "City of Edinburgh", areaname)) %>% # to ensure matches OK
  
  # add the geog codes, 
  merge(y=code_lookup, by=c("areaname", "areatype")) %>% 
  
  # select required variables
  select(-starts_with("area"), -stage)

# Aggregate denoms to Scotland
denoms_scotland <- denoms %>%
  group_by(year, quintile, split_name, split_value) %>%
  summarise(denominator = sum(denominator, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(code = "S00000001") 

# Combine
denoms <- rbind(denoms, denoms_scotland) %>%
  mutate(quint_type = "sc_quin")


##########################################################
### 2. Read in data ----
##########################################################

# A. First read in the national, SIMD and SIMDxCA level data.
# B. Then read in the data for other population splits (sex, ethnicity, urban/rural).

# Define some functions ----

# Function to read in data from any spreadsheet tab

get_attainment_data <- function (sheetname, startnumber) {
  
  df <- read.xlsx(paste0(cfe_profiles_data_folder, file),
                  sheet = sheetname,
                  startRow = startnumber,
                  colNames = TRUE) %>%
    clean_names() %>%
    # filter and recode if 'stage' is a column
    filter(if_any(matches("stage"), ~ grepl("^P", .))) %>% # select P1, P4, P7, and all combined
    mutate(across(any_of(c("stage")), ~ case_when(. %in% c("P1 - Early level", "P1 (Early Level)") ~ "P1",
                                                  . %in% c("P4 - First level", "P4 (First Level)") ~ "P4",
                                                  . %in% c("P7 - Second level", "P7 (Second Level)") ~ "P7",
                                                  . == "P1, P4 and P7 combined" ~ "Total"))) %>%
    # convert missing/suppressed values to NA
    mutate(across(.cols = -c(starts_with(c("organiser", "local", "simd", "year", "stage"))), # works even if these cols don't exist... result!
                  .fns = ~ case_when(. %in% c("[w]", "[c]", "[x]", "c") ~ NA, # replace the missing codes with NA
                                     TRUE ~ .))) %>%
    # convert indicator data columns to numeric
    mutate(across(.cols = -c(starts_with(c("organiser", "local", "simd", "year", "sex", "ethnicity", "urban", "stage"))), 
                  .fns = ~ as.numeric(.)))
}

# Function to perform the repeated processing steps on the national data

process_national_data <- function(data) {
  
  new_df <- data %>%
    
    # rename cols
    rename_with(~ substr(.x, 1, 8)) %>% # lose the reference to notes by shortening all col names
    
    # keep required rows
    filter(organise %in% c("Numeracy", "Literacy")) %>%
    rename(indicator = organise) %>%
    
    # reshape the data 
    pivot_longer(cols = c(x2016_17 : x2023_24), values_to="rate", names_to = "year", names_prefix = "x") %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    
    # add areatype column
    mutate(areatype = "Scotland") %>%
    
    # add areaname column
    mutate(areaname = "Scotland") %>%
    
    # add split columns
    mutate(split_name = "Stage") %>%
    rename(split_value = stage) %>%
    
    # add quintile column (will be Total unless subsequently processed by format_simd_data() )
    mutate(quintile = "Total")
  
}

# Function to perform the repeated processing steps on the SIMD data

format_simd_data <- function(data) {
  
  df <- data %>%
    
    # standardise quintile column
    mutate(quintile = case_when(quintile %in% c("SIMD Quintile 1 - most deprived", "SIMD Quintile 1", "SIMD Quintile 1 - Most Deprived") ~ "1",
                                   quintile=="SIMD Quintile 2" ~ "2",
                                   quintile=="SIMD Quintile 3" ~ "3",
                                   quintile=="SIMD Quintile 4" ~ "4",
                                   quintile %in% c("SIMD Quintile 5 - Least Deprived", "SIMD Quintile 5 - least deprived") ~ "5",
                                   quintile %in% c("Total", "Local Authority Total") ~ "Total",
                                   TRUE ~ as.character(NA))) %>%
    filter(!is.na(quintile)) 
  
}


# A. First read in the national, SIMD and SIMDxCA level data -----


# Scotland, overall (Table 1) (used for Scot splits by stage (P1, P4, P7))

scotland <- get_attainment_data(sheetname="Table_1", startnumber=5) %>%
  process_national_data()

# Scotland, by deprivation (Table 2.4) (used for Scot splits by SIMD)

simd <- get_attainment_data(sheetname="Table_2_4", startnumber=5) %>%
  process_national_data() %>%
  # SIMD column
  select(-quintile) %>%
  rename(quintile = simd_not) %>%
  format_simd_data()

# LAs, by deprivation and stage (Table 11) 
# (NB. can't split by dep AND stage in the app currently, so use this for LA splits by stage, and LA splits by SIMD, plus higher geographies)

simd_LA <- get_attainment_data(sheetname="Table_11", startnumber=5) %>%
  
  select(year = year_note_9, 
         split_value = stage,
         areaname = local_authority_note_7, 
         quintile = simd_note_3, 
         Literacy = english_literacy_note_1_note_2_note_8, 
         Numeracy = numeracy_note_1_note_2_note_8) %>%
  # reshape the data 
  pivot_longer(cols = c("Literacy", "Numeracy"), values_to="rate", names_to = "indicator") %>%
  # year column
  mutate(year = as.numeric(substr(year, 1, 4))) %>%
  # SIMD column
  format_simd_data() %>%
  # add areatype and split_name columns
  mutate(areatype = "Council area",
         split_name = "Stage")
# some 'NAs introduced by coercion' warnings: this is OK. 
# It's due to use of as.numeric() on cols that had suppression codes in them (here replaced with NA).

### Combine the data files
data_scot_la_simd <- simd %>%
  filter(quintile!="Total") %>% # total Scotland comes from the scotland df
  rbind(scotland, simd_LA) %>%
                           
  # add the geog codes 
  mutate(areaname = ifelse(areaname=="Edinburgh City", "City of Edinburgh", areaname)) %>% # to ensure matches OK
  merge(y=code_lookup, by.x=c("areaname", "areatype"), by.y=c("areaname", "areatype"), all.x=TRUE) %>% 
  select(-starts_with("area")) %>%
  
  # add in denominators 
  merge(y=denoms, by=c("year", "split_value", "split_name", "code", "quintile")) %>% # add denominators
  mutate(numerator = round(denominator * rate/100))  %>% # back calculate the numerators
# rate is NA if suppressed (based on small counts)
# which gives numerator of NA
# Don't drop, as the NA provide gaps in trend charts, and keeps x-axis spacing uniform
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) 



# Save subsets ready for the main analysis function
# NB. There are no NA/suppressed values in the LA data, so can remove Scotland values and recalculate them accurately during the aggregation

# Main data
saveRDS(subset(data_scot_la_simd, indicator=="Literacy" & split_value=="Total" & quintile=="Total" & code!="S00000001"), 
        file=paste0(profiles_data_folder, '/Prepared Data/Literacy_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Numeracy" & split_value=="Total" & quintile=="Total" & code!="S00000001"), 
        file=paste0(profiles_data_folder, '/Prepared Data/Numeracy_raw.rds'))

# Pop group data (by Stage)
saveRDS(subset(data_scot_la_simd, indicator=="Literacy" & split_value=="P1" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Literacy_P1_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Literacy" & split_value=="P4" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Literacy_P4_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Literacy" & split_value=="P7" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Literacy_P7_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Numeracy" & split_value=="P1" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Numeracy_P1_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Numeracy" & split_value=="P4" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Numeracy_P4_raw.rds'))
saveRDS(subset(data_scot_la_simd, indicator=="Numeracy" & split_value=="P7" & quintile=="Total" & code!="S00000001"),
        file=paste0(profiles_data_folder, '/Prepared Data/Numeracy_P7_raw.rds'))


# B. Then read in the data for other population splits (sex, ethnicity, urban/rural) ----

# Table 3: Scot by sex
# Table 4: Scot by ethnicity
# Table 5: Scot by urban-rural status

split_sex <- get_attainment_data(sheetname = "Table_3", startnumber = 5) %>%
  rename(split_value = sex) %>%
  mutate(split_name = "Sex")

split_ethnicity <- get_attainment_data(sheetname = "Table_4", startnumber = 5) %>%
  rename(split_value = ethnicity) %>%
  mutate(split_name = "Ethnicity")

split_urbanrural <- get_attainment_data(sheetname = "Table_5", startnumber = 5) %>%
  rename(split_value = "urban_rural_classification_note_5") %>%
  mutate(split_name = "Urban-Rural status")

data_popgroups <- rbind(split_sex, split_ethnicity, split_urbanrural) %>%
  filter(stage=="Total") %>% # splits by sex/ethnicity/urbanity x stage are available, but can't currently be presented in app.
  select(trend_axis = year_note_9,
         split_value,
         split_name,
         Literacy = "literacy_note_1_note_2",
         Numeracy = "numeracy_note_1_note_2") %>%
  # trend_axis and year
  mutate(trend_axis = gsub("-", "/", trend_axis)) %>%
  mutate(year = as.numeric(substr(trend_axis, 1, 4))) %>%
  # reshape the data 
  pivot_longer(cols = c(Literacy, Numeracy), values_to="rate", names_to = "indicator") %>%
  filter(!(split_value %in% c("Not Disclosed / Unknown", "Unknown"))) %>%
  mutate(split_value = case_when(split_value=="All pupils" ~ "Total", TRUE ~ split_value)) %>%
  # add geog code
  mutate(code = "S00000001") %>%
  # add numerator
  mutate(numerator = as.numeric(NA)) %>%
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%
  select(-indicator) %>%
  # add def_period
  mutate(def_period = paste0("School year (", trend_axis, ")")) %>%
  # add CI columns
  mutate(lowci = as.numeric(NA),
         upci = as.numeric(NA)) 


##########################################################
### 3. Final processing functions ----
##########################################################



# Pop group processing
#################################

# Function to prepare the popgroup data by stage (can aggregate up from CA to HB, etc, using main_analysis function)
prep_popgrp_data_by_stage <- function(ind, ind_num) {
  
  for (stage in c("P1", "P4", "P7")) {
    main_analysis(filename = paste0(ind, "_", stage), ind_id = ind_num, geography = "council", measure = "percent", 
                  yearstart = 2016, yearend = 2023, time_agg = 1, year_type = "school", QA = FALSE)
    df1 <- main_analysis_result %>% mutate(split_name="Stage", split_value=stage)
    df_name <- stage
    assign(df_name, df1)
  }
  
  df2 <- rbind(P1, P4, P7)
}

# SIMD data processing
##################################

process_simd_data <- function(ind, ind_num) {
  
  # Process SIMD data
  data <- data_scot_la_simd %>% 
    filter(indicator == ind & split_value == "Total") %>% 
    filter(!is.na(rate)) %>%
    unique() %>%
    select(-indicator, -split_name, -split_value) %>%
    mutate(quint_type = "sc_quin") %>%
    rename(sc_quin = quintile) %>%
    # join data with lookup
    left_join(geography_lookup, by = "code")
  
  # use functionality from create_quintile_data.R  to aggregate up from CA
  simd_df1 <- rbind(
    aggregate_by_simd(subset(data, !is.na(hb)), geo_col = "hb", simd_col = "sc_quin"), # health board data, split by scotland quintiles
    aggregate_by_simd(subset(data, !is.na(hb)), geo_col = "pd", simd_col = "sc_quin"), # police division data, split by scotland quintiles
    aggregate_by_simd(subset(data, !is.na(hb)), geo_col = "hscp", simd_col = "sc_quin") # HSCP data, split by scotland quintiles
  )
  # get the original Scotland and CA data, and add the aggregated data
  simd_df2 <- data %>%
    filter(substr(code, 1, 3) %in% c("S00", "S12")) %>% 
    select(year, code, quintile=sc_quin, numerator, denominator, quint_type) %>%
    rbind(simd_df1) %>%
    calculate_percent() # call helper function

  # calculate the inequality measures
  simd_data <- simd_df2 |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  # spot checks: the rates calculated for Scotland match those in the xlsx, but now have CIs
  
  # add 2019 back in (all as NA) so that trend plots retain the gap
  simd_data <- simd_data %>%
    filter(year==2018) %>%
    mutate(across(is.numeric, ~ as.numeric(NA))) %>%
    mutate(year = 2019) %>%
    rbind(simd_data) %>%
    mutate(ind_id = ind_num) %>%
    # create trend axis column (used to create axis labels on trend charts)
    create_trend_axis_column(year_type="school", agg=1) |>
    # create definition period column (used to show time period for charts looking at a single year)
    create_def_period_column(year_type="school", agg=1) %>%
    arrange(code, year, quintile)

  simd_data
  
}


  
  


##########################################################
### 4. Prepare final files -----
##########################################################

# Main data processing
#################################

main_analysis(filename = "Literacy", ind_id = 30157, geography = "council", measure = "percent", 
              yearstart = 2016, yearend = 2023, time_agg = 1, year_type = "school")

main_analysis(filename = "Numeracy", ind_id = 30158, geography = "council", measure = "percent", 
              yearstart = 2016, yearend = 2023, time_agg = 1, year_type = "school")

# QA and main_analysis_result df show that 2019/20 has been retained despite being all NA, which is the desired result



# Function to prepare final files: popgroup and inequals

prepare_final_files <- function(ind_name){

  if (ind_name == "Literacy") {
    ind_num = 30157
  } else if (ind_name == "Numeracy") {
    ind_num = 30158
  } else {
    print("Error!")
  }
  
  # 1 - Population groups data (ie data behind population groups tab)
  
  # prepare the popgroup data by stage (can use main_analysis to aggregate from CA)
  stage_popgrp_data <- prep_popgrp_data_by_stage(ind_name, ind_num) %>%
    mutate(def_period = paste0("School year (", trend_axis, ")"))  #add school year to the original less informative def_period
  
  # get the rest of the popgroups data (Scotland level only)
  all_popgrp_data <- data_popgroups %>% 
    filter(ind_id == ind_num ) %>% 
    rbind(stage_popgrp_data) %>%
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    # arrange so the points plot in right order in QA stage
    arrange(ind_id, code, split_name, split_value, year)

  # Save
  write_rds(all_popgrp_data, paste0(profiles_data_folder, "/Data to be checked/", ind_name, "_shiny_popgrp.rds"))
  write.csv(all_popgrp_data, paste0(profiles_data_folder, "/Data to be checked/", ind_name, "_shiny_popgrp.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  popgrp_data_result <<- all_popgrp_data
  
  
  
  # 2 - SIMD data
  
  #run the SIMD processing function:
  simd_data <- process_simd_data(ind = ind_name, ind_num = ind_num)

  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind_name, "_ineq.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  simd_data_result <<- simd_data
  

}

# Run the function:
prepare_final_files("Literacy")
prepare_final_files("Numeracy")


# Run QA checks

# ineq data: 
run_qa(type = "deprivation", filename = "Literacy", test_file=FALSE)
run_qa(type = "deprivation", filename = "Numeracy", test_file=FALSE)

# popgrp data:
run_qa(type = "popgrp", filename = "Literacy", test_file=FALSE)
run_qa(type = "popgrp", filename = "Numeracy", test_file=FALSE)




#END

