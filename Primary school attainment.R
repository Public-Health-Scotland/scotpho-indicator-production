# consider if possible to generate NHS board level data from the CA figures (should be possible if raw numerator and denmoniator are published)

#########################################################
# SG Curriculum for Excellence attainment: P1, P4 and P7 - data import
#########################################################

### ScotPHO indicators sourced from SG CfE attainment data: 
### Author: Liz Richardson, 7 Jan 2025

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

# For inequalities calcs: (Scotland, CA and HB level)
# Published pupil census data don't provide the denominator granularity we need (quintile x stage x council area). 
# Denominator data obtained directly from Keith Hoy (school.stats@gov.scot). 
# These data are imported, processed, and saved to the population lookup folder at the start of this script. 


### functions/packages -----
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

# Load additional packages
library(openxlsx)
library(hablar) # sum_ function from hablar keeps NA when there should be NA

##########################################################
### 1. Paths and lookups ----
##########################################################

# Identify data folder
cfe_profiles_data_folder <- paste0(profiles_data_folder, "/Received Data/Curriculum for Excellence/")
file <- "ACEL+2324+-+Publication+-+Supplementary+tables+-+final.xlsx"
cohort <- "ACEL 23-24 Table 11 with cohort for Elizabeth Richardson.xlsx"

## Geography lookup -----

# Read in geography profiles_lookups
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# LAs to HBs lookup
hb <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)

## Population lookup -----

# These pop denominators are used for 
# (a) back-calculating the numerators so LA data can be aggregated to HB, and 
# (b) calculating inequalities metrics)
# The cohort data had to be requested from SG.

# Process the cohort data (requested from SG) and save in LUT folder
cohort_simd_LA <- read.xlsx(paste0(cfe_profiles_data_folder, cohort),
                     sheet = "ACEL Table_11",
                     startRow = 5,
                     colNames = TRUE) %>%
  clean_names() %>%
  select(year = year_note_9, stage, spatial.unit=local_authority_note_7, quintile = simd_note_3, 
         denominator = cohort) %>%
  filter(stage == "P1, P4 and P7 combined") %>%
  
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
 # mutate(quint_type = "sc_quin") %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Council area") %>%
  
  # adjust the LA names so they match with geo lookup
  mutate(spatial.unit = ifelse(spatial.unit=="Edinburgh City", "City of Edinburgh", spatial.unit)) %>% # to ensure matches OK
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% 
  
  # select required variables
  select(-stage, -starts_with("spatial"))

# Aggregate cohort populations to HB level
cohort_simd_HB <- cohort_simd_LA %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code) %>%
  rename(code = hb2019) %>%
  group_by(code, year, quintile) |>
  summarise(denominator = sum(denominator, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(denominator = ifelse(denominator==0, as.numeric(NA), denominator))  # islands that are missing some quintiles

# Aggregate cohort populations to Scotland
cohort_simd_scotland <- cohort_simd_LA %>%
  group_by(year, quintile) %>%
  summarise(denominator = sum(denominator, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(code = "S00000001") 

# Combine
cohort_simd <- rbind(cohort_simd_LA, cohort_simd_HB, cohort_simd_scotland)

# Add in split_value = "None"
cohort_P1_P4_P7 <- cohort_simd %>%
  filter(quintile=="Total") %>%
  mutate(quintile="None") %>%
  rbind(cohort_simd) %>%
  mutate(quint_type = "sc_quin")

  
# Save file to lookups folder (for use in later calcs of inequalities metrics)
saveRDS(cohort_P1_P4_P7, paste0(profiles_lookups, "/Population/depr_pop_CYP_P1_P4_P7.rds"))



##########################################################
### 2. Read in data ----
##########################################################

# First read in the national, CA, SIMD and SIMDxCA level data.
# HB values can be created from the CA data.
# Then read in the data for other population splits (sex, ethnicity, urban/rural).
# Finally combine these. 

# Define some functions ----

# Function to read in the data

get_attainment_data <- function (sheetname, startnumber) {
  
  df <- read.xlsx(paste0(cfe_profiles_data_folder, file),
                  sheet = sheetname,
                  startRow = startnumber,
                  colNames = TRUE) %>%
    clean_names() %>%
    # filter if 'stage' is a column
    filter(if_any(matches("stage"), ~ . == "P1, P4 and P7 combined")) %>%
    # then drop 'stage' if it exists
    select(-any_of("stage")) %>%
    # convert missing/suppressed values to NA
    mutate(across(.cols = -c(starts_with(c("organiser", "local", "simd", "year"))), # works even if these cols don't exist... result!
                  .fns = ~ case_when(. %in% c("[w]", "[c]", "[x]", "c") ~ NA, # replace the missing codes with NA
                                     TRUE ~ .))) %>%
    # convert indicator data columns to numeric
    mutate(across(.cols = -c(starts_with(c("organiser", "local", "simd", "year", "sex", "ethnicity", "urban"))), 
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
    pivot_longer(cols = c(x2016_17 : x2023_24), values_to="rate", names_to = "trend_axis", names_prefix = "x") %>%
    mutate(trend_axis = gsub("_", "/", trend_axis)) %>%
    
    # add spatial.scale column
    mutate(spatial.scale = "Scotland") %>%
    
    # add spatial.unit column
    mutate(spatial.unit = "Scotland")
  
}

# Function to perform the repeated processing steps on the SIMD data

format_simd_data <- function(data) {
  
  df <- data %>%
    
    # standardise split_value column
    mutate(split_value = case_when(split_value %in% c("SIMD Quintile 1 - most deprived", "SIMD Quintile 1", "SIMD Quintile 1 - Most Deprived") ~ "1",
                                   split_value=="SIMD Quintile 2" ~ "2",
                                   split_value=="SIMD Quintile 3" ~ "3",
                                   split_value=="SIMD Quintile 4" ~ "4",
                                   split_value %in% c("SIMD Quintile 5 - Least Deprived", "SIMD Quintile 5 - least deprived") ~ "5",
                                   split_value %in% c("Total", "Local Authority Total") ~ "Total",
                                   TRUE ~ as.character(NA))) %>%
    filter(!is.na(split_value)) %>%
    
    # add split_name column
    mutate(split_name = "Deprivation (SIMD)") 
  
}


# Read in the national/CA/SIMD data ----


# Scotland data included in the LA data, so skip this step
# # Scotland, overall (Table 1)
# 
# scotland <- get_attainment_data(sheetname="Table_1", startnumber=5) %>%
#   
#   process_national_data() %>%
#   
#   # add split columns
#   mutate(split_name = "None",
#          split_value = "None") 


# Scotland, by deprivation (Table 2.4)

simd <- get_attainment_data(sheetname="Table_2_4", startnumber=5) %>%
  
  process_national_data() %>%
  
  # SIMD column
  rename(split_value = simd_not) %>%
  format_simd_data()


# Local Authorities (Table 10):

councils <- get_attainment_data(sheetname="Tables_10.4a_b_c_d_e", startnumber=7) %>%
  
  # Select relevant columns
  select(ends_with(c("_4", "_5"))) %>% # the numeracy (_5) and literacy (_4) columns
  
  # select one column for the local authority names
  rename(spatial.unit = local_authority_note_8_4) %>%
  mutate(spatial.unit = ifelse(spatial.unit=="Edinburgh City", "City of Edinburgh", spatial.unit)) %>% # to ensure matches OK
  select(-starts_with("local")) %>%
  
  # reshape the data 
  pivot_longer(-spatial.unit, values_to="rate", names_to = c("trend_axis"), names_prefix = "x") %>%
  mutate(indicator = substr(trend_axis, nchar(trend_axis), nchar(trend_axis))) %>%
  mutate(indicator = case_when(indicator=="4" ~ "Literacy",
                               indicator=="5" ~ "Numeracy")) %>%
  mutate(trend_axis = substr(trend_axis, 1,7)) %>%
  mutate(trend_axis = gsub("_", "/", trend_axis)) %>%
  
  # add spatial.scale column
  mutate(spatial.scale = ifelse(spatial.unit=="Scotland", "Scotland", "Council area")) %>%
  
  # add split columns
  mutate(split_name = "None",
         split_value = "None") 


# LAs, by deprivation (Table 11)

simd_LA <- get_attainment_data(sheetname="Table_11", startnumber=5) %>%
  
  select(trend_axis = year_note_9, spatial.unit=local_authority_note_7, split_value = simd_note_3, 
         Literacy = english_literacy_note_1_note_2_note_8, Numeracy = numeracy_note_1_note_2_note_8) %>%
  mutate(spatial.unit = ifelse(spatial.unit=="Edinburgh City", "City of Edinburgh", spatial.unit)) %>% # to ensure matches OK
  
  # reshape the data 
  pivot_longer(cols = c("Literacy", "Numeracy"), values_to="rate", names_to = "indicator") %>%
  
  # trend_axis column
  mutate(trend_axis = gsub("-", "/", trend_axis)) %>%
  
  # SIMD column
  format_simd_data() %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Council area")
# some 'NAs introduced by coercion' warnings: this is OK. 
# It's due to as.numeric() on cols that had suppression codes in them (here replaced with NA).



### Combine the data files
data_scot_la_simd <- rbind(#scotland, 
                            councils, # includes Scotland data
                            simd,
                            simd_LA) %>%
  
  # format year
  mutate(year = as.numeric(substr(trend_axis, 1, 4))) %>%
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% 
  select(-starts_with("spatial")) %>%
  
  # add in denominators for the cohort: so HB data can be calculated
  merge(y=cohort_P1_P4_P7, by.x=c("year", "split_value", "code"), by.y=c("year", "quintile", "code")) %>% # merges to rows with SIMD data and with split_value=="None"
  mutate(numerator = round(denominator * rate/100)) # back calculate the numerators

# Aggregate the CA data up to HB
data_hb <- data_scot_la_simd %>%
  filter(substr(code, 1, 3) == "S12") %>% # just the council areas
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code, -rate) %>%
  rename(code = hb2019) %>%
  group_by(indicator, code, split_name, split_value, year, trend_axis) |>
  summarise(numerator = sum_(numerator), # sum_ function from hablar keeps NA when there should be NA, and doesn't replace with 0 (if summed with na.rm=T). This helps to avoid Inf and NaN values from incomplete rate calcs. 
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  mutate(rate = 100 * numerator/denominator) %>%
  select(-denominator)

# Add in HB data
data_scot_la_simd_hb <- data_scot_la_simd %>%
  select(-denominator) %>%
  rbind(data_hb)
  

  
  
## Now read in the other population splits data -----

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
  mutate(split_value = case_when(split_value=="All pupils" ~ "Total", 
                                 TRUE ~ split_value)) %>%
  
  # add geog code
  mutate(code = "S0000000") %>%
  
  # add numerator
  mutate(numerator = as.numeric(NA))
  



### Combine all the data and finish processing
all_data <- rbind(data_scot_la_simd_hb,
                  data_popgroups) %>%
  
  # add def_period
  mutate(def_period = paste0("School year (", trend_axis, ")")) %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%

  # add CI columns
  mutate(lowci = as.numeric(NA),
         upci = as.numeric(NA)) %>% 
  
  # arrange so the points plot in right order in QA stage
  arrange(ind_id, code, split_name, split_value, year)


##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){

  # 1 - main data (ie data behind summary/trend/rank tab)

  main_data <- all_data %>% 
    filter(indicator == ind,
           split_name == "None") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  # save 
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # 2 - population groups data (ie data behind population groups tab)

  pop_grp_data <- all_data %>% 
  filter(indicator == ind & !(split_name %in% c("None", "Deprivation (SIMD)"))) %>% 
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year)

  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)

  
  # 3 - SIMD data (ie data behind deprivation tab)

  # Process SIMD data
  simd_data <- all_data %>% 
    filter(indicator == ind & split_name == "Deprivation (SIMD)") %>% 
    unique() %>%
    select(-indicator, -split_name) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type = "sc_quin") %>%
    arrange(code, year, quintile)
  
  # get arguments for the add_population_to_quintile_level_data() function: (done because the ind argument to the current function is not the same as the ind argument required by the next function)
  ind_name <- ind # dataset will already be filtered to a single indicator based on the parameter supplied to 'prepare final files' function
  ind_id <- unique(simd_data$ind_id) # identify the indicator number 
  
  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop="depr_pop_CYP_P1_P4_P7", # the lookup we processed above
                                          ind = ind_id,
                                          ind_name = ind_name) |>
    filter(!is.na(rate)) # not all years have data
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "Literacy")
prepare_final_files(ind = "Numeracy")

# Run QA reports 
# main data: 
run_qa(type = "main", filename = "Literacy", test_file = FALSE)
run_qa(type = "main", filename = "Numeracy", test_file = FALSE)

# ineq data: 
run_qa(type = "deprivation", filename = "Literacy", test_file=FALSE)
run_qa(type = "deprivation", filename = "Numeracy", test_file=FALSE)




#END

