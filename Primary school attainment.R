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

# For inequalities calcs: (Scotland and council level)
# Published pupil census data don't provide the denominator granularity we need (quintile x stage x council area). 
# Denominator data obtained directly from Keith Hoy (school.stats@gov.scot). 
# These data are imported, processed, and saved to the population lookup folder at the start of this script. 


### functions/packages -----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R")

# Load additional packages
library(openxlsx)

##########################################################
### 1. Paths and lookups ----
##########################################################

# Identify data folder
cfe_data_folder <- paste0(data_folder, "Received Data/Curriculum for Excellence/")
file <- "ACEL+2324+-+Publication+-+Supplementary+tables+-+final.xlsx"
cohort <- "ACEL 23-24 Table 11 with cohort for Elizabeth Richardson.xlsx"

## Geography lookup -----

# Read in geography lookup
geo_lookup <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

## Population lookup -----

# Process the cohort data (requested from SG) and save in LUT folder
cohort_simd_LA <- read.xlsx(paste0(cfe_data_folder, cohort),
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
  mutate(quint_type = "sc_quin") %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Council area") %>%
  
  # adjust the LA names so they match with geo lookup
  mutate(spatial.unit = ifelse(spatial.unit=="Edinburgh City", "City of Edinburgh", spatial.unit)) %>% # to ensure matches OK
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% 
  
  # select required variables
  select(-stage, -starts_with("spatial"))

# Aggregate cohort populations to Scotland
cohort_simd_scotland <- cohort_simd_LA %>%
  group_by(year, quintile, quint_type) %>%
  summarise(denominator = sum(denominator, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(denominator = ifelse(denominator==0, as.numeric(NA), denominator)) %>% # islands that are missing some quintiles
  mutate(code = "S00000001") 

# Combine
cohort_P1_P4_P7 <- rbind(cohort_simd_LA, cohort_simd_scotland)
  
# Save file to lookups folder (for use in later calcs of inequalities metrics)
saveRDS(cohort_P1_P4_P7, paste0(lookups, "Population/depr_pop_CYP_P1_P4_P7.rds"))



##########################################################
### 2. Read in data ----
##########################################################

# Scotland, overall (Table 1)

scotland <- read.xlsx(paste0(cfe_data_folder, file),
                sheet = "Table_1",
                startRow = 5,
                colNames = TRUE) %>%
  clean_names() %>%
  rename_with(~ substr(.x, 1, 8)) %>% # lose the reference to notes by shortening all col names
  filter(stage == "P1, P4 and P7 combined") %>%
  filter(organise %in% c("Numeracy", "Literacy")) %>%
  rename(indicator = organise) %>%
  mutate(x2019_20 = replace(x2019_20, x2019_20=="[w]", NA),
         x2019_20 = as.numeric(x2019_20),
         x2020_21 = as.numeric(x2020_21)) %>%
  # reshape the data 
  pivot_longer(cols = c(x2016_17 : x2023_24), values_to="rate", names_to = "trend_axis", names_prefix = "x") %>%
  mutate(trend_axis = gsub("_", "/", trend_axis)) %>%

  # add spatial.scale column
  mutate(spatial.scale = "Scotland") %>%
  
  # add spatial.unit column
  mutate(spatial.unit = "Scotland") %>%
  
  # add split columns
  mutate(split_name = "None",
         split_value = "None") %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%
  
  # select required variables
  select(-stage) 
  
# Local Authorities (Table 10):

councils <- read.xlsx(paste0(cfe_data_folder, file),
                      sheet = "Tables_10.4a_b_c_d_e",
                      startRow = 7,
                      colNames = TRUE) %>%
  clean_names() %>%
  
  # Select relevant columns
  select(ends_with(c("_4", "_5"))) %>% # the numeracy (_5) and literacy (_4) columns
  
  # select one column for the local authority names
  rename(spatial.unit = local_authority_note_8_4) %>%
  mutate(spatial.unit = ifelse(spatial.unit=="Edinburgh City", "City of Edinburgh", spatial.unit)) %>% # to ensure matches OK
  select(-starts_with("local")) %>%
  replace(.=="[w]", NA) %>% #turn all others numeric (after replacing the missing codes used in 2019/20)
  mutate(across(-spatial.unit, ~ as.numeric(.x))) %>%
  
  # reshape the data 
  pivot_longer(-spatial.unit, values_to="rate", names_to = c("trend_axis"), names_prefix = "x") %>%
  mutate(indicator = substr(trend_axis, nchar(trend_axis), nchar(trend_axis))) %>%
  mutate(indicator = case_when(indicator=="4" ~ "Literacy",
                               indicator=="5" ~ "Numeracy")) %>%
  mutate(trend_axis = substr(trend_axis, 1,7)) %>%
  mutate(trend_axis = gsub("_", "/", trend_axis)) %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Council area") %>%
  
  # add split columns
  mutate(split_name = "None",
         split_value = "None") %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) 


# Scotland, by deprivation (Table 2.4)
simd <- read.xlsx(paste0(cfe_data_folder, file),
                    sheet = "Table_2_4",
                    startRow = 5,
                    colNames = TRUE) %>%
  clean_names() %>%
  rename_with(~ substr(.x, 1, 8)) %>% # lose the reference to notes by shortening all col names
  filter(stage == "P1, P4 and P7 combined") %>%
  filter(organise %in% c("Numeracy", "Literacy")) %>%
  rename(indicator = organise) %>%
  mutate(x2019_20 = replace(x2019_20, x2019_20=="[x]", NA),
         x2019_20 = as.numeric(x2019_20)#,
         #x2020_21 = as.numeric(x2020_21)
         ) %>%
  
  # reshape the data 
  pivot_longer(cols = c(x2016_17 : x2023_24), values_to="rate", names_to = "trend_axis", names_prefix = "x") %>%
  mutate(trend_axis = gsub("_", "/", trend_axis)) %>%
  
  # SIMD column
  rename(split_value = simd_not) %>%
  mutate(split_value = case_when(split_value=="SIMD Quintile 1 - Most Deprived" ~ "1",
                                 split_value=="SIMD Quintile 2" ~ "2",
                                 split_value=="SIMD Quintile 3" ~ "3",
                                 split_value=="SIMD Quintile 4" ~ "4",
                                 split_value=="SIMD Quintile 5 - Least Deprived" ~ "5",
                                 split_value=="Total" ~ "Total")) %>%
  filter(!is.na(split_value)) %>%
  
  # add split columns
  mutate(split_name = "Deprivation (SIMD)") %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Scotland") %>%
  
  # add spatial.unit column
  mutate(spatial.unit = "Scotland") %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%
  
  # select required variables
  select(-stage)

# LAs, by deprivation (Table 11)
simd_LA <- read.xlsx(paste0(cfe_data_folder, file),
                  sheet = "Table_11",
                  startRow = 5,
                  colNames = TRUE) %>%
  clean_names() %>%
  select(trend_axis = year_note_9, stage, spatial.unit=local_authority_note_7, split_value = simd_note_3, 
         Literacy = english_literacy_note_1_note_2_note_8, Numeracy = numeracy_note_1_note_2_note_8) %>%
  filter(stage == "P1, P4 and P7 combined") %>%

  # reshape the data 
  pivot_longer(cols = c("Literacy", "Numeracy"), values_to="rate", names_to = "indicator") %>%
  
  # trend_axis column
  mutate(trend_axis = gsub("-", "/", trend_axis)) %>%
  
  # convert missing/suppressed values to NA
  mutate(rate = ifelse(rate %in% c("[w]", "[c]", "[x]", "c"), NA, rate), # replace the missing codes 
         rate = as.numeric(rate)) %>% # convert to numeric
  
  
  # SIMD column
  mutate(split_value = case_when(split_value %in% c("SIMD Quintile 1 - most deprived", "SIMD Quintile 1") ~ "1",
                                 split_value=="SIMD Quintile 2" ~ "2",
                                 split_value=="SIMD Quintile 3" ~ "3",
                                 split_value=="SIMD Quintile 4" ~ "4",
                                 split_value=="SIMD Quintile 5 - least deprived" ~ "5",
                                 split_value=="Local Authority Total" ~ "Total")) %>%
  filter(!is.na(split_value)) %>%
  
  # add split columns
  mutate(split_name = "Deprivation (SIMD)") %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Council area") %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%
  
  # select required variables
  select(-stage)


## Population splits -----

# Table 3: Scot by sex
# Table 4: Scot by ethnicity
# Table 5: Scot by urban-rural status

table3 <- read.xlsx(paste0(cfe_data_folder, file),
                    sheet = "Table_3",
                    startRow = 5,
                    colNames = TRUE) %>%
  rename(split_value = Sex) %>%
  mutate(split_name = "Sex")
table4 <- read.xlsx(paste0(cfe_data_folder, file),
                    sheet = "Table_4",
                    startRow = 5,
                    colNames = TRUE) %>%
  rename(split_value = Ethnicity) %>%
  mutate(split_name = "Ethnicity")
table5 <- read.xlsx(paste0(cfe_data_folder, file),
                    sheet = "Table_5",
                    startRow = 5,
                    colNames = TRUE) %>%
  rename(split_value = "Urban.Rural.Classification.[note.5]") %>%
  mutate(split_name = "Urban-Rural status")

popgroups <- rbind(table3, table4, table5) %>%
  clean_names() %>%
  rename_with(~ substr(.x, 1, 8)) %>% # lose the reference to notes by shortening all col names
  filter(stage == "P1, P4 and P7 combined") %>%
  rename(trend_axis = year_not,
         Literacy = literacy,
         Numeracy = numeracy,
         split_value = split_va,
         split_name = split_na) %>%
  mutate(trend_axis = gsub("-", "/", trend_axis)) %>%
  
  # reshape the data 
  pivot_longer(cols = c(reading_ : Numeracy), values_to="rate", names_to = "indicator") %>%
  filter(indicator %in% c("Numeracy", "Literacy")) %>%
  filter(!(split_value %in% c("Not Disclosed / Unknown", "Unknown"))) %>%
  mutate(rate = replace(rate, rate %in% c("[x]", "[w]"), NA),
         rate = as.numeric(rate),
         split_value = case_when(split_value=="All pupils" ~ "Total", 
                                 TRUE ~ split_value)) %>%
  
  # add spatial.scale column
  mutate(spatial.scale = "Scotland") %>%
  
  # add spatial.unit column
  mutate(spatial.unit = "Scotland") %>%
  
  # add ind_id column
  mutate(ind_id = case_when(indicator == "Numeracy" ~ 30158,
                            indicator == "Literacy" ~ 30157)) %>%
  
  # select required variables
  select(-stage) 



### Combine the data files
all_data <- rbind(scotland, 
                  councils,
                  popgroups, 
                  simd,
                  simd_LA) %>%
  
  # add the geog codes, 
  merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% 
  
  # add def_period
  mutate(def_period = paste0("School year (", trend_axis, ")")) %>%
  
  # format year
  mutate(year = as.numeric(substr(trend_axis, 1, 4))) %>%
  
  # add numerator and CI columns
  mutate(numerator = as.numeric(NA),
         lowci = as.numeric(NA),
         upci = as.numeric(NA)) %>% 
  
  # required columns
  select(-starts_with("spatial")) %>%
  
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
    unique() 
  
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)

  pop_grp_data <- all_data %>% 
  filter(indicator == ind & !(split_name %in% c("None", "Deprivation (SIMD)"))) %>% 
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value,) 

  # Save
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))

  
  # 3 - SIMD data (ie data behind deprivation tab)

  # Process SIMD data
  simd_data <- all_data %>% 
    filter(indicator == ind & split_name == "Deprivation (SIMD)") %>% 
    unique() %>%
    select(-indicator, -split_name) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type = "sc_quin")
  
  # Save intermediate SIMD file
  write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  #get ind_id argument for the analysis function 
  ind_id <- unique(simd_data$ind_id)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
                                 pop = "depr_pop_CYP_P1_P4_P7", # the lookup we processed above
                                 ind_id, 
                                 ind
  )
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "Literacy")
prepare_final_files(ind = "Numeracy")

# # Run QA reports 
# # main data: 
run_qa(filename = "Literacy")
run_qa(filename = "Numeracy")

# ineq data: # NOT RUNNING BECAUSE DOESN'T HAVE HBS???
run_ineq_qa(filename = "Literacy")
run_ineq_qa(filename = "Numeracy")



# Manually check the SIMD data instead:

# Plot the indicator(s)
# =================================================================================================================
# Let's now see what the series look like:
# (uses the last indicator processed)

# by pop group split 
pop_grp_data_result %>%
  ggplot(aes(year, rate, group = split_value, colour = split_value, shape = split_value)) + 
  geom_point() + geom_line() +
  facet_wrap(~split_name, scales = "free_y") 

# by SIMD 
simd_data_result %>%
  ggplot(aes(year, rate, group = quintile, colour = quintile, shape = quintile)) + 
  geom_point() + geom_line() +
  facet_wrap(~code, scales = "free_y") 



#END

