# ============================================================================
# ===== Processing PHS HOSPITAL STATS (SMR01 AND SMR04) =====
# ============================================================================

# NOTES ON THE INDICATOR

# 1 adult MH indicator: 
# ind_id = 30006 = "Rate of alcohol-related hospitalisation from mental and behavioural disorders due to alcohol use, per 100,000 adults."

# Whether rate of patients or stays was not specified. I opted to use stays, as this is the headline stat the ARHS team use in their publications. 

# Data provided by Scott Kilgariff, mailto:phs.alcohol@phs.scot, in Sept 2024
# Single xlsx containing rates and CIs: IR2024-00519-MBD_final.xlsx

# Please see attached for the data you have requested for patients/stays from 16 years and older.
# •	For the SIMD quintile figures I have used a top age group of 85+ whilst for the rest of the figures I have used a top age group of 90+ when calculating EASR.
# •	SIMD has been applied as follows:
# o	1996 to 2003 – SIMD 2004 has been used
# o	2004 to 2006 – SIMD 2006 has been used
# o	2007 to 2009 – SIMD 2009(v2) has been used
# o	2010 to 2011 – SIMD 2012 has been used (NB differs from the ISD GPD guidance which applies SIMD2012 to 2010-2013 data)
# o	2012 to 2016 – SIMD 2016 has been used
# o	2017+ – SIMD 2020(v2) has been used
# •	Only Scottish residents have been included
# •	Only those patients/stays where the sex is known have been included
# •	You’ll see in the attached that some of the population figures per group have decimal values. This is due to removing 0.2 of the 15-19 age group to create the 16-19 age group.
# •	o_lower is the lower end of the 95% CI for the number of observed cases
# •	o_upper is the higher end of the 95% CI for the number of observed cases


# Load in the packages

### functions/packages -----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R")

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
arhs_data_folder <- paste0(data_folder, "Received Data/Alc-related hosp stays - mental and behavioural disorders/")
file <- "IR2024-00519-MBD_final.xlsx"


# Import PHS ARHS data for MBD indicator

stays_sex <- read.xlsx(paste0(arhs_data_folder, file),
                                sheet = "stays_by_sex",
                                colNames = TRUE) %>%
  mutate(split_name = "Sex") %>%
  select(year, split_name,
         numerator= observed,
         code=SGcode,
         rate=easr_rate,
         lowci=easr_lower,
         upci=easr_upper,
         split_value=sex_label) 

stays_sex_hb <- read.xlsx(paste0(arhs_data_folder, file),
                            sheet = "stays_by_sex_hb",
                            colNames = TRUE) %>%
  mutate(split_name = "Sex") %>%
  select(year, split_name,
         numerator= observed,
         code=SGcode,
         rate=easr_rate,
         lowci=easr_lower,
         upci=easr_upper,
         split_value=sex_label) 

stays_sex_ca <- read.xlsx(paste0(arhs_data_folder, file),
                            sheet = "stays_by_sex_ca",
                            colNames = TRUE) %>%
  mutate(split_name = "Sex") %>%
  select(year, split_name,
         numerator= observed,
         code=SGcode,
         rate=easr_rate,
         lowci=easr_lower,
         upci=easr_upper,
         split_value=sex_label) 

stays <- rbind(stays_sex,
               stays_sex_ca,
               stays_sex_hb) %>%
  mutate(split_value = case_when(split_value=="Both" ~ "Total", TRUE ~ split_value)) %>% 
  mutate(trend_axis = as.character(year),
         def_period = paste0("Calendar year (", year, ")"),
         ind_id=30006)
  
stays_sex_simd <- read.xlsx(paste0(arhs_data_folder, file),
                            sheet = "stays_by_sex_simd",
                            colNames = TRUE) %>%
  mutate(code="S00000001") %>% #all are Scotland
  select(year, 
         numerator= observed,
         code,
         rate=easr_rate,
         lowci=easr_lower,
         upci=easr_upper,
         quintile=SIMDquintile,
         sex=sex_label) %>%
  mutate(sex = case_when(sex=="Both" ~ "Total", TRUE ~ sex),
         trend_axis = as.character(year),
         def_period = paste0("Calendar year (", year, ")"),
         ind_id=30006,
         quint_type = "sc_quin",
         quintile = as.character(quintile))
# get SIMD totals to add to the quintile data
stays_simd <- stays_sex_simd %>%
  group_by(year, sex, code) %>%
  summarise() %>% # gives the breakdowns we need totals for
  ungroup() %>%
  merge(y=stays, by.x=c("year", "code", "sex"), by.y=c("year", "code", "split_value")) %>%
  mutate(quint_type="sc_quin",
         quintile="Total") %>%
  select(-split_name) %>%
  rbind(stays_sex_simd)
  



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: 
prepare_final_files <- function(ind){
  
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data, total pop
  main_data <- stays %>% 
    filter(split_value == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() 
  
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland, LA and HB data by sex (including total)
  pop_grp_data <- stays %>% # includes the contents of main data, as the 'total' for each split
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value,) 
  
  # Save
  write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data, paste0(data_folder, "Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  
  # 3 - SIMD data (ie data behind deprivation tab)
  # Contains Scotland data by SIMD quintile 
  
  # Process SIMD data

  # Save intermediate SIMD file
  write_rds(stays_simd, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.rds"))
  write.csv(stays_simd, file = paste0(data_folder, "Prepared Data/", ind, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  #get ind_id argument for the analysis function 
  ind_id <- unique(stays_simd$ind_id)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(ind, "_shiny_depr"), 
                                 pop = "depr_pop_16+", 
                                 ind_id, 
                                 ind
  )
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data

  
}


# Run function to create final files
prepare_final_files(ind = "ARHS_mental_and_behav")

# Run QA reports
# main data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_qa(filename = "ARHS_mental_and_behav")
