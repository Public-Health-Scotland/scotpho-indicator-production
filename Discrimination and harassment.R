# to do:
# update file paths to non-test output locations when profiles tool developments go live
# update with more recent data once Data Explorer updated in December 2024


###   Update ScotPHO Care and Wellbeing indicators sourced from Scottish Household Survey: 
#   99134: Experience of discrimination
#   99135: Experience of harassment

# Data source is the Scottish Household Survey Data Explorer, Tables 4.28 and 4.29:
# https://scotland.shinyapps.io/sg-scottish-household-survey-data-explorer/

# Data on the Data Explorer only available to 2019 but due to be updated in December 2024 with data up to 2023

# Both indicators have breakdowns by LA, gender, age, SIMD, and long-term conditions


### functions/packages -----
source("1.indicator_analysis.R")


### 1. Read in data ----

# Identify data folder
shs_data_folder <- paste0(data_folder, "Received Data/Scottish Household Survey/")

# Identify data filse (create list of data files)
data_files <- paste0(shs_data_folder, list.files(path = shs_data_folder, pattern = "discrimination and harassment"))

# Define column names
column_names <- c("row_num", "year", "areaname", "split_value", 
                  "yes_dis", "no_dis", "yes_har", "no_har", "all", "base")

# Read in data files
data_raw <- read_csv(data_files, skip = 1, col_names = column_names, na = c("*", "-"))

# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  filter(areatype %in% c("Scotland", "Council area")) %>% 
  select(!c(parent_area, areaname_full))



### 2. Prepare data  -----

data <- data_raw %>% 
  
  # Remove irrelevant columns
  select(!c(row_num, no_dis, no_har, all, base)) %>% 
  
  # Pivot discrimination and harassment values into long format
  pivot_longer(cols = c("yes_dis", "yes_har"), names_to = "indicator", values_to = "rate") %>% 
  
         # Create required date columns
  mutate(trend_axis = year,
         def_period = paste0(year, " survey year"),
         
         # Tidy area names to match geography lookup
         areaname = str_replace_all(areaname, c(" and " = " & ",
                                                "Na-h Eileanan Siar" = "Na h-Eileanan Siar",
                                                "Edinburgh, City of" = "City of Edinburgh")),
         
         # Create new split name column from split value
         split_name = case_when(split_value == "All" ~ "Total",
                                split_value %in% c("Male", "Female", "Another way", "Refused") ~ "Gender",
                                split_value %in% c("16-39", "40-59", "60+") ~ "Age",
                                split_value %in% c("20% most deprived", "2", "3", "4", "20% least deprived") ~ "Scottish Index of Multiple Deprivation",
                                str_detect(split_value, "condition") ~ "Long-term physical/mental health condition"),
         
         # Tidy split value for long-term condition and SIMD
         split_value = str_replace_all(split_value, "Long-term physical/mental health condition - ", ""),
         split_value = str_replace_all(split_value, c("20% most deprived" = "1 - most deprived",
                                                      "20% least deprived" = "5 - most deprived")),
         
         # Create indicator ID column
         ind_id = case_when(indicator == "yes_dis" ~ 99134,
                            indicator == "yes_har" ~ 99135),
         
         # Create additional required columns
         numerator = NA,
         lowci = NA, upci = NA) %>% 
  
         # Remove breakdowns other than total, age, gender, SIMD, and long-term condition
  filter(!is.na(split_name),
         
         # Remove categories of breakdowns not of interest (suppressed due to small sample sizes)
         !split_value %in% c("Another way", "Refused", "Don't know", "Refusal")) %>% 
  
  # Join geography codes
  left_join(dictionary, by = "areaname") %>%
  
  # Remove duplicate Scotland and LA data (due to total present in each source file)
  unique() %>% 
  
  # Remove columns no longer needed
  select(!c(indicator, areatype, areaname))



### 3. Prepare final files -----

# 1 - main data (ie data behind summary/trend/rank tab)

# Remove pop groups breakdowns and associated columns
main_data <- data %>%
  filter(split_name == "Total") %>% 
  select(!c(split_name, split_value))

# Save discrimination main data (id 99134)
dis_main_data <- main_data %>% 
  filter(ind_id == 99134)

write.csv(dis_main_data, paste0(data_folder, "Data to be checked/experienced_discrimination_shiny.csv"), row.names = FALSE)
write_rds(dis_main_data, paste0(data_folder, "Data to be checked/experienced_discrimination_shiny.rds")) 

# Save harassment main data (id 99135)
har_main_data <- main_data %>% 
  filter(ind_id == 99135)

write.csv(har_main_data, paste0(data_folder, "Data to be checked/experienced_harassment_shiny.csv"), row.names = FALSE)
write_rds(har_main_data, paste0(data_folder, "Data to be checked/experienced_harassment_shiny.rds"))  



# 2 - population groups data (ie data behind population groups tab)

# Save the total rows as a separate data frame so we can add them
# back in more than once (in order to display an "all" category for
# breakdowns in the pop groups tab)
pop_grp_all_data <- data %>% 
  filter(split_name == "Total")


# Add total rows back into data frame for each breakdown (excl. SIMD)
pop_grp_data <- data %>% 
  
  # Rename split_name for existing total rows as "Age"
  mutate(split_name = str_replace_all(split_name, "Total", "Age")) %>% 
  
  # Add in total rows again and rename for gender
  bind_rows(pop_grp_all_data) %>% 
  mutate(split_name = str_replace_all(split_name, "Total", "Gender")) %>% 
  
  # Add in total rows again and rename for long-term conditions
  bind_rows(pop_grp_all_data) %>% 
  mutate(split_name = str_replace_all(split_name, "Total", "Long-term physical/mental health condition"))


# Save discrimination pop groups data (id 99134)
dis_pop_grp_data <- pop_grp_data %>% 
  filter(ind_id == 99134)

write.csv(dis_pop_grp_data, paste0(data_folder, "Test Shiny Data/experienced_discrimination_shiny_popgrp.csv"), row.names = FALSE)
write_rds(dis_pop_grp_data, paste0(data_folder, "Test Shiny Data/experienced_discrimination_shiny_popgrp.rds"))


# Save harassment pop groups data (id 99135)
har_pop_grp_data <- pop_grp_data %>% 
  filter(ind_id == 99135)

write.csv(har_pop_grp_data, paste0(data_folder, "Test Shiny Data/experienced_harassment_shiny_popgrp.csv"), row.names = FALSE)
write_rds(har_pop_grp_data, paste0(data_folder, "Test Shiny Data/experienced_harassment_shiny_popgrp.rds"))
  

