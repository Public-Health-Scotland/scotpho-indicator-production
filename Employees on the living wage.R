#Indicator developed during early stages of Care and Wellbeing portfolio however never published and subsequently suprlass to requirement so script moved to archive folder


# update file paths to non-test output locations when profiles tool developments go live

###   Update ScotPHO Care and Wellbeing indicator: 
#   99111: Employees on the living wage (earning less than the Real Living Wage)

# Geographies available: Scotland & local authority only 
# No SIMD deprivation split available. 
# Population group splits: age & sex at Scotland level

# Data source is ONS Annual Survey of Hours and Earnings published on the Scottish Government website:
# https://www.gov.scot/publications/annual-survey-of-hours-and-earnings-2023/


### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

# Identify ASHE tables in received data folder
source_file <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), 
                                                         pattern="Annual survey of hours and earnings 2023"))


## 1 - Main data (sits behind summary/trend/rank tabs) ----

# Read in table of LA breakdowns (Table 5.7.1) for numerator and rate
# as additional years of data are added data ranges will need to be adjusted
data_ca_num <- read_excel(source_file, sheet = "Table_5_7_1", range = "A6:M39", na = "[c]")
data_ca_prop <- read_excel(source_file, sheet = "Table_5_7_1", range = "A41:M74", na = "[c]")


## 2 - Pop groups data (sits behind pop groups tabs) ----

## Sex breakdown ---

# Set ranges for all, men and women tables (Table 5.2.1)
# as additional years of data are added data ranges will need to be adjusted
ranges <- c("A6:C18", "A21:C33", "A36:C48")

# Function to read in tables
data_func <- lapply(ranges, function(i){
  
  x = read_excel(source_file, sheet = "Table_5_2_1", range = i)
  colnames(x) = c("year", "numerator", "rate")
  x$split_value = i
  x
  
})

# Save as one data frame
sex_data_raw <- do.call("rbind.data.frame", data_func) 


## Age breakdown ---

# Read in tables of age breakdowns (Table 5.3.1) for numerator and rate
# as additional years of data are added data ranges will need to be adjusted
age_data_num <- read_excel(source_file, sheet = "Table_5_3_1", range = "A6:M11")
age_data_prop <- read_excel(source_file, sheet = "Table_5_3_1", range = "A13:M18")


# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  filter(areatype %in% c("Scotland", "Council area")) %>% 
  select(!c(parent_area, areaname_full, areatype))


### 2. Prepare data  -----


## 1 - Main data

# Pivot LA numerator data to long format
data_ca_num_long <- data_ca_num %>%
  pivot_longer(where(is.numeric), names_to = "year", values_to = "numerator")


# Join numerator and rate data and wrangle into necessary format
main_data <- data_ca_prop %>%
  
  # Transform rate data into long format
  pivot_longer(where(is.numeric), names_to = "year", values_to = "rate") %>% 
  
  # Combine numerator and rate data
  left_join(data_ca_num_long, by = c("Local Authority", "year")) %>% 
  
  # Rename some columns
  rename(areaname = `Local Authority`) %>% 
  
         # Tidy area names
  mutate(areaname = str_replace_all(areaname, " and ", " & "),
         
         # Remove characters from year column
         year = as.numeric(str_remove_all(year, " \\[.*\\]")),
         
         # Numerator is currently in thousands so x1000 for actual figure
         numerator = numerator * 1000,
         
         # Rename "all" as "Scotland"
         areaname = str_replace_all(areaname, "All", "Scotland"),
         
         # Create date columns
         trend_axis = year,
         def_period = paste0(year, " calendar year"),
         
         # Create additional columns
         ind_id = 99111,
         lowci = NA, upci = NA) %>% 
  
  # Add geography codes
  left_join(dictionary, by = "areaname") %>% 
  
  # Filter for 2021 data onwards as earlier data isn't comparable
  filter(year >= 2021) %>% 
  
  # Remove areaname column as no longer needed
  select(!areaname)
  

## 2 - Pop groups data

# Pivot age numerator data into long format
age_data_num_long <- age_data_num %>% 
  pivot_longer(-`Age Group`, names_to = "year", values_to = "numerator")


# Combine age data and prepare to join to sex data
age_data <- age_data_prop %>% 
  
  # Pivot age rate data into long format
  pivot_longer(-`Age Group`, names_to = "year", values_to = "rate") %>% 
  
  # Join numerator data
  full_join(age_data_num_long, by = c("Age Group", "year")) %>% 
  
  # Define split name as age
  mutate(split_name = "Age") %>% 
  
  # Rename age group column as split value
  rename(split_value = `Age Group`)


# Combine age and sex data and wrangle into necessary format
pop_grp_data <- sex_data_raw %>% 
  
  # Define split name as sex
  mutate(split_name = "Sex") %>% 
  
  # Join age data
  full_join(age_data, by = c("year", "split_name", "split_value", "rate", "numerator")) %>% 
  
         # Remove characters from year column
  mutate(year = as.numeric(str_remove_all(year, " \\[.*\\]")),
         
         # Numerator is currently in thousands so x1000 for actual figure
         numerator = numerator * 1000,
         
         # Create date columns
         trend_axis = year,
         def_period = paste0(year, " calendar year"),
         
         # Transform ranges into sex categories
         split_value = str_replace_all(split_value, c("A6:C18" = "All",
                                                      "A21:C33" = "Men",
                                                      "A36:C48" = "Women")),
         # Create additional required columns
         code = "S00000001",
         ind_id = 99111,
         lowci = NA, upci = NA) %>% 
  
  # Filter for 2021 data onwards as earlier data isn't comparable
  filter(year >= 2021)



### 3. Prepare final files -----

# Save main data files in folder to be checked
write.csv(main_data, paste0(data_folder, "Data to be checked/employees_on_the_living_wage_shiny.csv"), row.names = FALSE)
write_rds(main_data, paste0(data_folder, "Data to be checked/employees_on_the_living_wage_shiny.rds"))


# Save pop group data files in folder for checking
write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/employees_on_the_living_wage_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/employees_on_the_living_wage_shiny_popgrp.rds"))


# Run QA report and check the output file
run_qa(filename = "employees_on_the_living_wage")


# END
