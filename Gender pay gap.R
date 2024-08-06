###   Update ScotPHO Care and Wellbeing indicator: 
#   99110: Gender Pay Gap

# Data source for Local Authority breakdowns is Annual Survey of Hours and 
# Earnings (ASHE) - workplace analysis, downloaded from NOMIS:
# https://www.nomisweb.co.uk/datasets/ashe

# Data source for age breakdowns is Scottish Government ASHE tables:
# https://www.gov.scot/publications/annual-survey-of-hours-and-earnings-2023/


### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

## Create a new Nomis query for API link ##
# Geography: Scotland, Scottish Local Authorities
# Date: 2021, 2022, 2023 (estimates before 2021 not comparable)
# Pay and hours: Hourly pay - excluding overtime
# Sex & full/part-time: male full-time, female full-time
# Variable: median

# Link to download the Nomis data query using an API
API_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_99_1.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS2-latest&sex=1,3&item=2&pay=6&measures=20100,20701")

# Read in data
raw_data <- read_csv(API_link)


# Identify SG data file with age breakdowns
age_file <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), pattern="Annual survey of hours and earnings"))

# Read in age data file
# Gender pay gap by age is in Table 2.4
raw_age_data <- read_excel(age_file, sheet = "Table_2_4", skip = 4)



### 2. Prepare data  -----

## Main data ----

data <- raw_data %>%
  
  # Clean column names
  clean_names() %>%
  
  # Remove confidence estimates
  filter(measures_name == "Value") %>% 
  
  # Select relevant columns
  select(date, geography_name, geography_code, sex_name, obs_value) %>% 
  
  # Pivot male and female values to wide format
  pivot_wider(names_from = sex_name, values_from = obs_value) %>% 
  
  # Calculate difference between earnings as a % of male earnings and round to 1 dp
  mutate(rate = round(((`Male Full Time Workers` - `Female Full Time Workers`) / `Male Full Time Workers`) * 100, 1),
         
         # Rename Scotland area code
         geography_code = ifelse(geography_code == "S92000003", "S00000001", geography_code)) %>% 
  
  # Rename columns
  rename(year = date,
         code = geography_code) %>%
  
  # Remove unnecessary columns
  select(!c(geography_name, `Male Full Time Workers`, `Female Full Time Workers`)) %>% 
  
  # Create new columns
  mutate(trend_axis = year,
         def_period = paste0(year, " calendar year"),
         ind_id = 99110,
         lowci = NA, upci = NA, 
         numerator = NA)


## Population groups data ----

age_data <- raw_age_data %>% 
  
  # Remove notes and aged 65+ column (as mostly suppressed)
  select(!c(Notes, `65+`)) %>% 
  
  # Pivot age columns to long format
  pivot_longer(!Year, names_to = "split_value", values_to = "rate") %>% 
  
  # Clean column names
  clean_names() %>% 
  
  # Add Scotland geography code
  mutate(code = "S00000001",
         
         # Create new date columns
         year = as.numeric(str_sub(year, 1, 4)),
         trend_axis = year,
         def_period = paste0(year, " calendar year"),
         
         # Create other new columns
         split_name = "Age",
         ind_id = 99110,
         numerator = NA,
         lowci = NA, upci = NA) %>% 
  
  # Filter for 2021 onwards (previous estimates not comparable)
  filter(year >= 2021)


### 3. Prepare final files -----
  
# Main data
write.csv(data, paste0(data_folder, "Data to be checked/gender_pay_gap_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/gender_pay_gap_shiny.rds"))

# Population groups data
write.csv(age_data, paste0(data_folder, "Test Shiny Data/gender_pay_gap_shiny_popgrp.csv"), row.names = FALSE)
write_rds(age_data, paste0(data_folder, "Test Shiny Data/gender_pay_gap_shiny_popgrp.rds"))
  

# Run QA reports for each indicator check the output files
run_qa(filename = "gender_pay_gap")


# END