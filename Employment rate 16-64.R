# to do - change final output file location away from test once new tool ready to deploy

###   Update ScotPHO Care and Wellbeing indicator: 
#   99133: Employment rate for 16-64 year olds

# Geographies available: Scotland & local authority only 
# No SIMD deprivation split available. 
# Population group splits: Scotland (age & sex), Local Authority (age & sex)

# Data source is Annual Population Survey downloaded from Nomis
# https://www.nomisweb.co.uk/datasets/apsnew


### functions/packages -----

#source("1.indicator_analysis.R") 
source("functions/main_analysis.R")



### 1. Read in data ----

## Create a new Nomis query ##
# Data source: annual population survey
# Geography: Scotland, Local Authorities
# Date: 12 months to Dec, 2004 to 2024
# Variable: 
    # Category: Employment rate by age
        # Total: 16-64, 16-19, 20-24, 25-34, 35-49, 50-64
        # Males: 16-64
        # Females: 16-64


# Link to download the Nomis data using an API
API_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1807745207...1807745210,1807745218,1807745211...1807745217,1807745222,1807745224,1807745226...1807745230,1807745221,1807745231,1807745233,1807745234,1807745236...1807745244,2092957701&date=latestMINUS77,latestMINUS73,latestMINUS69,latestMINUS65,latestMINUS61,latestMINUS57,latestMINUS53,latestMINUS49,latestMINUS45,latestMINUS41,latestMINUS37,latestMINUS33,latestMINUS29,latestMINUS25,latestMINUS21,latestMINUS17,latestMINUS13,latestMINUS9,latestMINUS5,latestMINUS1&variable=45...49,51,54,63&measures=20599,21001,21002,21003")

# Reads the API as a csv using readr and assigns it the name raw_data
raw_data <- read_csv(API_link)



### 2. Prepare data  -----

data <- raw_data %>%
  
  # Clean column data
  clean_names() %>%
  
  # Renames the column "variable_name" to "split_value"
  rename(split_value = variable_name) %>%
  
  # Change Scotland area code
  mutate(geography_code = if_else(geography_code == "S92000003", "S00000001", geography_code),
         
         # Create a new column 'split_name' based on conditions applied to 'split_value'. 
         # If 'split_value' contains either "males" or "females", categorize as "Sex" within split_name
         split_name = case_when(str_detect(split_value,"males|females") ~ "Sex",
                                
                                # If split value exactly equals "Employment rate - aged 16-64" categorise as "Total" within split_name
                                split_value == "Employment rate - aged 16-64" ~ "Total",
                                
                                # If split value does not contain "16-64" categorise as "Age" within split_name 
                                !str_detect(split_value, "16-64") ~ "Age"),
         
         # Clean and modify 'split_value' by removing certain text and standardising the format
         split_value = str_replace_all(split_value, c("Employment rate - " = "",
                                                      "aged" = "Aged")),
         
         # Further categorise and clean 'split_value' based on conditions
         split_value = case_when(split_name == "Total" ~ "All", 
                                 split_value == "Employment rate males - Aged 16-64" ~ "Males",
                                 split_value == "Employment rate females - Aged 16-64" ~ "Females",
                                 str_detect(split_value, "Aged") ~ split_value)) %>%
  
  
  # Selects specific columns from the data frame                            
  select(geography_name, geography_code, split_value, split_name, date, measures_name, obs_value) %>%
  
  # Pivot the data to a wider format, spreading the 'obs_value' across new columns based on 'measures_name'
  pivot_wider(names_from = measures_name, values_from = obs_value) %>%
  
  # Calculate the lower and upper confidence intervals
  mutate(lowci = Variable - Confidence, 
         upci = Variable + Confidence, 
         
         # Extract year from the 'date' column and create new columns
         date = as.factor(str_sub(date, start = 1, end = 4)),
         def_period = paste0(date, " calendar year"),
         trend_axis = date,
         
         # Create indicator id column
         ind_id = 99133) %>% 
  
  # Rename some columns
  rename(rate = Variable,
         year = date,
         numerator = Numerator,
         code = geography_code) %>%
  
  # Remove columns no longer needed  
  select(!c(geography_name, Denominator, Confidence))



### 3. Prepare final files -----

# 1- main data file (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)

maindata <- data %>%
  filter(split_name=="Total") %>%
  select(-split_name,-split_value)

# Save files in folder for checking
write.csv(maindata, paste0(profiles_data_folder, "/Data to be checked/employment_rate_shiny.csv"), row.names = FALSE)
write_rds(maindata, paste0(profiles_data_folder, "/Data to be checked/employment_rate_shiny.rds"))


run_qa(filename="employment_rate", type="main")


# 2- population group data file (ie data behind population groups tab)

# Save the total rows as a separate data frame so we can add them
# back in more than once (in order to display an "all" category for
# breakdowns in the pop groups tab)
pop_grp_all_data <- data %>% 
  filter(split_name == "Total")


pop_grp_data <- data %>%
  
  # Rename split_name for existing total rows as "Age"
  mutate(split_name = str_replace_all(split_name, "Total", "Age")) %>% 
  
  # Add in total rows again and rename for sex
  bind_rows(pop_grp_all_data) %>% 
  mutate(split_name = str_replace_all(split_name, "Total", "Sex")) %>% 
  arrange(code, year)

# Save files in folder for checking
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/employment_rate_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/employment_rate_shiny_popgrp.rds"))

#no QA report yet - just eyeball fil

#END