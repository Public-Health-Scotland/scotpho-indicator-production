# todo - change final output file location away from test once new tool ready to deploy

###   Update ScotPHO Care and Wellbeing indicator: 
#   99131: Economic inactivity due to long term illness

# Geographies available: Scotland & local authority only 
# No SIMD deprivation split available. 
# Population group splits: Scotland (age & sex), Local Authority (sex)

# Data source is Annual Population Survey downloaded from Nomis

# For Scotland level with age and sex breakdowns:
# https://www.nomisweb.co.uk/datasets/aps181

# For local authority level with sex breakdowns:
# https://www.nomisweb.co.uk/datasets/apsnew


### functions/packages -----

source("1.indicator_analysis.R") 


### 1. Read in data ----

## Create a new Nomis query for Scotland level data ##
# Data source: annual population survey - regional - economic inactivity by reasons
# Geography: Scotland
# Date: 12 months to Dec, 2004 to 2023
# Age: 16 to 64, 16 to 24, 25 to 49, 50 to 64
# Percent: count, percent
# Sex: all persons, males, females
# Reasons: long-term sick


# Link to download the Nomis data query using an API
API_link_scot <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_181_1.data.csv?geography=2092957701&date=latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&c_sex=0...2&age=0...3&einact=4&c_wants=0&measure=1,3&measures=20100,20701")

# Read in data
raw_data_scot <- read_csv(API_link_scot)


## Create Nomis query for LA level data ##
# Data source: annual population survey
# Geography: Scottish local authorities
# Date: 12 months to Dec, 2004 to 2023
# Variable: economically inactive by reason category, % of economically inactive due to long-term sick (for all people, males and females)

# Link to download the Nomis data using an API
API_link_la <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1807745207...1807745210,1807745218,1807745211...1807745217,1807745222,1807745224,1807745226...1807745230,1807745221,1807745231,1807745233,1807745234,1807745236...1807745244,2092957701&date=latestMINUS77,latestMINUS73,latestMINUS69,latestMINUS65,latestMINUS61,latestMINUS57,latestMINUS53,latestMINUS49,latestMINUS45,latestMINUS41,latestMINUS37,latestMINUS33,latestMINUS29,latestMINUS25,latestMINUS21,latestMINUS17,latestMINUS13,latestMINUS9,latestMINUS5,latestMINUS1&variable=1496,1503,1510&measures=20599,21001,21002,21003")

# Read in data
raw_data_la <- read_csv(API_link_la)



### 2. Prepare data  -----

## Scotland level data ----

data_scot <- raw_data_scot %>% 
  
  # Clean column names
  clean_names() %>% 
  
  # Remove breakdowns by age *and* sex for now (keep breakdowns for totals)
  filter(!(c_sex_name %in% c("Males", "Females") & age_name %in% c("Aged 16-24", "Aged 25-49", "Aged 50-64"))) %>% 
  
  # Create new split name and split value columns
  mutate(split_name = case_when(c_sex_name %in% c("Males", "Females") ~ "Sex",
                                c_sex_name == "All persons" & age_name != "Aged 16-64" ~ "Age",
                                c_sex_name == "All persons" & age_name == "Aged 16-64" ~ "Total"),
         split_value = case_when(split_name == "Sex" ~ c_sex_name,
                                 split_name == "Age" ~ as.character(substr(age_name,6,10)),
                                 split_name == "Total" ~ "Total")) %>% 
  
  # Select relevant columns
  select(date, geography_code, split_name, split_value, measure_name, measures_name, obs_value) %>% 
  
  # Pivot value column to wider format
  pivot_wider(names_from = c(measure_name, measures_name),
              values_from = obs_value) %>% 
  
  # Rename columns
  rename(numerator = Count_Confidence,
         rate = Percent_Value,
         confidence = Percent_Confidence) %>% 
  
  # Remove column no longer needed
  select(!c(Count_Value))



## Local authority level data ----

data_la <- raw_data_la %>% 
  
  # Clean column names
  clean_names() %>%
  #remove scotland to avoid double counting
  filter(geography_name != "Scotland") %>%
  
  # Select relevant columns
  select(date, geography_code, variable_name, measures_name, obs_value) %>% 
  
  # Create new split name and split value columns
  mutate(split_name = case_when(str_detect(variable_name, "male|female") ~ "Sex",
                                !str_detect(variable_name, "male|female") ~ "Total"),
         
         split_value = case_when(str_detect(variable_name, "\\bmale") ~ "Males",
                                 str_detect(variable_name, "female") ~ "Females",
                                 !str_detect(variable_name, "male|female") ~ "Total")) %>% 
  
  # Pivot variables to wide format
  pivot_wider(names_from = measures_name, values_from = obs_value) %>% 
  
  # Rename columns
  rename(rate = Variable,
         numerator = Numerator,
         confidence = Confidence) %>% 
  
  # Remove columns no longer needed
  select(!c(variable_name, Denominator))



## Combine data files ----

data <- data_scot %>% 
  
  # Join LA data
  full_join(data_la) %>% 
  
  # Change Scotland area code
  mutate(geography_code = ifelse(geography_code == "S92000003", "S00000001", geography_code),
         
         # Create lower and upper CI variables
         lowci = rate - confidence,
         upci = rate + confidence,
         
         # Create additional date columns
         date = as.factor(str_sub(date, start = 1, end = 4)),
         def_period = paste0(date, " calendar year"),
         trend_axis = date,
         
         # Create indicator id column
         ind_id = 99131) %>% 
  
  # Rename columns
  rename(year = date,
         code = geography_code) %>% 
  
  # Remove confidence column no longer needed
  select(!confidence)




### 3. Prepare final files -----

# Save files in folder to be checked

# 1- main data file (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)

maindata <- data %>%
  filter(split_name=="Total") %>%
  select(-split_name,-split_value)

write.csv(maindata, paste0(data_folder, "Data to be checked/economic_inactivity_shiny.csv"), row.names = FALSE)
write_rds(maindata, paste0(data_folder, "Data to be checked/economic_inactivity_shiny.rds"))


# 2- population group data file (ie data behind population groups tab)

pop_grp_data <- data %>%
  filter(split_name!="Total")

write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/economic_inactivity_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/economic_inactivity_shiny_popgrp.rds"))


# Run QA reports and check the output files - QA report won't work until changes made to checking reports - come back to this
run_qa(filename = "economic_inactivity")



#END