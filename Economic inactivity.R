###   Update ScotPHO Care and Wellbeing indicator: 
#   99131: Economic inactivity due to long term illness

# Data source is Annual Population Survey - economic inactivity by reason, downloaded from Nomis:
# https://www.nomisweb.co.uk/datasets/aps181


### functions/packages -----

source("1.indicator_analysis.R") 


### 1. Read in data ----

## Create a new Nomis query for API link ##
# Geography: Scotland (no LA breakdowns available)
# Date: 12 months to Dec, 2004 to 2023
# Age: 16 to 64, 16 to 24, 25 to 49, 50 to 64
# Percent: count, percent
# Sex: all persons, males, females
# Reasons: long-term sick


# Link to download the Nomis data query using an API
API_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_181_1.data.csv?geography=2092957701&date=latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&c_sex=0...2&age=0...3&einact=4&c_wants=0&measure=1,3&measures=20100,20701")

# Read in data
raw_data <- read_csv(API_link)


### 2. Prepare data  -----

data <- raw_data %>% 
  
  # clean column names
  clean_names() %>% 
  
  # Select relevant columns
  select(date, geography_code, c_sex_name, age_name, measure_name, measures_name, obs_value) %>% 
  
  # Pivot value column to wider in order to calculate CIs
  pivot_wider(names_from = c(measure_name, measures_name),
              values_from = obs_value) %>% 
  
  # Remove breakdowns by age and sex for now (keep breakdowns for totals)
  filter(!(c_sex_name %in% c("Males", "Females") & age_name %in% c("Aged 16-24", "Aged 25-49", "Aged 50-64"))) %>% 
  
  # Change Scotland area code
  mutate(geography_code = ifelse(geography_code == "S92000003", "S00000001", geography_code),
         
         # Create lower and upper CI variables
         lowci = Percent_Value - Percent_Confidence,
         upci = Percent_Value + Percent_Confidence,
         
         # Create additional date columns
         date = as.factor(str_sub(date, start = 1, end = 4)),
         def_period = paste0(date, " calendar year"),
         trend_axis = date,
         
         # Create indicator id column
         ind_id = 99131,
         
         # Create new columns to identify age and sex splits
         split_name = case_when(c_sex_name %in% c("Males", "Females") ~ "Sex",
                                c_sex_name == "All persons" & age_name != "Aged 16-64" ~ "Age",
                                c_sex_name == "All persons" & age_name == "Aged 16-64" ~ "Total"),
         split_code = case_when(split_name == "Sex" ~ c_sex_name,
                                split_name == "Age" ~ age_name,
                                split_name == "Total" ~ "All persons")) %>% 
  
  # Rename columns
  rename(year = date,
         code = geography_code,
         rate = Percent_Value,
         numerator = Count_Value) %>% 
  
  # Remove columns no longer needed
  select(!c(Count_Confidence, Percent_Confidence, c_sex_name, age_name))



### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/economic_inactivity_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/economic_inactivity_shiny.rds"))


# Run QA reports and check the output files
run_qa(filename = "economic_inactivity")



#END