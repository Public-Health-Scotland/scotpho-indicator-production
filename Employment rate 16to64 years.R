# Indicators:  Employment rate (percentage of population in employment):
# 99133: Employment rate 16to64 years

# Description:  % of total population who are employed

# Geographies available: Scotland & local authority only - although we could feed through functions to generate NHS board HSCP figures
# No SIMD deprivation split available. 
# Population group splits: Scotland (age & sex), Local Authority (age & sex)


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R")


###############################################.
## Read in data  ----
###############################################.

# To find data table behind the data update follow these selections in the NOMIS data download tool found at the url above

#  - Data downloads (from https://www.nomisweb.co.uk/)
# 	-Query		(https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)
# 	-Annual population survey (APS)
#
# 	-STEP 1: Geography  (select countries>some> : Scotland ,  Local authorities - District (within Scotland)>some> then tick all within Scotland)
#   -STEP 2: Date (12 months to December) – pick time periods required – full series if refreshing all or single year to add one year to existing dataset)
# 	-STEP 3: 
#             in category drop down : select 'employment rate by age'
#                                     for all sexes tick : 16-64, 16-19, 20-24, 25-34, 35-49, 50-64
#                                     for males/females tick : 16-64   [so a total of 8 options in total]
#             in category drop down : select 'key variables'
#                                     tick: Employment rate females - aged 16-64
#                                           Employment rate males - aged 16-64
#                                           Employment rate - aged 16-64
#                                           aged 16-64 employment rate - ethnic minority
#   -STEP 4: choose option for 'Nomis API'
#   -STEP 5: Copy link to download data as csv into R script below (the link is the reference for API_link object)


# Link comes from the NOMIS website after completing the query building steps
# if rerunning check that in data returned specifies Jan-Dec year ending figures
API_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS79,latestMINUS75,latestMINUS71,latestMINUS67,latestMINUS63,latestMINUS59,latestMINUS55,latestMINUS51,latestMINUS47,latestMINUS43,latestMINUS39,latestMINUS35,latestMINUS31,latestMINUS27,latestMINUS23,latestMINUS19,latestMINUS15,latestMINUS11,latestMINUS7,latestMINUS3&variable=594,45...49,51,54,63&measures=20599,21001,21002,21003")


# Reads the API as a csv using readr and assigns it the name raw_data
raw_data <- read_csv(API_link) %>%
  clean_names()  # Clean column data

### 2. Prepare data  -----

data <- raw_data %>%
  
  #create numeric year column
  mutate(year = as.numeric(str_sub(date, start= 1,4)),
         #replace scotland geo code with scotpho version
         geography_code = if_else(geography_code == "S92000003", "S00000001", geography_code)) |>
  
  #select only the columns required so its easier to scan dataframe
  select(year, geography_name, geography_code, variable_name, measures_name, obs_value) |>
  
  # Renames the column "variable_name" to "split_value"
  rename(split_value = variable_name) %>%
  
  # Create 'split-name' column based on conditions applied to 'split_value'. 
  mutate(split_name = case_when(str_detect(split_value,"males|females") ~ "Sex",#If 'split_value' contains either "males" or "females", categorize as "Sex" within split_name
                                str_detect(split_value,"aged 16-64 employment rate - ethnic minority") ~ "Ethnic minority",
                                
                                # If split value exactly equals "Employment rate - aged 16-64" categorise as "Total" within split_name
                                split_value == "Employment rate - aged 16-64" ~ "Total",
                                
                                # If split value does not contain "16-64" categorise as "Age" within split_name 
                                !str_detect(split_value, "16-64") ~ "Age"),

         # Clean and modify 'split_value' by removing certain text and standardising the format
         split_value = str_replace_all(split_value, c("Employment rate - " = "",
                                                      "aged" = "Aged")),
         
         # Further categorise and clean 'split_value' based on conditions
         split_value = case_when(split_name == "Total" ~ "All (16-64 years)", 
                                 split_name == "Ethnic minority" ~ "Ethnic minority (16-64 years)", 
                                 split_value == "Employment rate males - Aged 16-64" ~ "Males (16-64 years)",
                                 split_value == "Employment rate females - Aged 16-64" ~ "Females (16-64 years)",
                                 str_detect(split_value, "Aged") ~ split_value)) %>%

  # Pivot the data to a wider format, spreading the 'obs_value' across new columns based on 'measures_name'
  pivot_wider(names_from = measures_name, values_from = obs_value) %>%
  
  # Calculate the lower and upper confidence intervals
  mutate(lowci = Variable - Confidence, 
         upci = Variable + Confidence, 
         
         # Extract year from the 'date' column and create new columns
         def_period = paste0(as.character(year), " calendar year"),
         trend_axis = year,
         
         # Create indicator id column
         ind_id = 99133) %>% 
  
  # Rename some columns
  rename(rate = Variable,
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
write.csv(maindata, paste0(profiles_data_folder, "/Data to be checked/employment_rate_16to64_shiny.csv"), row.names = FALSE)
write_rds(maindata, paste0(profiles_data_folder, "/Data to be checked/employment_rate_16to64_shiny.rds"))


run_qa(filename="employment_rate_16to64", type="main", test=FALSE)


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
  
  # Add in total rows again and rename for ethnic minority
  bind_rows(pop_grp_all_data) %>% 
  mutate(split_name = str_replace_all(split_name, "Total", "Ethnic minority")) %>% 
  
  arrange(code, year)

# Save files in folder for checking
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/employment_rate_16to64_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/employment_rate_16to64_shiny_popgrp.rds"))

#no QA report yet - just eyeball figures