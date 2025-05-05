# TO- DO - this indicator include numerator and denominator data - in theory we could switch processig to use ScotPHO functions that would enable NHS board & HSCP data to be derived.

# Indicator:    Economic inactivity rate (16-64 years)
# Indicator ID: 99141
# Description:  % of total population (aged 16-64) who are economically inactive

# Population group splits at Scotland & local authority level include age groups (16-19,20-24,25-29,30-34,35-49,50-64m, male/female/all, ethnic minorities)
# Geographies available: Scotland & local authority only - although we could feed through functions to generate NHS board HSCP figures
# No SIMD deprivation split available.

# NOTE: Don't confuse this indicator with similar ScotPHO indicator reporting % who are economically inactive due to long term ill health (ID99131) - where the denominator is total population economically inactive 
# (rather than total population as is the case with this indicator)

#  Data source is the Annual Population Survey (APS)/Labour force Survey published by NOMIS 
#  https://www.nomisweb.co.uk/


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R")

###############################################.
## Read in data  ----
###############################################.

# To find data table behind the data update follow these selections in the NOMIS data download tool found at the url above

# https://www.nomisweb.co.uk/default.asp [click 'query data']
# Data source: Annual Population Survey/Labour Force Survey > Current > Annual Population Survey

# Geography: countries (select 'some') and pick Scotland 
#             regional authorities (select some) then pick all within the Scotland region

# Date: 12 months to Dec, 2004 to 2023

# Data sections (its possible to tick multiple boxes) 
# selection from the 'category' dropdown: "economically inactive by age" (then pick: all sexes 16-64, plus all the age sub-groups e.g 16-19,20-24,25-29,30-34,35-49,50-64, for males/females only pick 16-64)
# selection from category dropwdown: "Key variables" = "% of ethnic minority aged 16-64 who are economically inactive"

# select option to download as Nomis API
# right click on option to download as csv and select 'copy link' then paste into API link
# (if re-running check that in data returned  the 'date_name' column specifies Jan-Dec year ending figures)

#API_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS79,latestMINUS75,latestMINUS71,latestMINUS67,latestMINUS63,latestMINUS59,latestMINUS55,latestMINUS51,latestMINUS47,latestMINUS43,latestMINUS39,latestMINUS35,latestMINUS31,latestMINUS27,latestMINUS23,latestMINUS19,latestMINUS15,latestMINUS11,latestMINUS7,latestMINUS3&variable=112...115,117,111,120,129,841&measures=20599,21001,21002,21003")

API_link <-c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=841,111...115,117,120,129&measures=20599,21001,21002,21003")

# Reads the API as a csv using readr and assigns it the name raw_data
raw_data <- read_csv(API_link) %>%
  clean_names()  # Clean column dataase

### 2. Prepare data  -----

data <- raw_data %>%
  
  #create numeric year column
  mutate(year = as.numeric(str_sub(date, start= 1,4)),
         #replace scotland geo code with scotpho version
         geography_code = if_else(geography_code == "S92000003", "S00000001", geography_code)) |>
  
  #select only the columns required so its easier to scan dataframe
  select(year, geography_name, geography_code, variable_name, measures_name, obs_value) |>
  
  
  # Renames the column "variable_name" to "split_value"
  rename(split_value = variable_name) |>
  

  # Create 'split-name' column based on conditions applied to 'split_value'.
  mutate(
         
         # Create a new column 'split_name' based on conditions applied to 'split_value'. 
         # If 'split_value' contains either "males" or "females", categorize as "Sex" within split_name
         split_name = case_when(str_detect(split_value,"males|females") ~ "Sex",
                                str_detect(split_value,"% of  ethnic minority aged 16-64 who are economically inactive") ~ "Ethnic minority",
                                
                                # If split value exactly equals "% economically inactive - aged 16-64" categorise as "Total" within split_name
                                split_value == "% who are economically inactive - aged 16-64" ~ "Total",
                                
                                # If split value does not contain "16-64" categorise as "Age" within split_name 
                                !str_detect(split_value, "16-64") ~ "Age"),
         
         # Clean and modify 'split_value' by removing certain text and standardising the format
         split_value = str_replace_all(split_value, c("% who are economically inactive - " = "",
                                                      "aged" = "Aged")),
         
         # Further categorise and clean 'split_value' based on conditions
         split_value = case_when(split_name == "Total" ~ "All (16-64 years)", 
                                 split_name == "Ethnic minority" ~ "Ethnic minority (16-64 years)", 
                                 split_value == "% of  males who are economically inactive - Aged 16-64" ~ "Males (16-64 years)",
                                 split_value == "% of females who are economically inactive - Aged 16-64" ~ "Females (16-64 years)",
                                 str_detect(split_value, "Aged") ~ split_value)) |>

  # Pivot the data to a wider format, spreading the 'obs_value' across new columns based on 'measures_name'
  pivot_wider(names_from = measures_name, values_from = obs_value) %>%
  

  # Calculate the lower and upper confidence intervals
  mutate(lowci = Variable - Confidence, 
         upci = Variable + Confidence, 
         
         # Extract year from the 'date' column and create new columns
         def_period = paste0(as.character(year), " calendar year"),
         trend_axis = year,
         
         # Create indicator id column
         ind_id = 99141) %>% 
  
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
write.csv(maindata, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_rate_shiny.csv"), row.names = FALSE)
write_rds(maindata, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_rate_shiny.rds"))


run_qa(filename="economic_inactivity_rate", type="main", test_file=FALSE)
#note QA report throws up errors around measure being outwith confidence intervals for some -this occurs for some of the orkney/shetland values which have no
#confidence intervals generated in later years - NOMIS meta data suggest values are too small to produce reliable estimates.In these cases we can't provide CI.


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
  
  # Do not Add in total rows for ethnic minority since the percentage ethnic minority represents
  # percentage of economically inactive population that are ethnic minority and not
  # percentage of the ethnic minor population who are economically inactive.

  arrange(code, year)

# Save files in folder for checking
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_rate_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_rate_shiny_popgrp.rds"))


# run qa report against pop groups indicator file
run_qa(filename="economic_inactivity_rate", type="popgrp", test_file=FALSE)



#END