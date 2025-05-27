# TO- DO - this indicator include numerator and denominator data - in theory we could switch processing to use ScotPHO functions that would enable NHS board & HSCP data to be derived.

# Indicator:    Economic inactivity due to long term ill health
# Indicator ID: 99131
# Description:  % who are economically inactive due to long term ill health (percentage of those who are economically inactive)

# Geographies available: Scotland & local authority only 
# No SIMD deprivation split available. 
# Population group splits: Scotland (age & sex), Local Authority (sex)

# NOTE: Don't confuse this indicator with similar ScotPHO indicator reporting economic inactive rate (ID 99141)- where the denominator is total population 


#  Data source is the Annual Population Survey (APS)/Labour force Survey published by NOMIS 
#  https://www.nomisweb.co.uk/


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R")

###########################################################################################.
## Read in data  ----
# Note two extracts required to obtain data from NOMIS which is held in two separate tables
###########################################################################################.

#DATA FILE 1:

## Create a new Nomis query for Scotland level data ##
# Economic inactivity due to long term ill health split by age can only be access at a Scotland level - this comes from different table 
# Data source: annual population survey - regional - economic inactivity by reasons
# (https://www.nomisweb.co.uk/datasets/aps181)
# Select "query data"
# Geography: Countries = Scotland
# Date: 12 months to Dec, 2004 to 2023
# Age: 16 to 64, 16 to 24, 25 to 49, 50 to 64
# Percent: count, percent
# Reasons: long-term sick (untick option for total)
# Sex: all persons, males, females
# Wants a job : only select total
# Step 8 : select NOMIS API
# Step 9 : copy link to CSV into api link in R script below


# Link to download the Nomis data query using an API (provides figures for those economically inactive due to long term ill health by age and sex but at scotland level only)
#api_scotland <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_181_1.data.csv?geography=2092957701&date=latestMINUS79,latestMINUS75,latestMINUS71,latestMINUS67,latestMINUS63,latestMINUS59,latestMINUS55,latestMINUS51,latestMINUS47,latestMINUS43,latestMINUS39,latestMINUS35,latestMINUS31,latestMINUS27,latestMINUS23,latestMINUS19,latestMINUS15,latestMINUS11,latestMINUS7,latestMINUS3&c_sex=0...2&age=0...3&einact=4&c_wants=0&measure=1,3&measures=20100,20701")

api_scotland<-c("https://www.nomisweb.co.uk/api/v01/dataset/NM_181_1.data.csv?geography=2092957701&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&c_sex=0...2&age=0...3&einact=4&c_wants=0&measure=1,3&measures=20100,20701")

# Read in data
raw_data_scot <- read_csv(api_scotland) |>
  clean_names()# Clean column names
  

#DATA FILE 2:

## Create a new Nomis query for local authority level data ##
# Economic inactivity due to long term ill health with splits by local authorities and local authuroity by sex available
# Data source: "annual population survey" (select from the current tables)
# (https://www.nomisweb.co.uk/query/construct/summary.asp?menuopt=200&subcomp=) - (selecting extract at variables level)
# Select "query data"
# 	-STEP 1: Geography: Select >  Local authorities > Some> then tick all within Scotland
#   -STEP 2: Date (12 months to December) – pick time periods required – full series if refreshing all or single year to add one year to existing dataset)
# 	-STEP 3: 
#             in category drop down : select 'economically inactive by reason'
#            tick % of economically inactive due to long-term sick (for all people, males and females) (so 3 options in total)
# Step 4 : select NOMIS API
# Step 5 : copy link to CSV into api link in R script below


#api_la <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792&date=latestMINUS79,latestMINUS75,latestMINUS71,latestMINUS67,latestMINUS63,latestMINUS59,latestMINUS55,latestMINUS51,latestMINUS47,latestMINUS43,latestMINUS39,latestMINUS35,latestMINUS31,latestMINUS27,latestMINUS23,latestMINUS19,latestMINUS15,latestMINUS11,latestMINUS7,latestMINUS3&variable=1496,1503,1510&measures=20599,21001,21002,21003")

api_la <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=1496,1503,1510&measures=20599,21001,21002,21003")

# Read in data
raw_data_la <- read_csv(api_la) |>
  clean_names()# Clean column names




###########################################################################################.
## Prepare data files ----
###########################################################################################.

## Scotland level data ----

data_scot <- raw_data_scot %>% 
  #create numeric year column
  mutate(year = as.numeric(str_sub(date, start= 1,4)),
         #replace scotland geo code with scotpho version
         geography_code = if_else(geography_code == "S92000003", "S00000001", geography_code)) |>

  
  # Remove breakdowns by age *and* sex for now (keep breakdowns for totals)
  filter(!(c_sex_name %in% c("Males", "Females") & age_name %in% c("Aged 16-24", "Aged 25-49", "Aged 50-64"))) %>% 
  
  # Create new split name and split value columns
  mutate(split_name = case_when(c_sex_name %in% c("Males", "Females") ~ "Sex",
                                c_sex_name == "All persons" & age_name != "Aged 16-64" ~ "Age",
                                c_sex_name == "All persons" & age_name == "Aged 16-64" ~ "Total"),
         split_value = case_when(split_name == "Sex" ~ c_sex_name,
                                 split_name == "Age" ~ as.character(substr(age_name,6,10)),
                                 split_name == "Total" ~ "All")) %>% #split name total (labelled all to denote all sexes or all age groups)
  
  # Select relevant columns
  select(year, geography_code, split_name, split_value, measure_name, measures_name, obs_value) %>% 

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

  #make sure theres not scotland level to avoid double counting
  filter(geography_name != "Scotland") %>%
  mutate(year = as.numeric(str_sub(date, start= 1,4))) |> #derive year field
  
  # Select relevant columns
  select(year, geography_code, variable_name, measures_name, obs_value) %>% 
  
  # Create new split name and split value columns
  mutate(split_name = case_when(str_detect(variable_name, "male|female") ~ "Sex",
                                !str_detect(variable_name, "male|female") ~ "Total"),
         
         split_value = case_when(str_detect(variable_name, "\\bmale") ~ "Males",
                                 str_detect(variable_name, "female") ~ "Females",
                                 !str_detect(variable_name, "male|female") ~ "All")) %>% 
  
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
  
    mutate(
         # Create lower and upper CI variables
         lowci = rate - confidence,
         upci = rate + confidence,
         
         # Create additional date columns
         def_period = paste0(as.character(year), " calendar year"),
         trend_axis = year,
         
         # Create indicator id column
         ind_id = 99131) %>% 
  
  # Rename columns
  rename(code = geography_code) %>% 
  
  # Remove confidence column no longer needed
  select(!confidence)



###########################################################################################.
## Prepare final shiny files ----
###########################################################################################.


# Save files in folder to be checked

# 1- main data file (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)

maindata <- data %>%
  filter(split_name=="Total") %>%
  select(-split_name,-split_value)

write.csv(maindata, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_illhealth_shiny.csv"), row.names = FALSE)
write_rds(maindata, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_illhealth_shiny.rds"))


run_qa(filename="economic_inactivity_illhealth", type="main", test=FALSE)



# 2- population group data file (ie data behind population groups tab)

# Save the total rows as a separate data frame so we can include
# them more than once (in order to display an "all" category for
# breakdowns in the pop groups tab)
pop_grp_all_data <- data %>%
  filter(split_name=="Total")

pop_grp_data <- data %>% 
  
  # Rename split_name for existing total rows as "Age"
  mutate(split_name = str_replace_all(split_name, "Total", "Age")) %>% 
  
  # Add in total rows again and rename for sex
  bind_rows(pop_grp_all_data) %>% 
  mutate(split_name = str_replace_all(split_name, "Total", "Sex"))


write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_illhealth_shiny_popgrp.csv"), row.names = FALSE)
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/economic_inactivity_illhealth_shiny_popgrp.rds"))

run_qa(filename="economic_inactivity_illhealth", type="popgrp", test=FALSE)

#END
