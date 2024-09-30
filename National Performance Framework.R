#new indicators introduced for care and wellbeing portfolio
# TO DO: enable automated checking at end of the indicator production
#         create a deprivation data file output (for any indicators that may have sutiable data)

###   Update ScotPHO Care and Wellbeing indicators: 
#   99116: Persistent poverty
#   99117: Young peoples mental wellbeing (was known as 'Child wellbeing and happiness' in NPF but naming conventioned expected to change and we are adopting new name)
#   99118: Child material deprivation
#   99121: Health risk behaviours
#   99123: Gender balance in organisations (for minority ethnic population)


# Data source is the National Performance Framework open data on statistics.gov.scot
# 2024 update: https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx

### functions/packages ----
source("1.indicator_analysis.R") 


### 1 - Read in data -----

# Specify url of the NPF file to download from stats.gov
url <- "https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx"

# Specify file name and where to save file to
file_name <- "NPF_database_2024.xlsx"
file_path <- paste0(data_folder, "Received Data/")

# Download file
download.file(url = url, destfile = paste(file_path, file_name, sep = ""))

# Read in data file
dat <- read_xlsx(paste0(file_path, file_name))


### 2. Prepare data  -----

data <- dat %>%
  
  # Clean column names
  clean_names() %>% 
  
  # Select relevant indicators
  filter(indicator %in% c("Persistent poverty", # capitalisation change in 2024 data?
                          "Child Wellbeing and Happiness", #NPF name for young peoples mental wellbeing indicator
                          "Child material deprivation",
                          "Children's material deprivation",
                          "Health risk behaviours",
                          "Gender balance in organisations")) %>%
  
  # Persistent poverty has splits labelled the wrong way round in 2024 data: reverse these
  mutate(temp_breakdown = ifelse(indicator=="Persistent poverty", disaggregation, breakdown),
         temp_disagg = ifelse(indicator=="Persistent poverty", breakdown, disaggregation)) %>%
  
  select(-c(breakdown, disaggregation)) %>%
  
  rename(breakdown = temp_breakdown,
         disaggregation = temp_disagg) %>%


         # Convert indicator names to lower case and hyphenate 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_"),
         
         indicator = str_replace_all(indicator, "children's", "child"),
         
         # Ensure age breakdowns are named consistently
         breakdown = str_replace_all(breakdown, "Age ", ""),
         breakdown = str_replace_all(breakdown, "-", " to "),
         # Add hyphen back in where needed:
         breakdown = if_else(breakdown == "Non to Limiting Longstanding Illness", "Non-Limiting Longstanding Illness", breakdown),
         breakdown = if_else(breakdown == "Working to age adults", "Working-age adults", breakdown),
         
         
         # Ensure SIMD breakdowns are named consistently
         breakdown = str_replace_all(breakdown, "SIMD ", ""),
         breakdown = if_else(str_detect(breakdown, "$1$|1st|(?i)most"), "1 - most deprived", breakdown),
         breakdown = if_else(str_detect(breakdown, "$2$|2nd"), "2", breakdown),
         breakdown = if_else(str_detect(breakdown, "$3$|3rd"), "3", breakdown),
         breakdown = if_else(str_detect(breakdown, "$4$|4th"), "4", breakdown),
         breakdown = if_else(str_detect(breakdown, "$5$|5th|(?i)least"), "5 - least deprived", breakdown),
         
         breakdown = str_replace_all(breakdown, "Total", "All"), # this series used in pop group data file
         
         # Remove characters from year column
         year = if_else(str_detect(year, "(excl. 2020)"), "2017-2021", year),
         
         # Add indicator ids
         ind_id = case_when(indicator == "persistent_poverty" ~ 99116,
                            indicator == "child_wellbeing_and_happiness" ~ 99117,
                            indicator == "child_material_deprivation" ~ 99118,
                            indicator == "health_risk_behaviours" ~ 99121,
                            indicator == "gender_balance_in_organisations" ~ 99123),
         
         # Create date variables
         trend_axis = year,
         year = case_when(indicator %in% c("health_risk_behaviours", "gender_balance_in_organisations") ~ as.numeric(year),
                          !indicator %in% c("health_risk_behaviours", "gender_balance_in_organisations") ~ as.numeric(str_sub(trend_axis, start= 1, end = 4))+2),
         def_period = case_when(indicator == "persistent_poverty" ~ paste0("5-year aggregate (",trend_axis,")"),
                                indicator == "child_wellbeing_and_happiness" ~ paste0("4-year aggregate (",trend_axis,")"),
                                indicator == "child_material_deprivation" ~ paste0("4-year aggregate (",trend_axis,")"),
                                indicator == "health_risk_behaviours" ~ paste0(year, " survey year"),
                                indicator == "gender_balance_in_organisations" ~ paste0(year, " calendar year")),
         
         # Create some other new variables
         numerator = NA, 
         lowci = NA, upci = NA,
         rate = as.numeric(figure),
         code = "S00000001") %>%
  
  # Rename columns
  rename(split_name = disaggregation,
         split_value = breakdown) %>% 
  
  # Select breakdowns of interest
  filter(split_name %in% c("Total",
                           "Age",
                           "Scottish Index of Multiple Deprivation",
                           "SIMD",
                           "Local Authority",
                           "HSC partnership",
                           "Health board",
                           "Gender",
                           "Sex",
                           "Disability", #gender balance
                           "Ethnicity", #gender balance
                           "Urban Rural classification",
                           "Total Difficulties Score",  
                           "Total Difficulties Score X Sex",
                           "Total Difficulties Score X Age",
                           "Total Difficulties Score X SIMD",
                           "Total Difficulties Score X Equivalised Income",
                           "Total Difficulties Score X Limiting Longstanding Illness",
                           	"Disability of household member(s)" # child mat deprivation
                           )) %>% 
  
  # Further tidy breakdown names
  mutate(split_name = str_replace_all(split_name, "Total Difficulties Score X ", ""),
         split_name = str_replace_all(split_name, "Total Difficulties Score", "Total"),
         split_name = case_when(split_name=="Equivalised Income" ~ "Income (equivalised)",
                                split_name=="SIMD" ~"Deprivation (SIMD)",
                                split_name=="Scottish Index of Multiple Deprivation"~"Deprivation (SIMD)",
                                split_name=="Urban Rural classification" ~"Urban/Rural",
                                TRUE ~ split_name)) %>%
  
  # Ensure equivalised income quintiles are named consistently
  mutate(split_value = case_when(split_value=="Top Quintile" ~ "1 - highest income",
                                 split_value=="Bottom Quintile" ~ "5 - lowest income", 
                                 TRUE ~ split_value)) %>%
  
  # Select relevant variables
  select(c(ind_id, indicator, code, split_name, split_value, year, trend_axis, def_period, rate, numerator, lowci, upci)) %>%
  
  #rename indicator to fit new name that NPF will adopt
  mutate(indicator = case_when (indicator=="child_wellbeing_and_happiness" ~ "young_peoples_mental_wellbeing", TRUE ~indicator)) %>%
  
  # Reorder data frame
  arrange(indicator, code, year)
  


### 3. Prepare final files -----

# Function to prepare final files:
# Creates two data files for each indicator (main data vs population group data)
prepare_final_files <- function(ind){
   
  #Save total rows (to later add back in to pop groups data)
  total <- data %>% 
    filter(indicator == ind,
           split_value == "All") # uses split_value instead of split_name as persistent poverty doesn't have "total" in split_name
  
  # 1 - Main data 
  # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
  maindata <- total %>%
    select(code, ind_id, year,numerator,rate,lowci,upci,def_period, trend_axis) %>% #select fields required for maindata file (ie summary/trend/rank tab)
    unique()
  

  # 2 - Population group data 
  # (ie data behind population groups tab)


  # Young people's mental wellbeing
  # Add additional total rows (to show an "all" category) for age, sex and LLI breakdowns
  if(ind == "young_peoples_mental_wellbeing"){
    
    #need to run horrible fix to ensure age groups sort in the correct order
    #select only the data that contains age group split and mutate values to desired sort order
    pop_grp_data_age <- data %>%
      filter(indicator == ind) %>%
      mutate(split_name = str_replace_all(split_name, "Total", "Age")) |>
            filter(split_name =="Age") |>
      mutate(split_value = case_when(split_value == "4 to 6" ~ "a_4 to 6", 
                                     split_value == "7 to 9" ~ "b_7 to 9",
                                     split_value == "10 to 12" ~ "c_10 to 12",
                                     split_value == "All" ~ "z_All",
                                     TRUE ~ split_value)) %>%
      arrange(ind_id,code,year,split_name, split_value) |>
      mutate(split_value = trimws(substr(split_value,3,11))) #trim white space and remove sort precursor to return split value to sensible string
    
    
    pop_grp_data  <- data |>
      filter(indicator == ind) %>%
      filter(split_name !="Age") %>% #remove the age split data (this group will be added back in next line with data sorted correctly)
      arrange(ind_id,code,year,split_name, split_value) %>%
      mutate(split_name = str_replace_all(split_name, "Total", "Sex")) %>% 
      bind_rows(total) %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Limiting Longstanding Illness")) %>% 
      bind_rows(total) %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Deprivation (SIMD)")) %>%
      bind_rows(total) %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Income (equivalised)")) %>% 
      bind_rows(pop_grp_data_age)

    # Child material deprivation
    # Add additional total rows (to show an "all" category) for age and disability breakdowns
  } else if(ind == "child_material_deprivation"){
    
    pop_grp_data <- data %>%
      filter(indicator == ind) %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Age")) %>% 
      bind_rows(total) %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Disability of household member(s)"))
    
    # Health risk behaviours
    # Add additional total rows (to show an "all" category) for age, gender, disability and urban/rural breakdowns
  } else if(ind == "health_risk_behaviours"){
    
    pop_grp_data <- data %>%
      filter(indicator == "health_risk_behaviours") %>% 
      mutate(split_name = str_replace_all(split_name, "Total", "Age")) %>% 
      filter(!split_value == "All" | !year %in% c(2012, 2013, 2014, 2016, 2021)) %>% # Removes total rows for years with no age breakdowns
      bind_rows(total %>% filter(year %in% c(2016, 2017, 2018, 2019))) %>%  # Only bind new rows of total data for years with gender breakdowns
      mutate(split_name = str_replace_all(split_name, "Total", "Gender")) %>% 
      bind_rows(total %>% filter(year %in% c(2016, 2017, 2018, 2019))) %>%  # Only bind new rows of total data for years with gender breakdowns
      mutate(split_name = str_replace_all(split_name, "Total", "Deprivation (SIMD)")) %>% 
      bind_rows(total %>% filter(year %in% c(2017, 2018, 2019))) %>%  # Only bind new rows of total data for years with disability breakdowns
      mutate(split_name = str_replace_all(split_name, "Total", "Disability"))  %>% 
      bind_rows(total %>% filter(year %in% c(2017, 2018, 2019))) %>%  # Only bind new rows of total data for years with urban/rural breakdowns
      mutate(split_name = str_replace_all(split_name, "Total", "Urban/Rural"))
    
    # Gender balance in organisations
    # Already have "all" categories for each breakdown
    # Remove "total" from split_name so it doesn't show as a breakdown
  } else if(ind == "gender_balance_in_organisations"){
    
    pop_grp_data <- data %>%
      filter(indicator == ind,
             split_name != "Total") 
    
    # Persistent poverty
    # Already includes "all" category for age breakdown and no "total" under split name to remove
  } else {
    
    pop_grp_data <- data %>%
      filter(indicator == ind) 
  }
   
    pop_grp_data <- pop_grp_data %>%
    select(ind_id, code, year, numerator,rate,lowci,upci,def_period, trend_axis, split_name, split_value) #select fields required for popgroup data file (linked to pop group tab)
   
    # Save files in folder to be checked
    write.csv(maindata, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.csv"), row.names = FALSE)
    write_rds(maindata, paste0(data_folder, "Test Shiny Data/", ind, "_shiny.rds"))
    
    write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
    write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
    
    # Make data created available outside of function so it can be visually inspected if required
    maindata_result <<- maindata
    popgrpdata_result <<- pop_grp_data
    
}


# Create final files and run QA reports - QA report won't work until changes made to checking reports - come back to this

# Indicator 99116: Persistent poverty ----
prepare_final_files(ind = "persistent_poverty")

#run_qa(filename = "persistent_poverty") #come back to fix qa report - failing because no NHS board or ca geographies ins some of these indcators

# Indicator 99117: Young peoples mental wellbeing  ----
prepare_final_files(ind = "young_peoples_mental_wellbeing")

  
# Indicator 99118: Child material deprivation ----
prepare_final_files(ind = "child_material_deprivation")


# Indicator  99121: Health risk behaviours ----
prepare_final_files(ind = "health_risk_behaviours")


# Indicator 99123: Gender balance in organisations (for minority ethnic population)
prepare_final_files(ind = "gender_balance_in_organisations")





#END

