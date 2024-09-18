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
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fnational-performance-framework


### functions/packages ----
source("1.indicator_analysis.R") 


### 1 - Read in data -----

# Specify url of the NPF file to download from stats.gov
url <- "https://statistics.gov.scot/downloads/file?id=30a07b3c-762f-41ee-bc20-8d0468b92c7f%2FALL+NPF+INDICATORS+-+2023+-+statistics.gov.scot+NPF+database+excel+file+-+May+2024.xlsx"

# Specify file name and where to save file to
file_name <- "NPF_database.xlsx"
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
  filter(indicator %in% c("Persistent Poverty",
                          "Child Wellbeing and Happiness", #NPF name for young peoples mental wellbeing indicator
                          "Child material deprivation",
                          "Children's material deprivation",
                          "Health risk behaviours",
                          "Gender balance in organisations")) %>%

         # Convert indicator names to lower case and hyphenate 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_"),
         
         indicator = str_replace_all(indicator, "children's", "child"),
         
         # Ensure age breakdowns are names consistently
         breakdown = str_replace_all(breakdown, "Age ", ""),
         breakdown = str_replace_all(breakdown, "-", " to "),
         
         # Ensure SIMD breakdowns are named consistently
         breakdown = str_replace_all(breakdown, "SIMD ", ""),
         breakdown = if_else(str_detect(breakdown, "$1$|1st|(?i)most"), "1 - most deprived", breakdown),
         breakdown = if_else(str_detect(breakdown, "$2$|2nd"), "2", breakdown),
         breakdown = if_else(str_detect(breakdown, "$3$|3rd"), "3", breakdown),
         breakdown = if_else(str_detect(breakdown, "$4$|4th"), "4", breakdown),
         breakdown = if_else(str_detect(breakdown, "$5$|5th|(?i)least"), "5 - least deprived", breakdown),
         
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
                                TRUE ~ split_name)) %>%
  
  # Ensure equivalised income quintiles are named consistently
  mutate(split_value = case_when(split_value=="Top Quintile" ~ "1 - highest income",
                                 split_value=="Bottom Quintile" ~ "5 - Lowest income", 
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
  
    # Filter for main data 
    # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
    maindata <- data %>%
      filter(indicator == ind,
             split_value == "Total") %>%
      select(code, ind_id, year,numerator,rate,lowci,upci,def_period, trend_axis) %>% #select fields required for maindata file (ie summary/trend/rank tab)
      unique()
    
    # Filter for population group data
    # (ie data behind population groups tab)
    pop_grp_data <- data %>%
      filter(indicator == ind,
             split_value != "Total") %>%
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

  # horrible fix for the age band sort order within this indicator - can't think of a way to implement in main
  # body of code since this would disturb sort order across other indicators or splits
  # age groups are character strings and won't sort correctly - could convert to a factor but this method works

  popgrpdata_result_age <- popgrpdata_result |>
  filter(split_name =="Age") |>
  mutate(split_value = case_when(split_value == "4 to 6" ~ "a_4 to 6", 
                                 split_value == "7 to 9" ~ "b_7 to 9",
                                 split_value == "10 to 12" ~ "c_10 to 12", TRUE ~ split_value)) %>%
  arrange(ind_id,code,year,split_name, split_value) |>
  mutate(split_value = trimws(substr(split_value,3,11))) #trim white space and remove sort precursor


    popgrpdata_result <- popgrpdata_result |>
      filter(split_name !="Age") |>
      arrange(ind_id,code,year,split_name, split_value)
  
  popgrpdata_result <-rbind(popgrpdata_result,popgrpdata_result_age)
  
  rm(popgrpdata_result_age)
  
  write.csv(popgrpdata_result, paste0(data_folder, "Test Shiny Data/young_peoples_mental_wellbeing_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(popgrpdata_result, paste0(data_folder, "Test Shiny Data/young_peoples_mental_wellbeing_shiny_popgrp.rds"))


  
# Indicator 99118: Child material deprivation ----
prepare_final_files(ind = "child_material_deprivation")




# Indicator  99121: Health risk behaviours ----
prepare_final_files(ind = "health_risk_behaviours")


# Indicator 99123: Gender balance in organisations (for minority ethnic population)
prepare_final_files(ind = "gender_balance_in_organisations")





#END

