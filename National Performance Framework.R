#new indicators introduced for care and wellbeing portfolio
# TO DO: enable automated checking at end of the indicator production
#         create a deprivation data file output (for any indicators that may have sutiable data)

###   Update ScotPHO Care and Wellbeing indicators: 
#   99116: Persistent poverty
#   99117: Young people's mental wellbeing
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
                          "Child Wellbeing and Happiness",
                          "Child material deprivation",
                          "Children's material deprivation",
                          "Health risk behaviours",
                          "Gender balance in organisations")) %>%
  
         # Convert indicator names to lower case and hyphenate 
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_"),
         
         indicator = str_replace_all(indicator, "children's", "child"),
         
         # Ensure age breakdowns are named consistently
         breakdown = str_replace_all(breakdown, "Age ", ""),
         breakdown = str_replace_all(breakdown, "-", " to "),
         
         # Ensure SIMD breakdowns are named consistently
         breakdown = str_replace_all(breakdown, "SIMD ", ""),
         disaggregation = str_replace_all(disaggregation, "SIMD", "Scottish Index of Multiple Deprivation"),
         breakdown = if_else(str_detect(breakdown, "$1$|1st|(?i)most"), "1 - most deprived", breakdown),
         breakdown = if_else(str_detect(breakdown, "$2$|2nd"), "2", breakdown),
         breakdown = if_else(str_detect(breakdown, "$3$|3rd"), "3", breakdown),
         breakdown = if_else(str_detect(breakdown, "$4$|4th"), "4", breakdown),
         breakdown = if_else(str_detect(breakdown, "$5$|5th|(?i)least"), "5 - least deprived", breakdown),
         
         disaggregation = str_replace_all(disaggregation, "Total Difficulties Score X ", ""),
         disaggregation = str_replace_all(disaggregation, "Total Difficulties Score", "Total"),
         breakdown = str_replace_all(breakdown, "Total", "All"),
         
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
  filter(split_name %in% c("Total", "Age", "Gender", "Sex",
                           "Scottish Index of Multiple Deprivation",
                           "Disability", "Disability of household member(s)",
                           "Religion", "Urban Rural classification",
                           "Ethnicity", "Limiting Longstanding Illness", 
                           "Equivalised Income")) %>%
  
  # Select relevant variables
  select(c(ind_id, indicator, code, split_name, split_value, year, trend_axis, def_period, rate, numerator, lowci, upci)) %>%
  
  # Reorder data frame
  arrange(indicator, code, year)
  


### 3. Prepare final files -----

# Function to prepare final files:
# Creates two data files for each indicator (main data vs population group data)
prepare_final_files <- function(ind){
  
    # Filter for main data 
    # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
    maindata <- data %>%
      filter(ind_id == ind,
             split_value == "All") %>%
      select(!c(split_name, split_value)) %>% #select fields required for maindata file (ie summary/trend/rank tab)
      unique()
    
    # Filter for population group data
    # (ie data behind population groups tab)
    pop_grp_data <- data %>%
      filter(ind_id == ind,
             split_name != "Total")
    
    # Save files in folder to be checked
    write.csv(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
    write_rds(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
    
    write.csv(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
    write_rds(pop_grp_data, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
    
    
    # Make data created available outside of function so it can be visually inspected if required
    maindata_result <<- maindata
    popgrpdata_result <<- pop_grp_data
    
}


# Create final files and run QA reports - QA report won't work until changes made to checking reports - come back to this

# Indicator 99116: Persistent povertyy
prepare_final_files(ind_id = 99116)

#run_qa(filename = "persistent_poverty") #come back to fix qa report - failing because no NHS board or ca geographies ins some of these indcators


# Indicator 99117: Young people's mental wellbeing
prepare_final_files(ind_id = 99117)


# Indicator 99118: Child material deprivation
prepare_final_files(ind_id = 99118)


# Indicator  99121: Health risk behaviours
prepare_final_files(ind_id = 99121)


# Indicator 99123: Gender balance in organisations (for minority ethnic population)
prepare_final_files(ind_id = 99123)







#END

