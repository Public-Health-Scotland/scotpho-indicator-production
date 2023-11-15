###   Update ScotPHO Care and Wellbeing indicators: 
#   Places to interact
#   Loneliness 
#   Children's voices
#   Children have positive relationships
#   Persistent poverty
#   Child wellbeing and happiness
#   Child material deprivation
#   Access to green and blue space
#   Journeys by active travel
#   Health risk behaviours
#   Quality of care experience
#   Gender balance in organisations


# Data source is the National Performance Framework open data on statistics.gov.scot
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fnational-performance-framework



### functions/packages ----
source("1.indicator_analysis.R") 




### 1 - Read in NPF data -----

# Read in data 
data <- read_xlsx(paste0(data_folder, "Received Data/NPF Database - 01.11.23.xlsx")) %>%
  clean_names()



### 2. Prepare data  -----

data <- data %>%
  
  # Select relevant indicators
  filter(indicator %in% c("Places to interact",
                          "Loneliness",
                          "Children's voices",
                          "Children have positive relationships",
                          "Persistent poverty",
                          "Child wellbeing and happiness",
                          "Child material deprivation",
                          "Access to green and blue space",
                          "Journeys by active travel",
                          "Health risk behaviours",
                          "Quality of care experience",
                          "Gender balance in organisations")) %>%
  
  # Rename indicators
  mutate(indicator = tolower(indicator),
         indicator = str_replace_all(indicator, " ", "_")) %>%
  
  # Select relevant variables
  select(!c(outcome, measure, source))
  
