###   Update ScotPHO Care and Wellbeing indicator: 
#   99110: Gender Pay Gap



# Data source is ONS Annual Survey of Hours and Earnings:
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/annualsurveyofhoursandearningsashegenderpaygaptables



### functions/packages -----
source("1.indicator_analysis.R") 
library(readxl)
library(dplyr)
library(janitor)



### 1. Read in data ----

# Identify files
files <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), pattern="Home Geography Table 8.12"))


# Function to read in files
data <- lapply(files, function(i){
  
  x = read_excel(i, sheet = "Full-Time", skip = 4, na = "x")
  x$file = i
  x

})


# Save as data frame
data <- do.call("rbind.data.frame", data) 



### 2. Prepare data  -----

data <- data %>%
  
  # Clean column names
  clean_names() %>%
  
  # Create year column from file name and rename Scotland code
  mutate(year = gsub(".*(\\d{4}).+", "\\1", file),
         code = ifelse(code == "S92000003", "S00000001", code),
         trend_axis = year,
         def_period = paste0(year, " survey year"),
         ind_id = 99110,
         lowci = NA, upci = NA, 
         numerator = NA) %>%
  
  # Rename columns
  rename(percent = median) %>%
  
  # Select relevant columns
  select(ind_id, code, year, percent, lowci, upci,
         numerator, def_period, trend_axis) %>%
  
  # Filter for Scotland data
  filter(str_detect(code, "S"))



### 3. Prepare final files -----
  
# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/", "gender_pay_gap_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/", "gender_pay_gap_shiny.rds"))
  

