###   Update ScotPHO Care and Wellbeing indicator: 
#   99111: Employees on the living wage


# Data source is ONS Annual Survey of Hours and Earnings published on the Scottish Government website:
# https://www.gov.scot/publications/annual-survey-of-hours-and-earnings-2022/


### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

# Identify file
source_file <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), 
                                                         pattern="Annual-Survey-of-Hours-and-Earnings-2022-Tables"))

# Set ranges for all, men and women tables (Table 5.2)
# as additional years of data are added data ranges will need to be adjusted
ranges <- c("A5:F17", "A20:F31", "A34:F45")


# Function to read in tables
data <- lapply(ranges, function(i){
  
  x = read_excel(source_file, sheet = "Table 5.2", range = i)
  colnames(x) = c("year", "less_number", "more_number", "blank", "rate", "more_prop")
  x$sex = i
  x
  
})


# Save as one data frame
data <- do.call("rbind.data.frame", data) 



### 2. Prepare data  -----

data <- data %>%
  
  # Select relevant columns
  select(sex, year, rate) %>%
  
  # Remove all
  filter(year != "All") %>%
  
  # Transform range into sex and create columns
  mutate(sex = str_replace(sex, "A5:F17", "all"),
         sex = str_replace(sex, "A20:F31", "men"),
         sex = str_replace(sex, "A34:F45", "women"),
         ind_id = 99111,
         code = "S00000001",
         def_period = paste0(year, " survey year"),
         trend_axis = year,
         lowci = NA, upci = NA,
         numerator = NA) %>%
  
  # Filter for 2021 and 2022 (earlier data not directly comparable)
  filter(year %in% c("2021", "2022")) %>%
  arrange(code, year, sex)
  


### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/", "employees_on_the_living_wage_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/", "employees_on_the_living_wage_shiny.rds"))
