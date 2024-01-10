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
  colnames(x) = c("year", "less_number", "more_number", "blank", "less_prop", "rate")
  x$sex = i
  x
  
})


# Save as one data frame
data <- do.call("rbind.data.frame", data) 


# Read in table of LA breakdowns (Table 5.7)
data_ca <- read_excel(source_file, sheet = "Table 5.7", range = "X44:Y76", na = "..")


# Read in LA lookup
CAdictionary <- readRDS(paste0(lookups, "Geography/CAdictionary.rds")) %>%
  arrange(areaname) %>%
  select(code)



### 2. Prepare data  -----

# LA level data
data_ca <- data_ca %>%
  
  # Bind area codes
  bind_cols(CAdictionary) %>%
  
  # Transform to long format
  pivot_longer(where(is.numeric), names_to = "year", values_to = "rate") %>%
  
  # Add sex column
  mutate(sex = "all")


# Combine LA and Scotland level data
data <- data %>%
  
  # Filter for 2021 and 2022 (earlier data not directly comparable)
  filter(year %in% c("2021", "2022")) %>%
  
  # Select relevant columns
  select(sex, year, rate) %>%
  
  # Transform range into sex and create code column
  mutate(sex = str_replace(sex, "A5:F17", "all"),
         sex = str_replace(sex, "A20:F31", "men"),
         sex = str_replace(sex, "A34:F45", "women"),
         code = "S00000001") %>%
  
  # Bind LA rows
  rbind(data_ca) %>%
  
  # Create new columns
  mutate(ind_id = 99111,
         def_period = paste0(year, " survey year"),
         trend_axis = year,
         lowci = NA, upci = NA,
         numerator = NA) %>%
  
  arrange(code, year, sex)



### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/", "employees_on_the_living_wage_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/", "employees_on_the_living_wage_shiny.rds"))


# Run QA report and check the output file
run_qa(filename = "employees_on_the_living_wage")


# END
