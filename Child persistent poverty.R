# Child persistent poverty


###   Update CYP mental health indicator: 
#   30155: Children living in persistent poverty


# Data source is the National Performance Framework open data on statistics.gov.scot
# 2024 update: https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx
# No sample sizes provided, so CIs can't be calculated.

### functions/packages ----
source("1.indicator_analysis.R") 


### 1 - Read in data -----

# Specify url of the NPF file to download from stats.gov
url <- "https://statistics.gov.scot/downloads/file?id=ca23e4da-4aa2-49e7-96e2-38f227f9d0de%2FALL+NPF+INDICATORS+-+2024+-+statistics.gov.scot+NPF+database+excel+file+-+August+2024.xlsx"

# Specify file name and where to save file to
file_name <- "NPF_database_2024.xlsx"
file_path <- paste0(data_folder, "Received Data/NPF/")

# Download file
download.file(url = url, destfile = paste(file_path, file_name, sep = ""))

# Read in data file
dat <- read_xlsx(paste0(file_path, file_name))


### 2. Prepare data  -----

data <- dat %>%
  
  # Clean column names
  clean_names() %>% 
  
  # Select relevant indicators 
  filter(indicator == "Persistent poverty" & disaggregation=="Children") %>%

  select(trend_axis = year, 
         rate = figure) %>%

  # Add indicator ids
  mutate(ind_id = 30155) %>%
  
  # Create date variables 
  mutate(year = as.numeric(substr(trend_axis, 1, 4)) + 2) %>% # add 2 to the start year of 5-year ranges as an approximate midpoint for plotting
  mutate(def_period = paste0("5-year aggregate (",trend_axis,")")) %>%
  
  # Add geography codes
  mutate(code = "S00000001") %>%
  
  # Create other variables required
  mutate(numerator = NA, 
         lowci = NA, upci = NA)

### 3. Prepare final file -----

  # 1 - Main data 
  # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
  
  # Save files
  write_rds(data, paste0(data_folder, "Test Shiny Data/cyp_persistent_poverty_shiny.rds"))
  write.csv(data, paste0(data_folder, "Test Shiny Data/cyp_persistent_poverty_shiny.csv"), row.names = FALSE)
  
  
# Run QA reports 
run_qa(filename = "cyp_persistent_poverty")

#END

