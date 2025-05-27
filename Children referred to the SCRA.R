# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following 2 indicators:
# 13001 - Children referred to the Children's Reporter for care and protection
# 20803 - Children referred to the Children's Reporter for offences

# Data requested each year following the release of SCRA publication (usually in July):
# https://www.scra.gov.uk/resources_articles_category/official-statistics/

# update instructions:-
# 1. Data to be saved in folder (see below)
# 2. Update file name below if required
# 3. Change yearend parameters in the analysis functions


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions/packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(openxlsx)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare data for analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get filepath to data file
folder <- file.path(profiles_data_folder, "Received Data", "Children referred to childrens reporter")
file <- "ScotPHO NHS Non-offence and offence children.xlsx"
path <- file.path(folder, file)

# read in data
data <- rbind(
  read.xlsx(path, sheet = "2. Referral Type", startRow = 8, rows = c(8:40), colNames = TRUE), # children referred for non-offences 
  read.xlsx(path, sheet = "2. Referral Type", startRow = 8, rows = c(45:77), colNames = TRUE) # children referred for offences 
)

# rename cols
data <- data |>
  rename("areaname" = "Children/YP",
         "indicator" = "X2")

# clean numeric cols 
data <- data |>
  mutate(across(-c(areaname, indicator), ~ if_else(. == "<5", NA, .))) |> # replace suppressed vals with NA
  mutate(across(-c(areaname, indicator), ~ as.numeric(.))) # convert cols to class numeric 


# pivot data longer to create year column
data <- pivot_longer(
  data = data,
  cols = !c(areaname, indicator), 
  names_to = "year", 
  values_to = "numerator", 
  # changing year format to 4-digit year (using start of financial year)
  names_transform = list(year = ~ paste0("20", substr(.,1, 2)))
)



# get council area lookup 
geo_lookup <- readRDS(file.path(profiles_data_folder,"Lookups/Geography/CAdictionary.rds"))


# clean some council area names to allow join with lookup 
data <- data |>
  mutate(areaname = case_when(str_detect(areaname,"&") ~ str_replace(areaname,"&","and"),
                              areaname == "Dundee" ~ "Dundee City",
                              areaname == "Glasgow" ~ "Glasgow City",
                              areaname == "Orkney" ~ "Orkney Islands",
                              areaname == "Shetland" ~ "Shetland Islands",
                              str_detect(areaname, "Edinburgh") ~ "City of Edinburgh",
                              str_detect(areaname,"Siar") ~ "Na h-Eileanan Siar",
                              TRUE ~ areaname))

# join with lookup 
data <- left_join(data, geo_lookup, by = "areaname") |>
  select(-areaname)


# split data by indicator 
indicator <- split(data, data$indicator)


# save temp files to be used in analysis function in next step
saveRDS(indicator$`Non-offence`, file.path(profiles_data_folder, "Prepared Data", "13001_scra_care_protection_raw.rds"))
saveRDS(indicator$Offence, file.path(profiles_data_folder, "Prepared Data", "20803_scra_offence_raw.rds"))  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run analysis functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Children referred to the Children's Reporter for care and protection
main_analysis(filename = "13001_scra_care_protection", measure = "crude", ind_id = 13001,
              geography = "council", year_type = "financial", time_agg = 1,
              pop = "CA_pop_under16", crude_rate = 1000, yearstart = 2004, yearend = 2023)


# Children referred to the Children's Reporter for offences
main_analysis(filename = "20803_scra_offence", measure = "crude", ind_id = 20803,
              geography = "council", year_type = "financial", time_agg = 1,
              pop = "CA_pop_8to15", crude_rate = 1000, yearstart = 2004, yearend = 2023)

## END