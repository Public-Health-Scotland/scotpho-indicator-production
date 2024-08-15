### 1. Notes -------

# This script produces indicator data for: 13040 - Active travel to school
# Data is supplied annually  Hands Up Scotland (HandsUpScotland@sustrans.org.uk) after the release of their annual publication
# They usually sent across without having to request as it's a regular request


### 2. Packages/dependencies ------
source("./1.indicator_analysis.R")
library(purrr)
library(rio) ##for reading in Excel data across multiple sheets


### 3. Clean data ------

# filepath 
path <- paste0(data_folder,"Received Data/Active Travel to School/Hands_up_Scotland.xlsx")

# get name of sheets
sheet <- excel_sheets(path)
sheet <- sheet[-c(1, 18)] #dropping contents page and footnotes

# read in data from each sheet and apply sheet names as a df column 
# this is because each years data is on a seperate tab
# & sheet names contain info on year of data as there is no year column on each tab
data <- lapply(setNames(sheet, sheet), 
               function(x) read_excel(path, sheet=x, skip = 10)[,c(1, # local authority 
                                                                   10:12, # active travel primary school
                                                                   18:20, # active travel secondary school
                                                                   45, # total survey respondents primary school
                                                                   47)]) |>  # total survey respondents secondary school
  clean_names()


# convert columns to class numeric, except local authority column
data <- map(data, ~ .x |>
              mutate(across(-`Local Authority`, as.numeric)))


# combine list of data frames to one dataframe
data <- bind_rows(data, .id="Sheet")


# calculate numerator and denominator
data <- data |>
  mutate(numerator = rowSums(select(data, contains(c("Walk", "Cycle", "Scooter")))),
         denominator = rowSums(select(data, contains(c("Responses")))),
         year = str_sub(Sheet, start = 2))


# bring in LA dictionary and include LA codes
la_lookup <- readRDS(paste0(lookups, "Geography/CAdictionary.rds"))


data <- data |>
  mutate(`Local Authority` = str_replace(`Local Authority`, "&","and"),
         `Local Authority` = str_replace(`Local Authority`, "Eilean Siar","Na h-Eileanan Siar"),
         `Local Authority` = str_replace(`Local Authority`, "Edinburgh City","City of Edinburgh")
  ) |>
  left_join(la_lookup, by = c("Local Authority" = "areaname")) |>
  rename(ca = code)


# select final columns 
data <- data |> 
  select(ca, year, numerator, denominator)


# drop N/A rows
data <- data |>
  filter(if_any(c(ca, numerator, denominator), complete.cases))


# save file to be used in analysis functions 
saveRDS(data, paste0(data_folder, "Prepared Data/active_travel_to_school_raw.rds"))




### 4. Run analysis functions ------
analyze_first(filename = "active_travel_to_school", geography = "council", 
              measure = "percent", yearstart = 2008, yearend = 2023, time_agg = 1)


analyze_second(filename = "active_travel_to_school", measure = "percent", time_agg = 1,
               ind_id = 13040, year_type = "school")


### END
