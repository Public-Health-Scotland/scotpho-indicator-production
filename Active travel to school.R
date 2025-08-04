### 1. Notes -------

# This script produces indicator data for: 13040 - Active travel to school
# Data is supplied annually  Hands Up Scotland (HandsUpScotland@sustrans.org.uk) after the release of their annual publication
# They usually sent across without having to request as it's a regular request

# Note Aug 2025: We're provided data for primary and secondary schools separately.
# Could look to develop popgroups file at next update?

### 2. Packages/dependencies ------
source("./functions/main_analysis.R")
source("./functions/data cleaning functions/ca_names_to_codes.R")
library(readxl)

### 3. Clean data ------

# filepath 
path <- paste0(profiles_data_folder,"/Received Data/Active Travel to School/hands_up_24.xlsx")

# get name of sheets
sheet <- readxl::excel_sheets(path)
sheet <- sheet[-c(1, 19)] #dropping contents page and footnotes. Add 1 to second number each year.

# read in data from each sheet and apply sheet names as a df column 
# this is because each years data is on a separate tab
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


#Fix CA names and convert to S-codes
data <- ca_names_to_codes(data, `Local Authority`)

# select final columns 
data <- data |> 
  select(code, year, numerator, denominator)

# drop N/A rows
data <- data |>
  filter(if_any(c(code, numerator, denominator), complete.cases))


# save file to be used in analysis functions 
saveRDS(data, paste0(data_folder, "Prepared Data/active_travel_to_school_raw.rds"))

### 4. Run analysis functions ------
main_analysis(filename = "active_travel_to_school", geography = "council",
              measure = "percent", yearstart = 2008, yearend = 2024, time_agg = 1,
              ind_id = 13040, year_type = "school")


### END
