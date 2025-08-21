# ~~~~~~~~~~~~~~~~~~~~~~~
# ---- Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Indicator: Active travel to work (id = 20206)

# Description: Percentage of adults travelling to work by either walking or cycling.
# Adults employed, self-employed or in full-time education and not working from home, 
# who responded with either walking or cycling to SHS question RD3: 
# "How do you usually travel to work (or school/college/university if in full-time education)?".

# Data source: Scottish Household Survey (contact - Karren.Friel@transport.gov.scot)


# ~~~~~~~~~~~~~~
# Libraries ----
# ~~~~~~~~~~~~~~

library(openxlsx)

source("1.indicator_analysis.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in received data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in Scotland worksheet and clean data

active_work_scot <- read.xlsx(paste0(data_folder,"Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Scotland") |>
  clean_names() |> # converts column names to lowercase
  select(year, geography, percentage) |> # select only required columns
  rename(rate = percentage) |> # rename percentage column to rate
  mutate(area_type = "Scotland") |> # add in column for area type
  relocate(area_type, .before = geography) # change position of area_type column
print(active_work_scot)

# Read in local authority (council area) worksheet and clean data

active_work_la <- read.xlsx(paste0(data_folder,"Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Local Authority") |> 
  clean_names() |> # converts column names to lowercase
  select(year, geography, percentage) |> # select only required columns
  rename(rate = percentage) |> # rename percentage column to rate
  mutate(area_type = "Council area") |> # add in column for geography type
  relocate(area_type, .before = geography) |> # change position of area_type column
  mutate(geography = case_when(geography == "Edinburgh, City of" ~ "City of Edinburgh",
                               geography == "Eilean Siar" ~ "Na h-Eileanan Siar",
                               TRUE ~ geography)) |> # update LA names to match OPT style
  mutate(geography = str_replace(geography,"&","and")) # replace '&' with 'and' in LA names
print(active_work_la)

# Read in health board worksheet and clean data

active_work_hb <- read.xlsx(paste0(data_folder,"Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Health Board") |>
  clean_names() |> # converts column names to lowercase
  select(year, geography, percentage) |> # select only required columns
  rename(rate = percentage) |> # rename percentage column to rate
  mutate(area_type = "Health board") |> # add in column for area type
  relocate(area_type, .before = geography) |> # change position of area_type column
  mutate(geography = case_when(geography == "Forth" ~ "Forth Valley", TRUE ~ geography)) |> # correct name of Forth Valley HB
  mutate(geography = paste('NHS', geography, sep=' ')) |> # prefix HB name with NHS
  mutate(geography = str_replace(geography,"&","and")) # replace '&' with 'and' in HB names
print(active_work_hb)

# Combine Scotland, local authority and health board data into a single table / data frame

active_work_all <- bind_rows(active_work_scot, active_work_la, active_work_hb) # Bind together into a single data frame the Scotland, LA and HB data frames
print(active_work_all)

# Read in area code lookup file and filter to only include relevant geographies

area_codes <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |>
  filter(str_detect(code, "S00|S08|S12")) # filter to only include Scotland (S00)/health board (S08)/local authority (S12) codes
print(area_codes)

# Add area code to active travel data using area code lookup

active_work_all <- active_work_all |>
  left_join(area_codes, by = c("geography" = "areaname")) 
print(active_work_all)