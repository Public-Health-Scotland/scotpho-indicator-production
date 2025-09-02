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

source("functions/main_analysis.R") # source functions & libraries to run script

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in received data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in Scotland worksheet and clean data
active_work_scot <- read.xlsx(paste0(profiles_data_folder,"/Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Scotland") |>
  clean_names() |> # converts column names to lowercase
  mutate(area_type = "Scotland") # add in column for area type


# Read in local authority (council area) worksheet and clean data
active_work_la <- read.xlsx(paste0(profiles_data_folder,"/Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Local Authority") |> 
  clean_names() |> # converts column names to lowercase
  mutate(area_type = "Council area", # add in column for geography type
         geography = str_replace(geography,"&","and"),# replace '&' with 'and' in LA names
         geography = case_when(geography == "Edinburgh, City of" ~ "City of Edinburgh",
                               geography == "Eilean Siar" ~ "Na h-Eileanan Siar",
                               TRUE ~ geography)) # update LA names to match OPT style

# Read in NHS board worksheet and clean data
active_work_hb <- read.xlsx(paste0(profiles_data_folder,"/Received Data/Active travel to work (20206)/Percentage walking or cycling to work to 2023.xlsx"), sheet = "Health Board") |>
  clean_names() |> # converts column names to lowercase
  mutate(area_type = "Health board", # add in column for area type
         #relocate(area_type, .before = geography) |> # change position of area_type column
         geography = str_replace(geography,"&","and"),# replace '&' with 'and' in HB names
         geography = case_when(geography == "Forth" ~ "Forth Valley", TRUE ~ geography), # correct name of Forth Valley HB
         geography = paste('NHS', geography, sep=' ')) # prefix HB name with NHS


# Combine Scotland, local authority and health board data into a single table / data frame
active_travel_work <- bind_rows(active_work_scot, active_work_la, active_work_hb) |> # Bind together into a single data frame the Scotland, LA and HB data frames
  rename(rate = percentage,# rename columns to names used by scotpho convention
         lowci = lower_95_percent_ci,
         upci = upper_95_percent_ci ) |> 
  select(year, geography, numerator, rate, lowci, upci, area_type) # select only required columns

# Read in area code lookup file and filter to only include relevant geographies
area_codes <- readRDS(paste0(profiles_data_folder,"/Lookups/Geography/codedictionary.rds")) |>
  filter(str_detect(code, "S00|S08|S12")) # filter to only include Scotland (S00)/health board (S08)/local authority (S12) codes

# Add area code to active travel data using area code lookup
active_travel_work <- active_travel_work |>
  left_join(area_codes, by = c("geography" = "areaname"))|>
  rename(trend_axis=year) |> # rename year as trend_axis column (displayed in charts within profiles tool app)
  mutate(year = substr(trend_axis,1,4), # year column containing nothing but first year of time period - used in profiles tool to sort data in correct order
         def_period = paste0(trend_axis," survey year"),# create definition_period column with description of time period
         ind_id = 20206) |> #add indicator_id code
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci) 

# ~~~~~~~~~~~~~~~~~~~~~~
# save final files ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Save files in folder to be checked
write.csv(active_travel_work, file.path(profiles_data_folder, "/Data to be checked/active_travel_to_work_shiny.csv"), row.names = FALSE)
write_rds(active_travel_work, file.path(profiles_data_folder, "/Data to be checked/active_travel_to_work_shiny.rds"))


# Call to QA report
run_qa(filename = "active_travel_to_work", type="main",test_file = FALSE) 


