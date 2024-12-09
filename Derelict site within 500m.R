########################.
# Analyst notes ----
########################.

# This script updates the 'Population within 500 metres of a derelict site' indicator

# A data extract is provided each year by the SG. This should be saved in following folder: - 
# "Received Data/Population within 500 metres of derelict site/latest_extract/"

# 2 bits of this script need updated each year:
# a.  the variable called 'new_data_filename' and b. the end_year in the analyse_second() function 

########################.
# Set up ----
########################.

# source functions / load additional packages
source("1.indicator_analysis.R") 
library(data.table) # for reading in and combining files

# filepaths 
data_filepath <- paste0(data_folder, "Received Data/Population within 500 metres of derelict site/") # derelict site data filepath
data_clean_filepath <- paste0(data_filepath, "data clean/") # filepath to sub-folder containing data from previous years that has been cleaned
new_data_filepath <- paste0(data_filepath, "latest extract/") # filepath to sub-folder containing latest received data that needs cleaned
new_data_filename <- "SVDLS - 2023 - Ad Hoc - Public Health Scotland" # name of latest received data

###################################################.
# Step 1 - clean latest provided data extract ----
###################################################.

## read in data 
new_data <- read.csv(file = paste0(new_data_filepath, new_data_filename, ".csv"))

## rename columns 
new_data <- new_data |>
  rename("datazone" = "DataZone_Code", 
         "numerator" = "u500pop", 
         "denominator" = "totpop")

## replace negative numerator with 0
# otherwise this will result in rates outwith confidence intervals
new_data <- new_data |>
  mutate(numerator = if_else(numerator < 0, 0, numerator))

## get year of data
new_data_year <- new_data$year[1]

## save clean data
write.csv(new_data, file = paste0(data_clean_filepath, "data ", new_data_year, ".csv"), row.names = FALSE)


###########################################.
# Step 2 - Prepare data for analysis ----
###########################################.

## get name of files in clean data sub-folder 
clean_data_files <- list.files(data_clean_filepath, pattern = ".csv$", recursive = TRUE, full.names = TRUE)

## read in and combine clean data 
DZ11_data <- rbindlist(lapply(clean_data_files, fread), use.names = TRUE)

## save combined data, ready to be used in analysis functions 
saveRDS(DZ11_data, file=paste0(data_folder, 'Prepared Data/derelict_site_raw.rds')) 


###########################################.
# Step 3 - Run analysis functions ----
###########################################.

analyze_first(filename = "derelict_site", geography = "datazone11",
              measure = "percent", yearstart = 2016, yearend = 2023,
              time_agg = 1)

analyze_second(filename = "derelict_site", measure = "percent",
               time_agg = 1, ind_id = "20901", year_type = "calendar")


##########################################.
# Step 4 - Prepare final files ----
##########################################.

# note: data pre 2016 uses 2001 datazones instead of 2011
# For this reason the data cannot be run through the analysis functions 
# or presented at particular geography levels. It is limited to Scotland, CA and HB level
# previously prepared data pre-2016 is therefore lifted from the 'live' shiny file and combined with the post-2016 data
# to create a new final file

## get pre-2016 data from shiny file
data_pre_2016 <- read_csv(paste0(data_folder, "Shiny Data/derelict_site_shiny.csv")) |> 
  subset(year<=2015) |>
  ## Historic files contain reference to outdated GSS codes
  ## We can't rederive the data so we just have to recode data and ignore the fact they will be imperfect presentations of these geographie
  ## details of archived GSS codes 
  ## https://www.gov.scot/collections/small-area-statistics/
  mutate(code = case_when(code=="S08000018" ~"S08000029", #2019 fife code
                          code=="S08000021" ~"S08000031", # GGC
                          code=="S08000023" ~"S08000032", # Lanarkshire
                          code=="S08000027" ~"S08000030", # Tayside
                          # recode council areas affected by boundary changes
                          code=="S12000015" ~"S12000047", # Fife council
                          code=="S12000024" ~"S12000048", # Perth and Kinross
                          code=="S12000044" ~"S12000050", # North Lanarkshire
                          code=="S12000046" ~"S12000049", # Glasgow City
                          TRUE ~ code))

## get latest prepared file
data_2016_onwards <- readRDS(paste0(data_folder, "Data to be checked/derelict_site_shiny.rds"))

## combine to create full time period
new_complete_file <- rbind(data_pre_2016 , data_2016_onwards) |>
  arrange(code,year)

## save final files
saveRDS(new_complete_file, file = paste0(data_folder, "Data to be checked/derelict_site_shiny.rds"))
write_csv(new_complete_file, file = paste0(data_folder, "Data to be checked/derelict_site_shiny.csv"))

## QA final files
run_qa("derelict_site")


## END