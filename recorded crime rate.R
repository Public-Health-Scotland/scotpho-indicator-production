# Analyst notes ----------------------------------------------------------------

# This script produces data for the following indicator:
#  99104 - Recorded Crime 

# This data is requested jointly by ScotPHO/Improvement Services to Police Scotland via an FOI
# FOI published here: https://www.scotland.police.uk/access-to-information/freedom-of-information/disclosure-log/disclosure-log-2023/june/23-1198-crime-stats-by-data-zone-earliest-to-apr-23/ 
# Note that each year of data is published in a seperate file, so all files need to be saved in a folder on the network


# Note that this is the first time we have requested this data
# This indicator differs from the 'Crime' indicator, which uses crime classifications from SIMD

# Future revisions: Police Scotland provided datazone names instead of codes
# Some datazones share the same name, meaning for this update (Oct 2023) some datazones have been excluded
# where we cannot accuartely identify which datazone code they should map to


# Approx 15% of DZ are NULL, and approx 5% can't accurate be mapped, and are therefore excluded from the Scotland total (~ 20%)


# 1 - load dependencies
# 2 - Prepare data
# 3 - Run analysis functions


# 1. load dependencies ---------------------------------------------------------
source("1.indicator_analysis.R")
library(purrr)
library(readxl)
library(janitor)





# 2. Prepare data --------------------------------------------------------------

# ensure latest data files are saved in folder 
crime_data_folder <- paste0(data_folder, "Received Data/Crime data/")


# combine the data files to create 1 dataframe
data <- purrr::map_df(
  list.files(paste0(crime_data_folder, "data/"), full.names = TRUE),
  readxl::read_excel,
  sheet = "Table 1"
)


# select and rename required columns
data <- data %>%
  janitor::clean_names() %>%
  select(datazone,
         year = year_2,
         numerator = number_of_crimes)



# aggregate to all crimes by year and datazone
data <- data %>%
  group_by(datazone, year) %>%
  summarise_all(sum) %>%
  ungroup()


# Amending DZ names to account for typo corrections in 'Standard Geography Code register'
# this step is temporary: it's required because Police Scotland have used an older datazone2011 lookup where the typos still exist
# issue won't persist in the next update when codes are provided instead of DZ names
data <- data %>%
  mutate(
    datazone = str_replace(datazone,"Lochlash", "Lochalsh"),
    datazone = str_replace(datazone, "Pettiesmuir", "Pattiesmuir"),
    datazone = str_replace(datazone, "Siverknowes and Davidson's Mains", "Silverknowes and Davidson's Mains"),
    datazone = str_replace(datazone, "West Neilston and Uplawmoor", "Neilston and Uplawmoor"),
    datazone = str_replace(datazone, "West Arthurlie and North Neilston", "Arthurlie and Gateside"),
  )



# read in DZ 2011 lookup 
dz_lookup <- read_excel(paste0(crime_data_folder, "DZ Lookup from IS.xlsx")) %>%
  select(DZ2011_Code, DZ2011_Name)


# identify which DZcodes share the same DZ name
duplicate_dz_names <- dz_lookup %>%
  group_by(DZ2011_Name) %>% 
  filter(n() > 1) %>% 
  ungroup()


# remove null DZs and DZs that can't accurately be mapped to a DZ code
data <- data %>%
  filter(!datazone %in% duplicate_dz_names$DZ2011_Name & datazone != "NULL")


# add DZ codes to the dataframe 
data <- data %>%
  left_join(dz_lookup, by = c("datazone" = "DZ2011_Name"))

# select final columns 
data <- data %>%
  select(datazone = DZ2011_Code,
         year,
         numerator)


# save file to be used in analysis functions
saveRDS(data, paste0(data_folder,"Prepared Data/crime_raw.rds"))




# 3. Run analysis functions ----------------------------------------------------
analyze_first(filename = "crime", geography = "datazone11", measure = "crude",
              pop = "DZ11_pop_allages", yearstart = 2007, yearend = 2021,
              time_agg = 1)



analyze_second(filename = "crime", measure = "crude", time_agg = 1, 
               crude_rate = 1000, ind_id = 99104, year_type = "calendar")



# END


