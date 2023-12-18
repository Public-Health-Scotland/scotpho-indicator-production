# Analyst notes ---------------------------------------------------------------

# This script updates the following indicator:-
# 13032 - Uptake of the HPV vaccine in S3 girls

# Data is provided by the immunisations team and should be saved in the HPV vaccine uptake subfolder
# This data can be provided following the release of the 'HPV Immunisation Statistics Scotland' publication which is usually published at the end of November
# Note they provide one years worth of data each year, therefore we append the latest received data extract onto the historic data
# Then save a 'new' version of the historic data to be used the next year 

# important: change the year for the variable 'max_year' when updating each year

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions
#   Part 3 - create new historic data extract


# dependencies -----------------------------------------------------------------
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


# 1. Prepare base file ----------------------------------------------------------


max_year <- 2022 # change each year



# read in historic data
hpv_data_historic <- readRDS(paste0(data_folder, "Received Data/HPV vaccine uptake/historic_data_DO_NOT_DELETE.rds"))
# save backup copy
saveRDS(hpv_data_historic, paste0(data_folder, "Received Data/HPV vaccine uptake/historic_data_backup.rds"))

# read in latest years data 
hpv_data_latest <- readRDS(paste0(data_folder, "Received Data/HPV vaccine uptake/2023 - HPV uptake.rds")) %>%
  rename(datazone = geography) %>% 
  filter(substr(datazone,1,3) == "S01") %>% 
  mutate(numerator = numerator_female, 
         denominator = denominator_female) %>%
  mutate(year = max_year) %>% # change each year
  select(datazone, year, numerator, denominator) 


# combine new and historic data
hpv_data <- rbind(hpv_data_historic, hpv_data_latest)


# save a version to be used in analysis functions 
saveRDS(hpv_data, file=paste0(data_folder, 'Prepared Data/hpv_uptake_raw.rds'))



# Part 2 - Run analysis functions ----------------------------------------------
analyze_first(filename = "hpv_uptake", geography = "datazone11", measure = "percent", 
              yearstart = 2009, yearend = max_year, time_agg = 3)

analyze_second(filename = "hpv_uptake", measure = "percent", time_agg = 3, 
               ind_id = 13032, year_type = "school")

# Deprivation analysis function
analyze_deprivation(filename="hpv_uptake", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend = max_year,   year_type = "school", 
                    ind_id = 13032)


# If everything looks as expected, then save a new version of the historic data which includes the latest year
# This will overwrite the older version in the folder 
# Then move the latest years file to the archive sub-folder
saveRDS(hpv_data, file=paste0(data_folder, 'Received Data/HPV vaccine uptake/historic_data_DO_NOT_DELETE.rds'))

##END
