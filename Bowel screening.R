# ScotPHO indicators: bowel screening uptake

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
source("functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
# Reading data provided by cancer team
# Not all boards started in 2008, some started late in 2009. However as we present 
# percentages and the total numbers are correct we present the whole period for all HBs.

bowel_data <- readRDS(file.path(profiles_data_folder, "Received Data/Bowel Screening Uptake/scotPHO_bowel2023.rds")) %>% 
  group_by(year, datazone2011) %>% 
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) %>% 
  ungroup()

saveRDS(bowel_data, file.path(profiles_data_folder, 'Prepared Data/bowel_screening_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.

# main analysis function 
main_analysis(filename = "bowel_screening", ind_id = 21102, geography = "datazone11", measure = "percent", 
              yearstart = 2008, yearend = 2023, time_agg = 3, year_type = "calendar")



# Deprivation analysis function
deprivation_analysis(filename="bowel_screening", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2023,  year_type = "calendar", 
                    ind_id = 21102)

##END