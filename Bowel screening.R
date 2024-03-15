# ScotPHO indicators: bowel screening uptake

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
# Reading data provided by cancer team
# Not all boards started in 2008, some started late in 2009. However as we present 
# percentages and the total numbers are correct we present the whole period for all HBs.

bowel_data <- read_rds(paste0(data_folder, "Received Data/Bowel Screening Uptake/scotPHO_bowel2022.rds")) %>% 
  rename(datazone = datazone2011) %>% 
  group_by(year, datazone) %>% 
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) %>% ungroup()

saveRDS(bowel_data, file=paste0(data_folder, 'Prepared Data/bowel_screening_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "bowel_screening", geography = "datazone11", measure = "percent", 
              yearstart = 2008, yearend = 2022, time_agg = 3)

analyze_second(filename = "bowel_screening", measure = "percent", time_agg = 3, 
               ind_id = 21102, year_type = "calendar")

#Deprivation analysis function

analyze_deprivation(filename="bowel_screening", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2022,   year_type = "calendar", 
                    ind_id = 21102)

##END