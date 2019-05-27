# ScotPHO indicator: population prescribed drugs for anxiety/depression/psychosis 

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
#Data comes from prescribing team
presc_anx <- read_csv(file=paste0(data_folder, 'Received Data/prescribing_anxiety_depression Datazone 2010 for 2017.csv')) %>% 
  group_by(datazone2011, year) %>% summarise(numerator = sum(patients)) %>% 
  ungroup() %>% rename(datazone = datazone2011)

saveRDS(presc_anx, file=paste0(data_folder, 'Prepared Data/prescriptions_anxiety_raw.rds'))
saveRDS(presc_anx, file=paste0(data_folder, 'Prepared Data/prescriptions_anxiety_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "prescriptions_anxiety", geography = "datazone11", pop= "DZ11_pop_allages",
              measure = "percent", yearstart = 2010, yearend = 2017, time_agg = 1)

analyze_second(filename = "prescriptions_anxiety", measure = "percent", time_agg = 1, 
               ind_id = 20401, year_type = "financial", profile = "HN", min_opt = 1497994)

#Deprivation analysis function
analyze_deprivation(filename="prescriptions_anxiety_depr", measure="percent", time_agg=1, 
                    yearstart= 2014, yearend=2017,   year_type = "calendar", 
                    pop= "depr_pop_allages", ind_id = 20401)

##END
