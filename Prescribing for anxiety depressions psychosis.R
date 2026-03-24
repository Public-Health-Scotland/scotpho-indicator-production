# ScotPHO indicator: population prescribed drugs for anxiety/depression/psychosis 

# Data source: PHS prescribing team request: phs.prescribing@phs.scot
# Data provided contains numbers of unique individuals in receipt of one or more prescription item for anxiety/depression/psychosis.
# Individuals are counted once per year and assigned to a geography based on their last prescription during a financial year.
# This means people who move within a year should only be counted once at each geography level e.g. once within scotland, once within NHS board, once within council etc
# Prescribing team provide ScotPHO with a data extracts at datazone level but there are also Scotland/NHS board/HSCP/Council extracts.
# The prescribing dataset is extremely large and can only be accessed through BOXI extraction. The Scotland, NHS board, council and HSCP level extractions are distinct BOXI queries that
# count individuals once per geography that they get a prescription in (ie it may double count people who move residence within a year (e.g. if they move outwith their original nhs board))

# Comparisons were carried out to see what difference counting people once per year or counting them once per geography type - this suggests little difference between in the measure value
# but having multiple data extract for different geo types adds complexity to code to generate indicator data. Therefore decision made to count individuals once per year.


#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function
source("./functions/data cleaning functions/exclude_small_geogs.R") #helper function which allows post-hoc removal of small geographies

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
#Data comes from prescribing team (Rx team provide a new FYE of data, rows can be copy pasted onto the bottom of the previous years data)
presc_anx <- read_csv(file.path(profiles_data_folder, '/Received Data/Population prescribed drugs for anxiety depression psychosis/prescrib_anx_depression_ScotPHO Indicator by Datazone 2010 to 2024.csv')) %>% 
  group_by(datazone2011, year) %>% summarise(numerator = sum(patients)) %>% 
  ungroup() %>% rename(datazone = datazone2011)

saveRDS(presc_anx, file.path(profiles_data_folder, 'Prepared Data/prescriptions_anxiety_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "prescriptions_anxiety", measure = "crude", geography = "datazone11",
              year_type = "financial", ind_id = 20401, time_agg = 1, yearstart = 2010, 
              yearend = 2024, crude_rate = 100, pop= "DZ11_pop_allages")

#Deprivation analysis function
deprivation_analysis(filename ="prescriptions_anxiety", measure = "crude", time_agg = 1, 
                    yearstart = 2014, yearend = 2024, year_type = "financial", crude_rate = 100,
                    pop_sex = "all", pop = "depr_pop_allages", ind_id = 20401)


#Remove intermediate zones
exclude_geog_codes(filename = "prescriptions_anxiety", iz = TRUE)


##END
