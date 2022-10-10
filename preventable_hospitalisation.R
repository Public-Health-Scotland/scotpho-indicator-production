# ScotPHO indicators: Preventable emergency hospitalisation for a chronic disease

#   Part 1 - Hospitalisations data basefiles
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(lubridate)

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Hospitalisations data basefiles ----
###############################################.
# Extracts admissions with a chronic ambulatory care-sensitive condition and valid sex. Selecting only record per admission.
# select from 02/03.

data_chronic <- tibble::as_tibble(dbGetQuery(channel, statement=
    "SELECT distinct link_no,
            MIN(age_in_years) OVER (PARTITION BY link_no) age,
            FIRST_VALUE(sex) OVER (PARTITION BY link_no
                ORDER BY admission_date, discharge_date) sex_grp,
            FIRST_VALUE(DR_POSTCODE) OVER (PARTITION BY link_no
                ORDER BY admission_date, discharge_date) pc7,
      CASE WHEN extract(month from admission_date) > 3 THEN extract(year from admission_date) 
        ELSE extract(year from admission_date) -1 END as year 
   FROM ANALYSIS.SMR01_PI z
   WHERE admission_date between '1 April 2002' and '31 March 2022'
      AND sex <> 0 
      AND (admission_type between '20' and '22' or admission_type between '30' and '39')
      AND regexp_like(main_condition, 'E1[0-4]|D51|D52|F0[0-3]|G40|I20|I25|I50|J20|
                                       J41|J4[3-5]|B180|B181|D501|D508|D509|I10X|I110|
                                       I119|I130|I148X|J81X|J42X|J46X|J47X') ")) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  create_agegroups() # Creating age groups for standardization

#check that a patient is only counted once per year 
data_checklink<-data_chronic %>%
count(link_no,year)

# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2022_1.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, ca2019)

data_chronic <- left_join(data_chronic, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

#######################

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
# Datazone2011
chronic_dz11 <- data_chronic %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(chronic_dz11, file=paste0(data_folder, 'Prepared Data/chronic_dz11_raw.rds'))

# Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
chronic_dz01_dep <- data_chronic %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(chronic_dz01_dep, chronic_dz11 %>% 
                    subset(year>=2014)) #joing dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/chronic_depr_raw.rds'))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#All patients asthma
analyze_first(filename = "chronic_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2022,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "chronic_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20304, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="chronic_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2022,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20304)

