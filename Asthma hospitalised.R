# ScotPHO indicators: Patients hospitalised with asthma and 
#   children hospitalised with asthma (under16). 

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
            uid=.rs.askForPassword("SMRA Username:"), 
            pwd=.rs.askForPassword("SMRA Password:")))

#Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year. 
#Creates one record per CIS and selects only one case per patient/year.
data_asthma <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no,
            MIN(age_in_years) OVER (PARTITION BY link_no) age,
            FIRST_VALUE(sex) OVER (PARTITION BY link_no
                ORDER BY admission_date, discharge_date) sex_grp,
            FIRST_VALUE(DR_POSTCODE) OVER (PARTITION BY link_no
                ORDER BY admission_date, discharge_date) pc7,
      CASE WHEN extract(month from admission_date) > 3 THEN extract(year from admission_date) 
        ELSE extract(year from admission_date) -1 END as year 
   FROM ANALYSIS.SMR01_PI z
   WHERE admission_date between '1 April 2002' and '31 March 2024'
      AND sex <> 0 
      AND regexp_like(main_condition, 'J4[5-6]') ")) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  create_agegroups() # Creating age groups for standardization

# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, ca2019)

data_asthma <- left_join(data_asthma, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
# Datazone2011
asthma_dz11 <- data_asthma %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(asthma_dz11, file=paste0(data_folder, 'Prepared Data/asthma_dz11_raw.rds'))

# CA file for under 16 cases 
asthma_ca_under16 <- data_asthma %>% 
  subset(age<16) %>% 
  group_by(year, ca2019, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(asthma_ca_under16, file=paste0(data_folder, 'Prepared Data/asthma_under16_raw.rds'))

# Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
asthma_dz01_dep <- data_asthma %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(asthma_dz01_dep, asthma_dz11 %>% 
                    subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/asthma_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#All patients asthma
analyze_first(filename = "asthma_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2023,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "asthma_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20304, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="asthma_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2023,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20304)

#Under 16 asthma patients
analyze_first(filename = "asthma_under16", geography = "council", measure = "stdrate", 
              pop = "CA_pop_under16", yearstart = 2002, yearend = 2023, hscp = T,
              time_agg = 3, epop_age = '<16')

analyze_second(filename = "asthma_under16", measure = "stdrate", time_agg = 3, 
               epop_total = 34200, ind_id = 13051, year_type = "financial")

##END