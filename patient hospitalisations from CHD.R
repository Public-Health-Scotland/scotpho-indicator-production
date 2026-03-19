# ScotPHO indicators: patients hospitalised with coronary heart disease

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
source("functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

#Looking to admissions with a main diagnosis of CHD (ICD-10: I20-I25), 
# excluding unknown sex, by financial year. 
#Creates one record per CIS and selects only one case per patient/year.
hospitalisation_CHD <- as_tibble(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkno, min(AGE_IN_YEARS) age, min(SEX) sex_grp, min(DR_POSTCODE) pc7,
    CASE WHEN extract(month from admission_date) > 3 
        THEN extract(year from admission_date) 
        ELSE extract(year from admission_date) -1 END as year
    FROM ANALYSIS.SMR01_PI z 
      WHERE admission_date between  '1 April 2002' and '31 March 2025'
      AND sex <> 0 
      AND regexp_like(main_condition, 'I2[0-5]')
    GROUP BY link_no,
      CASE WHEN extract(month from admission_date) > 3 
          THEN extract(year from admission_date) 
          ELSE extract(year from admission_date) -1 END" )) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
hospitalisation_CHD <- create_agegroups(hospitalisation_CHD)

# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, ca2011)

hospitalisation_CHD <- left_join(hospitalisation_CHD, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
# Datazone2011
hospitalisation_CHD_dz11 <- hospitalisation_CHD %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(hospitalisation_CHD_dz11, file.path(profiles_data_folder, 'Prepared Data/hospitalisation_CHD_dz11_raw.rds'))

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
hospitalisation_CHD_dz01 <- hospitalisation_CHD %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(hospitalisation_CHD_dz01, hospitalisation_CHD_dz11 %>% subset(year>=2014)) %>% #join dz01 and dz11
  mutate(age_grp = factor(age_grp))


saveRDS(dep_file, file.path(profiles_data_folder, 'Prepared Data/hospitalisation_CHD_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
main_analysis(ind_id = 20303, filename = "hospitalisation_CHD_dz11", geography = "datazone11", 
              measure = "stdrate", epop_age = "normal", epop_total = 200000, pop = "DZ11_pop_allages", 
              yearstart = 2002, yearend = 2024, year_type = "financial", time_agg = 3)



#Deprivation analysis function
deprivation_analysis(filename="hospitalisation_CHD_depr", measure="stdrate", time_agg = 3, 
                    yearstart= 2002, yearend=2023, year_type = "financial", 
                    epop_age= "normal", epop_total = 200000, ind_id = 20303)

##END
