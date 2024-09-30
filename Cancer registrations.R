# ScotPHO indicators: New cancer registrations

#   Part 1 - Extract data from SMRA
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# Extracting cancer registry records with a diagnosis of cancer(ICD10 codes C, excluding C44).
# excluding records with unknown sex. It counts tumours, not different patients,
#  e.g. a patient can have several tumours over the years.
cancer_reg <- as_tibble(dbGetQuery(channel, statement=
           "SELECT count(*) count, extract (year from incidence_date) year, sex sex_grp, 
                   postcode pc7, age_in_years age 
            FROM ANALYSIS.SMR06_PI
            WHERE incidence_date between '1 January 2002' and '31 December 2022'
                AND regexp_like(ICD10S_CANCER_SITE, 'C') 
                AND not (regexp_like(ICD10S_CANCER_SITE, 'C44')) 
                AND sex <> 9
            GROUP BY extract (year from incidence_date), sex, postcode, age_in_years")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing  LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

cancer_reg <- left_join(cancer_reg, postcode_lookup, by = "pc7") %>% #merging with lookup
  filter(!is.na(datazone2011)) # excluding non-Scottish residents

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
canreg_dz11 <- cancer_reg %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = sum(count)) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(canreg_dz11, file=paste0(data_folder, 'Prepared Data/cancer_reg_dz11_raw.rds'))

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
canreg_dz01 <- cancer_reg %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = sum(count)) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

canreg_depr <- rbind(canreg_dz01, canreg_dz11 %>% subset(year>=2014)) 

saveRDS(canreg_depr, file=paste0(data_folder, 'Prepared Data/cancer_reg_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "cancer_reg_dz11", geography = "datazone11", 
              measure = "stdrate", yearstart = 2002, yearend = 2022, time_agg = 3,
              epop_age = "normal", pop = "DZ11_pop_allages")

analyze_second(filename = "cancer_reg_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20301, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="cancer_reg_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2022, year_type = "calendar", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20301)

##END