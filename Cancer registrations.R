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
cancer_reg <- tbl_df(dbGetQuery(channel, statement=
           "SELECT count(*) count, extract (year from incidence_date) year, sex sex_grp, 
                   postcode pc7, floor(((incidence_date-date_of_birth)/365)) age 
            FROM ANALYSIS.SMR06_PI
            WHERE incidence_date between '1 January 2002' and '31 December 2017'
                AND regexp_like(ICD10S_CANCER_SITE, 'C') 
                AND not (regexp_like(ICD10S_CANCER_SITE, 'C44')) 
                AND sex <> 9
            GROUP BY extract (year from incidence_date), sex, postcode, 
                     floor(((incidence_date-date_of_birth)/365))")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing  LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

cancer_reg <- left_join(cancer_reg, postcode_lookup, by = "pc7") %>% #merging with lookup
  mutate(age_grp = case_when( # recoding age into age groups
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19,  TRUE ~ NA_real_)) %>% 
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
              measure = "stdrate", yearstart = 2002, yearend = 2017, time_agg = 3,
              epop_age = "normal", pop = "DZ11_pop_allages")

analyze_second(filename = "cancer_reg_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20301, year_type = "calendar", 
               profile = "HN", min_opt = 1122245)

#Deprivation analysis function
analyze_deprivation(filename="cancer_reg_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2017, year_type = "calendar", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20301)

##END