# ScotPHO indicators: Lung cancer registrations

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(odbc) #for reading oracle databases

server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# Extracting data on people over 16, with a diagnosis of lung cancer (ICD10 codes C33-C34) 
# and excluding records with unknown sex .
# It counts tumours, not different patients, e.g. a patient can have several tumours over the years.
# If we were to use SMRA geographical information, the code could be simplified.
lung_reg <- tbl_df(dbGetQuery(channel, statement=
  "SELECT count(*), extract (year from INCIDENCE_DATE) year, SEX sex_grp, 
    POSTCODE pc7, floor(((incidence_date-date_of_birth)/365)) age 
   FROM ANALYSIS.SMR06_PI
   WHERE incidence_date between '1 January 2002' and '31 December 2017'
    AND ((incidence_date-date_of_birth)/365)>=16 
    AND regexp_like(ICD10S_CANCER_SITE, 'C3[34]')
    AND sex <> 9
  GROUP BY extract (year from incidence_date), sex, postcode, 
  floor(((incidence_date-date_of_birth)/365))")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing  LA info.
postcode_lookup <- read_csv('/conf/linkage/output/lookups/geography/Scottish_Postcode_Directory_2017_2.csv') %>% 
  setNames(tolower(names(.))) %>% select(pc7, ca2011) #variables to lower case

lung_reg <- left_join(lung_reg, postcode_lookup, by = "pc7") %>% #merging with lookup
  mutate(age_grp = case_when( # recoding age into age groups
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19,  TRUE ~ NA_real_)) %>% 
  # aggregating by council area
  group_by(year, ca2011, sex_grp, age_grp) %>% count() %>% ungroup() %>% 
  rename(ca = ca2011, numerator = n) %>% 
  filter(!is.na(ca)) # excluding non-Scottish residents

saveRDS(lung_reg, file=paste0(data_folder, 'Prepared Data/lungcancer_reg_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "lungcancer_reg", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "lungcancer_reg", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1549, year_type = "calendar", 
               profile = "TP", min_opt = 1002068)

odbcClose(channel) # closing connection to SMRA

##END