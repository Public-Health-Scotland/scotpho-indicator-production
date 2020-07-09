# ScotPHO indicators: Lung cancer registrations

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# Extracting data on people over 16, with a diagnosis of lung cancer (ICD10 codes C33-C34) 
# and excluding records with unknown sex .
# It counts tumours, not different patients, e.g. a patient can have several tumours over the years.
# If we were to use SMRA geographical information, the code could be simplified.
lung_reg <- tbl_df(dbGetQuery(channel, statement=
  "SELECT count(*) count, extract (year from INCIDENCE_DATE) year, SEX sex_grp, 
      POSTCODE pc7, age_in_years age 
   FROM ANALYSIS.SMR06_PI
   WHERE incidence_date between '1 January 2002' and '31 December 2018'
      AND age_in_years>=16 
      AND regexp_like(ICD10S_CANCER_SITE, 'C3[34]')
      AND sex <> 9
  GROUP BY extract (year from incidence_date), sex, postcode, age_in_years")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing  LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_1.rds') %>% 
  setNames(tolower(names(.))) %>% select(pc7, ca2011) #variables to lower case

lung_reg <- left_join(lung_reg, postcode_lookup, by = "pc7") %>% #merging with lookup
  # aggregating by council area
  group_by(year, ca2011, sex_grp, age_grp) %>% summarize(numerator = sum(count)) %>% 
  ungroup() %>% rename(ca = ca2011) %>% 
  filter(!is.na(ca)) # excluding non-Scottish residents

saveRDS(lung_reg, file=paste0(data_folder, 'Prepared Data/lungcancer_reg_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "lungcancer_reg", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2018, hscp = T,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "lungcancer_reg", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1549, year_type = "calendar")

##END