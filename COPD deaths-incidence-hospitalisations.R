# ScotPHO indicators: Patients hospitalised with COPD, COPD incidence and COPD deaths

#   Part 1 - Deaths data basefiles
#   Part 2 - Hospitalisations data basefiles
#   Part 3 - Incidence data file
#   Part 4 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
lapply(c("odbc", "lubridate"), library, character.only = TRUE)
server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Deaths data basefiles ----
###############################################.
#Extracting deaths with a main cause of copd, excluding unknown sex, for over 16, 
# Scottish residents by financial and calendar year. 
copd_deaths <- tbl_df(dbGetQuery(channel, statement=
                  "SELECT LINK_NO linkno, YEAR_OF_REGISTRATION year, AGE, SEX sex_grp, 
                    POSTCODE pc7, date_of_registration dodth,
                    CASE WHEN extract(month from date_of_registration) > 3 THEN extract(year from date_of_registration) 
                    ELSE extract(year from date_of_registration) -1 END as finyear 
                   FROM ANALYSIS.GRO_DEATHS_C
                   WHERE date_of_registration between '1 January 2002' and '31 March 2018'
                    AND sex <> 9 
                    AND age>=16 
                    AND country_of_residence= 'XS'
                    AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'J4[0-4]')")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
copd_deaths <- copd_deaths %>% mutate(age_grp = case_when( 
  age > 14 & age <20 ~ 4, age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, 
  age > 29 & age <35 ~ 7, age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, 
  age > 44 & age <50 ~ 10, age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, 
  age > 59 & age <65 ~ 13, age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, 
  age > 74 & age <80 ~ 16,age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, 
  age > 89 ~ 19,  TRUE ~ NA_real_ ))

# Bringing datazones and LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2018_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, ca2011)

copd_deaths <- left_join(copd_deaths, postcode_lookup, by = "pc7") %>% 
  mutate_if(is.character, factor) # converting variables into factors

#Creating basefile for COPD deaths indicator by calendar year.
copd_deaths_cal <- copd_deaths %>% 
  filter(year<2018) %>% #excluding incomplete year 
  group_by(year, age_grp, sex_grp, ca2011) %>% count() %>% #aggregating
  ungroup() %>% rename(ca = ca2011, numerator = n)

saveRDS(copd_deaths_cal, file=paste0(data_folder, 'Prepared Data/copd_deaths_raw.rds'))

#Creating deaths file for COPD incidence indicator by finantial year.
copd_deaths_fin <- copd_deaths %>% 
  filter(finyear>2001) %>% #excluding incomplete year 
  select(finyear, linkno, age, age_grp, sex_grp, dodth, ca2011) %>% 
  rename(ca = ca2011, doadm = dodth, year = finyear)

###############################################.
## Part 2 - Hospitalisations data basefiles ----
###############################################.
# Extracts admissions with a copd main diagnosis and valid sex. Selecting only record per admission.
# select from 1992 as for incidence it only counts one admission every 10 years.
# this requires using historical data and icd9 codes for the earlier years
copd_adm <- tbl_df(dbGetQuery(channel, statement=
   "SELECT distinct link_no linkno,cis_marker cis, min(AGE_IN_YEARS) age, 
      min(SEX) sex_grp, min(DR_POSTCODE) pc7, min(admission_date) doadm,
      CASE WHEN extract(month from min(admission_date)) > 3 
        THEN extract(year from min(admission_date)) 
        ELSE extract(year from min(admission_date)) -1 END as year 
   FROM ANALYSIS.SMR01_PI z
   WHERE admission_date between '1 January 1997' and '31 March 2018'
      AND sex <> 9 
      AND exists (select * from ANALYSIS.SMR01_PI  
          where link_no=z.link_no and cis_marker=z.cis_marker
            and admission_date between '1 January 1997' and '31 March 2018'
            and regexp_like(main_condition, 'J4[0-4]') )
   GROUP BY link_no, cis_marker
  UNION ALL 
  SELECT distinct link_no linkno, cis_marker cis, min(AGE_IN_YEARS) age, 
    min(SEX) sex_grp, min(DR_POSTCODE) pc7, min(admission_date) doadm,  
    CASE WHEN extract(month from min(admission_date)) > 3 THEN extract(year from min(admission_date)) 
      ELSE  extract(year from min(admission_date)) -1 END as year 
   FROM ANALYSIS.SMR01_HISTORIC z 
    WHERE admission_date between '1 April 1992' and  '31 December 1997'   
    AND sex <> 9 
    AND exists (select * from ANALYSIS.SMR01_HISTORIC  
        where link_no=z.link_no and cis_marker=z.cis_marker 
          AND admission_date between '1 April 1992' and  '31 December 1997'     
          AND (regexp_like(main_condition, 'J4[0-4]') 
              OR regexp_like(main_condition, '-49[0-2,6]')) ) 
   GROUP BY link_no, cis_marker")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

copd_adm <- left_join(copd_adm, postcode_lookup, by = "pc7") %>% 
  mutate_if(is.character, factor) %>%  # converting variables into factors
  filter(!(is.na(datazone2011))) %>%  # excluding non-scottish
  mutate(age_grp = case_when(#recoding ages
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, TRUE ~ NA_real_)) %>% 
  select(-pc7) %>% rename(ca = ca2011)
  
###############################################.
# Preparing stays indicator raw files 
copd_adm_indicator <- copd_adm %>% filter(year>2001) %>% 
# select the first stay within each year.
  arrange(year, linkno, doadm) %>% group_by(year, linkno) %>% 
  filter(row_number()==1 ) %>% ungroup() 

#Datazone2011 raw file
copd_admind_dz11 <- copd_adm_indicator %>% group_by(year, datazone2011, age_grp, sex_grp) %>% 
  count() %>% ungroup() %>% rename(datazone = datazone2011, numerator = n)

saveRDS(copd_admind_dz11, file=paste0(data_folder, 'Prepared Data/copd_hospital_dz11_raw.rds'))

#Datazone2001 raw file
copd_admind_dz01 <- copd_adm_indicator %>% group_by(year, datazone2001, age_grp, sex_grp) %>% 
  count() %>% ungroup() %>% rename(datazone = datazone2001, numerator = n)

#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
copd_admind_depr <- rbind(copd_admind_dz01 %>% subset(year<=2013), 
                  copd_admind_dz11 %>% subset(year>=2014)) 

saveRDS(copd_admind_depr, file=paste0(data_folder, 'Prepared Data/copd_hospital_depr_raw.rds'))

###############################################.
## Part 3 - Incidence data file ----
###############################################.
# Add deaths and admissions data together.
copd_incidence <- bind_rows(copd_adm, copd_deaths_fin) %>%
  filter(age>15) %>% #select 16 and over
# keep only those that did not have a COPD related discharge within the previous 10 years.  
  arrange(linkno, doadm) %>% 
  mutate(doadm = as.Date(doadm), lookback = doadm - years(10),
         exclude = case_when(linkno == lag(linkno) & lookback <lag(doadm) ~ 1,
                             TRUE ~0)) %>% 
  filter(exclude==0 & year>2001) %>%  # also excluding years not needed in profiles
  #aggregating by council
  group_by(year, ca, age_grp, sex_grp) %>% count() %>% ungroup() %>% 
  rename(numerator = n)

saveRDS(copd_incidence, file=paste0(data_folder, 'Prepared Data/copd_incidence_raw.rds'))

###############################################.
## Part 4 - Run analysis functions ----
###############################################.
#COPD deaths
analyze_first(filename = "copd_deaths", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "copd_deaths", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1547, year_type = "calendar", 
               profile = "TP", min_opt = 1008507)

###############################################.
# COPD incidence
analyze_first(filename = "copd_incidence", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "copd_incidence", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1550, year_type = "financial", 
               profile = "TP", min_opt = 1009165)

###############################################.
# COPD hospitalisations 
analyze_first(filename = "copd_hospital_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "copd_hospital_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20302, year_type = "financial", 
               profile = "HN", min_opt = 1464237)

#Deprivation analysis function
analyze_deprivation(filename="copd_hospital_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2017,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20302)

odbcClose(channel) # closing connection to SMRA

##END