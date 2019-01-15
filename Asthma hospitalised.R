# ScotPHO indicators: Patients hospitalised with asthma and 
#   children hospitalised with asthma (under16). 

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(odbc) #for reading oracle databases

server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
            uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

#Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year. 
#Creates one record per CIS and selects only one case per patient/year.
data_asthma <- tbl_df(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkno, min(AGE_IN_YEARS) age, min(SEX) sex_grp, min(DR_POSTCODE) pc7,
      CASE WHEN extract(month from admission_date) > 3 THEN extract(year from admission_date) 
        ELSE extract(year from admission_date) -1 END as year 
   FROM ANALYSIS.SMR01_PI z
   WHERE admission_date between '1 April 2002' and '31 March 2018'
      AND sex <> 0 
      AND regexp_like(main_condition, 'J4[5-6]') 
   GROUP BY link_no, CASE WHEN extract(month from admission_date) > 3 THEN
      extract(year from admission_date) ELSE extract(year from admission_date) -1 END")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
data_asthma <- data_asthma %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Bringing  LA and datazone info.
postcode_lookup <- read_csv('/conf/linkage/output/lookups/geography/Scottish_Postcode_Directory_2017_2.csv') %>% 
  setNames(tolower(names(.)))  #variables to lower case

data_asthma <- left_join(data_asthma, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data_asthma %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/asthma_dz11_raw.rds'))
datadz <- readRDS(paste0(data_folder, 'Prepared Data/asthma_dz11_raw.rds'))

###############################################.
# CA file for under 16 cases 
ca_under16 <- data_asthma %>% subset(age<16) %>% group_by(year, ca2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%   rename(ca = ca2011)

saveRDS(ca_under16, file=paste0(data_folder, 'Prepared Data/asthma_under16_raw.rds'))

###############################################.
# Datazone2001. Only used for IRs
dz01 <- data_asthma %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001)

dz01_dep <- dz01 # to user later for deprivation basefile

dz01 <- dz01 %>% subset(year<2011) 

saveRDS(dz01, file=paste0(data_folder, 'Prepared Data/asthma_dz01_raw.rds'))

###############################################.
# IR basefile
ir_file <- dz11 %>% subset(year>2010)
ir_file <- rbind(dz01, ir_file) #joining together

saveRDS(ir_file, file=paste0(data_folder, 'Prepared Data/DZ_asthma_IR_raw.rds'))

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dep_file <- rbind(dz01_dep %>% subset(year<=2013), dz11 %>% subset(year>=2014)) 

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/asthma_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#All patients asthma
analyze_first(filename = "asthma_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "asthma_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20304, year_type = "financial", 
               profile = "HN", min_opt = 2999)

#Deprivation analysis function
analyze_deprivation(filename="asthma_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2017,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20304)

#############################################.
#Under 16 asthma patients
analyze_first(filename = "asthma_under16", geography = "council", measure = "stdrate", 
              pop = "LA_pop_under16", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = '<16')

analyze_second(filename = "asthma_under16", measure = "stdrate", time_agg = 3, 
               epop_total = 34200, ind_id = 13051, year_type = "financial", 
               profile = "CP", min_opt = 2999)

odbcClose(channel) # closing connection to SMRA

##END