#   ScotPHO indicators: Alcohol-related hospital stays (all ages) and 
#   Alcohol-related hospital stays (ages 11 to 25 years)

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

#libraries

# library(odbc)  # run odbc first
# library(dplyr)
# library(readr)
library(lubridate) #requires lubridate to derive financial year of stay

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

# Extract alcohol stay CIS data 
# This extraction should give figures that match the methodology used by ISD alcohol team who publish national statistics for alcohol related admissions.
# ScotPHO should not publish updated indicator until after the national statistics publication has been released.
# Diagnostic codes should match those use by the ISD ARHS national statistics publication.  The codes used may differ from alcohol related deaths indicators since
# alcohol related deaths focus on conditions wholly attributable to alcohol.

# SMRA extraction returns all episodes data within a continuous inpatient stay
# The CIS are restricted to stays where an individual has one or more alcohol related ICD10 diagnosis codes (in any position)
# Date restrictions are based on financial year of hospital discharge (not episode discharge) therefore date filters in extract are set to the 
# year before desired data (eg if period for reporting is 2002/2003 then request data from 2001 if you want to be sure to capture all CIS that end in 2002/03)


data_alcohol_episodes <- tbl_df(dbGetQuery(channel, statement=
       "SELECT link_no linkno, cis_marker cis, AGE_IN_YEARS age, admission_date, discharge_date, DR_POSTCODE pc7, SEX sex_grp, ADMISSION, DISCHARGE, URI
  FROM ANALYSIS.SMR01_PI z
  where discharge_date between  '1 April 2001' and '31 March 2018'
  and sex <> 9
  and exists (select * from ANALYSIS.SMR01_PI  where link_no=z.link_no and cis_marker=z.cis_marker
  and discharge_date between '1 April 2001' and '31 March 2018'
  and (regexp_like(main_condition,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')
  or regexp_like(other_condition_1,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')
  or regexp_like(other_condition_2,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')
  or regexp_like(other_condition_3,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')
  or regexp_like(other_condition_4,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')
  or regexp_like(other_condition_5,'E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721')))")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Group episode level alcohol data into hospital stays (number of rows will reduce)
# Keep age, sex, postcode on admission data but take date of discharge from last episode in a hospital stay. 

data_alcoholstays <- data_alcohol_episodes  %>%
  arrange(linkno,admission_date, discharge_date, admission, discharge, uri) %>%
  group_by (linkno,cis) %>%
  summarise(age=first(age), #age on hospital admission
            sex_grp=first(sex_grp), #sex at admission
            pc7=first(pc7), 
            ddisch=last(discharge_date),
            staymonth=month(ddisch),
            year = case_when(staymonth >3 ~ year(ddisch), staymonth <= 3 ~ year(ddisch)-1, TRUE ~ 0)) %>% # generate financial year of stay field
  subset(year>=2002) %>% # profiles only require figures from fye 2002/03 onwards
  ungroup()

# Creating age groups for standardization.
data_alcoholstays <- data_alcoholstays %>%
  # subset(year>=2002) %>%
  mutate(age_grp = case_when( 
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
    TRUE ~ as.numeric(age)
  ))

# # Bringing  LA and datazone info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

# Match geography information (datazone) to stays data
data_alcoholstays <- left_join(data_alcoholstays, postcode_lookup, "pc7")

# Select out unmatched rows and keep required fields
data_alcoholstays <- data_alcoholstays %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% # will need to change to new ca codes once new dz populations available
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors


###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.

# Datazone2011
dz11 <- data_alcoholstays %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/alcohol_stays_dz11_raw.rds'))
datadz <- readRDS(paste0(data_folder, 'Prepared Data/alcohol_stays_dz11_raw.rds')) %>%
  mutate_if(is.character, factor)



###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD

dz01_dep <- data_alcoholstays %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(dz01_dep, dz11 %>% subset(year>=2014)) #joing dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/alcohol_stay_depr_raw.rds'))


###############################################.
# CA (council area) file for separate indicator in CYP profile for those aged 11 to 25 years

alcoholstays_11to25 <- data_alcoholstays %>%
  subset(age>=11 & age<=25) %>% 
  group_by(year, ca2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2011)

saveRDS(alcoholstays_11to25, file=paste0(data_folder, 'Prepared Data/alcohol_stays_11to25_raw.rds'))



###############################################.
## Part 3 - Run analysis functions ----
###############################################.

##Run macros to generate HWB and Alcohol Profile indicator data
#All ages alcohol related hospital stays ----
analyze_first(filename = "alcohol_stays_dz11", geography = "datazone11", adp = TRUE, measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 1, epop_age = "normal")

analyze_second(filename = "alcohol_stays_dz11", measure = "stdrate", time_agg = 1, 
               epop_total = 200000, ind_id = 20203, year_type = "financial", 
               profile = "HN", min_opt = 2999)

#Deprivation analysis function (runs against admissions all ages)
analyze_deprivation(filename="alcohol_stays_depr", measure="stdrate", time_agg=1, 
                    yearstart= 2002, yearend=2017,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20203)

##Run macros again to generate CYP indicator data
#Alcohol related stays in 11 to 25 year olds ----
analyze_first(filename = "alcohol_stays_11to25", geography = "council", measure = "stdrate", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = '11to25')

analyze_second(filename = "alcohol_stays_11to25", measure = "stdrate", time_agg = 3, 
               epop_total = 34200, ind_id = 13024, year_type = "financial", 
               profile = "CP", min_opt = 2999)



obcClose(channel) # closing connection to SMRA

##END




