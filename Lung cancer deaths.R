# ScotPHO indicators: Lung cancer deaths

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
# Extracting data on deaths of people over 16, Scottish residents, excluding records 
# with unknown sex and age, and with a diagnosis of lung cancer (ICD10 codes C33-C34).
# It differs slightly from what is reported nationally by ISD, as their death figures 
# include all ages and non-Scotland residents.
lung_deaths <- tbl_df(dbGetQuery(channel, statement=
      "SELECT year_of_registration year, AGE, SEX sex_grp, POSTCODE pc7 
       FROM ANALYSIS.GRO_DEATHS_C
       WHERE date_of_registration between '1 January 2002' and '31 December 2017'
          AND country_of_residence= 'XS' 
          AND regexp_like(PRIMARY_CAUSE_OF_DEATH, 'C3[34]')
          AND age >= 16
          AND sex <> 9")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing  LA info.
postcode_lookup <- read_csv('/conf/linkage/output/lookups/geography/Scottish_Postcode_Directory_2017_2.csv') %>% 
  setNames(tolower(names(.))) %>% select(pc7, ca2011) #variables to lower case
  
lung_deaths <- left_join(lung_deaths, postcode_lookup, by = "pc7") %>% #merging with lookup
  mutate(age_grp = case_when( # recoding ages into age groups
    age > 14 & age <20 ~ 4, age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, 
    age > 29 & age <35 ~ 7, age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, 
    age > 44 & age <50 ~ 10,age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, 
    age > 59 & age <65 ~ 13, age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, 
    age > 74 & age <80 ~ 16, age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, 
    age > 89 ~ 19, TRUE ~ NA_real_)) %>% 
  # aggregating by council area
  group_by(year, ca2011, sex_grp, age_grp) %>% count() %>% ungroup() %>% 
  rename(ca = ca2011, numerator = n)

saveRDS(lung_deaths, file=paste0(data_folder, 'Prepared Data/lungcancer_deaths_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "lungcancer_deaths", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "lungcancer_deaths", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1546, year_type = "calendar", 
               profile = "TP", min_opt = 1007426)

odbcClose(channel) # closing connection to SMRA

##END