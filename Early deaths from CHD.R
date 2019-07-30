# ScotPHO indicators: Early deaths from CHD <75
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

#Extract deaths data where: Valid sex exists, Scottish resident, Aged less than 75, 
#Cause of death was coded as CHD (ICD-10: I20-I25)

deaths_CHD <- tbl_df(dbGetQuery(channel, statement=
  "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
   FROM ANALYSIS.GRO_DEATHS_C
     WHERE sex <> 9
     AND country_of_residence = 'XS'
     AND age < 75
     AND date_of_registration between '1 January 2002' and '31 December 2017'
     AND regexp_like(underlying_cause_of_death, '^I2[0-5]')" )) %>% 
  setNames(tolower(names(.)))  #variables to lower case
  
# Creating age groups for standardization.
deaths_CHD <- deaths_CHD %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.5.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

deaths_CHD <- left_join(deaths_CHD, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
deaths_CHD_dz11 <- deaths_CHD %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_CHD_dz11, file=paste0(data_folder, 'Prepared Data/deaths_CHD_dz11_raw.rds'))
###############################################.

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_CHD_dz01 <- deaths_CHD %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(deaths_CHD_dz01, deaths_CHD_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/deaths_CHD_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#All patients asthma
analyze_first(filename = "deaths_CHD_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_under75", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "deaths_CHD_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 182000, ind_id = 20105, year_type = "calendar", 
               profile = "HN", min_opt = 1288078)

#Deprivation analysis function
analyze_deprivation(filename="deaths_CHD_depr", measure="stdrate", time_agg = 3, 
                    yearstart= 2002, yearend=2017,   year_type = "calendar", 
                    pop = "depr_pop_under75", epop_age= "normal",
                    epop_total = 182000, ind_id = 20105)
