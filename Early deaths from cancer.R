# ScotPHO indicators: early deaths from cancer (under 75 years)

#   Part 1 - Extract data from SMRA - Deaths file.
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
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extracting data on deaths of Scottish residents from cancer reasons, under 75, 
# excluding records with unknown sex and age. C44 is not included as is not lethal.
cancer_deaths <- tbl_df(dbGetQuery(channel, statement=
    "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
     FROM ANALYSIS.GRO_DEATHS_C 
     WHERE date_of_registration between '1 January 2002' AND '31 December 2017'
           AND country_of_residence ='XS'
           AND age < 75
           AND regexp_like(PRIMARY_CAUSE_OF_DEATH, 'C') 
           AND not (regexp_like(PRIMARY_CAUSE_OF_DEATH, 'C44'))
           AND sex <> 9")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
cancer_deaths <- cancer_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19))

# Open LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2018_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

cancer_deaths <- left_join(cancer_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011) %>% 
  mutate_if(is.character, factor) #converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
candeath_dz11 <- cancer_deaths %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(candeath_dz11, file=paste0(data_folder, 'Prepared Data/early_cancer_deaths_dz11_raw.rds'))

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
candeath_dz01 <- cancer_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

candeath_depr <- rbind(candeath_dz01, candeath_dz11 %>% subset(year>=2014)) 

saveRDS(candeath_depr, file=paste0(data_folder, 'Prepared Data/early_cancer_deaths_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "early_cancer_deaths_dz11", geography = "datazone11", 
              measure = "stdrate", yearstart = 2002, yearend = 2017, time_agg = 3,
              epop_age = "normal", pop = "DZ11_pop_under75")

analyze_second(filename = "early_cancer_deaths_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 182000, ind_id = 20106, year_type = "calendar", 
               profile = "HN", min_opt = 1260149)

#Deprivation analysis function
analyze_deprivation(filename="early_cancer_deaths_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2017, year_type = "calendar", 
                    pop = "depr_pop_under75", epop_age="normal",
                    epop_total =182000, ind_id = 20106)

##END