# ScotPHO indicators: 
# Alcohol specific mortality (5 year aggregate) (Scotland, NHS Board, CA, ADP, HSCP Partnership 
# & Locality - IZ data generated but not included in shiny tool to reduce disclosure risk)
# Alcohol specific mortality by Deprivation (5 year aggregate)
# Female alcohol related mortality (5 year aggregate) (Scotland, NHS Board, CA, ADP  only)
# Male alcohol related mortality (5 year aggregate) (Scotland, NHS Board, CA, ADP only)

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run macros

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

# Select alcohol specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death 
# ICD10 codes to match NRS definitions of alcohol-specific deaths (ie wholly attributable to alcohol) 

data_deaths <- tbl_df(dbGetQuery(channel, statement=
 "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2002' AND '31 December 2018'
        AND country_of_residence ='XS'
        AND regexp_like(underlying_cause_of_death,'E244|F10|G312|G621|G721|I426|K292|K70|K852|K860|Q860|R78|X45|X65|Y15|E860') 
        AND age is not NULL
        AND sex <> 9")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
data_deaths <- data_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Open LA and datazone info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

data_deaths <- left_join(data_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% #ca2019 to be used once dz populations updated and move to using new GSS codes.
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data_deaths %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/alcohol_deaths_dz11_raw.rds'))
datadz <- readRDS(paste0(data_folder, 'Prepared Data/alcohol_deaths_dz11_raw.rds'))

# Datazone2001. Only used for IRs
dz01 <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<2011) %>% rename(datazone = datazone2001)

saveRDS(dz01, file=paste0(data_folder, 'Prepared Data/alchohol_deaths_dz01_raw.rds'))

###############################################.
#Deprivation indicator numerator file

# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dz01_dep <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

# Deprivation basefile
dep_file <- dz11 %>% subset(year>=2014)
dep_file <- rbind(dz01_dep, dep_file) #joining together

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/alcohol_deaths_depr_raw.rds'))

###############################################.
# CA (council area) file for gender specific indicators in Alcohol profile
# Female alcohol mortality

alcohol_deaths_female <- data_deaths %>%
  subset(sex_grp==2) %>% 
  group_by(year, ca2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2011)

saveRDS(alcohol_deaths_female, file=paste0(data_folder, 'Prepared Data/alcohol_deaths_female_raw.rds'))

###############################################.
# CA (council area) file for gender specific indicators in Alcohol profile
# Male alcohol mortality

alcohol_deaths_male <- data_deaths %>%
  subset(sex_grp==1) %>% 
  group_by(year, ca2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2011)

saveRDS(alcohol_deaths_male, file=paste0(data_folder, 'Prepared Data/alcohol_deaths_male_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#Alcohol mortality indicator functions
analyze_first(filename = "alcohol_deaths_dz11", geography = "datazone11", adp=TRUE, measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2018,
              time_agg = 5, epop_age = "normal")

analyze_second(filename = "alcohol_deaths_dz11", measure = "stdrate", time_agg = 5, 
               epop_total = 200000, ind_id = 20204, year_type = "calendar")

###############################################.
#Alcohol mortality by deprivation indicator functions
analyze_deprivation(filename="alcohol_deaths_depr", measure="stdrate", time_agg=5, 
                    yearstart= 2002, yearend=2018,  
                    year_type = "calendar", pop = "depr_pop_allages", 
                    epop_age="normal", epop_total =200000, ind_id = 20204)

###############################################.
#FEMALE Alcohol mortality indicator functions
analyze_first(filename = "alcohol_deaths_female", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2018,
              adp=TRUE, time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "alcohol_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12537, year_type = "calendar")

###############################################.
#MALE Alcohol mortality indicator functions
analyze_first(filename = "alcohol_deaths_male", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2018,
              adp=TRUE, time_agg = 5, epop_age = "normal")

#epop is only 100000 as only male half population
analyze_second(filename = "alcohol_deaths_male", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12536, year_type = "calendar")

#END
