# ScotPHO indicators: Deaths from suicide #

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

# Extracting data on deaths by excluding records with unknown sex and
# with any icd10 code of suicide in any cause (includes non-Scottish residents).
deaths_suicide <- as_tibble(dbGetQuery(channel, statement=
  "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
      WHERE  year_of_registration between '2002' and '2022'
      AND sex <> 9
      AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'X[67]|X8[01234]|Y1|Y2|Y3[01234]|Y870|Y872')" )) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_1.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case

# join the data sets with postcode info
deaths_suicide <- left_join(deaths_suicide, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
# Datazone2011
suicides_dz11 <- deaths_suicide %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(suicides_dz11, file=paste0(data_folder, 'Prepared Data/deaths_suicide_dz11_raw.rds'))

###############################################.
# Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
suicides_dz01 <- deaths_suicide %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(suicides_dz01, suicides_dz11 %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/suicide_depr_raw.rds'))

###############################################.
# FEMALE
suicides_female <- deaths_suicide %>%
  subset(sex_grp==2) %>% 
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_female, file=paste0(data_folder, 'Prepared Data/suicides_female_raw.rds'))

###############################################.
# MALE
suicides_male <- deaths_suicide %>%
  subset(sex_grp==1) %>% 
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_male, file=paste0(data_folder, 'Prepared Data/suicides_male_raw.rds'))

###############################################
# YOUNG PEOPLE
suicides_young <- deaths_suicide %>%
  subset(age > 10 & age < 26) %>% 
  group_by(year, ca2019) %>% summarize(numerator = n()) %>% ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_young, file=paste0(data_folder, 'Prepared Data/suicides_young_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
# All suicides
analyze_first(filename = "deaths_suicide_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2021,
              time_agg = 5, epop_age = "normal")

analyze_second(filename = "deaths_suicide_dz11", measure = "stdrate", time_agg = 5, 
               epop_total = 200000, ind_id = 20403, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="suicide_depr", measure="stdrate", time_agg=5,
                    pop = "depr_pop_allages", epop_total =200000, epop_age="normal",
                    yearstart= 2002, yearend=2021, year_type = "financial", ind_id = 20403)

###############################################.
# Female and male suicides
mapply(analyze_first, filename = c("suicides_female", "suicides_male"), 
       geography = "council", measure = "stdrate", pop = "CA_pop_allages", 
       yearstart = 2002, yearend = 2022, time_agg = 5, epop_age = "normal")

#Female suicides: epop is only 100000 as only female half population
analyze_second(filename = "suicides_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12539, year_type = "calendar")
                                      
#Male suicides: epop is only 100000 as only male half population
analyze_second(filename = "suicides_male", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12538, year_type = "calendar")

###############################################.
# Young people suicides
# Crude rates as numbers are too small for standardization.
analyze_first(filename = "suicides_young", geography = "council", measure = "crude", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2022,
              time_agg = 5, epop_age = "11to25")

analyze_second(filename = "suicides_young", measure = "crude", time_agg = 5, crude_rate = 100000,
               epop_total = 34200, ind_id = 13033, year_type = "calendar")

##END
