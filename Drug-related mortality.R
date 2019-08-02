# WORK N PROGRESS - Drug-related mortality indicator code - WORK IN PROGRESS

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

# Select drug specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death 
# ICD10 codes to match NRS definitions of drug-specific deaths (ie wholly attributable to drugs)

drug_deaths <- tbl_df(dbGetQuery(channel, statement=
      "SELECT year_of_registration year, age, SEX sex_grp, UNDERLYING_CAUSE_OF_DEATH cod1, POSTCODE pc7

       FROM ANALYSIS.GRO_DEATHS_C 
          WHERE date_of_registration between '1 January 2002' and '31 December 2017'
          AND country_of_residence = 'XS'
          AND age is not NULL
          AND sex <> 9
          AND regexp_like(underlying_cause_of_death,'^F1[1-69]|^X4[0-4]|^X6[0-4]|^X85|^Y1[0-4]')
         ")) %>%
  setNames(tolower(names(.)))  #variables to lower case
#11067 records
# AND (supplementary_code_0 = 21 or supplementary_code_1 = 21 or supplementary_code_2 = 21 )
drug_deaths %>% group_by(year) %>%  count() %>% View()
#recode age groups
drug_deaths <- drug_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Open LA and datazone info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.5.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

drug_deaths <- left_join(drug_deaths, postcode_lookup, "pc7") %>% 
  select(year, hb2014, ca2011, datazone2011, pc7, sex_grp, age_grp, rdno, entry_no, recid, dodeath, datereg, cod1) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  rename(datazone = datazone2011, ca = ca2011)

saveRDS(drug_deaths, file=paste0(data_folder, 'Prepared Data/drug_deaths_raw.rds'))


####################################
## ADP LEVEL #######################
####################################

ADP_lookup <- read_rds(paste0(data_folder, "Lookups/Geography/ADP_CA_lookup.rds")) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  rename(ca = ca2011)

drug_deaths_ADP <- left_join(drug_deaths, ADP_lookup, "ca") %>% 
  select(year, hb2014, ca, datazone, adp, pc7, sex_grp, age_grp, rdno, entry_no, recid, dodeath, datereg, cod1) %>% 
  subset(!(is.na(datazone))) %>%  #select out non-scottish
  mutate_if(is.character, factor)  # converting variables into factors

saveRDS(drug_deaths_ADP, file=paste0(data_folder, 'Prepared Data/drug_deaths_ADP_raw.rds'))

##SELECT CA DATA##

drug_deaths_ADP2 <- left_join(drug_deaths, ADP_lookup, "ca") %>%
  select(year, ca, sex_grp, age_grp) %>%
  subset(!(is.na(ca))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup()

saveRDS(drug_deaths_ADP2, file=paste0(data_folder, 'Prepared Data/drug_deaths_ADP2_raw.rds'))

#analyse by CA#

analyze_first(filename = "drug_deaths_ADP2", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 1, epop_age = "normal")

analyze_second(filename = "drug_deaths_ADP2", measure = "stdrate", time_agg = 1, 
               epop_total = 200000, ind_id = 4121, year_type = "calendar", 
               profile = "HN", min_opt = 1295131)

###############################################.
# CA (council area) file for gender specific indicators in drug profile
# Female alcohol mortality

drug_deaths_female <- drug_deaths_ADP2 %>%
  subset(sex_grp==2) %>% 
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() 
 
saveRDS(drug_deaths_female, file=paste0(data_folder, 'Prepared Data/drug_deaths_female_raw.rds'))

###############################################.
#FEMALE Alcohol mortality indicator functions
analyze_first(filename = "drug_deaths_female", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2017, 
              time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "drug_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12535, year_type = "calendar", 
               profile = "HN", min_opt = 1245385)

# Male alcohol mortality

drug_deaths_male <- drug_deaths_ADP2 %>%
  subset(sex_grp==1) %>% 
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() 

saveRDS(drug_deaths_male, file=paste0(data_folder, 'Prepared Data/drug_deaths_male_raw.rds'))

###############################################.
#FEMALE Alcohol mortality indicator functions
analyze_first(filename = "drug_deaths_male", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2017, 
              time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "drug_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12534, year_type = "calendar", 
               profile = "HN", min_opt = 1245385)