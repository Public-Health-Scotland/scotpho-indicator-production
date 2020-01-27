# WORK N PROGRESS - Drug-related mortality indicator code

#   Part 1 - Extract data from SMRA
#   Part 2 - Prepare geographies
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


# Select drug specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death 
# ICD10 codes to match NRS definitions of drug-specific deaths (ie wholly attributable to drugs)

drug_deaths_02_05 <- read.csv(paste0(data_folder, "Received Data/drugs_deaths_2002-05_DO_NOT_DELETE.csv")) %>%
                                setNames(tolower(names(.)))

drug_deaths_NRS <- read.spss(paste0(data_folder, "Received Data/NRS_DRDs0618.sav"), 
                to.data.frame=TRUE, use.value.labels=FALSE) %>%
                setNames(tolower(names(.)))

drug_deaths <- tbl_df(dbGetQuery(channel, statement=
      "SELECT year_of_registration year, age, SEX sex_grp, UNDERLYING_CAUSE_OF_DEATH cod1, POSTCODE pc7, 
        REGISTRATION_DISTRICT rdno, ENTRY_NUMBER entry_no
        FROM ANALYSIS.GRO_DEATHS_C 
          WHERE date_of_registration between '1 January 2006' and '31 December 2018'
          AND country_of_residence = 'XS'
          AND age is not NULL
          AND sex <> 9
          AND regexp_like(underlying_cause_of_death,'^F1[1-69]|^X4[0-4]|^X6[0-4]|^X85|^Y1[0-4]')
         ")) %>%
  setNames(tolower(names(.)))  #variables to lower case
 
drug_deaths <- drug_deaths %>% mutate(rdno_entry_no_yr = paste0(drug_deaths$rdno, drug_deaths$entry_no, drug_deaths$year)) %>%
  create_agegroups()

drug_deaths <- left_join(drug_deaths_NRS, drug_deaths, "rdno_entry_no_yr")
           
#8480 records
# drug_deaths %>% group_by(year) %>% count() %>% View()

###############################################.
## Part 2 - Geography basefiles ----
###############################################.

# Open LA and datazone info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

drug_deaths <- left_join(drug_deaths, postcode_lookup, "pc7") %>% 
  select(year, hb2019, ca2019, datazone2011, pc7, sex_grp, age_grp, rdno_entry_no_yr) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  rename(datazone = datazone2011, ca = ca2019) %>%
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup()

saveRDS(drug_deaths, file=paste0(data_folder, 'Prepared Data/drug_deaths_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

# Analysis by CA
analyze_first(filename = "drug_deaths", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2006, yearend = 2018,
              time_agg = 1, epop_age = "normal")

analyze_second(filename = "drug_deaths_ca", measure = "stdrate", time_agg = 1, 
               epop_total = 200000, ind_id = 4121, year_type = "calendar")

# CA (council area) file for gender specific indicators in drug profile
#########Female drug mortality
drug_deaths_female <- drug_deaths_ca %>%
  subset(sex_grp==2) %>% 
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup()
 
saveRDS(drug_deaths_female, file=paste0(data_folder, 'Prepared Data/drug_deaths_female_raw.rds'))

# Analysis
analyze_first(filename = "drug_deaths_female", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2018, 
              time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "drug_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12535, year_type = "calendar")

#########Male drug mortality
drug_deaths_male <- drug_deaths_ca %>%
  subset(sex_grp==1) %>% 
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() 

saveRDS(drug_deaths_male, file=paste0(data_folder, 'Prepared Data/drug_deaths_male_raw.rds'))

# Analysis
analyze_first(filename = "drug_deaths_male", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2018, 
              time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "drug_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12534, year_type = "calendar")