# WORK N PROGRESS - Drug-related mortality indicator code

#   Part 1 - Extract data from SMRA
#   Part 2 - Prepare geographies
#   Part 3 - Run analysis functions
#   Part 4 - Female drug related mortality
#   Part 5 - Male drug related mortality

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

# Old file for 2002-2005 data - all
drug_deaths_02_05 <- read.csv(paste0(data_folder, "Received Data/drugs_deaths_2002-05_DO_NOT_DELETE.csv")) %>%
                                setNames(tolower(names(.)))

# Old file for 2002-2005 data - females
drug_deaths_female_02_05 <- read.csv(paste0(data_folder, "Received Data/drugs_deaths_female_DO_NOT_DELETE.csv")) %>%
                                setNames(tolower(names(.)))

# Old file for 2002-2005 data - males
drug_deaths_male_02_05 <- read.csv(paste0(data_folder, "Received Data/drugs_deaths_male_DO_NOT_DELETE.csv")) %>%
                                setNames(tolower(names(.)))

# NRS file for drug deaths 2006-2018 - match to SQL query by rdno_entry_no_yr
drug_deaths_NRS <- read.spss(paste0(data_folder, "Received Data/NRS_DRDs0618.sav"), 
                to.data.frame=TRUE, use.value.labels=FALSE) %>%
                setNames(tolower(names(.)))

# SQL query for drug deaths 2006-2018
drug_deaths_smr <- tbl_df(dbGetQuery(channel, statement=
      "SELECT year_of_registration year, age, SEX sex_grp, UNDERLYING_CAUSE_OF_DEATH cod1, POSTCODE pc7, 
        REGISTRATION_DISTRICT rdno, ENTRY_NUMBER entry_no
        FROM ANALYSIS.GRO_DEATHS_C 
          WHERE date_of_registration between '1 January 2006' and '31 December 2018'
          AND age is not NULL
          AND sex <> 9
          AND regexp_like(underlying_cause_of_death,'^F1[1-69]|^X4[0-4]|^X6[0-4]|^X85|^Y1[0-4]')
         ")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Adding padding to the entry_no variable so it matches NRS format
drug_deaths_smr <- drug_deaths_smr %>% 
  mutate(entry_no = case_when(nchar(entry_no) == 2 ~ paste0("0", entry_no),
                              nchar(entry_no) == 1 ~ paste0("00", entry_no),
                              TRUE ~paste0(entry_no)))

drug_deaths_smr <- drug_deaths_smr %>% 
# Creating identifier used to match NRS and SMR data
  mutate(rdno_entry_no_yr = paste0(rdno, entry_no, year)) %>%
  create_agegroups() # 5 year age-bands grop

drug_deaths <- left_join(drug_deaths_NRS, drug_deaths_smr, "rdno_entry_no_yr")
           
# checking if totals match with NRS publication
drug_deaths %>% group_by(year) %>% count() %>% View()

###############################################.
## Part 2 - Creating basefiles ----
###############################################.
# Bring council area info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

# Aggregating data by CA
drug_deaths <- left_join(drug_deaths, postcode_lookup, "pc7") %>% 
  rename(datazone = datazone2011, ca = ca2019) %>%
  group_by(year, ca, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup()

saveRDS(drug_deaths, file=paste0(data_folder, 'Prepared Data/drug_deaths_raw.rds'))

# Females
saveRDS(drug_deaths %>% subset(sex_grp==2), 
        file=paste0(data_folder, 'Prepared Data/drug_deaths_female_raw.rds'))

# Males 
saveRDS(drug_deaths %>% subset(sex_grp==1),
        file=paste0(data_folder, 'Prepared Data/drug_deaths_male_raw.rds'))

###############################################.
## Part 3 - All drug-related mortality analysis functions ----
###############################################.

# Analysis by CA
analyze_first(filename = "drug_deaths", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2006, yearend = 2018,
              time_agg = 1, epop_age = "normal", adp = TRUE, hscp = TRUE)

analyze_second(filename = "drug_deaths", measure = "stdrate", time_agg = 1, 
               epop_total = 200000, ind_id = 4121, year_type = "calendar")

all_drug_deaths <- rbind(final_result, drug_deaths_02_05)

# save this for shiny
saveRDS(all_drug_deaths, file = paste0(data_folder, "Data to be checked/all_drug_deaths_shiny.rds"))
write_csv(all_drug_deaths, path = paste0(data_folder, "Data to be checked/all_drug_deaths_shiny.csv"))

###############################################.
## Part 4 - Female drug related mortality analysis functions ----
###############################################.
analyze_first(filename = "drug_deaths_female", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2006, yearend = 2018,
              time_agg = 5, epop_age = "normal", adp = TRUE, hscp = TRUE)

#epop is only 100000 as only female half population
analyze_second(filename = "drug_deaths_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12535, year_type = "calendar")

all_female_drug_deaths <- rbind(final_result, drug_deaths_female_02_05)

# save for shiny
saveRDS(all_female_drug_deaths, file = paste0(data_folder, "Data to be checked/all_female_drug_deaths_shiny.rds"))
write_csv(all_female_drug_deaths, path = paste0(data_folder, "Data to be checked/all_female_drug_deaths_shiny.csv"))

###############################################.
## Part 5 - Male drug related mortality analysis functions ----
###############################################.
analyze_first(filename = "drug_deaths_male", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2006, yearend = 2018, 
              time_agg = 5, epop_age = "normal", adp = TRUE, hscp = TRUE)

#epop is only 100000 as only male half population
analyze_second(filename = "drug_deaths_male", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12534, year_type = "calendar")

all_male_drug_deaths <- rbind(final_result, drug_deaths_male_02_05)

# save for shiny
saveRDS(all_male_drug_deaths, file = paste0(data_folder, "Data to be checked/all_male_drug_deaths_shiny.rds"))
write_csv(all_male_drug_deaths, path = paste0(data_folder, "Data to be checked/all_male_drug_deaths_shiny.csv"))
