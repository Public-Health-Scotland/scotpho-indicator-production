# ScotPHO indicators: early deaths from cancer (under 75 years)

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extracting data on deaths of Scottish residents from cancer reasons, under 75, 
# excluding records with unknown sex and age. C44 is not included as is not lethal.
cancer_deaths <- as_tibble(dbGetQuery(channel, statement=
    "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
     FROM ANALYSIS.GRO_DEATHS_C 
     WHERE date_of_registration between '1 January 2002' AND '31 December 2024'
           AND country_of_residence ='XS'
           AND age < 75
           AND regexp_like(underlying_cause_of_death, 'C') 
           AND not (regexp_like(underlying_cause_of_death, 'C44'))
           AND sex <> 9")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2026_2.rds') %>% 
  clean_names() %>%   #variables to lower case
  select(pc7, datazone2022)

cancer_deaths <- left_join(cancer_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, sex_grp, datazone2022) %>% 
  mutate_if(is.character, factor) #converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
candeath_dz22 <- cancer_deaths %>% group_by(year, datazone2022, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2022)

saveRDS(candeath_dz22, file.path(profiles_data_folder, 'Prepared Data/early_cancer_deaths_dz22_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
main_analysis(filename = "early_cancer_deaths_dz11", geography = "datazone11", 
              measure = "stdrate", yearstart = 2002, yearend = 2024, time_agg = 3,
              epop_age = "normal", pop = "DZ11_pop_under75", epop_total = 182000,
              ind_id = 20106, year_type = "calendar")

#Deprivation analysis function
#Run when new SIMD is released
# deprivation_analysis(filename ="early_cancer_deaths", measure = "stdrate", time_agg= 3, 
#                     yearstart = 2014, yearend = 2024, year_type = "calendar", 
#                     pop = "depr_pop_under75", epop_age = "normal",
#                     epop_total = 182000, ind_id = 20106, pop_sex = "all")


##END
