# ScotPHO indicators: Lung cancer deaths

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

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
       WHERE date_of_registration between '1 January 2002' and '31 December 2018'
          AND country_of_residence= 'XS' 
          AND regexp_like(underlying_cause_of_death, 'C3[34]')
          AND age >= 16
          AND sex <> 9")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing  LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>% select(pc7, ca2019)
  
lung_deaths <- left_join(lung_deaths, postcode_lookup, by = "pc7") %>% #merging with lookup
  # aggregating by council area
  group_by(year, ca2019, sex_grp, age_grp) %>% count() %>% ungroup() %>% 
  rename(ca = ca2019, numerator = n)

saveRDS(lung_deaths, file=paste0(data_folder, 'Prepared Data/lungcancer_deaths_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "lungcancer_deaths", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2018,
              time_agg = 3, epop_age = "16+")

analyze_second(filename = "lungcancer_deaths", measure = "stdrate", time_agg = 3, 
               epop_total = 165800, ind_id = 1546, year_type = "calendar")

##END