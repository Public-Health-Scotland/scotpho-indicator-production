# ScotPHO indicators: early deaths from cancer (under 75 years)

#   Part 1 - Extract data from SMRA - Deaths file.
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

# Extracting data on deaths of Scottish residents from cancer reasons, under 75, 
# excluding records with unknown sex and age. C44 is not included as is not lethal.
cancer_deaths <- tbl_df(dbGetQuery(channel, statement=
    "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
     FROM ANALYSIS.GRO_DEATHS_C 
     WHERE date_of_registration between '1 January 2002' AND '31 December 2020'
           AND country_of_residence ='XS'
           AND age < 75
           AND regexp_like(underlying_cause_of_death, 'C') 
           AND not (regexp_like(underlying_cause_of_death, 'C44'))
           AND sex <> 9")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

cancer_deaths <- left_join(cancer_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, sex_grp, datazone2001, datazone2011) %>% 
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
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "early_cancer_deaths_dz11", geography = "datazone11", 
              measure = "stdrate", yearstart = 2002, yearend = 2020, time_agg = 3,
              epop_age = "normal", pop = "DZ11_pop_under75")

analyze_second(filename = "early_cancer_deaths_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 182000, ind_id = 20106, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="early_cancer_deaths_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2020, year_type = "calendar", 
                    pop = "depr_pop_under75", epop_age="normal",
                    epop_total =182000, ind_id = 20106)

##END