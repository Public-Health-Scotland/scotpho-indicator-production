# ScotPHO indicators: Female deaths from suicide

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

# Extracting data on deaths of female Scottish residents by excluding records with unknown sex 
# and age and with any icd10 code of suicide in any cause.

deaths_suicide <- tbl_df(dbGetQuery(channel, statement=
  "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7,
    CASE WHEN (year_of_registration >2010
        AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'Y1')
        AND (regexp_like(CAUSE_OF_DEATH_CODE_0, 'F1[123456789]')
        OR regexp_like(CAUSE_OF_DEATH_CODE_1, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_2, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_3, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_4, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_5, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_6, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_7, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_8, 'F1[123456789]') 
        OR regexp_like(CAUSE_OF_DEATH_CODE_9, 'F1[123456789]'))) 
    THEN '1' else '0' END added_new_coding
        FROM ANALYSIS.GRO_DEATHS_C
        WHERE  year_of_registration between '2002' and '2017'
        AND country_of_residence = 'XS' 
        AND sex <> 9
        AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'X[67]|X8[01234]|Y1|Y2|Y3[01234]|Y870|Y872')" )) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
deaths_suicide <- deaths_suicide %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
)) 

# Bringing  LA and datazone info.
postcode_lookup <- read_csv('/conf/linkage/output/lookups/geography/Scottish_Postcode_Directory_2017_2.csv') %>% 
  setNames(tolower(names(.)))  #variables to lower case

# join the data sets with postcode info
deaths_suicide <- left_join(deaths_suicide, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for geography basefile ----
###############################################.
###############################################.


suicides_female <- deaths_suicide %>%
  subset(sex_grp==2) %>% 
  group_by(year, age_grp, age, sex_grp, ca2011) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2011)

saveRDS(suicides_female, file=paste0(data_folder, 'Prepared Data/suicides_female_raw.rds'))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.

## FEMALE SUICIDE DATA ########################.

analyze_first(filename = "suicides_female", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 5, epop_age = "normal")

#epop is only 100000 as only female half population
analyze_second(filename = "suicides_female", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 12539, year_type = "calendar", 
               profile = "MH", min_opt = 7658)