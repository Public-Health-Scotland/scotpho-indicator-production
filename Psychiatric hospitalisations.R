# ScotPHO indicators: Patients with a Psychiatric Hospitalisation. 

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

#Extracting one record per patient with a pshychiatric admission per financial year
#Only cases with a valid sex, excluding learning disabilities and those with no datazone. 
#Selects last value as its mh team methodology and potentially more accurate.
#Also using smra geo variables as mh team approach
data_psychiatric <- as_tibble(dbGetQuery(channel, statement= 
    "SELECT distinct link_no linkno,
        CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date) 
            ELSE extract(year from discharge_date) -1 END as year,
        MAX(age_in_years ) KEEP ( DENSE_RANK LAST ORDER BY discharge_date) as age, 
        MAX( sex ) KEEP ( DENSE_RANK LAST ORDER BY discharge_date) as sex_grp, 
        MAX( datazone_2011 ) KEEP ( DENSE_RANK LAST ORDER BY discharge_date) as datazone_2011, 
        MAX( datazone_2001 ) KEEP ( DENSE_RANK LAST ORDER BY discharge_date) as datazone_2001 
   FROM ANALYSIS.SMR04_PI z
   WHERE discharge_date between '1 April 2002' and '31 March 2023'
         AND specialty <> 'G5' 
         AND sex in ('1', '2')
         AND datazone_2011 is not null 
   GROUP BY link_no, CASE WHEN extract(month from discharge_date) > 3 THEN
      extract(year from discharge_date) ELSE extract(year from discharge_date) -1 END")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.


###############################################.
## Part 2 - Prepare geography basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data_psychiatric %>% group_by(year, datazone_2011, age_grp, sex_grp) %>% 
  summarise(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone_2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/psychiatric_discharges_dz11_raw.rds'))

###############################################.
# Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dz01 <- data_psychiatric %>% group_by(year, datazone_2001, sex_grp, age_grp) %>% 
  subset(year<=2013) %>% summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone_2001)

dep_file <- rbind(dz01, dz11 %>% subset(year>=2014)) 

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/psychiatric_discharges_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
# All patients psychiatric discharge
analyze_first(filename = "psychiatric_discharges_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2022,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "psychiatric_discharges_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20402, year_type = "financial")

# Deprivation analysis function
analyze_deprivation(filename="psychiatric_discharges_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2020,   year_type = "financial", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20402)

##END
