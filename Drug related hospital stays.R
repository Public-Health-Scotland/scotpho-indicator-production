# ScotPHO indicators: Drug-related hospital admissions for all and for 11-25.
# indicators can be updated following the release of the drug-related hospital statistics publication (typically in April)
# Note this publication looks at combined general acute (SMR01) AND psychiatric (SMR04) drug-related stays whereas we only look at SMR01.
# Selecting hospital type:general acute in their publication dashboard will match our figures.
# https://publichealthscotland.scot/publications/drug-related-hospital-statistics/

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
source("functions/deprivation_analysis.R") # deprivation function
source("/PHI_conf/ScotPHO/Profiles/Code/stat_disclosure_drug_stays.R") # statistical disclosure methodology - confidential - do not share

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extract drug stay CIS data: provides figures that match the methodology used by ISD drug 
# team who publish national statistics for drug related stays
# ScotPHO should not publish updated indicator until after the national 
# statistics publication has been released.
# Diagnostic codes should match those use by the ISD DRHS publication.  
# SMRA extraction returns all episodes within a continuous inpatient stay
# where an individual has an episode with one or more drug 
# related ICD10 diagnosis codes (in any position)
# This indicator only uses general acute admissions and no psychiatric ones (SMR04)
# Date restrictions are based on financial year of hospital discharge (not episode discharge) 
# therefore date filters in extract are set to a few 
# years before desired data to make sure to capture all CIS that end in 2002/03
# Sorting is done to replicate their methodology

# No need to include codes T402, T404, T423, T424, T436, T52 in the list below 
#  because they are only included if in the same hospital stay at least one of the 
#  ICD-10 Mental and Behavioural Disorder codes F11-F16, F18 or F19 is present.
drug_diag <- "^F1[1-689]|^T40[0135-9]" #drug-related diagnosis

drug_hosp <- as_tibble(dbGetQuery(channel, statement= paste0(
  "SELECT link_no, cis_marker, AGE_IN_YEARS age, DR_POSTCODE pc7, SEX sex_grp,
      CASE WHEN extract(month from discharge_date) > 3 THEN extract(year from discharge_date) 
        ELSE extract(year from discharge_date) -1 END as year 
  FROM ANALYSIS.SMR01_PI z
  WHERE discharge_date between  '1 April 2002' and '31 March 2024'
      AND sex <> 9 AND sex <> 0
      AND exists (
          SELECT * 
          FROM ANALYSIS.SMR01_PI  
          WHERE link_no=z.link_no and cis_marker=z.cis_marker
            AND discharge_date between '1 April 1997' and '31 March 2024'
            AND (regexp_like(main_condition, '", drug_diag ,"')
              OR regexp_like(other_condition_1,'", drug_diag ,"')
              OR regexp_like(other_condition_2,'", drug_diag ,"')
              OR regexp_like(other_condition_3,'", drug_diag ,"')
              OR regexp_like(other_condition_4,'", drug_diag ,"')
              OR regexp_like(other_condition_5,'", drug_diag ,"')))
  ORDER BY link_no, admission_date, cis_marker, discharge_date, admission, discharge, uri"))) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Group episode level drug data into hospital stays 
drug_hosp  %<>%
  group_by(link_no,cis_marker) %>%
  summarise(age=first(age), #age, sex, postcode on hospital admission
            sex_grp=first(sex_grp), 
            pc7=first(pc7), 
            year=max(year)) %>% #select last discharge episode date
  ungroup() %>%  
  create_agegroups() # Creating age groups for standardization.

# Bringing CA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds') %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  select(pc7, datazone2001, datazone2011, ca2019)

# Match geography information (datazone) to stays data
drug_hosp <- left_join(drug_hosp, postcode_lookup, "pc7") %>%
  subset(!(is.na(datazone2011)))  #select out non-scottish

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.

# Datazone2011
drug_hosp_dz11 <- drug_hosp |>
  group_by(year, datazone2011, sex_grp, age_grp) |>
  summarise(numerator = n(), .groups = "drop") |>
  rename(datazone = datazone2011)



saveRDS(drug_hosp_dz11, file.path(profiles_data_folder, 'Prepared Data/drug_stays_dz11_raw.rds'))

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD

dz01_dep <- drug_hosp |> 
  group_by(year, datazone2001, sex_grp, age_grp) |> 
  summarise(numerator = n(), .groups = "drop") |>
  rename(datazone = datazone2001) |>
  subset(year<=2013)

dep_file <- rbind(dz01_dep, drug_hosp_dz11 %>% subset(year>=2014)) #joing dz01 and dz11

saveRDS(dep_file, file.path(profiles_data_folder, 'Prepared Data/drug_stays_depr_raw.rds'))

###############################################.
# CA (council area) file for separate indicator in CYP profile for those aged 11 to 25 years
# Drugs publication publishes 0-14, 15-24, 25-34 only for Scotland
drugstays_11to25 <- drug_hosp |>
  subset(age>=11 & age<=25) |>
  group_by(year, ca2019, sex_grp, age_grp) |>  
  summarize(numerator = n(), .groups = "drop")

saveRDS(drugstays_11to25, file.path(profiles_data_folder, 'Prepared Data/drug_stays_11to25_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
##Run macros to generate HWB and Drug Profile indicator data
main_analysis(filename = "drug_stays_dz11", geography = "datazone11", measure = "stdrate", ind_id = 20205, 
              epop_age = "normal", epop_total = 200000, pop = "DZ11_pop_allages",
              time_agg = 3, year_type = "financial", yearstart = 2002, yearend = 2023)

apply_stats_disc("drug_stays_dz11_shiny") # statistical disclosure applied to final values

#Deprivation analysis function 
deprivation_analysis(filename="drug_stays_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2023, year_type = "financial", 
                    pop_sex = "all", epop_age="normal",
                    epop_total =200000, ind_id = 20205)

apply_stats_disc("drug_stays_depr_ineq")  # statistical disclosure applied to final values

###############################################.
##Run macros again to generate Drug related admissions in 11 to 25 year olds

main_analysis(filename = "drug_stays_11to25", geography = "council", measure = "stdrate", ind_id = 13025,
              pop = "CA_pop_11to25", epop_age = '11to25', epop_total = 34200,
              year_type = "financial", time_agg = 3, yearstart = 2002, yearend = 2023)

apply_stats_disc("drug_stays_11to25_shiny") # statistical disclosure applied to final values

##END