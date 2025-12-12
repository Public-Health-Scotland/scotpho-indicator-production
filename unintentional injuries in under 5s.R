# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 13050 - unintentional injuries in under 5s

# Data can be extracted each year following the release of unintentional injuries publication (usually around Oct/Nov): 
# https://publichealthscotland.scot/publications/unintentional-injuries/ 

# The dates in the SQL query should be updated to extract admissions up to the latest financial year that matches publication
# E.g. if publication reports up to 2024/25, data should extracted up to '31 March 2025'
# Note that part of the SQL query formats the year column so that those last 3 months of the financial year
# are converted to the starting year of the FY i.e. Jan-March data for 2025 will show as 2024 in the year column

# The 'yearend' parameter in the `main_analysis` function should then match the maximum year in the data
# i.e. if your data goes up to 2024/25 (showing as 2024 in the SQL extract) then `yearend` should be set to 2024

# If in doubt, figures can be sense checked against the injuries aged 0-4 in the open data: 
# https://www.opendata.nhs.scot/dataset/unintentional-injuries/resource/aee43295-2a13-48f6-bf05-92769ca7c6cf


#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filepaths/Functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R") # for creating 'main' indicator file


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 1 - Extract data from SMRA ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")))


# Diagnoses used for unintentional injuries, all V, W, X up to X5, and Y85 and Y86
unint_diag <- '^V|^W|^X[0-5]|^Y8[56]'


# Extract data from SMRA: one row per hospital admission for those aged under 5,
# with a valid sex recorded, and admission_type 32 on its own or 33 and 35 combined
# with a diagnosis of unintentional injury
unintentional_under5 <- tibble::as_tibble(dbGetQuery(channel, statement=paste0(
  "SELECT distinct link_no linkno, cis_marker cis,
    min(AGE_IN_YEARS) age, min(SEX) sex_grp, min(DR_POSTCODE) pc7,
    CASE WHEN extract(month from admission_date) > 3
        THEN extract(year from admission_date)
        ELSE extract(year from admission_date) -1 END as year
  FROM ANALYSIS.SMR01_PI z
  WHERE admission_date between  '1 April 2005' and '31 March 2025'
    AND sex <> 0
    AND AGE_IN_YEARS <=4
    AND CASE WHEN admission_type = '32' THEN 1
        WHEN admission_type between '33' and '35'
          AND (regexp_like(main_condition, '", unint_diag, "')
            OR regexp_like(other_condition_1, '", unint_diag, "')
            OR regexp_like(other_condition_2, '", unint_diag, "')
            OR regexp_like(other_condition_3, '", unint_diag, "')
            OR regexp_like(other_condition_4, '", unint_diag, "')
            OR regexp_like(other_condition_5, '", unint_diag, "') ) THEN 1 ELSE 0 END=1
  GROUP BY link_no, cis_marker,
    CASE WHEN extract(month from admission_date) > 3
      THEN extract(year from admission_date)
      ELSE extract(year from admission_date) -1 END" ))) %>%
  setNames(tolower(names(.)))  #variables to lower case

 #compute age group
 unintentional_under5 <- unintentional_under5 %>% 
   mutate(age_grp = 1)

 # Bringing council area info.
 postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_2.rds') %>%
   setNames(tolower(names(.))) %>%   #variables to lower case
   select(pc7, ca2019)

 # aggregate the data by council area
 unintentional_under5 <- left_join(unintentional_under5, postcode_lookup, "pc7") %>%
   subset(!(is.na(ca2019))) %>%  # exclude records with no ca2019
   group_by(year, ca2019, sex_grp, age_grp) %>%
   summarize(numerator = n()) %>% 
   ungroup()

 # save temp file to be used in analysis function
 saveRDS(unintentional_under5, file.path(profiles_data_folder, 'Prepared Data/unintentional_under5_raw.rds'))

 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Part 2 - Run analysis function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
main_analysis(filename = "unintentional_under5", ind_id = 13050, 
              geography = "council", measure = "stdrate",
              pop = "CA_pop_under5", epop_age = 'normal', epop_total = 10000, 
              yearstart = 2005, yearend = 2024, time_agg = 3, year_type = "financial")


##END