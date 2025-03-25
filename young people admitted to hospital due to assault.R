# ScotPHO indicators: Young people admitted to hospital due to assault

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extracts one row per admission of people between 15 and 25 with known sex
# and a diagnosis of assault
#1 subtracted from year for admissions in Jan-March to get first year of financial year (line 29)
young_assault <- as_tibble(dbGetQuery(channel, statement=
  "SELECT distinct link_no linkno, cis_marker cis, min(AGE_IN_YEARS) age, 
      min(SEX) sex_grp, min(DR_POSTCODE) pc7,
      min(CASE WHEN extract(month from admission_date) > 3 
        THEN extract(year from admission_date)
        ELSE extract(year from admission_date) -1 END) as year
  FROM ANALYSIS.SMR01_PI z 
  WHERE admission_date between '1 April 2005' and '31 March 2024' 
   AND sex <> 0 
   AND (main_condition between 'X850' and 'Y099' 
      or other_condition_1 between 'X850' and 'Y099'  
      or other_condition_2 between 'X850' and 'Y099'  
      or other_condition_3 between 'X850' and 'Y099' 
      or other_condition_4 between 'X850' and 'Y099'  
      or other_condition_5 between 'X850' and 'Y099')
   AND AGE_IN_YEARS between 15 and 25
   GROUP BY link_no, cis_marker" )) |> 
  janitor::clean_names() |>   #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing council area info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds') |>  
  clean_names() |>    #variables to lower case
  select(pc7, ca2019)

# aggregate the data with council area info
young_assault <- left_join(young_assault, postcode_lookup, "pc7") |>  
  subset(!(is.na(ca2019))) |>   # exclude records with no ca2011 
  mutate_if(is.character, factor) |>   # converting variables into factors
# group and aggregate by year, ca2011, sex, age
  group_by(year, ca2019, sex_grp, age_grp) |>  
  summarize(numerator = n()) |> ungroup() |>  rename(ca = ca2019)

saveRDS(young_assault, file=paste0(profiles_data_folder, '/Prepared Data/youngassault_ca2019_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "youngassault_ca2019", geography = "council", measure = "stdrate",
              year_type = "financial", ind_id = 13049, time_agg = 3, yearstart = 2005,
              yearend = 2023, pop = "CA_pop_15to25", epop_total = 25400, epop_age = "15to25")


##END