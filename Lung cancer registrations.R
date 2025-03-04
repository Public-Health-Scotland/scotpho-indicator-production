# ScotPHO indicators: Lung cancer registrations

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# Extracting data on people over 16, with a diagnosis of lung cancer (ICD10 codes C33-C34) 
# and excluding records with unknown sex .
# It counts tumours, not different patients, e.g. a patient can have several tumours over the years.
# If we were to use SMRA geographical information, the code could be simplified.
lung_reg <- as_tibble(dbGetQuery(channel, statement=
  "SELECT count(*) count, extract (year from INCIDENCE_DATE) year, SEX sex_grp, 
      POSTCODE pc7, age_in_years age 
   FROM ANALYSIS.SMR06_PI
   WHERE incidence_date between '1 January 2002' and '31 December 2022'
      AND age_in_years>=16 
      AND regexp_like(ICD10S_CANCER_SITE, 'C3[34]')
      AND sex <> 9
  GROUP BY extract (year from incidence_date), sex, postcode, age_in_years")) |>  
  janitor::clean_names() |>  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Bringing  LA info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.rds')  |>  
  clean_names() |>  select(pc7, ca2019) #variables to lower case

lung_reg <- left_join(lung_reg, postcode_lookup, by = "pc7")  |>  #merging with lookup
  # aggregating by council area
  group_by(year, ca2019, sex_grp, age_grp) |> summarize(numerator = sum(count)) |> 
  ungroup() |> rename(ca = ca2019) |>  
  filter(!is.na(ca)) # excluding non-Scottish residents

saveRDS(lung_reg, file=paste0(data_folder, 'Prepared Data/lungcancer_reg_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "lungcancer_reg", measure = "stdrate", geography = "council",
              year_type = "calendar", ind_id = 1549, time_agg = 3, yearstart = 2002, 
              yearend = 2022, pop = "CA_pop_16+", epop_total = 165800, epop_age = "16+")

##END