# ~~~~~~~~~~~~~~~~~~~~~~~
# ---- Analyst Notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Indicators:
# - Drug-related deaths (id = 4121)
# - Drug-related deaths, males (id = 12534)
# - Drug-related deaths, females (id = 12535)

# Description:
# (id = 4121) Number of drug-related deaths: actual number and european age sex standardised rate per 100,000 population, single years.
# (id = 12534) Number of drug-related deaths for males: 5-year rolling average and directly age standardised rate per 100,000 male population.
# (id = 12535) Number of drug-related deaths for females: 5-year rolling average and directly age standardised rate per 100,000 female population.

# Data source: National Records of Scotland (NRS)
# NRS drug-related deaths (DRD) file supplied by PHS Drugs Team (phs.drugsteam@phs.scot).
# (Not to be confused with the PHS National Drug-Related Deaths Database (NDRDD) publication, also produced by the PHS Drugs Team)
# SMRA deaths file is then matched to the NRS DRD file.

# PART 1 - Read in received data

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries, functions and filepaths ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R") # Brings in the main_analysis() function
source("functions/deprivation_analysis.R") # Brings in the deprivation_analysis() function

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PART 1 - Read in received data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in files with DRD data for 2002 to 2005 (taken from previous profiles updates) as drugs team only provide NRS data from 2006 onwards
# Data file for 2002-2005 - All
drug_deaths_02_05 <- read.csv(paste0(profiles_data_folder, "/Received Data/drugs_deaths_2002-05_DO_NOT_DELETE.csv")) |>
  clean_names()
# Data file for 2002-2005 - Females
drug_deaths_female_02_05 <- read.csv(paste0(profiles_data_folder, "/Received Data/drugs_deaths_female_DO_NOT_DELETE.csv")) |>
  clean_names()
# Data file for 2002-2005 - Males
drug_deaths_male_02_05 <- read.csv(paste0(profiles_data_folder, "/Received Data/drugs_deaths_male_DO_NOT_DELETE.csv")) |>
  clean_names()

# Read in NRS DRD file supplied by PHS Drugs Team.
drug_deaths_NRS <- readRDS(paste0(profiles_data_folder, "/Received Data/Drug-related deaths (4121)/nrs_drugrelateddeaths_2006-24_IR2025-00814.rds")) |>
  clean_names()

# Extract all deaths from SMRA from 2006 onwards and then match SMRA deaths details with the death records in the NRS DRD file provided
# by the drugs team.

# SMRA login - If you don't already have it, you will need to request access to the smr gro deaths analysis view on SMRA
# (email Clare.Campbell3@phs.scot for access).
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# SQL query to extract ALL deaths from SMRA from 2006 onwards extracting specified variables only, excluding those records where
# age is null and sex is unknown (9).
# (ICD10 codes have been removed from the SQL query as it was not finding all deaths in the NRS extract - Sep 21)
drug_deaths_smr <- as_tibble(dbGetQuery(channel, statement=
  "SELECT year_of_registration year, 
  age, 
  SEX sex_grp, 
  UNDERLYING_CAUSE_OF_DEATH cod1, 
  POSTCODE pc7, 
  REGISTRATION_DISTRICT rdno, 
  ENTRY_NUMBER entry_no, 
  PLACE_OF_DEATH_POSTCODE pc_death
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2006' and '31 December 2024' 
  AND age is not NULL 
  AND sex <> 9")) |>
  clean_names() #variable names to lower case

# Create a key variable in SMR extract (rdno_entry_no_yr) so can join to NRS DRD file.

# Add padding to the entry_no variable so it matches NRS format.
drug_deaths_smr <- drug_deaths_smr |> 
  mutate(entry_no = case_when(nchar(entry_no) == 2 ~ paste0("0", entry_no),
                              nchar(entry_no) == 1 ~ paste0("00", entry_no),
                              TRUE ~paste0(entry_no)))
# Create rdno_entry_no_yr key variable in SMR extract by concatenating relevant existing variables.
drug_deaths_smr <- drug_deaths_smr |> 
  mutate(rdno_entry_no_yr = paste0(rdno, entry_no, year))

# Create age_grp variable with 5 yr age-bands from age variable.
drug_deaths_smr <- drug_deaths_smr |>
  create_agegroups()

# Using drug_deaths_NRS as base file, join to this variables from drug_deaths_smr matching on rdno_entry_no_yr as key
drug_deaths <- left_join(drug_deaths_NRS, drug_deaths_smr, "rdno_entry_no_yr")

# If postcode is NA (missing) then use place of death postcode (added to ensure HB/CA breakdown matches NRS publication).
drug_deaths <- drug_deaths |> 
  mutate(pc7 = case_when(is.na(pc7) ~ pc_death, 
                         TRUE ~ pc7))

# Checking totals match with NRS publication.
drug_deaths |> group_by(year) |> count() |> View()