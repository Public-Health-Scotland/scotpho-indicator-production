# ScotPHO indicators: 
# Alcohol specific mortality (20204)

# files produced:
# main: Y 
# poproups: Y (sex splits)
# deprivation: Y 

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#        2a - Aggregate admissions up to datazones
#        2b - Combine dz01 and dz11 data to assign each datazone to correct SIMD quintile
#        2c - Create council-level data file for pop groups file
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function
source("//PHI_conf/ScotPHO/Profiles/Code/stat_disclosure_alcohol_stays.R") # statistical disclosure methodology - confidential - do not share

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Select alcohol specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death 
# ICD10 codes to match NRS definitions of alcohol-specific deaths (ie wholly attributable to alcohol)
# ICD10 codes reviewed and revised April 2026 against NRS publication

data_deaths <- tibble::as_tibble(dbGetQuery(channel, statement=
 "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2002' AND '31 December 2024'
        AND country_of_residence ='XS'
        AND regexp_like(underlying_cause_of_death,'E244|F10|G312|G621|G721|I426|K292|K70|K852|K860|Q860|R780|X45|X65|Y15') 
        AND age is not NULL
        AND sex <> 9")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Call age groups for standardization function
data_deaths <- data_deaths %>% create_agegroups()

# Open LA and datazone info.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2026_1.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

data_deaths <- left_join(data_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.

#2a - Aggregate deaths up to datazones

# Datazone2011
dz11 <- data_deaths %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n(), .groups = "drop") %>% rename(datazone = datazone2011)

saveRDS(dz11, file.path(profiles_data_folder, 'Prepared Data/alcohol_deaths_dz11_raw.rds'))

# Datazone2001 - used for data from 2002-2011
dz01 <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n(), .groups = "drop") %>% subset(year<2011) %>% rename(datazone = datazone2001)

saveRDS(dz01, file.path(profiles_data_folder, 'Prepared Data/alchohol_deaths_dz01_raw.rds'))

###############################################.
#2b - Create deprivation basefile

# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dz01_dep <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n(), .groups = "drop") %>% subset(year<=2013) %>% rename(datazone = datazone2001)

# Deprivation basefile
dep_file <- dz11 %>% subset(year>=2014)
dep_file <- rbind(dz01_dep, dep_file) #joining together

saveRDS(dep_file, file.path(profiles_data_folder, 'Prepared Data/alcohol_deaths_depr_raw.rds'))

###############################################.
#2c - Create popgroups basefile


# CA file for gender specific indicators in Alcohol profile
# Female alcohol mortality
#Create two files split by sex then run through analysis functions
alcohol_deaths_males <- data_deaths |> 
  filter(sex_grp == 1) |> 
  group_by(year, ca2019, age_grp, sex_grp) |> 
  summarise(numerator = n(), .groups = "drop") |> 
  rename(ca = ca2019)

saveRDS(alcohol_deaths_males, file.path(profiles_data_folder, 'Prepared Data/alcohol_deaths_males_raw.rds'))

alcohol_deaths_females <- data_deaths |> 
  filter(sex_grp == 2) |> 
  group_by(year, ca2019, age_grp, sex_grp) |> 
  summarise(numerator = n(), .groups = "drop") |> 
  rename(ca = ca2019)

saveRDS(alcohol_deaths_females, file.path(profiles_data_folder, 'Prepared Data/alcohol_deaths_females_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#Alcohol mortality indicator functions
main_analysis(filename = "alcohol_deaths_dz11", geography = "datazone11", measure = "stdrate",
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2024, time_agg = 5, 
              epop_age = "normal", epop_total = 200000, ind_id = 20204, year_type = "calendar")


###############################################.
#Alcohol mortality by deprivation indicator functions
analyze_deprivation(filename="alcohol_deaths_depr", measure="stdrate", time_agg=5, 
                    yearstart= 2002, yearend=2023,  
                    year_type = "calendar", pop = "depr_pop_allages", 
                    epop_age="normal", epop_total =200000, ind_id = 20204)

###############################################.

#Alcohol deaths in males and females
main_analysis(filename = "alcohol_deaths_males", geography = "council", measure = "stdrate",
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2024,
              time_agg = 5, epop_age = "normal", epop_total = 100000, ind_id = 20204,
              year_type = "calendar")

main_analysis(filename = "alcohol_deaths_females", geography = "council", measure = "stdrate",
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2024,
              time_agg = 5, epop_age = "normal", epop_total = 100000, ind_id = 20204,
              year_type = "calendar")


#read male and female results back in and combine with main analysis results (i.e. totals)
males <- readRDS(file.path(profiles_data_folder, "Data to be checked", "alcohol_deaths_males_shiny.rds")) |> 
  mutate(split_value = "Males")

females <- readRDS(file.path(profiles_data_folder, "Data to be checked", "alcohol_deaths_females_shiny.rds")) |> 
  mutate(split_value = "Females")

all <- readRDS(file.path(profiles_data_folder, "Data to be checked", "alcohol_deaths_dz11_shiny.rds")) |>
  mutate(split_value = "All")

# combine into one dataset
# filter on Scotland, council, board and HSCP only
# dont want to report IZ/HSC locality level sex splits as too granular
popgroups_data <- rbind(males, females, all) |>
  mutate(split_name = "Sex") |>
  filter(grepl("S00|S12|S08|S11", code))

# save file 
saveRDS(popgroups_data, file.path(profiles_data_folder, "Data to be checked", "alcohol_deaths_shiny_popgrp.rds"))
write.csv(popgroups_data, file.path(profiles_data_folder, "Data to be checked", "alcohol_deaths_shiny_popgrp.csv"), row.names = FALSE)

#finally apply disclosure
apply_stats_disc("alcohol_deaths_shiny_popgrp")

#END
