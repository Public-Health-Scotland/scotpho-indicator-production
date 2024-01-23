# LE for small areas (e.h. Intermediate Zone or HSCP locality)
# 08/10/2019

# Program generates life expectancy at birth estimates for small areas such as 2011 intermediate zone
# geogrpahies

# Uses 85+ years as maximum age group (this is different to national stats which NRS/ONS and national level analysis which use 90+ years)
# 85+ years is still required to avoid issues where no deaths or population are registered within some smaller areas
# smaller georgraphies have smaller populations and death events - 90+ population not sufficient for many IZ to produce robust estimates

# Prior to 2018 ScotPHO imputed some deaths at IZ level - non-scottish resident deaths were apportioned across all IZ
# This imputation is no longer applied after investigation suggest it makes little difference/no to final figures & increases complexity of code.

## Part 1 - Extract deaths data from ISD deaths tables
## Part 2 - Read in Scotland Populations based on 2011 datazone ISD lookup
## Part 3 - Call life expectancy function - generates aggregated raw data file, life table and life expectancy at birth (LE0) RDS ouput files 

###############################################.
## Packages/Filepaths ----
###############################################.

source("Life Expectancy Indicators/1.Functions_life_expectancy.R") #Life expectancy function where max age band is 85+ years (deliberate choice to use 85+ not 90+ because of small geographic units & small numbers)


###########################################################.
## Part 1 - Extract deaths data from NRS deaths view ----
###########################################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Select data from deaths file - adjust years to select min and max year of registration needed
# Deaths for scottish residents are coded as (XS) non-residents codes as (non-res)
data_deaths_raw <- tbl_df(dbGetQuery(channel, statement=
                                       "SELECT year_of_registration year, age, sex sex_grp, POSTCODE pc7,
        CASE WHEN country_of_residence='XS'THEN 'XS'ELSE 'nonres' END as nonres 
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE year_of_registration between 2001 AND 2021")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Check of numbers of non-residents - can compare to NRS published estimates.
table(data_deaths_raw$nonres, data_deaths_raw$year)

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
# Recode unknown to male in line with NRS custom when calculating life expectancy
data_deaths_raw <- data_deaths_raw %>%
  create_agegroups_85() %>%
  mutate(sex_grp=recode(data_deaths_raw$sex_grp,"9"="1"))

# Read in geographic reference file.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2022_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

# Join geogrpaphy lookup and deaths data.
data_deaths_raw <- left_join(data_deaths_raw, postcode_lookup, "pc7") %>% 
  select(year, age_grp, sex_grp, datazone2011, intzone2011) 

# Frequencies of deaths where no match to DZ/IZ by year -
# Check earlier years don't have 'excessive' non-matching deaths (non matching deaths are excluded from calculations leading to potential inconsistencies at different geography levels).
# Non matched should be the same for DZ and IZ
table(filter(data_deaths_raw, is.na(intzone2011))$year)
table(filter(data_deaths_raw, is.na(datazone2011))$year)

## Prepare IZ level deaths file 
#  Aggregate IZ level deaths by year, age, sex for scottish residents only 
data_deaths_iz <- data_deaths_raw %>%
  subset(!(is.na(intzone2011))) %>%  #exclude non scottish residents
  group_by(year,age_grp,sex_grp,intzone2011) %>%
  summarise(deaths=n()) %>%
  ungroup() %>%
  rename(geography=intzone2011)

## Prepare HSCP Locality deaths file

# Read in dz to localities and izs lookup
dz_geo_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")

# Join deaths data to DZ to Partnership geogrpaphy lookup.
data_deaths_locality <- left_join(data_deaths_raw, dz_geo_lookup, by ="datazone2011") 

data_deaths_locality <- data_deaths_locality %>%
  subset(!(is.na(hscp_locality))) %>%  #exclude non scottish residents
  group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(deaths=n()) %>%
  ungroup() %>%
  rename(geography=hscp_locality)

# Add HSCP locality and IZ deaths files
data_deaths_all <- bind_rows(data_deaths_iz, data_deaths_locality)

# Check deaths totals are the same for each geography type
xtabs(deaths~year+(substr(geography,1,3)), data_deaths_all)

# Save deaths file
saveRDS(data_deaths_all, file=paste0(temp_network,'data_deaths.rds'))

rm(data_deaths_raw, data_deaths_iz,data_deaths_locality) #optional cleaning 

###############################################.
## Part 2 - Read in Scotland Populations from ISD lookup ----
###############################################.

# Read in small area population lookup based on 2011 datazones for years prior to 2011.
# NRS back calculated these populations
data_pop1 <- readRDS(paste0(cl_out_pop,"DataZone2011_pop_est_2001_2010.rds"))%>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2001) %>%
  mutate(sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, datazone2011name))

# Read in small area population lookup, select required data and recode to permit matching.
data_pop2 <- readRDS(paste0(cl_out_pop,"DataZone2011_pop_est_2011_2021.rds")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2011) %>%
  mutate(sex = case_when(sex=="m"~"M", sex=="f"~"F", TRUE~as.character(sex)),
         sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, datazone2011name))

data_pop <- bind_rows(data_pop1,data_pop2) %>%
  select(year, datazone2011,age0:sex_grp)

rm(data_pop1,data_pop2)

# Reshape data to long format
data_pop <- data_pop %>% 
  pivot_longer(
    cols=starts_with("age"),
    names_to="age",
    names_prefix = "age",
    values_to="pop")

#Converting pop & age to numeric
#data_pop$pop <- as.numeric(data_pop$pop)
data_pop$age <- as.numeric(gsub("age|plus", "", data_pop$age)) #remove prefix/suffix found in age fields

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
data_pop <- data_pop %>%
  create_agegroups_85() %>%
  group_by (year,sex_grp, age_grp, datazone2011) %>%
  summarise(pop=sum(pop)) %>%
  ungroup()

# Read in dz to localities and izs lookup
dz_geo_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")

# Join IZ pops data to Partnership geogrpaphy lookup.
data_pop <- left_join(data_pop, dz_geo_lookup, "datazone2011") 

# Aggregate populations for hscp localities
data_pop_locality <- data_pop %>%
  group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(geography=hscp_locality)

# Aggregate populations for hscp localities
data_pop_iz <- data_pop %>%
  group_by(year,age_grp,sex_grp,intzone2011) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(geography=intzone2011)

# Add HSCP locality and IZ pops files
data_pop_all <- bind_rows(data_pop_iz, data_pop_locality)

# Check pops totals are the same for each geography type
xtabs(pop~year+(substr(geography,1,3)), data_pop_all)

# Save population file
saveRDS(data_pop_all, file=paste0(temp_network,'data_pop.rds'))

rm(postcode_lookup, data_pop, data_pop_iz, data_pop_locality, dz_geo_lookup) #optional cleaning

##########################################################################################.
## Part 3 - Generating Life Expectancy Estimates ----
##########################################################################################.
# Running part 1 & 2 of this program generate a population & death file that are picked up & joined in the macro
# Call life expectancy function 

# Parameters:

# max_agegrp - Set to max age group used in calculating LE 85 or 90.
# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of existing network where output files are saved to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)

LE_function(max_agegrp=85,run_name="2001to2021 IZ&Locality LE(85+)_20210927",fp_deaths="data_deaths", 
            fp_pop="data_pop", fp_output="4_Intermediate Zone LE (annual)",
            yearstart=2001, yearend=2021, time_agg=5)


# Function generates 3 output RDS files than can be used for checking or analysis
# Once function runs these should appear in local environment
# all_data: all_data should be raw population and deaths data files added priort to calculations
# final_lifetable : lifetable contains invidual age groups plus all stages of calculation
# final_le0_data : le0 just a summary of life expectancy at birth data.



##END