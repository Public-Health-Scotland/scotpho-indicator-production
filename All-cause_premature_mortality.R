# ScotPHO indicators: All-cause_premature_mortality. 

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

#Extract deaths data where: Valid sex exists, Scottish resident, Aged less than 75

deaths_all <- as_tibble(dbGetQuery(channel, statement=
                                  "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
                                FROM ANALYSIS.GRO_DEATHS_C
                                WHERE sex <> 9
                                AND country_of_residence = 'XS'
                                AND age < 75
                                AND date_of_registration between '1 January 2002' and '31 December 2020'")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.


###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.

# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

deaths_all <- left_join(deaths_all, postcode_lookup, "pc7") %>% 
  mutate_if(is.character, factor) # converting variables into factors


# Deaths all ages
# Datazone2011 basefile
deaths_all_dz11 <- deaths_all %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_all_dz11, file=paste0(data_folder, 'Prepared Data/deaths_all_prem_dz11_raw.rds'))

# Deprivation basefile
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_all_dz01 <- deaths_all %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

dep_all_file <- rbind(deaths_all_dz01, deaths_all_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_all_file, file=paste0(data_folder, 'Prepared Data/deaths_all_prem_depr_raw.rds'))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#Deprivation analysis function
analyze_deprivation(filename="deaths_all_prem_depr", measure="stdrate", time_agg = 3, 
                    yearstart= 2002, yearend=2020,   year_type = "calendar", 
                    pop = "depr_pop_under75", epop_age= "normal",
                    epop_total = 182000, ind_id = 8)


#creating csv file, prefer to do checks from that!
#prem_deathsrds <- readRDS('//PHI_conf/ScotPHO/Profiles/Data/Data to be checked/deaths_all_prem_depr_ineq.rds')

#write_csv(prem_deathsrds, '//PHI_conf/ScotPHO/Profiles/Data/Data to be checked/deaths_all_prem_depr_ineq.csv')
  
