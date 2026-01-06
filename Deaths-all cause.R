# Deaths - all cause.R (previously named Deaths all ages -15to44.R)

# This script generates multiple ScotPHO indicators based deaths from any cause (AKA all cause mortality)
# The profiles tool deliberately doesn't label these indicators as mortality as this is felt not to be as accessible to a lay audience
# Some of these deaths indicators include a deprivation analysis but this is not done for under 1 and 1 to 15 as the numerators are unlikely to be suffient for robust estimates.

# Deaths all ages 
# Deaths (under 1 year) (the live_births lookup (in scotpho-lookups repo) needs updated before this can be run using data requested from NRS)
# Deaths (1 to 15 years)
# Deaths (ages 15-44)
# Deaths (under 75 years) - Prior to Jan 2023 this was only available in inequalities module as was labeled 'premature all cause mortality'. 
#                           Methodology now in line with other inequalities indicators and indicator in both main & inequalities modules.

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
source("functions/deprivation_analysis.R") # source functions & libraries to run script

# calls to old function scripts which can be removed once satisfied new function scripts are working
#source("1.indicator_analysis.R") #Normal indicator functions
#source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

#Extracting deaths data for scottish residents (XS)
#exclude any with null age group and where sex is unknown (9)
data_deaths <- as_tibble(dbGetQuery(channel, statement=
 "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7, COUNCIL_AREA_2019 ca
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2002' AND '31 December 2024'
        AND country_of_residence ='XS'
        AND age is not NULL")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Open LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_2.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case

data_deaths <- left_join(data_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca) %>% 
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.

###############################################.
# Deaths all ages
# Datazone2011 basefile
deaths_all_dz11 <- data_deaths %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_all_dz11, file=paste0(profiles_data_folder, '/Prepared Data/deaths_allages_dz11_raw.rds'))

# Deprivation basefile
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_all_dz01 <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

dep_all_file <- rbind(deaths_all_dz01, deaths_all_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_all_file, file=paste0(profiles_data_folder, '/Prepared Data/deaths_allages_depr_raw.rds'))


###############################################.
# Deaths under 1
deaths_under1 <- data_deaths %>% 
  filter(age<1) %>% 
  group_by(year, ca) %>%
  summarize(numerator = n()) %>% ungroup() |>
  rename(ca2019=ca)

saveRDS(deaths_under1, file=paste0(profiles_data_folder, '/Prepared Data/deaths_under1_raw.rds'))


###############################################.
# Deaths aged 1-15
deaths_1to15 <- data_deaths %>% 
  filter(between(age, 1, 15)) %>% 
  group_by(year, ca) %>%
  summarize(numerator = n()) %>% ungroup()

saveRDS(deaths_1to15, file=paste0(profiles_data_folder, '/Prepared Data/deaths_1to15_raw.rds'))


###############################################.
# Deaths aged 15-44
# Datazone2011 basefile
deaths_1544_dz11 <- data_deaths %>% 
  filter(between(age, 15, 44)) %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_1544_dz11, file=paste0(profiles_data_folder, '/Prepared Data/deaths_15to44_dz11_raw.rds'))

# Deprivation basefile (15-44 years)
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_1544_dz01 <- data_deaths %>% 
  filter(between(age, 15, 44)) %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

dep_1544_file <- rbind(deaths_1544_dz01, deaths_1544_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_1544_file, file=paste0(profiles_data_folder, '/Prepared Data/deaths_15to44_depr_raw.rds'))


###############################################.
# Deaths under 75
deaths_under75_dz11 <- data_deaths %>% 
  filter(age<75) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_under75_dz11, file=paste0(profiles_data_folder, '/Prepared Data/deaths_under75_dz11_raw.rds'))

# Deprivation basefile (under 75 years)
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_under75_dz01 <- data_deaths %>% 
  filter(age<75) %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

dep_under75_file <- rbind(deaths_under75_dz01, deaths_under75_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_under75_file, file=paste0(profiles_data_folder, '/Prepared Data/deaths_under75_depr_raw.rds'))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.

# N.B. Any indicator using the DZ or SIMD population files limited to 2023 as at Oct 2025, until 2024 population data are available.

###############################################.
#Deaths all ages

#call main analysis function 
main_analysis(filename = "deaths_allages_dz11", #max year 2023 
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",  
              ind_id = 20103, 
              time_agg = 3,  
              yearstart = 2002,   
              yearend = 2023, 
              pop = "DZ11_pop_allages",
              epop_total = 200000,
              epop_age = "normal",
              test_file = FALSE, 
              QA = TRUE)


#Deprivation analysis function
deprivation_analysis(filename="deaths_allages_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2023,  
                    year_type = "calendar",  
                    pop_sex = "all",
                    pop_age = NULL,
                    epop_age="normal", epop_total =200000, ind_id = 20103,
                    QA = TRUE, test_file = FALSE)
                                 
  

###############################################.
# Deaths aged 1-15

#call main analysis function 
main_analysis(filename = "deaths_1to15",
              measure = "crude",
              geography = "council",
              year_type = "calendar",  
              ind_id = 13034, 
              time_agg = 5,  
              yearstart = 2002,   
              yearend = 2024, 
              pop = "CA_pop_1to15",
              crude_rate = 100000,
              test_file = FALSE, 
              QA = TRUE)

###############################################.
# Deaths under 1 (indicator name : Infant deaths, aged 0-1 years)
# no deprivation split for this indicator as figures too small

#call main analysis function 
main_analysis(filename = "deaths_under1",  measure = "crude",
              geography = "council",  year_type = "calendar",  ind_id = 13026, 
              time_agg = 5,  yearstart = 2002,   yearend = 2024, pop = "live_births", 
              crude_rate = 1000, # rate is crude rate per 1000
              test_file = FALSE, QA = TRUE)

#remove ADPs as these are not in the live births lookup (alternatively: amend the lookup)
deaths_under1 <- main_analysis_result %>%
  filter(!(substr(code, 1, 3)=="S11"))

# save the data as both an RDS and CSV file
saveRDS(deaths_under1, paste0(output_folder, "/deaths_under1_shiny.rds"))
write.csv(deaths_under1, paste0(output_folder, "/deaths_under1_shiny.csv"), row.names = FALSE)

# run QA again
run_qa(type = "main", filename="deaths_under1",test_file=FALSE)

rm(postcode_lookup)

###############################################.
# Deaths 15-44
#epop_age can set to normal even though a subset of whole pop (since only matches on pop which are present in file)

#call main analysis function 
main_analysis(filename = "deaths_15to44_dz11", #max year 2023 
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",  
              ind_id = 20104, 
              time_agg = 3,  
              yearstart = 2002,   
              yearend = 2023, 
              pop = "DZ11_pop_15to44",
              epop_total = 76000,
              epop_age = "normal",
              test_file = FALSE, 
              QA = TRUE)


#Deprivation analysis function
deprivation_analysis(filename="deaths_15to44_depr", measure="stdrate", time_agg=3, 
                     yearstart= 2002, yearend=2023,  
                     year_type = "calendar",  
                     pop_sex = "all",
                     pop_age = c(15, 44),
                     epop_age="normal", epop_total =76000, ind_id = 20104,
                     QA = TRUE, test_file = FALSE)



###############################################.
# Deaths under 75 
#epop_age can set to normal even though a subset of whole pop (since only matches on pop which are present in file)


#call main analysis function 
main_analysis(filename = "deaths_under75_dz11", #max year 2023 
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",  
              ind_id = 8, 
              time_agg = 3,  
              yearstart = 2002,   
              yearend = 2023, 
              pop = "DZ11_pop_under75",
              epop_total = 182000,
              epop_age = "normal",
              test_file = FALSE, 
              QA = TRUE)


#Deprivation analysis function
deprivation_analysis(filename="deaths_under75_depr", measure="stdrate", time_agg=3, 
                     yearstart= 2002, yearend=2023,  
                     year_type = "calendar",  
                     pop_sex = "all",
                     pop_age = c(0,74),
                     epop_age="normal", epop_total =182000, ind_id = 8,
                     QA = TRUE, test_file = FALSE)





#END
