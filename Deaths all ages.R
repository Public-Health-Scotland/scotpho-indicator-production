# ScotPHO indicators: 2 indicator outputs from this script
#   Deaths all ages (could also be called All Cause Mortality (All Ages))
#   Deaths all ages inequalities indicator

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run macros

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

#select data from deaths file
#select only deaths for scottish residents (XS)
#exclude any with null age group
#exclude deaths where sex is unknown (9)

data_deaths <- tbl_df(dbGetQuery(channel, statement=
 "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2002' AND '31 December 2019'
        AND country_of_residence ='XS'
        AND age is not NULL
        AND sex <> 9")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

# Open LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_2.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case

data_deaths <- left_join(data_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011 basefile
deaths_dz11 <- data_deaths %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(deaths_dz11, file=paste0(data_folder, 'Prepared Data/deaths_allages_dz11_raw.rds'))

# Deprivation basefile
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
deaths_dz01 <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

dep_file <- rbind(deaths_dz01, deaths_dz11 %>% subset(year>=2014)) #join dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/deaths_allages_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#Deaths all ages

analyze_first(filename = "deaths_allages_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2019,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "deaths_allages_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20103, year_type = "calendar")

#############################################.
#Deprivation analysis function

analyze_deprivation(filename="deaths_allages_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2019,  
                    year_type = "calendar", pop = "depr_pop_allages", 
                    epop_age="normal", epop_total =200000, ind_id = 20103)

#END
