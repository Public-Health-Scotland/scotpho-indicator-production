# ScotPHO indicators: All-cause mortality among 15 to 44 years 

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))
#select data from deaths file
#select only deaths for scottish residents (XS)
#exclude any with null age group
#exclude deaths where sex is unknown (9)

data_deaths_15to44 <- tbl_df(dbGetQuery(channel, statement=
                                          "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
                                 FROM ANALYSIS.GRO_DEATHS_C 
                                 WHERE date_of_registration between '1 January 2002' AND '31 December 2018'
                                 AND country_of_residence ='XS'
                                 AND age >=15 AND age<=44
                                 AND sex <> 9")) %>%
  setNames(tolower(names(.)))  #variables to lower case


# Creating age groups for standardization (this indicator is for 15-44 year old so only groups 4 to 9 should be present)
data_deaths_15to44 <- data_deaths_15to44 %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Open LA and datazone info.
postcode_lookup <- read_csv('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.csv') %>% 
  setNames(tolower(names(.)))  #variables to lower case

data_deaths_15to44 <- left_join(data_deaths_15to44, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data_deaths_15to44 %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/deaths_15to44_dz11_raw.rds'))
datadz <- readRDS(paste0(data_folder, 'Prepared Data/deaths_15to44_dz11_raw.rds'))

# Datazone2001. Only used for IRs
dz01 <- data_deaths_15to44 %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<2011) %>% rename(datazone = datazone2001)

saveRDS(dz01, file=paste0(data_folder, 'Prepared Data/deaths_15to44_dz01_raw.rds'))

###############################################.
#Deprivation indicator numerator file

# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dz01_dep <- data_deaths_15to44 %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

# Deprivation basefile
dep_file <- dz11 %>% subset(year>=2014)
dep_file <- rbind(dz01_dep, dep_file) #joining together

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/deaths_15to44_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#Deaths ages 15 to 44 ages
#epop_age can set to normal even though a subset of whole pop (since only matches on pop which are present in file)

analyze_first(filename = "deaths_15to44_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_15to44", yearstart = 2002, yearend = 2018,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "deaths_15to44_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 76000, ind_id = 20104, year_type = "calendar")

#############################################.
#Deprivation analysis function

analyze_deprivation(filename="deaths_15to44_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2018,  
                    year_type = "calendar", pop = "depr_pop_15to44", 
                    epop_age="normal", epop_total =200000, ind_id = 20104)


#END
