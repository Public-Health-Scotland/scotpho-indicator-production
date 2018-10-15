# ScotPHO indicators: Deaths all ages (could also be called All Cause Mortality (All Ages))

#   Part 1 - Extract data from SMRA - Deaths file.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run macros



###############################################.
## Packages/Filepaths/Functions ----
###############################################.
lapply(c("odbc", "readr", "dplyr"), library, character.only = TRUE)


server_desktop <- "server" # change depending if you are using R server or R desktop
if (server_desktop == "server") {
  prepared_data <- "/PHI_conf/ScotPHO/Profiles/Data/Prepared Data/"
} else if (server_desktop == "desktop") {
  prepared_data <- "//stats/ScotPHO/Profiles/Data/Prepared Data/"
}

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

data_deaths <- tbl_df(dbGetQuery(channel, statement=
                                   "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
                                 FROM ANALYSIS.GRO_DEATHS_C 
                                 WHERE date_of_registration between '1 January 2002' AND '31 December 2017'
                                 AND country_of_residence ='XS'
                                 AND age is not NULL
                                 AND sex <> 9")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
data_deaths <- data_deaths %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Open LA and datazone info.
# Pc 2017_2 used even though 2018 available to avoid issues with changing codes (but no diff to geographies) of fife/tayside LA/NHS board

postcode_lookup <- read_csv('/conf/linkage/output/lookups/geography/Scottish_Postcode_Directory_2017_2.csv') %>% 
  setNames(tolower(names(.)))  #variables to lower case


data_deaths <- left_join(data_deaths, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data_deaths %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(prepared_data, 'deaths_allages_dz11_raw.rds'))
datadz <- readRDS(paste0(prepared_data, 'deaths_allages_dz11_raw.rds'))

# Datazone2001. Only used for IRs
dz01 <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<2011) %>% rename(datazone = datazone2001)

saveRDS(dz01, file=paste0(prepared_data, 'deaths_allages_dz01_raw.rds'))

###############################################.
# IR basefile
ir_file <- dz11 %>% subset(year>2010)
ir_file <- rbind(dz01, ir_file) #joining together

saveRDS(ir_file, file=paste0(prepared_data, 'DZ_deaths_allages_IR_raw.rds'))

###############################################.
#Deprivation indicator numerator file

##deprivation script not yet finished so dep macro may fail.

# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
dz01_dep <- data_deaths %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

# Deprivation basefile
dep_file <- dz11 %>% subset(year>=2014)
dep_file <- rbind(dz01_dep, dep_file) #joining together

saveRDS(dep_file, file=paste0(prepared_data, 'deaths_allages_depr_raw.rds'))



###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#Deaths all ages

analyze_first(filename = "deaths_allages_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "deaths_allages_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20103, year_type = "calendar", 
               profile = "HN", min_opt = 1245385)



#############################################.
#Deprivation analysis function

analyze_deprivation(filename="deaths_allages_depr", measure="stdrate", time_agg=3, 
                    crude_rate = 1000, yearstart= 2004, yearend=2017,  
                    year_type = "calendar", pop = "depr_pop_allages", 
                    epop_age="normal", epop_total =200000, ind_id = 10101)

#END


