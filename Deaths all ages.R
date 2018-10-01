# ScotPHO indicators: Deaths all ages

#   Part 1 - Extract data from SMRA.
#   Part 2 -  Create the different geographies basefiles
#   Part 3 - Run macros

library(odbc) #load first or packages seem to conflict
library(haven) #for SPPS file reading

library(dplyr)
library(readr)


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

server_desktop <- "server" # change depending if you are using R server or R desktop

server_desktop <- "server" # change depending if you are using R server or R desktop
if (server_desktop == "server") {
  prepared_data <- "/PHI_conf/ScotPHO/Profiles/Data/Prepared Data/"
  functions <- "/PHI_conf/ScotPHO/Profiles/Data/2. Functions code/"
} else if (server_desktop == "desktop") {
  prepared_data <- "//stats/ScotPHO/Profiles/Data/Prepared Data/"
  functions <- "//stats/ScotPHO/Profiles/Data/2. Functions code/"
}

source(paste0(functions, "function_analysis.R")) #Normal indicator functions


###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# # SMRA login information
# channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
#        id=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))
# 
#  
# #Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year. 
# 
# data <- tbl_df(dbGetQuery(channel, statement=
#                             "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
#    FROM ANALYSIS.GRO_DEATHS_C z
#    where date_of_registration between '2002-01-01' and '2017-12-31'
#    AND regexp_like(country_of_residence, 'XS')      
#    AND sex <> 9
#    AND age is not NULL")) %>% 
#   setNames(tolower(names(.))  #variables to lower case
           
##can't access SMRA tables yet so data coming from spss extract
data<- read_csv("/conf/phip/Projects/Profiles/Data/Indicators/Deaths, Injury and Disease/Raw Data/Prepared Data/deaths_allages_smraextract.csv")%>%
  setNames(tolower(names(.))) %>% #variables to lower case
  mutate_if(is.character, factor) # converting variables into factors
#  mutate_all(factor) # converting variables into factors
data$sex_grp <- as.factor(data$sex_grp) #reading in data needed to cheat to get factors correct
  
  
# Creating age groups for standardization.
data <- data %>% mutate(age_grp = case_when( 
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


data <- left_join(data, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create denominator files for the different geographies basefiles ----
###############################################.
###############################################.
# Datazone2011
dz11 <- data %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(dz11, file=paste0(prepared_data, 'deaths_allages_dz11_raw.rds'))
datadz <- readRDS(paste0(prepared_data, 'deaths_allages_dz11_raw.rds'))

###############################################.
# # CA file for under 16 cases 
# ca_under16 <- data %>% subset(age<16) %>% group_by(year, ca2011, sex_grp, age_grp) %>%  
#   summarize(numerator = n()) %>% ungroup() %>%   rename(ca = ca2011)
# 
# saveRDS(ca_under16, file=paste0(prepared_data, 'asthma_under16_raw.rds'))

###############################################.
# Datazone2001. Only used for IRs
dz01 <- data %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<2011) %>% rename(datazone = datazone2001)

saveRDS(dz01, file=paste0(prepared_data, 'deaths_allages_dz01_raw.rds'))

###############################################.
# IR basefile
ir_file <- dz11 %>% subset(year>2010)
ir_file <- rbind(dz01, ir_file) #joining together

saveRDS(ir_file, file=paste0(prepared_data, 'DZ_deaths_allages_IR_raw.rds'))


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
