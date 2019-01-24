# ScotPHO indicators: emergency admissions and multiple emergency admissions(+65). 
# Parts 1 and 2 take about ~20 minutes to run

#   Part 1 - Extract data from SMRA
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

# TODO
#NEED to be checked
#how to deal with deprivation

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(odbc) #for reading oracle databases

server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

#Function to create data for different geography levels
create_geo_levels <- function(geography, type) {
  #If there are multiple admissions in one year it selects one.
  data_agg <- data_adm %>% rename_(code = geography) %>% #renames using NSE
    arrange(year, link_no, doadm) %>% 
    group_by(link_no, year, code) %>% 
    summarise(sex_grp = first(sex_grp), age_grp = first(age_grp), admissions = n()) %>%
    ungroup()

  if (type == "ea") { #if emergency admissions just count
    #And now it aggregates total count of patients.
    data_agg <- data_agg %>% group_by(year, code, sex_grp, age_grp) %>% 
      count() %>% ungroup() %>% rename(numerator = n)
      
  } else if (type == "ma") {
    #select only patients who have had 2 or more admissions and 65+.
    data_agg <- data_agg %>% 
      filter(age_grp >= 14 & admissions >= 2 ) %>% 
      group_by(year, code, sex_grp, age_grp) %>% 
      count() %>% ungroup() %>% rename(numerator = n)
  }
}

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

#read in SMR01 data. Following Secondary Care Team definitions.
#people with no valid sex or age.
#Only emergency or urgent admissions. Selecting one record per admission.
data_adm <- tbl_df(dbGetQuery(channel, statement=
   "SELECT distinct link_no, cis_marker, min(AGE_IN_YEARS) age, min(SEX) sex_grp, 
      min(dr_postcode) pc7, max(extract(year from discharge_date)) year,
      min(admission_date) doadm
   FROM ANALYSIS.SMR01_PI 
   WHERE discharge_date between '1 January 2002' and '31 December 2017'
      AND sex not in ('9', '0')
      AND AGE_IN_YEARS is not null 
      AND (admission_type between '20' and '22' or admission_type between '30' and '40') 
   GROUP BY link_no, cis_marker
   ORDER BY link_no, cis_marker, min(admission_date) ")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing geography info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2018_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, intzone2011, ca2011, hb2014, hscp2016)

geo_lookup <- readRDS(paste0(lookups, 'Geography/DataZone11_All_Geographies_Lookup.rds')) %>% 
  select(datazone2011, hscp_locality) #as locality not present in the postcode one

## Matching with geography lookups.
data_adm <- left_join(x=data_adm, y=postcode_lookup, c("pc7")) #first with postcode 

data_adm <- left_join(x=data_adm, y=geo_lookup, c("datazone2011")) %>% 
  mutate(scotland = "S00000001") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  # Creating age groups for standardization.
  mutate(age_grp = case_when( 
    age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
    age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
    age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
    age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
    age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
    age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
    TRUE ~ as.numeric(age))) %>% 
  arrange(year, link_no, doadm) %>% #sorting needed for part 2
  select(-pc7, -cis_marker, -age)

saveRDS(data_adm, paste0(data_folder, 'Prepared Data/smr01_emergency_basefile.rds'))
data_adm <- readRDS(paste0(data_folder, 'Prepared Data/smr01_emergency_basefile.rds'))

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
#creating file for emergency admissions
data_ea <- rbind(create_geo_levels(geography = "scotland", type = "ea"), 
  create_geo_levels(geography = "hb2014", type = "ea"),
  create_geo_levels(geography = "ca2011", type = "ea"), 
  create_geo_levels(geography = "hscp2016", type = "ea"),
  create_geo_levels(geography = "hscp_locality", type = "ea"), 
  create_geo_levels(geography = "intzone2011", type = "ea")
)

saveRDS(data_ea, paste0(data_folder, 'Prepared Data/ea_raw.rds'))
data_ea <- readRDS(paste0(data_folder, 'Prepared Data/ea_raw.rds'))

#creating file for ea deprivation
# data_ea_depr <- data_adm %>% mutate(datazone = case_when(year <= 2013 ~ datazone2001,
#                                                          year >= 2014 ~ datazone2011))

###############################################.
#creating file for multiple admissions
data_ma <- rbind(create_geo_levels(geography = "scotland", type = "ma"), 
  create_geo_levels(geography = "hb2014", type = "ma"),
  create_geo_levels(geography = "ca2011", type = "ma"), 
  create_geo_levels(geography = "hscp2016", type = "ma"),
  create_geo_levels(geography = "hscp_locality", type = "ma"), 
  create_geo_levels(geography = "intzone2011", type = "ma")
)

saveRDS(data_ma, paste0(data_folder, 'Prepared Data/ma_raw.rds'))

#creating file for ma deprivation
# data_ma_depr <- data_ma %>% mutate(datazone = case_when(year <= 2013 ~ datazone2001,
#                                                          year >= 2014 ~ datazone2011))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
# The function call uses a different geogrpahy to datazone11 or council as this way,
# it skips the parts of the function that bring the geographical info.
#Emergency admissions
analyze_first(filename = "ea", geography = "all", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "ea", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20305, year_type = "calendar", 
               profile = "HN", min_opt = 2999)

#Deprivation analysis function
# analyze_deprivation(filename="ea_depr", measure="stdrate", time_agg=3, 
#                     yearstart= 2002, yearend=2017,   year_type = "calendar", 
#                     pop = "depr_pop_allages", epop_age="normal",
#                     epop_total =200000, ind_id = 20305)

###############################################.
#Multiple emergency admissions for 65+
analyze_first(filename = "ma", geography = "all", measure = "stdrate", 
              pop = "DZ11_pop_65+", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "ma", measure = "stdrate", time_agg = 3, 
               epop_total = 39000, ind_id = 20306, year_type = "calendar", 
               profile = "HN", min_opt = 2999)

#Deprivation analysis function
# analyze_deprivation(filename="ma_depr", measure="stdrate", time_agg=3, 
#                     yearstart= 2002, yearend=2017,   year_type = "calendar", 
#                     pop = "depr_pop_allages", epop_age="normal",
#                     epop_total =39000, ind_id = 20306)

##END

