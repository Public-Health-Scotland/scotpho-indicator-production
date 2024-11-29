# ScotPHO indicators: emergency admissions and multiple emergency admissions(+65). 
# Parts 1 and 2 take about ~20 minutes to run

#   Part 1 - Extract data from SMRA
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

# TODO
#how to deal with deprivation

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

#Function to create data for different geography levels
create_geo_levels <- function(geography, list_pos) {
  #If there are multiple admissions in one year it selects one per geography.
  # So if a patient is admitted twice but with different residence in each, it will be counted twice
  data_agg <- data_adm %>% rename(code = {{geography}} ) %>% 
    group_by(link_no, year, code) %>% 
    summarise(sex_grp = first(sex_grp), age_grp = first(age_grp), admissions = n()) %>%
    ungroup()

    #And now it aggregates total count of patients.
    data_ea <- data_agg %>% group_by(year, code, sex_grp, age_grp) %>% 
      count() %>% ungroup() %>% rename(numerator = n)
      
    data_ea_list[[list_pos]] <<- data_ea #assigning to list
    
    #select only patients who have had 2 or more admissions and 65+.
    data_ma <- data_agg %>% 
      filter(age_grp >= 14 & admissions >= 2 ) %>% 
      group_by(year, code, sex_grp, age_grp) %>% 
      count() %>% ungroup() %>% rename(numerator = n)
  
    data_ma_list[[list_pos]] <<- data_ma #assigning to list
    
}

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

sort_var <- "link_no, admission_date, discharge_date, admission, discharge, uri"
#read in SMR01 data. Following Secondary Care Team definitions.
#Excluding people with no valid sex or age.
#Only emergency or urgent admissions. Selecting one record per admission.
# Information from first episode in stay that meets criteria
data_adm <- as_tibble(dbGetQuery(channel, statement=paste0(
   "SELECT distinct link_no, cis_marker, 
            min(AGE_IN_YEARS) OVER (PARTITION BY link_no, cis_marker) age,
            FIRST_VALUE(sex) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") sex_grp,
            FIRST_VALUE(dr_postcode) OVER (PARTITION BY link_no, cis_marker 
            ORDER BY ", sort_var, ") pc7,
            max(extract(year from discharge_date)) OVER (PARTITION BY link_no, cis_marker) year, 
            min(admission_date) OVER (PARTITION BY link_no, cis_marker) doadm
   FROM ANALYSIS.SMR01_PI 
   WHERE discharge_date between '1 January 2002' and '31 December 2023'
      AND sex not in ('9', '0')
      AND AGE_IN_YEARS is not null 
      AND (admission_type between '20' and '22' or admission_type between '30' and '39') 
   ORDER BY link_no, cis_marker"))) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing geography info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, intzone2011, ca2019, hb2019, hscp2019)

geo_lookup <- readRDS(paste0(lookups, 'Geography/DataZone11_All_Geographies_Lookup.rds')) %>% 
  select(datazone2011, hscp_locality) #as locality not present in the postcode one

## Matching with geography lookups.
data_adm <- left_join(x=data_adm, y=postcode_lookup, c("pc7")) #first with postcode 

data_adm <- left_join(x=data_adm, y=geo_lookup, c("datazone2011")) %>% 
  mutate(scotland = "S00000001") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  create_agegroups() %>% # Creating age groups for standardization.
  arrange(year, link_no, doadm) %>% #sorting needed for part 2
  select(-pc7, -cis_marker, -age)

saveRDS(data_adm, paste0(data_folder, 'Prepared Data/smr01_emergency_basefile.rds'))
data_adm <- readRDS(paste0(data_folder, 'Prepared Data/smr01_emergency_basefile.rds'))

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.
#creating file for emergency admissions and multiple admissions
data_ea_list <- list() #creating empty lists for placing data created by function
data_ma_list <- list() 

# This will run the function for all those columns and for both indicators
# It takes 20-30 minutes to run
mapply(create_geo_levels, geography = c("scotland", "hb2019", "ca2019", "hscp2019",
                                        "hscp_locality", "intzone2011"), list_pos = 1:6)

data_ea <- do.call("rbind", data_ea_list) # converting from list into dataframe
data_ma <- do.call("rbind", data_ma_list) # converting from list into dataframe

saveRDS(data_ea, paste0(data_folder, 'Prepared Data/ea_raw.rds'))
saveRDS(data_ma, paste0(data_folder, 'Prepared Data/ma_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
# The function call uses a different geography to datazone11 or council as this way,
# it skips the parts of the function that bring the geographical info.
mapply(analyze_first, filename = c("ea", "ma"), geography = "all", measure = "stdrate", 
       pop = c("DZ11_pop_allages", "DZ11_pop_65+"), yearstart = 2002, yearend = 2023,
       time_agg = 3, epop_age = "normal", hscp = T)

#Emergency admissions
analyze_second(filename = "ea", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20305, year_type = "calendar")

#Multiple emergency admissions for 65+
analyze_second(filename = "ma", measure = "stdrate", time_agg = 3, 
               epop_total = 39000, ind_id = 20306, year_type = "calendar")

##END

