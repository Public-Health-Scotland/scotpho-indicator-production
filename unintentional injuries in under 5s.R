#Syntax to produce file for Children & Young People Profiles 
# Indicators: unintentional injuries in under 5s

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

# Extract data from SMRA
unintentional_under5 <- tbl_df(dbGetQuery(channel, statement=
 "SELECT distinct link_no linkno, cis_marker cis, min(admission_type), min(AGE_IN_YEARS) age, min(SEX) sex, min(DR_POSTCODE) pc7,
  CASE WHEN extract(month from admission_date) > 3 
  THEN extract(year from admission_date) 
    ELSE extract(year from admission_date) -1 END as year
  FROM ANALYSIS.SMR01_PI z
    WHERE admission_date between  '1 April 2005' and '31 March 2018'
    AND sex <> 0
    AND AGE_IN_YEARS <=4
    AND (admission_type = '32'
      OR (admission_type between '33' and '35'
    AND ((main_condition between 'V010' and 'X599' or other_condition_1 between 'V010' and 'X599'  or other_condition_2 between 'V010' and 'X599'  or other_condition_3 between 'V010' and 'X599' 
      OR other_condition_4 between 'V010' and 'X599'  or other_condition_5 between 'V010' and 'X599') 
      OR (main_condition between 'Y850' and 'Y869' or other_condition_1 between 'Y850' and 'Y869'  or other_condition_2 between 'Y850' and 'Y869'  or other_condition_3 between 'Y850' and 'Y869' 
      OR other_condition_4 between 'Y850' and 'Y869'  or other_condition_5 between 'Y850' and 'Y869'))))
  GROUP BY link_no, cis_marker,
  CASE WHEN extract(month from admission_date) > 3 
  THEN extract(year from admission_date) 
  ELSE extract(year from admission_date) -1 END" )) %>% 
 setNames(tolower(names(.)))  #variables to lower case
 
 #compute age group
 unintentional_under5 <- unintentional_under5 %>% mutate(age_grp = case_when( 
    age <5 ~ 1, TRUE ~ as.numeric(age)
 ))
 
 #assign sex labels
 unintentional_under5 <- unintentional_under5 %>% 
   mutate(sex_grp = if_else(sex == 1, "male", "female"))
 
 # Bringing council area info.
 postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.rds') %>% 
   setNames(tolower(names(.))) %>%   #variables to lower case
   select(pc7, ca2011)
 
 # aggregate the data with healthboard info
 unintentional_under5 <- left_join(unintentional_under5, postcode_lookup, "pc7") %>% 
   subset(!(is.na(ca2011))) %>%  # exclude records with no ca2011 
   mutate_if(is.character, factor) # converting variables into factors
 
 # save file
 saveRDS(unintentional_under5, file=paste0(data_folder, 'Prepared Data/unintentional_under5_raw.rds'))
 
 # group by year, ca2011, sex, age
 unintentionalunder5_ca2011 <- unintentional_under5 %>% group_by(year, ca2011, sex_grp, age_grp) %>%  
   summarize(numerator = n()) %>% ungroup() %>% rename(ca = ca2011)
 
 # save file
 saveRDS(unintentionalunder5_ca2011, file=paste0(data_folder, 'Prepared Data/unintentionalunder5_ca2011_raw.rds'))
 
 ###############################################.
 ## - Run analysis functions ----
 ###############################################.
 
 
analyze_first(filename = "unintentionalunder5_ca2011", geography = "council", measure = "stdrate", 
               pop = "CA_pop_under5", yearstart = 2005, yearend = 2017,
               time_agg = 3, epop_age = 'under5')
 
analyze_second(filename = "unintentionalunder5_ca2011", measure = "stdrate", time_agg = 3, 
                epop_total = 10000, ind_id = 13050, year_type = "financial", 
                profile = "CP", min_opt = 234388)