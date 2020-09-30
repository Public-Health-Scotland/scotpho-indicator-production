# ScotPHO indicators: unintentional injuries in under 5s #

#   Part 1 - Extract data from SMRA
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Diagnoses used for unintentional injuries, all V, W, X up to X5, and Y85 and Y86
unint_diag <- '^V|^W|^X[0-5]|^Y8[56]' 

# Extract data from SMRA: one row per hospital admission for those aged under 5, 
# with a valid sex recorded, and admission_type 32 on its own or 33 and 35 combined
# with a diagnosis of unintentional injury
unintentional_under5 <- tbl_df(dbGetQuery(channel, statement=paste0(
  "SELECT distinct link_no linkno, cis_marker cis,  
    min(AGE_IN_YEARS) age, min(SEX) sex_grp, min(DR_POSTCODE) pc7,
    CASE WHEN extract(month from admission_date) > 3 
        THEN extract(year from admission_date) 
        ELSE extract(year from admission_date) -1 END as year
  FROM ANALYSIS.SMR01_PI z
  WHERE admission_date between  '1 April 2005' and '31 December 2019'
    AND sex <> 0
    AND AGE_IN_YEARS <=4
    AND CASE WHEN admission_type = '32' THEN 1
        WHEN admission_type between '33' and '35'
          AND (regexp_like(main_condition, '", unint_diag, "')
            OR regexp_like(other_condition_1, '", unint_diag, "') 
            OR regexp_like(other_condition_2, '", unint_diag, "')   
            OR regexp_like(other_condition_3, '", unint_diag, "') 
            OR regexp_like(other_condition_4, '", unint_diag, "')   
            OR regexp_like(other_condition_5, '", unint_diag, "') ) THEN 1 ELSE 0 END=1 
  GROUP BY link_no, cis_marker,
    CASE WHEN extract(month from admission_date) > 3 
      THEN extract(year from admission_date) 
      ELSE extract(year from admission_date) -1 END" ))) %>% 
  setNames(tolower(names(.)))  #variables to lower case

 #compute age group
 unintentional_under5 <- unintentional_under5 %>% mutate(age_grp = 1)
 
 # Bringing council area info.
 postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2020_2.rds') %>% 
   setNames(tolower(names(.))) %>%   #variables to lower case
   select(pc7, ca2019)
 
 # aggregate the data by council area
 unintentional_under5 <- left_join(unintentional_under5, postcode_lookup, "pc7") %>% 
   subset(!(is.na(ca2019))) %>%  # exclude records with no ca2011 
   mutate_if(is.character, factor) %>%  # converting variables into factors
   group_by(year, ca2019, sex_grp, age_grp) %>%  
   summarize(numerator = n()) %>% ungroup() %>% rename(ca = ca2019)
 
 # save file
 saveRDS(unintentional_under5, file=paste0(data_folder, 'Prepared Data/unintentional_under5_raw.rds'))
 
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
 
analyze_first(filename = "unintentional_under5", geography = "council", measure = "stdrate", 
               pop = "CA_pop_under5", yearstart = 2005, yearend = 2019, hscp = T,
               time_agg = 3, epop_age = 'normal')
 
analyze_second(filename = "unintentional_under5", measure = "stdrate", time_agg = 3, 
                epop_total = 10000, ind_id = 13050, year_type = "financial")

##END