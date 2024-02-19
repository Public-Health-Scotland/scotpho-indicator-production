# **** ACUTE MYOCARDIAL INFARCTION (AMI) HOSPITAL DATA - EMERGENCY ADMISSIONS AGED UNDER 75. ONLY IF FIRST-EVER FOR PATIENT.
# based on Long-term Monitoring of Health Inequalities IR routinely done for SG e.g IR2022-00912
# data used in SG long-term-monitoring-of-health-inequalities publication

#1.Setup ----
source("1.indicator_analysis.R") 
source("2.deprivation_analysis.R")
library(lubridate)

channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))


postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2023_2.rds') %>% 
  setNames(tolower(names(.)))  %>% 
  select(pc7, datazone2001, datazone2011, ca2019)

#2.Extract and Process data ------

# extract list of patients with AMI admissions in historic SMR database
patients_historic <- tbl_df(dbGetQuery(channel, statement=
                               "
  SELECT DISTINCT link_no
  FROM ANALYSIS.SMR01_HISTORIC
  WHERE old_smr1_tadm_code between 4 and 8 
    AND (old_disc_code <6 OR old_disc_code > 7)
    AND regexp_like(main_condition, '^(I21|I22|410|-410)')
              
 ")) %>%  
  setNames(tolower(names(.)))

# extract first time patient admissions from SMR01
#### Emergency admissions are normally defined as:.
######## SMR01A: OLD_SMR1_TADM_CODE 4-8.
######## SMR01B: ADMISSION_TYPE 30-39.
#### However, in this case, OLD_SMR1_TADM_CODE must also be used for SMR01B (without reference to ADMISSION_TYPE) to maintain consistency with previously .
#### published data for 2013.
patients_smr1 <- tbl_df(dbGetQuery(channel, statement=
                           " 
  WITH RankedData AS(
          SELECT link_no,dob,age_in_years,sex,dr_postcode,admission_date,admission_type,discharge_date,main_condition,
                row_number() over (partition by link_no order by admission_date,discharge_date,admission,discharge,uri) as rn
          FROM ANALYSIS.SMR01_PI
          WHERE old_smr1_tadm_code between 4 and 8 
            AND age_in_years <= 74
            AND (
                (discharge_date <= date '2020-12-31' AND discharge_type not between 40 AND 43) 
                OR
                discharge_date >= date '2021-01-01'
              )
            AND regexp_like(main_condition, '^(I21|I22|410|-410)')
        )

  SELECT * FROM RankedData WHERE rn = 1 ")) %>% 
  setNames(tolower(names(.))) %>% select(-rn)


patients <- patients_smr1 %>%
# check patient has not had an earlier admission recorded in SMR historic
filter(!(link_no %in% patients_historic$link_no)) %>% 
mutate(year = year(admission_date)) %>%
filter(year>=2001) %>% 
select("link_no","year","sex","age"="age_in_years","pc7"="dr_postcode")

# merge with postcode lookup, create age groups 
patients <- left_join(patients, postcode_lookup, "pc7")  %>% 
  create_agegroups() %>% 
  select(year, age_grp, age, sex_grp=sex, datazone2001, datazone2011, ca2019) %>% 
  filter(!(is.na(datazone2011))) %>% 
  mutate_if(is.character, factor)

# aggregate data
patients_dz11 <- patients %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

## Create deprivation basefile
patients_dz01_dep <- patients %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(patients_dz01_dep, patients_dz11 %>% 
                    subset(year>=2014)) #joing dz01 and dz11

#3.save files----- 
saveRDS(patients_dz11, file=paste0(data_folder, 'Prepared Data/ami75_dz11_raw.rds'))
saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/ami75_depr_raw.rds'))


#4.Run analysis functions -------
#TODO modify time agg, ind_id arguments

analyze_first(filename = "ami75_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_under75", yearstart = 2001, yearend = 2021,
              time_agg = 1, epop_age = "normal")

analyze_second(filename = "ami75_dz11", measure = "stdrate", time_agg = 1, 
               epop_total = 182000, ind_id = 123456789, year_type = "calendar",qa=FALSE)

analyze_deprivation(filename="ami75_depr", measure="stdrate", time_agg=1, 
                    yearstart= 2002, yearend=2021,   year_type = "calendar", 
                    pop = "depr_pop_under75", epop_age="normal",
                    epop_total =182000, ind_id = 123456789,qa = FALSE)




# Rough .using dbplyr as in original IR code 

# AMI_codes = c("I21","I22","410","-410")
# 
# AMI_codelist <-   paste0("^(", paste(AMI_codes, collapse = "|"), ")")
# 
# patients_historic = tbl(smra, dbplyr::in_schema("ANALYSIS", "SMR01_HISTORIC")) %>%
#   # select(LINK_NO, DOB, OLD_SMR1_TADM_CODE, DISCHARGE_DATE, MAIN_CONDITION, OLD_DISC_CODE) %>%
#   filter( regexp_like(MAIN_CONDITION,AMI_codelist),
#           between(OLD_SMR1_TADM_CODE, 4, 8),
#           (OLD_DISC_CODE < 6 | OLD_DISC_CODE > 7)
#   ) %>% 
#   select(LINK_NO) %>% 
#   distinct() %>% 
#   collect()
# 
# patients_smr1 = tbl(smra, dbplyr::in_schema("ANALYSIS", "SMR01_PI")) %>%
#   select(LINK_NO, DOB, SEX, DR_POSTCODE, ADMISSION_DATE, ADMISSION_TYPE, DISCHARGE_DATE,
#          DISCHARGE_TYPE, MAIN_CONDITION, OLD_SMR1_TADM_CODE,
#          AGE_IN_YEARS, URI, ADMISSION, DISCHARGE) %>%
#   # Using SQL to check the age at admission is below 75
#   # filter(sql("EXTRACT(YEAR FROM ADMISSION_DATE) - EXTRACT(YEAR FROM DOB) <= 75")) %>%
#   filter( # ADMISSION_DATE <= as.Date('2021-12-31') &
#     between(OLD_SMR1_TADM_CODE, 4, 8) &
#       ((DISCHARGE_DATE <= as.Date('2020-12-31') & (DISCHARGE_TYPE < 40 | DISCHARGE_TYPE > 43)) | 
#          DISCHARGE_DATE >= as.Date('2021-01-01')) & 
#       AGE_IN_YEARS <= 74) %>%
#   filter(regexp_like(MAIN_CONDITION, AMI_codelist)) %>%
#   arrange(LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI) %>% 
#   group_by(LINK_NO) %>% 
#   filter(row_number() == 1) %>% 
#   ungroup() %>% 
#   collect()
# 


