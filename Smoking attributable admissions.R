# ScotPHO indicator: Smoking attributable admissions

# Part 1 - Extract data from SMRA
# Part 2 - add in relative risks of each disease as a result of smoking
# Part 3 - Keeping only one record per CIS and aggregating geographic areas
# Part 4 - Add in prevalence info
# Part 5 - Calculate smoking attributable fraction
# Part 6 - Run analysis functions

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

# Extracting admissions from Scottish residents 35 and over with a smoking 
# attributable diagnosis code within the main diagnosis field. 
smoking_adm <- tbl_df(dbGetQuery(channel, statement=
    "SELECT distinct link_no || '-' || cis_marker admission_id, substr(main_condition,1,3) diag, 
            min(council_area) ca, min(hbres_currentdate) hb, min(sex) sex_grp,
            max(extract (year from discharge_date)) year, min(age_in_years) age, max(discharge_date) 
    FROM ANALYSIS.SMR01_PI  
    WHERE discharge_date between '1 January 2012' and '31 December 2017'  
         AND sex <> 0 
         AND age_in_years > 34 
         AND hbres_currentdate between 'S08000015' AND 'S08000028' 
         AND council_area is not null 
         AND regexp_like(main_condition, 'C3[34]|C0|C1[0-6]|C25|C32|C53|C6[4-8]|C80|C92|J4[0-4]|J1[0-8]|I0|I[234]|I5[01]|I6|I7[0-8]|K2[567]|K50|K05|H25|O03|S72[012]') 
     GROUP BY link_no || '-' || cis_marker, main_condition
     ORDER BY link_no || '-' || cis_marker, max(discharge_date)")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.

smoking_adm <- smoking_adm %>% #adding council codes
  mutate(ca = recode(ca, '01'='S12000033', '02'='S12000034', '03'='S12000041', 
                     '04'='S12000035', '05'='S12000026', '06'='S12000005', 
                     '07'='S12000039', '08'='S12000006', '09'='S12000042', '10'='S12000008',
                     '11'='S12000045', '12'='S12000010', '13'='S12000011', '14'='S12000036', 
                     '15'='S12000014', '16'='S12000015', '17'='S12000046', '18'='S12000017', 
                     '19'='S12000018', '20'='S12000019', '21'='S12000020', '22'='S12000021', 
                     '23'='S12000044', '24'='S12000023', '25'='S12000024', '26'='S12000038', 
                     '27'='S12000027', '28'='S12000028', '29'='S12000029', '30'='S12000030',  
                     '31'='S12000040', '32'='S12000013'))

###############################################.
## Part 2 - add in relative risks of each disease as a result of smoking ----
###############################################.
# Taken from Public Health England profiles
smoking_adm <- smoking_adm %>% 
  mutate(current = case_when( #Current smokers risk
    sex_grp == 1 & diag >= "C00" & diag <= "C14" ~ 10.89, #Upper respiratory sites cancers
    sex_grp == 2 & diag >= "C00" & diag <= "C14" ~ 5.08,
    sex_grp == 1 & diag == "C15" ~ 6.76, #Oesaphagus cancers
    sex_grp == 2 & diag == "C15" ~ 7.75,
    sex_grp == 1 & diag == "C16" ~ 1.96, #Stomach cancers
    sex_grp == 2 & diag == "C16" ~ 1.36,
    sex_grp == 1 & diag == "C25" ~ 2.31, #Panchreas cancers
    sex_grp == 2 & diag == "C25" ~ 2.25,
    sex_grp == 1 & diag == "C32" ~ 14.60, #Larynx cancers
    sex_grp == 2 & diag == "C32" ~ 13.02,
    sex_grp == 1 & diag %in% c("C33", "C34") ~ 23.26, #Trachea, lung, bronchus cancers
    sex_grp == 2 & diag %in% c("C33", "C34") ~ 12.69,
    sex_grp == 1 & diag == "C53" ~ 1, #Cervical cancers
    sex_grp == 2 & diag == "C53" ~ 1.59,
    sex_grp == 1 & diag %in% c("C64", "C65", "C66", "C68") ~ 2.50, #Kidney, renal pelvis cancers
    sex_grp == 2 & diag %in% c("C64", "C65", "C66", "C68") ~ 1.40,
    sex_grp == 1 & diag == "C67" ~ 3.27, #Bladder cancers
    sex_grp == 2 & diag == "C67" ~ 2.22,
    sex_grp == 1 & diag == "C80" ~ 4.40, #Unspecified site cancers
    sex_grp == 2 & diag == "C80" ~ 2.20,
    sex_grp == 1 & diag == "C92" ~ 1.80, #Myeloid leukaemia
    sex_grp == 2 & diag == "C92" ~ 1.20,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp <= 11 ~ 4.20, #Ischaemic heart disease
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp %in% c(12, 13) ~ 2.50,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp %in% c(14, 15) ~ 1.80,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp >= 16 ~ 1.40,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp <= 11 ~ 5.30,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp %in% c(12, 13) ~ 2.80,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp %in% c(14, 15) ~ 2.10,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp >= 16 ~ 1.40,
    sex_grp == 1 & ((diag >= "I00" & diag <= "I09") | #Other heart disease
                      (diag >= "I26" & diag <= "I51")) ~ 1.78,
    sex_grp == 2 & ((diag >= "I00" & diag <= "I09") |
                      (diag >= "I26" & diag <= "I51")) ~ 1.49,
    sex_grp == 1 & diag >= "I60" & diag <= "I69" & age_grp <= 11 ~ 4.40, #Cerebrovascular disease
    sex_grp == 1 & diag >= "I60" & diag <= "I69" & age_grp %in% c(12, 13) ~ 3.10,
    sex_grp == 1 & diag >= "I60" & diag <= "I69" & age_grp %in% c(14, 15) ~ 2.20,
    sex_grp == 1 & diag >= "I60" & diag <= "I69" & age_grp >= 16 ~ 1.60,
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp <= 11 ~ 5.40,
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp %in% c(12, 13) ~ 3.70,
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp %in% c(14, 15) ~ 2.60,
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp >= 16 ~ 1.30,
    sex_grp == 1 & diag == "I70" ~ 2.44, #Atherosclerosis.
    sex_grp == 2 & diag == "I70" ~ 1.83,
    sex_grp == 1 & diag == "I71" ~ 6.21, #Aortic aneurysm
    sex_grp == 2 & diag == "I71" ~ 7.07,
    sex_grp == 1 & diag >= "I72" & diag <= "I78" ~ 2.07, #Other arterial disease
    sex_grp == 2 & diag >= "I72" & diag <= "I78" ~ 2.17,
    sex_grp == 1 & diag >= "J10" & diag <= "J18" & age_grp <=13 ~ 2.50, #Pneumonia, influenza
    sex_grp == 1 & diag >= "J10" & diag <= "J18" & age_grp >= 14 ~ 2.00,
    sex_grp == 2 & diag >= "J10" & diag <= "J18" & age_grp <=13 ~ 4.30, 
    sex_grp == 2 & diag >= "J10" & diag <= "J18" & age_grp >= 14 ~ 2.20,
    sex_grp == 1 & diag %in% c("J40", "J41", "J42", "J43") ~ 17.10, #Chronic obstructive lung disease
    sex_grp == 2 & diag %in% c("J40", "J41", "J42", "J43")  ~ 12.04,
    sex_grp == 1 & diag == "J44" ~ 10.58, #Chronic airway obstruction
    sex_grp == 2 & diag == "J44" ~ 13.08,
    sex_grp == 1 & diag %in% c("K25", "K26", "K27") ~ 5.40, #Stomach / duodenal ulcer
    sex_grp == 2 & diag %in% c("K25", "K26", "K27") ~ 5.50,
    diag == "K50" ~ 2.10, #Crohns disease
    diag == "K05" ~ 3.97, #Periodontal disease / Periodonitis
    diag == "H25" & age_grp >= 10 ~ 1.54, #Age related cataract
    diag == "S72" & age_grp %in% c(12, 13) ~ 1.17, #Hip fracture
    diag == "S72" & age_grp %in% c(14, 15) ~ 1.41,
    sex_grp == 1 & diag == "S72" & age_grp >= 16 ~ 1.76,
    sex_grp == 2 & diag == "S72" & age_grp >= 16 ~ 1.85,
    sex_grp == 2 & diag == "O03" ~ 1.28, #Spontaneous abortion
    TRUE ~ 0
)) %>% 
  mutate(ex = case_when( #Ex-smokers risk
    sex_grp == 1 & diag >= "C00" & diag <= "C14" ~ 3.40, #Upper respiratory sites cancers
    sex_grp == 2 & diag >= "C00" & diag <= "C14" ~ 2.29,
    sex_grp == 1 & diag == "C15" ~ 4.46, #Oesaphagus cancers
    sex_grp == 2 & diag == "C15" ~ 2.79,
    sex_grp == 1 & diag == "C16" ~ 1.47, #Stomach cancers
    sex_grp == 2 & diag == "C16" ~ 1.32,
    sex_grp == 1 & diag == "C25" ~ 1.15, #Panchreas cancers
    sex_grp == 2 & diag == "C25" ~ 1.55,
    sex_grp == 1 & diag == "C32" ~ 6.34, #Larynx cancers
    sex_grp == 2 & diag == "C32" ~ 5.16,
    sex_grp == 1 & diag %in% c("C33", "C34") ~ 8.70, #Trachea, lung, bronchus cancers
    sex_grp == 2 & diag %in% c("C33", "C34") ~ 4.53,
    sex_grp == 1 & diag == "C53" ~ 1, #Cervical cancers
    sex_grp == 2 & diag == "C53" ~ 1.14,
    sex_grp == 1 & diag %in% c("C64", "C65", "C66", "C68") ~ 1.70, #Kidney, renal pelvis cancers
    sex_grp == 2 & diag %in% c("C64", "C65", "C66", "C68") ~ 1.10,
    sex_grp == 1 & diag == "C67" ~ 2.09, #Bladder cancers
    sex_grp == 2 & diag == "C67" ~ 1.89,
    sex_grp == 1 & diag == "C80" ~ 2.50, #Unspecified site cancers
    sex_grp == 2 & diag == "C80" ~ 1.30,
    sex_grp == 1 & diag == "C92" ~ 1.40, #Myeloid leukaemia
    sex_grp == 2 & diag == "C92" ~ 1.30,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp <= 11 ~ 2.00, #Ischaemic heart disease
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp %in% c(12, 13) ~ 1.60,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp %in% c(14, 15) ~ 1.30,
    sex_grp == 1 & diag >= "I20" & diag <= "I25" & age_grp >= 16 ~ 1.10,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp <= 11 ~ 2.60,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp %in% c(12, 13) ~ 1.10,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp %in% c(14, 15) ~ 1.20,
    sex_grp == 2 & diag >= "I20" & diag <= "I25" & age_grp >= 16 ~ 1.20,
    sex_grp == 1 & ((diag >= "I00" & diag <= "I09") | #Other heart disease
                      (diag >= "I26" & diag <= "I51")) ~ 1.22,
    sex_grp == 2 & ((diag >= "I00" & diag <= "I09") |
                      (diag >= "I26" & diag <= "I51")) ~ 1.14,
    sex_grp == 1 & diag >= "I60" & diag <= "I69" ~ 1.10, #Cerebrovascular disease
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp <= 15 ~ 1.30,
    sex_grp == 2 & diag >= "I60" & diag <= "I69" & age_grp >= 16 ~ 1,
    sex_grp == 1 & diag == "I70" ~ 1.33, #Atherosclerosis.
    sex_grp == 2 & diag == "I70" ~ 1,
    sex_grp == 1 & diag == "I71" ~ 3.07, #Aortic aneurysm
    sex_grp == 2 & diag == "I71" ~ 2.07,
    sex_grp == 1 & diag >= "I72" & diag <= "I78" ~ 1.01, #Other arterial disease
    sex_grp == 2 & diag >= "I72" & diag <= "I78" ~ 1.12,
    sex_grp == 1 & diag >= "J10" & diag <= "J18"~ 1.40, #Pneumonia, influenza
    sex_grp == 2 & diag >= "J10" & diag <= "J18" ~ 1.10,
    sex_grp == 1 & diag %in% c("J40", "J41", "J42", "J43") ~ 15.64, #Chronic obstructive lung disease
    sex_grp == 2 & diag %in% c("J40", "J41", "J42", "J43")  ~ 11.77,
    sex_grp == 1 & diag == "J44" ~ 6.80, #Chronic airway obstruction
    sex_grp == 2 & diag == "J44" ~ 6.78,
    sex_grp == 1 & diag %in% c("K25", "K26", "K27") ~ 1.80, #Stomach / duodenal ulcer
    sex_grp == 2 & diag %in% c("K25", "K26", "K27") ~ 1.40,
    diag == "K50" ~ 1, #Crohns disease
    diag == "K05" ~ 1.68, #Periodontal disease / Periodonitis
    diag == "H25" & age_grp >= 10 ~ 1.11, #Age related cataract
    diag == "S72" & age_grp %in% c(12, 13) ~ 1.02, #Hip fracture
    diag == "S72" & age_grp %in% c(14, 15) ~ 1.08,
    sex_grp == 1 & diag == "S72" & age_grp >= 16 ~ 1.14,
    sex_grp == 2 & diag == "S72" & age_grp >= 16 ~ 1.22,
    sex_grp == 2 & diag == "O03" ~ 1, #Spontaneous abortion
    TRUE ~ 0))

###############################################.
## Part 3 - Keeping only one record per CIS and aggregating geographic areas ----
###############################################.
smoking_adm <- smoking_adm %>% group_by(admission_id) %>% 
  # selecting first value of an admission for all variables, including the risks 
  # to follow PHE methodology
  summarise_at(c("sex_grp", "age_grp", "year", "current", "ex", "ca", "hb"), first) %>% 
  # Excluding cases where young people has a disease for which only risk for older people.
  filter(current > 0) %>% ungroup() %>% 
  mutate(scotland = "S00000001") %>%  # creating variable for Scotland
  #creating code variable with all geos and then aggregating to get totals
  gather(geolevel, code, ca:scotland) %>% 
  ungroup() %>% select(-c(geolevel)) %>% 
  group_by(code, year, sex_grp, age_grp, current, ex) %>% count() %>% ungroup() 

saveRDS(smoking_adm, file=paste0(data_folder, 'Temporary/smoking_adm_part3.rds'))

###############################################.
## Part 4 - Add in prevalence info ----
###############################################.
# Create raw data on smoking prevalence. Uses raw data requested from Scottish 
# Household Survey on current and ex smoker prevalence, by age, sex and area.
smok_prev_area <- read_excel(paste0(data_folder, "Received Data/SHOS_smoking_prevalence_formatted.xlsx"), 
                             sheet = "Area prev") %>% mutate(code = NA) %>% 
  setNames(tolower(names(.)))   #variables to lower case

# Recoding names into codes. Firs councils and then HBs and Scotland
smok_prev_area$code[which(smok_prev_area$type=="ca")] <- recode(smok_prev_area$area[which(smok_prev_area$type=="ca")],
                                                                'Aberdeen City' = 'S12000033', 'Aberdeenshire' = 'S12000034',
                                                                'Angus' = 'S12000041', 'Argyll & Bute' = 'S12000035',
                                                                'Clackmannanshire' = 'S12000005', 'Dumfries & Galloway' = 'S12000006', 
                                                                'Dundee City' = 'S12000042','East Ayrshire' = 'S12000008',
                                                                'East Dunbartonshire' = 'S12000045','East Lothian' = 'S12000010', 
                                                                'East Renfrewshire' = 'S12000011', 'Edinburgh, City of' = 'S12000036',
                                                                'Na h-Eileanan Siar' = 'S12000013','Falkirk' = 'S12000014', 
                                                                'Fife' = 'S12000015', 'Glasgow City' = 'S12000046',
                                                                'Highland' = 'S12000017', 'Inverclyde' = 'S12000018',
                                                                'Midlothian' = 'S12000019', 'Moray' = 'S12000020',
                                                                'North Ayrshire' = 'S12000021','North Lanarkshire' = 'S12000044',  
                                                                'Orkney Islands' = 'S12000023', 'Perth & Kinross' = 'S12000024',
                                                                'Renfrewshire' = 'S12000038', 'Scottish Borders' = 'S12000026',
                                                                'Shetland Islands' = 'S12000027','South Ayrshire' = 'S12000028',
                                                                'South Lanarkshire' = 'S12000029','Stirling' = 'S12000030', 
                                                                'West Dunbartonshire' = 'S12000039', 'West Lothian' = 'S12000040')

smok_prev_area$code[which(smok_prev_area$type=="hb")] <- recode(smok_prev_area$area[which(smok_prev_area$type=="hb")],
                                                                'Ayrshire & Arran' = 'S08000015', 'Borders' = 'S08000016',
                                                                'Dumfries & Galloway' = 'S08000017','Fife' = 'S08000018',
                                                                'Forth Valley' = 'S08000019','Grampian' = 'S08000020',
                                                                'Greater Glasgow & Clyde' = 'S08000021','Highland' = 'S08000022',
                                                                'Lanarkshire' = 'S08000023', 'Lothian' = 'S08000024',
                                                                'Orkney' = 'S08000025', 'Shetland' = 'S08000026',
                                                                'Tayside' = 'S08000027','Western Isles' = 'S08000028','Scotland' = 'S00000001')

smok_prev_area <- smok_prev_area %>% rename(sex_grp = sex) %>% select(-area, -type) %>% 
  mutate(sex_grp = as.character(sex_grp)) #to allow merging in next section

###############################################.
# Format current and ex smoker data for age groups
smok_prev_age <- read_excel(paste0(data_folder, "Received Data/SHOS_smoking_prevalence_formatted.xlsx"), 
                            sheet = "Age prev") %>% rename(sex_grp = sex) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  mutate(age_grp2 = case_when(agegrp=='35-54' ~ 2, agegrp=='55-64' ~ 3, 
                              agegrp=='65-74' ~ 4, agegrp=='75+' ~ 5)) %>% 
  select(-agegrp, -code) %>% 
  mutate(sex_grp = as.character(sex_grp)) #to allow merging in next section

###############################################.
# Merging prevalence with smoking admissions basefile 
smoking_adm <- left_join(smoking_adm, smok_prev_area, by = c("code", "year", "sex_grp")) %>% 
  #recode age groups to match prevalence by age file
  mutate(age_grp2 = case_when(age_grp>=8 & age_grp<=11 ~ 2,
                              age_grp>=12 & age_grp<=13 ~ 3,
                              age_grp>=14 & age_grp<=15 ~ 4,
                              age_grp>=16 ~ 5))
  
#And now merging with the file with prevalence by age and sex 
smoking_adm <- left_join(smoking_adm, smok_prev_age, by = c("age_grp2", "year", "sex_grp")) 

###############################################.
## Part 5 - Calculate smoking attributable fractions ----
###############################################.
# Calculate age, sex and area specific esimtated prevalence info using 
# Public Health England formula. divide by 100 to get a proportion.
smoking_adm <- smoking_adm %>% 
  mutate(# current and ex smoker prevalence specific to area, age and sex group.
    prev_current = (current_area/scot_current)*current_age/100,
    prev_ex=(ex_area/scot_ex)*ex_age/100,
    # Calculating smoking attributable fraction
    saf = (prev_current*(current-1) + prev_ex*(ex-1))/ 
      (1 + prev_current*(current-1) + prev_ex*(ex-1)),
    # compute total number of admissions attributable to smoking, using SAF
    numerator = n * saf) %>% 
# sum up safs to get total deaths attributable to smoking.
  group_by(code, year, sex_grp, age_grp) %>% 
  summarise(numerator = sum(numerator)) %>% ungroup()

saveRDS(smoking_adm, file=paste0(data_folder, 'Prepared Data/smoking_adm_raw.rds'))

###############################################.
## Part 6 - Run analysis functions ----
###############################################.
analyze_first(filename = "smoking_adm",  measure = "stdrate", geography = "all",
              pop = "CA_pop_allages", yearstart = 2012, yearend = 2017,
              time_agg = 2, epop_age = "normal")

analyze_second(filename = "smoking_adm", measure = "stdrate", time_agg = 2, 
               epop_total = 120000, ind_id = 1548, year_type = "calendar")

##END
