# ScotPHO indicator: Smoking attributable admissions

# Notes for analyst:
# Smoking attributable admissions/deaths are modelled estimates rather than measured values
# Put simply calculation involves multiplying certain types of admissions/deaths by the attributable fractions that are 
# associated with smoking and then also factoring percentage of the population that are current or ex smokers.

# For 2019 onwards the indicator switched from using SHoS (Household Survey) as the source of smoking prevalence to using SHeS (Health Survey).
# This was because around this time SHoS stopped asking about ex-smoking status, which is essential to calculate attributable hospitalisations/deaths.
# This change was recommended by the SG SHoS team as SHeS is deemed the best source of health data.
# SHes was not previously used as the sample size did not allow for robust estimates at LA level.
# Therefore we now need to use aggregated years to provide smoking status for areas, which is not ideal as the ScotPHO indicator is a rolling average.
# However as smoking attributable admissions/deaths are modelled estimates use of the best available data should be acceptable. 

# As of September 2025 2 teams in PHS produce smoking attributable figures for ScotPHO - the tobacco team (lead by Scott Kilgariff,
# who host their estimates on scotpho website under tobacco data pages) and ScotPHO team (who produce indicator data for scotpho profiles tool).
# The estimates produced by the two teams serve different purposes and are generated using different scripts - the outputs are 
# therefore slightly different but figures should not be drastically different. Once indicator data has been generated the Scotland
# totals can be compared to output published https://www.scotpho.org.uk/risk-factors/tobacco-use/data/smoking-attributable-deaths/

# Although both teams now use SHeS as source of smoking prevalence our scotland totals will differ as ScotPHO indicator is a 2 year rolling figure
# (tobacco team produce scotland only data for individual years)

# Part 1 - Compile smoking prevalence data
# Part 2 - Extract data from SMRA
# Part 3 - Add in relative risks of each disease as a result of smoking
# Part 4 - Aggregate geographic areas
# Part 5 - Calculate smoking attributable fraction
# Part 6 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/data cleaning functions/ca_names_to_codes.R") #HB lookup function
source("./functions/data cleaning functions/hb_names_to_codes.R") #CA lookup functions

library(readxl) #for reading in xlsx files

###############################################.
# Part 1  - Compile smoking prevalence data ----
###############################################.

#Requires two data series. Both are trends in the percentage of ex and current smokers aged 35+ split by sex.
#Series 1 - prevalence across all age groups but split by CA/HB with overall Scotland values. 
#Series 2 - prevalence at Scotland level in over 35s split by age groups

###############################################.
# Prevalence data series 1:  AREA PREVALENCE ----
###############################################.

#These data are stored in 3 data files
#1 - SHoS data (2012-2018)
#2 - SHeS data file 1 (2019, used as a proxy for 2020 due to SHeS not taking place)
#3 - SHeS data file 2 (2022-2023, 2022 used as a proxy for 2021 in line with tobacco team)

#Note - ideally get a full time series from the SG for age 35+ with smoking prevalence data 2019 onwards for 2024 data update
#SHeS data obtained from the SG via IR

#1. SHos Data (2012-18)
area_prevalence_shos <- readRDS(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/SHoS_area_prevalence_DO_NOT_DELETE.rds"))

#2. SHeS Data (2019)
area_prevalence_shes_2019 <- read_excel(file.path(profiles_data_folder, "Received Data/Smoking Attributable/shes smoking prevalence_for scottish areas.xlsx"),
                                   sheet = "area_prev_shes")|> 
  janitor::clean_names() |> #variables to snake case
  select(-c("lci", "uci", "frequency")) |> 
  filter(sex != 3, #drop sex = all
         year == 2019) |>  #keep only 2019 figures
  rename(smoking_status = smoking_categories)

#3. SHeS Data (2022-2023)
area_prevalence_shes_2022_2023 <- read_csv(file.path(profiles_data_folder, "/Received Data/Smoking prevalence data/Smoking_HB_LA_35andover_sup.csv")) |> 
  janitor::clean_names() |> #variables to snake case
  filter(sex != "All") |> 
  rename(period = year, #changing to match SHoS format
         type = geography) |> 
  mutate(sex = case_when(sex == "Male" ~ 1, #recoding sex to 1/2
                       sex == "Female" ~ 2,
                       TRUE ~ NA_real_),
       type = case_when(type %in% c("Health Board", "Scotland") ~ "hb",
                        type == "Local Authority" ~ "ca",
                        TRUE ~ type),
       area = coalesce(health_board, local_authority), #Taking the half-populated health_board and local_authority columns and combining. 
       area = tidyr::replace_na(replace_na(area, "Scotland")), #Replace NAs with Scotland
       source = "SHeS", #adding source
       year = as.numeric(substr(period, 6, 9))) |> #extracting last year of period and converting to numeric  
  select(-c("lower_ci", "upper_ci", "health_board", "local_authority")) |> #dropping unnecessary cols
  filter(year > 2021)  #filtering for 2019 onwards, when SHoS stopped reporting on ex-smoker status

#Join both SHeS data frames
area_prevalence_shes <- rbind(area_prevalence_shes_2019, area_prevalence_shes_2022_2023) |> 
  mutate(smoking_status = case_when(smoking_status == "Never smoked/Used to smoke occasionally" ~ "never", #recoding to match SHoS
                                    smoking_status == "Used to smoke regularly" ~ "ex_area",
                                    smoking_status == "Current smoker" ~ "current_area",
                                    TRUE ~ smoking_status)) |> 
  tidyr::pivot_wider(id_cols = c("area", "sex", "source", "period", "type", "year"),
                     names_from = smoking_status, values_from = percent) |> 
  select(-never) #drop never-smokers
  
#Use helper functions to convert CA and HB names to codes
ca <- area_prevalence_shes |> filter(type == "ca") |> ca_names_to_codes(area) #filter out the HBs then convert names to codes
hb <- area_prevalence_shes |> filter(type == "hb") |> hb_names_to_codes(area) |> #filter out the CAs then convert names to codes
  mutate(code = replace_na(code, "S00000001")) #Add Scotland code manually

area_prevalence_shes <- bind_rows(ca, hb)  #recombine into 1 df

#Add columns containing Scotland current and ex smoking prevalence rate to entire dataset (used in calculations later)
area_prevalence_shes_scot <- area_prevalence_shes |> 
  filter(code=="S00000001") |> 
  select(-c("type","source","code")) |> 
  rename(scot_current=current_area, scot_ex=ex_area)

area_prevalence_shes <-left_join(area_prevalence_shes, area_prevalence_shes_scot, by = c("sex","period","year"))

#Duplicate 2019 prevalence figures for 2020 as no survey that year
#And duplicate 2022 for 2021 as odd trend (in line with tobacco team)
area_prevalence_shes_20_21 <- area_prevalence_shes |> 
  filter(year %in% c(2019, 2022)) |> 
  mutate(year = case_when(year == 2022 ~ 2021,
                          year == 2019 ~ 2020,
                          TRUE ~ year),
         source = case_when(year == 2020 ~ "shes_duplicate2019",
                            year == 2021 ~ "shes_duplicate2020",
                            TRUE ~ source))
area_prevalence_shes <- bind_rows(area_prevalence_shes, area_prevalence_shes_20_21)  #bind proxy years

#bind shos and shes area prevalence
area_prevalence <- rbind(area_prevalence_shos, area_prevalence_shes) |> 
  mutate(sex_grp=as.character(sex)) |> 
  select (-sex) |> 
  arrange(code, year, sex_grp)

rm(area_prevalence_shes, area_prevalence_shos, area_prevalence_shes_20_21, area_prevalence_shes_2019, area_prevalence_shes_2022_2023, ca, hb)

###############################################.
## Prevalence data series 2: AGE PREVALENCE ----
###############################################.

# read in SHoS age data (for period 2012-2018)
age_prevalence_shos <- read_excel(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/SHOS_smoking_prevalence_formatted (DO NOT DELETE old data before move to shes).xlsx"),
                                 sheet = "Age prev") |>
  clean_names() |> 
  mutate(source = "SHoS",
         sex_grp = as.character(sex),
         age_grp2 = case_when(agegrp=='35-54' ~ 2, agegrp=='55-64' ~ 3, 
                              agegrp=='65-74' ~ 4, agegrp=='75+' ~ 5)) |> 
  select(-c("agegrp", "sex", "code")) |> 
  arrange(sex_grp, year, source, ex_age, current_age, age_grp2)

# read in SHeS age data (for period 2019 onwards)
# This data file obtained from the Tobacco team

age_prevalence_shes <- read_csv(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/SHES_prevalence_35plus.csv"))# |> 
  clean_names() |> 
  select(c("status", "age_grp", "sex", "year2019", "year2022", "year2023")) |> 
  mutate(status=case_when(
    status=="Ex-regular cigarette smoker" ~ "ex_age",
    status=="Current cigarette smoker" ~ "current_age",
    TRUE~"other"),
    source = "SHeS",
    sex = as.character(sex),
    year2020 = year2019, #To align with tobacco team using 2019 data for 2020 since no SHeS that year
    year2021 = year2022) |>  #2021 SHeS figure was very low so using 2022 figure as a proxy
tidyr::pivot_longer(cols = c("year2019", "year2020", "year2021", "year2022", "year2023"), names_to = "year", values_to = "percent") |> 
  tidyr::pivot_wider(id_cols = c("age_grp", "sex", "year", "source"), names_from = "status", values_from = "percent") |> 
  filter(sex != "all", 
         age_grp != "All") |> 
  rename(sex_grp = sex,
         agegrp = age_grp) |> 
  mutate(year = as.numeric(substr(year, 5, 9)))
  
#bind shos and shes area prevalence
age_prevalence <- rbind(age_prevalence_shes, age_prevalence_shos) |> 
  arrange(year, sex_grp)
d
rm(age_prevalence_shos, age_prevalence_shes, age_prevalence_shes_2020)  # remove df not needed


###############################################.
## Part 2 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Smoking attributable diagnosis
# Based on royal college of physicians so do not change often - changed in 2025 and in the early 00s before that
smoking_diag <- paste0("A1[5-9]|C1[0145689]|C2[025]|C3[0-4]|C4[34]|C5[03]|C6[4-7]|C92|E11|F0[1-3]|",
                       "F2[0-5]|F2[89]|F3[23]|F502|F5081|G30|G473|H25|H35[3-9]|H3[6-9]|H4[0-9]|",
                       "H5[01]|H5(20|21|22|23|24)|H9[01]|I2[0-6]|I6[0-7]|I71|I739|I8[0-2]|J09|J1[0-8]|",
                       "J4[0-7]|J841|K227|K50|L40|M0[56]|M32|M545|N18[0-9]|O0[03]|O4[245]|S72[0-2]|",
                       "T814|Y83")

# Sorting variables
sort_var <- "link_no, admission_date, discharge_date, admission, discharge, uri"
# Extracting one row per hospital admission in which there was at least one episode containing
# a smoking attributable condition in the main condition field.
# The indicator counts hospital stays but will only includes stays if there was at least one episode with 
# related diagnosis within the financial year if interest. Patients must also be aged over 34 years on admission to be counted. 

# For each admission it extracts the information from the first episode within a hospital stay.
# Then of these admissions selecting the ones that had an smoking attributable
# diagnosis as their first main diagnosis (to follow PHE methodology); with an
# age on admission of 35+, valid sex group, Scottish resident, and with a final
# discharge date in the period of interest
smoking_adm <- tibble::as_tibble(dbGetQuery(channel, statement= paste0(
    "WITH adm_table AS (
        SELECT distinct link_no || '-' || cis_marker admission_id, 
            FIRST_VALUE(council_area_2019) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") ca,
            FIRST_VALUE(hbres_currentdate) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") hb,
            FIRST_VALUE(sex) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") sex_grp,
            FIRST_VALUE(main_condition) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") diag,
                FIRST_VALUE(POSTCODE) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") pc7,
            MIN(age_in_years) OVER (PARTITION BY link_no, cis_marker) age,
            MAX(extract (year from discharge_date)) OVER (PARTITION BY link_no, cis_marker) year,
            MIN(admission_date) OVER (PARTITION BY link_no, cis_marker) start_cis,
            MAX(discharge_date) OVER (PARTITION BY link_no, cis_marker) end_cis
        FROM ANALYSIS.SMR01_PI  z
        WHERE exists(
          SELECT * 
          FROM ANALYSIS.SMR01_PI  
          WHERE link_no=z.link_no and cis_marker=z.cis_marker
              AND regexp_like(main_condition, '", smoking_diag, "')
              AND age_in_years > 34
              AND discharge_date between '1 January 2012' and '31 December 2023' 
        )
    )
    SELECT admission_id, substr(diag, 1, 3) diag, sex_grp, age, year, 
           start_cis, end_cis, ca, hb, pc7
    FROM adm_table 
    WHERE end_cis between '1 January 2012' and '31 December 2023' 
        AND age > 34 
        AND sex_grp in ('1', '2') 
        AND pc7 IS NOT NULL 
        AND regexp_like(diag, '", smoking_diag, "')"))) |>  
  clean_names() |> 
  create_agegroups() # Creating age groups for standardization.

##########################
#Temp code to output SMR file and re-read in to save running over and over again

saveRDS(smoking_adm, file.path(profiles_data_folder, "/Received Data/Smoking Attributable/smoking_attrib_hosp_test_290825.rds"))

smoking_adm <- readRDS(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/smoking_attrib_hosp_test_290825.rds")) |> 
  filter(!is.na(pc7)) |> 
  mutate(scotland = "S00000001",   # creating variable for Scotland
  ten_year_age_band = case_when(age > 34 & age < 55 ~ "35 - 54",
                       age > 54 & age < 65 ~ "55 - 64",
                       age > 64 & age < 75 ~ "65 - 74",
                       age > 74 ~ "75+",
                       TRUE ~ as.character(age)),
  fifty_plus = case_when(age > 49 ~ 1,
                         TRUE ~ 0))
    
###############################################.
## Part 3 - add in relative risks of each disease as a result of smoking ----
###############################################.
smoking_risks <- read.csv(file.path(profiles_data_folder, "Received Data/Smoking Attributable/smoking_risks.csv")) |> 
  mutate(age_text = case_when(
    age_text == "35+" ~ NA_character_,
    TRUE ~ age_text),
    gender = as.character(gender)) |> 
  rename(diag = icd,
         sex_grp = gender)

#Separate into age-specific conditions and non-age specific conditions
smoking_risks_non_spec <- smoking_risks |> 
  filter(is.na(age_text)) |> 
  select(-age_text)

smoking_risks_ten_year_age_band <- smoking_risks |> 
  filter(!is.na(age_text) & age_text != "50+") |> 
  rename(age_t)

smoking_risks_fifty_plus <- smoking_risks |> 
  filter(age_text == "50+")

#Joining each group to the main SMR df
joined_non_spec <- left_join(smoking_adm, smoking_risks_non_spec, by = c("diag", "sex_grp")) 
joined_ten_year <- left_join(smoking_adm, smoking_risks_age_spec, by = c("diag", "sex_grp", "age_text"))

#Coalescing the 3 ds together to overwrite the NAs
smoking_joined <- joined_age_spec |> 
  mutate(
    current = coalesce(current, joined_non_spec$current),
    ex = coalesce(ex, joined_non_spec$ex),
    disease = coalesce(disease, joined_non_spec$disease),
    group = coalesce(group, joined_non_spec$group)
  )


#Dealing with two separate types of age banding - ten year bands and 50+



# Taken from Public Health England profiles
smoking_adm %<>% 
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
## Part 4 - Aggregating geographic areas ----
###############################################.
smoking_adm <- smoking_adm |>  
  # Excluding cases where young people has a disease for which only risk for older people.
  filter(current > 0) |> 
  mutate(scotland = "S00000001") |>   # creating variable for Scotland
  #creating code variable with all geos and then aggregating to get totals
  gather(geolevel, code, c(ca, hb, scotland)) |>  
  select(-geolevel) |>  
  group_by(code, year, sex_grp, age_grp, current, ex)  |>  count() |>  ungroup() |>  
  # filter out cases with NA, cases with a valid hb but no ca, just a few hundred
  filter(!(is.na(code)))

saveRDS(smoking_adm, file.path(profiles_data_folder, '/Temporary/smoking_adm_part3.rds'))

###############################################.
# Merging prevalence with smoking adm basefile 
smoking_adm <- left_join(smoking_adm, area_prevalence, by = c("code", "year", "sex_grp")) |> 
  #recode age groups to match prevalence by age file
  mutate(age_grp = as.numeric(age_grp),
         age_grp2 = case_when(age_grp>=8 & age_grp<=11 ~ 2,
                              age_grp>=12 & age_grp<=13 ~ 3,
                              age_grp>=14 & age_grp<=15 ~ 4,
                              age_grp>=16 ~ 5))

#And now merging with the file with prevalence by age and sex 
smoking_adm <- left_join(smoking_adm, age_prevalence, by = c("age_grp2", "year", "sex_grp")) 

#smoking_adm2 <- readRDS(file=paste0(data_folder, 'Temporary/smoking_adm_part3.rds'))


###############################################.
## Part 5 - Calculate smoking attributable fractions ----
###############################################.
# Calculate age, sex and area specific esimtated prevalence info using 
# Public Health England formula. divide by 100 to get a proportion.
smoking_adm <- smoking_adm |> 
  mutate(# current and ex smoker prevalence specific to area, age and sex group.
    prev_current = (current_area/scot_current)*current_age/100,
    prev_ex=(ex_area/scot_ex)*ex_age/100,
    # Calculating smoking attributable fraction
    saf = (prev_current*(current-1) + prev_ex*(ex-1))/ 
      (1 + prev_current*(current-1) + prev_ex*(ex-1)),
    # compute total number of admissions attributable to smoking, using SAF
    numerator = n * saf) |>  
# sum up safs to get total deaths attributable to smoking.
  group_by(code, year, sex_grp, age_grp) |>  
  summarise(numerator = sum(numerator), .groups = "drop")

saveRDS(smoking_adm, file.path(profiles_data_folder, '/Prepared Data/smoking_adm_raw.rds'))

###############################################.
## Part 6 - Run analysis functions ----
###############################################.

main_analysis(filename = "smoking_adm", measure = "stdrate", geography = "multiple",
             pop = "CA_pop_allages", yearstart = 2012, yearend = 2021,
             time_agg = 2, epop_age = "normal", epop_total = 120000, ind_id = 1548, 
             year_type = "calendar")

  
# Rounding figures - they are estimates and rounding helps to understand that
# they are not precise
data_shiny <- readRDS(file.path(profiles_data_folder, "/Data to be checked/smoking_adm_shiny.rds")) |>  
  mutate(numerator = round(numerator, -1)) |>  #to nearest 10
  mutate_at(c("rate", "lowci", "upci"), round, 0) # no decimals

saveRDS(data_shiny, file.path(profiles_data_folder, "Data to be checked/smoking_adm_shiny.rds"))
write_csv(data_shiny, file.path(profiles_data_folder, "Data to be checked/smoking_adm_shiny.rds"))

##END
