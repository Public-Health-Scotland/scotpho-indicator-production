# ScotPHO indicator: Smoking attributable admissions

# Notes for analyst:
# Smoking attributable admissions/deaths are estimates only - it is calculated by following methodology laid out by PHE.
# Put simply calculation involves multiplying certain types of admissions/deaths by the attributable fractions that are 
# associated with smoking and then also factoring percentage of the population that are current or ex smokers.

# In June 2023 the indicator switched from using SHoS(household survey) data as the source of smoking prevalence to using SHeS (health survey) data instead.
# Around 2020 the SHoS survey stopped asking questions on ex-smoking status - which is essential for attributable calculation. 
# The switch was also made on the recommendation of SG SHoS team as SHeS is deemed best source of health data
# SHeS was not previously used as the sample size did not allow for robust estimates at LA level.
# Since sample size of SHeS is small and local authority level data not robust we need to use aggregated years to provide smoking status for 
# areas - this isn't ideal as scotpho indicator is a rolling average
# but smoking attributable admissions/deaths are artificial construct and using best available data should be acceptable.  

# As of June 2023 2 teams in PHS produce smoking attributable figures for ScotPHO - the tobacco team (lead by Scott Kilgariff,
# who host their estimates on scotpho website under tobacco data pages) and ScotPHO team (who produce indicator data for scotpho profiles tool).
# The estimates produced by the two teams serve different purposes and data is generated using different scripts - the outputs are 
# therefore slightly different but figures should not be drastically different. Once indicator data has been generated the Scotland
# totals can be compared to output published https://www.scotpho.org.uk/risk-factors/tobacco-use/data/smoking-attributable-deaths/

# Although both teams now use SHeS as source of smoking prevalence our scotland totals will differ as ScotPHO indicator is a 2 year rolling figure
# (tobacco team produce scotland only data for individual years)
# scotpho estimates that include data prior to 2019 will still be based on SHoS (when we switched we did not back calculate historic data)

# Part 1 - Compile smoking prevalence data
# Part 2 - Extract data from SMRA
# Part 3 - add in relative risks of each disease as a result of smoking
# Part 4 - Aggregating geographic areas
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
# Requires 2 data series both are trends in percentage of ex & current smokers in those aged 35 year plus, by year, by sex
# Series 1 : prevalence across all age groups but split by ca/NHS board with overall Scotland values
# Series 2 : Scotland level prevalence buy split by age groups
###############################################.

# Historically there are 2 potential sources of smoking prevalence data,both national surveys but due to differences in sample size and available data meant that scotpho indicator 
# needed to switch from using SHoS (Scottish Household survey) to SHeS (Scottish Health survey) as the source of smoking prevalence data for generation the smoking attributable deaths/admissions indicators. 

###############################################.
# Prevalence data series 1:  AREA PREVALENCE ----
###############################################.

# read in SHoS data (for period 2012-2018)
area_prevalence_shos <- read_excel(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/SHOS_smoking_prevalence_formatted (DO NOT DELETE old data before move to shes).xlsx"), 
                                  sheet = "Area prev") |>  
  janitor::clean_names() |>  #variables to lower case
  mutate(period=as.character(year),
         source="SHoS") 

#Apply ca_names_to_codes and hb_names_to_codes helper functions to convert area names to codes

names_to_codes <- function(df) {
  ca <- df |> filter(type == "ca") |> ca_names_to_codes(area) #filter out the HBs then convert names to codes
  hb <- df |> filter(type == "hb") |> hb_names_to_codes(area) |> #filter out the CAs then convert names to codes
    mutate(code = replace_na(code, "S00000001")) #Add Scotland code manually
  bind_rows(ca, hb) #recombine into 1 df
}

area_prevalence_shos <- names_to_codes(area_prevalence_shos)
                                     
# read in SHsS data (for period 2019 onward)
area_prevalence_shes <- read_excel(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/shes smoking prevalence_for scottish areas.xlsx"), 
                                   sheet = "area_prev_shes") |> 
  janitor::clean_names() |>   #variables to lower case
  select(-c("frequency","lci","uci")) |> 
  mutate(smoking_categories=case_when(
    smoking_categories=="Never smoked/Used to smoke occasionally" ~ "never",
    smoking_categories=="Used to smoke regularly" ~ "ex_area",
    smoking_categories=="Current smoker" ~ "current_area",
    TRUE~"other")) |> 
  pivot_wider(names_from ="smoking_categories",
              values_from = "percent") %>%
  filter(sex!=3) |>  #exclude sex 3 (all)
  select(-"never")
  
#Apply HB and CA lookups
area_prevalence_shes <- names_to_codes(area_prevalence_shes)

# add columns containing Scotland current and ex smoking prevalence rate to entire dataset (used in calculations later)
area_prevalence_shes_scot <- area_prevalence_shes |> 
  filter(code=="S00000001") |> 
  select(-c("type","source","code")) |> 
  rename(scot_current=current_area, scot_ex=ex_area)

area_prevalence_shes <-left_join(area_prevalence_shes, area_prevalence_shes_scot, by = c("sex","period","year"))

# 2020 prevalence data not available due to issues with pandemic period survey collection - to compensate apply 2019 prevalence rates to 2020 data
# scotpho indicator a 2 year rolling average therefore doesn't cope well with missing years of data
area_prevalence_shes_2020 <-area_prevalence_shes  |> 
  filter(year==2019) |> 
  mutate(source = "shes duplicate2019",
         year = 2020)

#bind shos and shes area prevalence
area_prevalence <- rbind(area_prevalence_shes,area_prevalence_shos, area_prevalence_shes_2020) |> 
  mutate(sex_grp=as.character(sex)) |> 
  select (-sex) |> 
  arrange(code, year, sex_grp)

rm(area_prevalence_shes, area_prevalence_shos, area_prevalence_shes_scot, area_prevalence_shes_2020)

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
age_prevalence_shes <- read_excel(file.path(profiles_data_folder, "/Received Data/Smoking Attributable/shes smoking prevalence_for agegroups.xlsx"),
                                  sheet = "age_prev_shes") |> 
  clean_names() |> 
  mutate(smoking_category=case_when(
    smoking_category=="Never smoked/Used to smoke occasionally" ~ "never",
    smoking_category=="Used to smoke regularly" ~ "ex_age",
    smoking_category=="Current smoker" ~ "current_age",
    TRUE~"other")) |> 
  select(-frequency) |> 
  pivot_wider(names_from ="smoking_category",
              values_from = "percent") |> 
  filter(sex_grp!=3) |>  #exclude sex 3 (all)
  mutate(age_grp2 = case_when(agegrp=='35-54' ~ 2, agegrp=='55-64' ~ 3, 
                              agegrp=='65-74' ~ 4, agegrp=='75+' ~ 5)) %>% 
  select(-agegrp, -code) |>  
  mutate(sex_grp = as.character(sex_grp)) 

# 2020 prevalence data not available due to issues with pandemic period survey collection - to compensate apply 2019 prevalence rates to 2020
age_prevalence_shes_2020 <- age_prevalence_shes |> 
  filter(year == 2019) |> 
  mutate(source = "shes duplicate2019",
         year = 2020)

#bind shos and shes area prevalence
age_prevalence <- rbind(age_prevalence_shes, age_prevalence_shos, age_prevalence_shes_2020) |> 
  arrange(year, sex_grp)

rm(age_prevalence_shos, age_prevalence_shes, age_prevalence_shes_2020)  # remove df not needed


###############################################.
## Part 2 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Smoking attributable diagnosis
smoking_diag <- paste0("C3[34]|C0|C1[0-6]|C25|C32|C53|C6[4-8]|C80|C92|J4[0-4]|",
                       "J1[0-8]|I0|I[234]|I5[01]|I6|I7[0-8]|K2[567]|K50|K05|H25|O03|S72[012]")
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
              AND discharge_date between '1 January 2012' and '31 December 2022' 
        )
    )
    SELECT admission_id, substr(diag, 1, 3) diag, sex_grp, age, year, 
           start_cis, end_cis, ca, hb, pc7
    FROM adm_table 
    WHERE end_cis between '1 January 2012' and '31 December 2022' 
        AND age > 34 
        AND sex_grp in ('1', '2') 
        AND regexp_like(diag, '", smoking_diag, "')"))) |>  
  clean_names() |> 
  create_agegroups() # Creating age groups for standardization.

postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds') |>  
  clean_names() |>   
  select(pc7, ca2019, hb2019)

# Bringing  LA and datazone info.

smoking_adm <- left_join(smoking_adm, postcode_lookup, "pc7") |>  
  mutate(scotland = "S00000001") # creating variable for Scotland

###############################################.
## Part 3 - add in relative risks of each disease as a result of smoking ----
###############################################.
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
