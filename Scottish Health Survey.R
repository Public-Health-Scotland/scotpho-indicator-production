
#########################################################
# Scottish Health Survey indicator prep
#########################################################

# There are almost 50 ScotPHO indicators that are sourced from the Scottish Health Survey.
# This script produces dashboard-ready files for all SHeS indicators EXCEPT FOR the 7 smoking prevalence indicators.

#########################################################
# STEP 1: 
#########################################################

# This script uses the data published on the SHeS dashboard https://scotland.shinyapps.io/sg-scottish-health-survey/ 
# for the indicators that are published there (currently 21, as of May 2026, see below). 
# (It can be easiest to request the published data direct from the SHeS team, scottishhealthsurvey@gov.scot) 
# NB. Some indicators on their dashboard look similar but aren't defined/grouped in the way we want/need for ScotPHO indicators.

## ADULTS (N=15)
#### 4170:  Alcohol consumption: Binge drinking (drinking over (6/8) units in a day (includes non-drinkers): Over 8 units for men, over 6 units for women" (previous indicator definition excluded non-drinkers from denom)
#### 4171:  Alcohol consumption: Hazardous/Harmful drinker" (% consuming over 14 units per week) (NB. original ScotPHO indicator excluded non-drinkers from denominator... it's not clear whether they are included here)
#### 4172:  Alcohol consumption (mean weekly units)
#### 14001: Adults meeting muscle strengthening guidelines. 2011 CMO guidelines recommend 2x 30 minute muscle strengthening sessions per week
#### 14002: Adult very low PA: Adults with very low activity levels. Also in CWB, AMH profiles. 2011 CMO guidelines recommend 150 mins/week MVPA.
#### 30001: Mental wellbeing (WEMWBS): Mean score on the WEMWBS scale (adults). WEMWBS stands for Warwick-Edinburgh Mental Wellbeing Scale. N.B. This indicator is also available from the ScotPHO Online Profiles (national, health board, and council area level, but not by SIMD). The questionnaire consists of 14 positively worded items designed to assess: positive affect (optimism, cheerfulness, relaxation) and satisfying interpersonal relationships and positive functioning (energy, clear thinking, self-acceptance, personal development, mastery and autonomy). It is scored by summing the response to each item answered on a 1 to 5 Likert scale ('none of the time', 'rarely', 'some of the time', often', 'all of the time'). The total score ranges from 14 to 70 with higher scores indicating greater wellbeing. The variable used was WEMWBS.
#### 30002: Life satisfaction: % scoring above the mode: Percentage with the highest levels of life satisfaction: responses above the mode (9 to 10-Extremely satisfied) when asked "All things considered, how satisfied are you with your life as a whole nowadays?"
#### 30003: General health questionnaire (GHQ-12)Percentage of adults with a possible common mental health problem. N.B. This indicator is also available from the ScotPHO Online Profiles (national, health board, and council area level, but not by SIMD). A score of four or more on the General Health Questionnaire-12 (GHQ-12) indicates a possible mental health problem over the past few weeks. GHQ-12 is a standardised scale which measures mental distress and mental ill-health. There are 12 questions which cover concentration abilities, sleeping patterns, self-esteem, stress, despair, depression, and confidence in the past few weeks. For each of the 12 questions one point is given if the participant responded 'more than usual' or 'much more than usual'. Scores are then totalled to create an overall score of zero to twelve. A score of four or more (described as a high GHQ-12 score) is indicative of a potential psychiatric disorder. Conversely a score of zero is indicative of psychological wellbeing. As GHQ-12 measures only recent changes to someone's typical functioning it cannot be used to detect chronic conditions. The variable used was GHQg2.
#### 30013: Fruit & vegetable consumption (guidelines): Percentage of adults who met the daily fruit and vegetable consumption recommendation - five or more portions - in the previous day (survey variables porftvg3Intake and porftvg3). According to the guidelines, it is recommended for adults to consume at least five varied portions of fruit and vegetables per day. The module includes questions on consumption of the following food types in the 24 hours to midnight preceding the interview: vegetables (fresh, frozen or canned); salads; pulses; vegetables in composites (e.g. vegetable chilli); fruit (fresh, frozen or canned); dried fruit; fruit in composites (e.g. apple pie); fresh fruit juice. Fruit and vegetable consumption figures for 2021 have been calculated from online dietary recalls using INTAKE24. In 2021, less than half a portion of fruit and vegetables is defined as none. This is due to the inclusion of fruit and vegetables from composite dishes which has led to a decrease in the proportion consuming no fruit or vegetables. Data from earlier years were taken from the fruit and vegetable module. Fruit and vegetable consumption data for NHS health boards and council area areas for 2017-2021 combined are not available, as due to the different method of data collection, it was not possible to combine data for these years. Respondents to the INTAKE24 food diary were included if they had provided data for two days.
#### 99105: Food insecurity
#### 99106: Adult Healthy Weight
#### 99107: Summary activity levels (ADULT) Percentage of adults who met the recommended moderate or vigorous physical activity guideline in the previous four weeks. In July 2011, the Chief Medical Officers of each of the four UK countries agreed and introduced revised guidelines on physical activity. Adults are recommended to accumulate 150 minutes of moderate activity or 75 minutes of vigorous activity per week, or an equivalent combination of both, in bouts of 10 minutes or more. The variable used was adt10gpTW. This bandings used for this variable include the new walking definition for those aged 65 years and over.
#### 99108: Self-assessed health of adults (age 16+) Percentage of adults who, when asked "How good is your health in general?", selected "good" or "very good". The five possible options ranged from very good to very bad, and the variable was GenHelf2.
#### 99109: Limiting long-term conditions (age 16+) Percentage of adults who have a limiting long-term illness. Long-term conditions are defined as a physical or mental health condition or illness lasting, or expected to last, 12 months or more. A long-term condition is defined as limiting if the respondent reported that it limited their activities in any way. The variable used was limitill.
#### 99121: Health risk behaviours (NO DATA EXTRACTED FROM UKDS DATA AS UNCLEAR HOW TO CODE)

## CHILDREN (N=6)
#### 30111: Children's PA guidelines:  % children meeting 1 hour PA per day (INCL. SCHOOL)
#### 14003: Children with very low PA levels
#### 14006: Children participating in sport
#### 30114: Children's general health. Percentage of children who, when asked "How good is your health in general?", selected "good" or "excellent". The five possible options ranged from very good to very bad, and the variable was GenHelf2.
#### 30115: Children with Limiting long-term conditions.	Percentage of children who have a limiting long-term illness. Long-term conditions are defined as a physical or mental health condition or illness lasting, or expected to last, 12 months or more. A long-term condition is defined as limiting if the respondent reported that it limited their activities in any way. The variable used was limitill.
#### 14012 % children meeting activity guidelines (NOT inc school) (var ch00sum7)


#########################################################
# STEP 2:
#########################################################

# Additional indicators (21 as of May 2026, listed below) can only be sourced from the UK Data Service (UKDS) microdata. 
# The UKDS data are imported and processed in a separate git repo https://github.com/Public-Health-Scotland/ScotPHO_survey_data
# In that repo we calculate estimates for all available splits for all available indicators, including those available on the SHeS dashboard (listed above), 
# as there could be additional splits that aren't published, despite being above the SHeS suppression threshold of denominators <30). 
# Suppression of splits below that threshold is done in the ScotPHO_survey_data ukds_shes_calcs.R script, 
# and any splits where more than 25% of the estimates have to be suppressed are removed entirely 
# (25% was an entirely arbitrary figure from Liz's head, which seemed reasonable at the time).

##### ADULTS (N=11)
#### 30004 = depsymp	Percentage of adults who had a symptom score of two or more on the depression section of the Revised Clinical Interview Schedule (CIS-R). A score of two or more indicates symptoms of moderate to high severity experienced in the previous week. The variable used was depsymp (or dvg11 in 2008). 
#### 30005 = anxsymp	Percentage of adults who had a symptom score of two or more on the anxiety section of the Revised Clinical Interview Schedule (CIS-R). A score of two or more indicates symptoms of moderate to high severity experienced in the previous week. The variable used was anxsymp (or dvj12 in 2008). 
#### 30009 = suicide2	Percentage of adults who made an attempt to take their own life, by taking an overdose of tablets or in some other way, in the past year. The variable used was suicide2. 
#### 30010 = dsh5sc	Percentage of adults who deliberately harmed themselves but not with the intention of killing themselves in the past year. The variable used was DSH5 from 2008 to 2011, or DSH5SC from 2013 onwards. 
#### 30021 = involve	Percentage of adults who, when asked "How involved do you feel in the local community?", responded "a great deal" or "a fair amount". The four possible options ranged from "a great deal" to "not at all". The variables used were Involve and INVOLV19. 
#### 30023 = p_crisis	Percentage of adults with a primary support group of three or more to rely on for comfort and support in a personal crisis. Respondents were asked "If you had a serious personal crisis, how many people, if any, do you feel you could turn to for comfort and support?", and the variables were PCrisis or PCRIS19. 
#### 30026 = rg17a_new	Percentage of adults who provide 20 or more hours of care per week to a member of their household or to someone not living with them, excluding help provided in the course of employment. Participants were asked whether they look after, or give any regular help or support to, family members, friends, neighbours or others because of a long-term physical condition, mental ill-health or disability; or problems related to old age. Caring which is done as part of any paid employment is not asked about. From 2014 onwards, this question explicitly instructed respondents to exclude caring as part of paid employment. The variables used to construc this indicator were RG15aNew (Do you provide any regular help or care for any sick, disabled, or frail people?) and RG17aNew (How many hours do you spend each week providing help or unpaid care for him/her/them?). 
#### 30051 = str_work2	Percentage of adults who find their job "very stressful" or "extremely stressful". Respondents were asked "In general, how do you find your job?" and were given options from "not at all stressful" to "extremely stressful". The variable was StrWork2. 
#### 30052 = work_bal	Mean score for how satisfied adults are with their work-life balance (paid work). Respondents were asked "How satisfied are you with the balance between the time you spend on your paid work and the time you spend on other aspects of your life?" on a scale between 0 (extremely dissatisfied) and 10 (extremely satisfied). The intervening scale points were numbered but not labelled. The variable was WorkBal. 
#### 30053 = contrl	Percentage of adults who often or always have a choice in deciding how they do their work, in their current main job. The five possible responses ranged from "always" to "never". The variable was Contrl. 
#### 30054 = support1	Percentage of adults who "strongly agree" or "tend to agree" that their line manager encourages them at work. The five options ranged from "strongly agree" to "strongly disagree". The variables used were Support1 and Support1_19. 
##### CHILDREN (N=10)
#### 14007 - ch30plyg - Children engaging in active play
#### 30129 = ch_audit  Percentage of children aged 15 years or under with a parent/carer who reports consuming alcohol at hazardous or harmful levels (AUDIT questionnaire score 8+)
#### 30130 = ch_ghq  Percentage of children aged 15 years or under who have a parent/carer who scores 4 or more on the General Health Questionnaire-12 (GHQ-12)
#### 30170	Peer relationship problems - Percentage of children with a 'slightly raised', 'high' or 'very high' score (a score of 3-10) on the peer relationship problems scale of the Strengths and Difficulties Questionnaire (SDQ)
#### 30172	Emotional symptoms - Percentage of children with a 'slightly raised', 'high' or 'very high' score (a score of 4-10) on the emotional symptoms scale of the Strengths and Difficulties Questionnaire (SDQ)
#### 30173	Conduct problems - Percentage of children with a 'slightly raised', 'high' or 'very high' score (a score of 3-10) on the conduct problems scale of the Strengths and Difficulties Questionnaire (SDQ)
#### 30174	Hyperactivity/inattention - Percentage of children with a 'slightly raised', 'high' or 'very high' score (a score of 6-10) on the hyperactivity/inattention scale of the Strengths and Difficulties Questionnaire (SDQ)
#### 30175	Prosocial behaviour - Percentage of children with a 'close to average' score (a score of 8-10) on the prosocial scale of the Strengths and Difficulties Questionnaire (SDQ)
#### 99117	Total difficulties - Percentage of children with a 'slightly raised', 'high' or 'very high' total difficulties score (a score of 14-40) on the Strengths and Difficulties Questionnaire (SDQ). A total difficulties score of 14 or over is also referred to as borderline (14-16) or abnormal (17-40).
#### 99144: Children at risk of obesity (2-15y) (Monica's indicator: need to check how it compares with the data she's been provided with)

#########################################################
# STEP 3: 
#########################################################

# Check availability of the indicators from these two sources.
# Use the SHeS dashboard data wherever available.
# Prep the final ScotPHO dashboard indicator files


#########################################################
### Functions/packages -----
#########################################################

source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA
library(arrow) # for parquet files

## SHeS folder
shes_folder <- file.path(profiles_data_folder, "Received Data", "Scottish Health Survey")



#########################################################
### Read in the data -----
#########################################################

## Pre-processed UKDS data (UK data service)
# The data read in below has been prepared in separate git repo https://github.com/Public-Health-Scotland/ScotPHO_survey_data
# Last updated June 2026 (SHeS data up to 2024)
shes_from_ukds <- readRDS(file.path(profiles_data_folder, "Prepared Data", "shes_raw.rds")) %>%
  mutate(code = as.character(code)) %>%
  mutate(source = "UKDS") %>%
  mutate(areatype = case_when(substr(code, 1, 3)=="S00" ~ "Scot",
                              substr(code, 1, 3)=="S08" ~ "HB",
                              substr(code, 1, 3)=="S12" ~ "CA",
                              substr(code, 1, 3)=="S11" ~ "ADP",
                              substr(code, 1, 3)=="S32" ~ "PD",
                              substr(code, 1, 3)=="S37" ~ "HSCP",
                              TRUE ~ "NA")) %>%
  # correct the equivalised income splits: (to be added to the survey data processing ultimately)
  # NB. quintiles have opposite ordering to SIMD (to match SHeS dashboard)
  ## NEED TO CHANGE THIS IN THE PROCESSING FILE CODE
  mutate(split_value = case_when(split_name == "Income (equivalised)" & split_value == "Q1 (lowest)" ~ "Q5 (lowest income)",
                                 split_name == "Income (equivalised)" & split_value == "Q5 (highest)" ~ "Q1 (highest income)",
                                 TRUE ~ split_value))


# which geogs do we want to keep/drop?
# drop ADP and PD for PA profile indicators:
pa_inds <- c(14001, 14002, 30111, 14003, 14006, 14007, 14012)
shes_from_ukds <- shes_from_ukds %>%
  filter(!(ind_id %in% pa_inds & areatype %in% c("PD", "ADP")))

# make a lookup for ind_id
ind_ids <- shes_from_ukds %>%
  select(indicator, ind_id) %>%
  unique()


## SHeS dashboard data: 
## Can be downloaded from there but easier to get direct from the SHeS team (see open_data_to_2024 folder)
SHeS_SCOTLAND <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_trend_sex_opendata.xlsx")) %>% mutate(split_name = "Sex")
SHeS_LA <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_rank_sex_opendata.xlsx")) %>% mutate(split_name = "Sex")
SHeS_SIMD <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_trend_simd_opendata.xlsx")) %>% mutate(split_name = "Deprivation (SIMD)")
SHeS_AGE <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_trend_age_opendata.xlsx")) %>% mutate(split_name = "Age group")
SHeS_INCOME <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_trend_ei_opendata.xlsx")) %>% mutate(split_name = "Income (equivalised)")
SHeS_CONDITIONS <- readxl::read_xlsx(file.path(shes_folder, "open_data_to_2024", "shes_trend_ltc_opendata.xlsx")) %>% mutate(split_name = "Long-term Illness")


## Alternative route to the published data: 
# ### Do this if you need to extract the data from statistics.gov.scot (as of May 2026: not updated as quickly as the SHeS dashboard is) ---- 
# Run lines below to install the opendata scotland package
# devtools::install_github("ScotGovAnalysis/opendatascot",upgrade = "never",build_vignettes = TRUE)
# library(opendatascot) # for getting data from stats.gov.scot
# # # # Download each of the datasets 
# # # # (N.B. only do this if reading in new data. Latest downloaded = 2023, published in Nov 2024)
# # # # (persist if gives HTTP errors such as 302...)
# # SHeS_SCOTLAND <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data")
# # SHeS_LA <- opendatascot::ods_get_csv("scottish-health-survey-local-area-level-data")
# # SHeS_SIMD <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-simd")
# # SHeS_AGE <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-age")
# # SHeS_INCOME <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-equivalised-income")
# # SHeS_CONDITIONS <- opendatascot::ods_get_csv("scottish-health-survey-scotland-level-data-by-long-term-conditions")
# # 
# # #Write the datasets to the Received Data folder in .parquet format
# # write_parquet(SHeS_SCOTLAND, file.path(shes_folder, "SHeS_SCOTLAND.parquet"))
# # write_parquet(SHeS_LA, file.path(shes_folder, "SHeS_LA.parquet"))
# # write_parquet(SHeS_SIMD, file.path(shes_folder, "SHeS_SIMD.parquet"))
# # write_parquet(SHeS_AGE, file.path(shes_folder, "SHeS_AGE.parquet"))
# # write_parquet(SHeS_INCOME, file.path(shes_folder, "SHeS_INCOME.parquet"))
# # write_parquet(SHeS_CONDITIONS, file.path(shes_folder, "SHeS_LONGTERM_CONDITIONS.parquet"))
# 
# ### Read in the downloaded and saved data ----
# SHeS_SCOTLAND <- read_parquet(file.path(shes_folder, "SHeS_SCOTLAND.parquet")) %>% mutate(split_name = "Sex")
# SHeS_LA <- read_parquet(file.path(shes_folder, "SHeS_LA.parquet")) %>% mutate(split_name = "Sex")
# SHeS_SIMD <- read_parquet(file.path(shes_folder, "SHeS_SIMD.parquet")) %>% mutate(split_name = "Deprivation (SIMD)")
# SHeS_AGE <- read_parquet(file.path(shes_folder, "SHeS_AGE.parquet")) %>% mutate(split_name = "Age")
# SHeS_INCOME <- read_parquet(file.path(shes_folder, "SHeS_INCOME.parquet")) %>% mutate(split_name = "Income (equivalised)")
# SHeS_CONDITIONS <- read_parquet(file.path(shes_folder, "SHeS_LONGTERM_CONDITIONS.parquet")) %>% mutate(split_name = "Long-term illness")


#########################################################
### Process the published data -----
#########################################################

### Combine data and get column data and formats right----
shes_from_dashboard <- mget(ls(pattern="^SHeS_")) %>% # get all the dataframes in the environment starting with "SHeS_"
  bind_rows() %>% # append them together
  mutate(code = ifelse(GeographyCode=="S92000003", "S00000001", GeographyCode)) %>% # recode Scotland
  mutate(stat = case_when(Measurement=="95% Lower Confidence Limit" ~ "lowci", # recode the measures
                          Measurement=="95% Upper Confidence Limit" ~ "upci",
                          Measurement %in% c("Mean", "Percent") ~ "rate")) %>%
  rename(trend_axis = DateCode) %>%
  # create a new split_value column: coalesce combines non-NA values into a single column
  mutate(split_value = coalesce(Age, Sex, LongtermConditions, EquivalisedIncome, SIMDquintiles)) %>% # this works because there is only ever one non-NA cell in these 5 columns
  mutate(split_value = if_else(split_value == "All", "Total", split_value)) %>% # recode All -> Total
  mutate(split_value = case_when(# Deprivation:
    split_name == "Deprivation (SIMD)" & split_value=="1 - most deprived" ~ "1", # format needed for the inequalities analysis
    split_name == "Deprivation (SIMD)" & split_value=="2nd quintile" ~ "2",
    split_name == "Deprivation (SIMD)" & split_value=="3rd quintile" ~ "3",
    split_name == "Deprivation (SIMD)" & split_value=="4th quintile" ~ "4",
    split_name == "Deprivation (SIMD)" & split_value=="5 - least deprived" ~ "5",
    # equivalised household income
    split_name == "Income (equivalised)" & split_value=="1st-Top quintile" ~ "Q1 (highest income)", # match ScotPHO format
    split_name == "Income (equivalised)" & split_value=="2nd quintile" ~ "Q2", 
    split_name == "Income (equivalised)" & split_value=="3rd quintile" ~ "Q3", 
    split_name == "Income (equivalised)" & split_value=="4th quintile" ~ "Q4", 
    split_name == "Income (equivalised)" & split_value=="5th-Bottom quintile" ~ "Q5 (lowest income)",
    # Age group:
    split_name == "Age group" & split_value=="0-3" ~ "0 to 3y",
    split_name == "Age group" & split_value=="11-12" ~ "11 to 12y",
    split_name == "Age group" & split_value=="12-15" ~ "12 to 15y",
    split_name == "Age group" & split_value=="13-15" ~ "13 to 15y",
    split_name == "Age group" & split_value=="2-4" ~ "2 to 4y",
    split_name == "Age group" & split_value=="4-7" ~ "4 to 7y",
    split_name == "Age group" & split_value=="5-7" ~ "5 to 7y",
    split_name == "Age group" & split_value=="8-10" ~ "8 to 10y",
    split_name == "Age group" & split_value=="8-11" ~ "8 to 11y",
    # long-term illness
    split_name == "Long-term Illness" & split_value=="Limiting long-term conditions" ~ "Limiting Long-term Illness",
    split_name == "Long-term Illness" & split_value=="No long-term conditions" ~ "No Long-term Illness",
    split_name == "Long-term Illness" & split_value=="Non-limiting long-term conditions" ~ "Non-limiting Long-term Illness",
    TRUE ~ split_value)) %>%
  
  # keep the required columns
  select(code, ind = ScottishHealthSurveyIndicator, trend_axis, split_name, split_value, stat, value = Value) %>%
  
  # reshape to wide
  pivot_wider(names_from=stat, values_from = value) %>%
  
  # add def_period column
  # first need to calculate the difference (year_diff) between the first and last year in the trend_axis to work out what def_period to add
  # this calc copes with single year (e.g. year_diff for 2008 = 0), two years (year_diff for 2010-2011 = 1),
  # and the aggregates used for lower geogs (year_diff = 3 if not including 2020, or 4 if 2020 is in the range, because 2020 data are excluded)
  mutate(year_diff =
           as.numeric(substr(trend_axis, nchar(trend_axis) - 3, nchar(trend_axis))) # the last year in trend_axis
         - as.numeric(substr(trend_axis, 1, 4))) %>% # minus the first year in trend_axis
  filter(year_diff!=1) %>% #drop the two-year aggregates: we don't present these
  mutate(year = case_when(year_diff == 0 ~ as.numeric(substr(trend_axis, 1, 4)), # only one year in the label
                          year_diff %in% c(3:4) ~ as.numeric(substr(trend_axis, 1, 4))+2)) %>% # year = first year in the label + 2 (=mid point or midpoint rounded up to nearest whole year)
  mutate(def_period = ifelse(year_diff==0,
                             paste0("Survey year (", trend_axis, ")"),
                             paste0("Aggregated survey years (", trend_axis, ")"))) %>%
  mutate(numerator = NA, # columns needed to get into same format as the imported UKDS data
         sex = case_when(split_name=="Sex" ~ split_value,
                         TRUE ~ "Total")) %>%
  # identify the areatype
  mutate(areatype = case_when(substr(code, 1, 3)=="S00" ~ "Scot",
                              substr(code, 1, 3)=="S08" ~ "HB",
                              substr(code, 1, 3)=="S12" ~ "CA",
                              TRUE ~ "NA")) 


### Which published indicators should be kept? ----

# print out list of all available indicators in the data:
unique(shes_from_dashboard$ind)
# look through to check which ones we need to keep

# List all the indicators we want to keep:
dashboard_vars_to_keep <- c("Alcohol consumption: Hazardous/Harmful drinker",                                                                               
                            "Alcohol consumption (mean weekly units)",                                                                                      
                            "Drinking over 6/8 units in a day (includes non-drinkers): Over 8 units for men/6 units for women",                             
                            "Fruit & vegetable consumption: 5 portions or more",                                                                            
                            "General health questionnaire (GHQ-12): Score 4+",                                                                              
                            "Healthy weight: Healthy weight",                                                                                               
                            "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)",                                                              
                            "Long-term conditions: Limiting long-term conditions",                                                                          
                            "Long-term conditions (children): Limiting long-term conditions",                                                               
                            "Mental wellbeing",                                                                                           
                            "Multiple risks for poor health: Two or more",
                            "Participating in sport (children): Yes",                                                                                       
                            "Self-assessed general health: Very good/Good",                                                                                 
                            "Self-assessed general health (children): Very good/Good",                                                                      
                            "Summary activity levels: Meets recommendations", 
                            "Summary activity levels: Very low activity",
                            "Summary activity levels (including school) (children): Meets recommendations",                                                 
                            "Summary activity levels (including school) (children): Low activity",  
                            "Summary activity levels (excluding school) (children): Meets recommendations",
                            "Whether meets muscle strengthening recommendations: Yes",                                                                      
                            "Worried would run out of food: Yes"   )                                                                                                             

shes_from_dashboard <- shes_from_dashboard %>%
  filter(ind %in% dashboard_vars_to_keep) %>%
  # add indicator names to match the UKDS data
  mutate(indicator = case_when( ind == "Alcohol consumption: Hazardous/Harmful drinker" ~ "haz_or_harmful_drinker",
                                ind == "Alcohol consumption (mean weekly units)" ~ "alc_consumption_units",
                                ind == "Drinking over 6/8 units in a day (includes non-drinkers): Over 8 units for men/6 units for women" ~ "alc_binge_drinking",
                                ind == "Fruit & vegetable consumption: 5 portions or more" ~ "fruit_veg_consumption",  
                                ind == "General health questionnaire (GHQ-12): Score 4+" ~ "common_mh_probs",    
                                ind == "Healthy weight: Healthy weight" ~ "adult_healthy_weight",
                                ind == "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)" ~ "life_satisfaction",  
                                ind == "Long-term conditions: Limiting long-term conditions" ~ "limiting_long_term_condition",  
                                ind == "Long-term conditions (children): Limiting long-term conditions" ~ "child_llti",
                                ind == "Mental wellbeing" ~ "mental_wellbeing",    
                                ind == "Multiple risks for poor health: Two or more" ~ "multi_health_risks", 
                                ind == "Participating in sport (children): Yes" ~ "children_participating_sport",
                                ind == "Self-assessed general health: Very good/Good" ~ "self_assessed_health",  
                                ind == "Self-assessed general health (children): Very good/Good" ~ "child_general_health",
                                ind == "Summary activity levels: Meets recommendations" ~ "physical_activity",
                                ind == "Summary activity levels: Very low activity" ~ "adults_very_low_activity",    
                                ind == "Summary activity levels (including school) (children): Meets recommendations" ~ "cyp_pa_over_1h_per_day",
                                ind == "Summary activity levels (including school) (children): Low activity" ~ "children_very_low_activity",
                                ind == "Summary activity levels (excluding school) (children): Meets recommendations" ~ "children_meet_pa_recs_excl_school",
                                ind == "Whether meets muscle strengthening recommendations: Yes" ~ "meeting_muscle_strengthening_recommendations",
                                ind == "Worried would run out of food: Yes" ~ "food_insecurity",
                                TRUE ~ as.character(NA)  )) %>%
  # Add ind_id column using lookup created above
  merge(y=ind_ids, by="indicator", all.x=TRUE) %>%
  mutate(ind_id = ifelse(indicator=="multi_health_risks", 99121, ind_id)) %>% #this indicator is not in UKDS data, so add its ind_id manually
  # Select relevant columns
  select(ind_id, indicator, code, areatype, year, trend_axis, def_period, sex, split_name, split_value, rate, lowci, upci, numerator) 

# which indicator ids?
inds_in_db <- shes_from_dashboard$ind_id %>% unique() #n=21

### Add totals for the popgroup and SIMD splits ----
# This is needed for subsequent calculation of the inequalities metrics, and is nice-to-have for the popgroup data:
# Add split_value = "Total" for the splits without totals.

# 1st Find the splits without totals:
splits_w_no_total <- shes_from_dashboard %>%
  group_by(indicator, split_name) %>%
  mutate(has_total = "Total" %in% split_value) %>%
  ungroup() %>%
  filter(has_total==FALSE) %>%
  select(code, indicator, ind_id, trend_axis, year, def_period, sex, split_name, areatype) %>%
  unique()

# 2nd Get the totals for those splits
totals_to_add <- shes_from_dashboard %>%
  filter(split_value=="Total") %>%
  select(code, indicator, trend_axis, split_value, rate, lowci, upci, numerator, areatype) %>%
  unique() %>%
  merge(y = splits_w_no_total, by=c("code", "indicator", "trend_axis", "areatype"), all.y=TRUE)

shes_from_dashboard <- shes_from_dashboard %>%
  rbind(totals_to_add) %>%
  mutate(source="dashboard")

#########################################################
### Compare availability -----
#########################################################

#indicators with different age groups to the dashboard data:
# = adult low activity, adult meet muscle recs, child v low PA, child sport, child meets recs (incl school), child meets recs (excl school)
pa_inds_agegp <- c(14001, 14002, 14006, 14003, 30111, 14012)
# remove the dashboard age group splits for these indicators

source_comparison <- shes_from_dashboard %>%
  filter(!(ind_id %in% pa_inds_agegp & split_name=="Age group")) %>%
  filter(!split_name=="Long-term Illness") %>% # we have opted to produce 2 llti splits (yes/no) rather than the 3 used on dashboard.
  merge(y=shes_from_ukds, by=c("indicator", "ind_id", "code", "areatype", "year", "trend_axis", "def_period", "sex", "split_name", "split_value"), all=TRUE) %>%
  filter(areatype %in% c("Scot", "HB", "CA") & ind_id %in% inds_in_db) %>% # UKDS provides any PD, HSCP or ADP splits, as these aren't available in the dashboard. 
  mutate(final_source = ifelse(is.na(source.x), source.y, source.x))

# This comparison is limited to the indicators that are available from both sources (n=20 as of June 2026).
# Another 21 (as of June 2026) are only available from the UKDS extract. One is only available from the dashboard data.
ftable(source_comparison$indicator, 
       source_comparison$split_name, 
       source_comparison$final_source, 
       source_comparison$areatype, 
       row.vars=c(1:2))

# Key points about the two sources:
## Aggregated UKDS data start from 2008-11, while the aggregated dashboard data starts at 2012-15. 
## Single year data start at 2008 for both sources
## Dashboard only presents single year data for Scotland...
## We want Scotland figures for aggregate years too, for plotting against lower geogs: so any aggregated Scotland data present in the UKDS extract will be used too. 
## Dashboard only presents lower geogs for adult indicators (total, male, female) but no child indicators.
## Dashboard presents all splits (except adult indicators by sex) for Scotland only, no lower geogs.
## UKDS data have been processed for splits where less than 25% of the data need suppression. This means up to 1 in 4 data points could be suppressed for some splits.
## Dashboard data don't provide numerators. Numerators from the UKDS processing will be used where available. 
## UKDS provides SIMD splits by sex, dashboard doesn't.
## UKDS provides urban/rural splits, while dashboard doesn't

# additionals for deprivation = splits by sex, + aggregate years and lower geogs
# additionals for age group = aggregate years + lower geogs
# additionals for equiv invome = aggregate years + lower geogs
# additionals for sex = earlier aggregate data



# keep UKDS where available
shes_combined <- shes_from_dashboard %>%
  filter(!(ind_id %in% pa_inds_agegp & split_name=="Age group")) %>%
  filter(!split_name=="Long-term Illness") %>% # we have opted to produce 2 llti splits (yes/no) rather than the 3 used on dashboard.
  merge(y=shes_from_ukds, by=c("indicator", "ind_id", "split_name", "split_value", "sex", "code", "areatype", "trend_axis", "year", "def_period"), all=TRUE) %>%
  # .x is dashboard, .y is ukds, so we keep dashboard for Scotland, where available
  # opted to take lower geog data from UKDS processing, rather than dashboard data (even when available), so that all coincident geographies have the same data.
  mutate(rate = ifelse(areatype=="Scot" & !is.na(rate.x), rate.x, rate.y), 
         lowci = ifelse(areatype=="Scot" & !is.na(rate.x), lowci.x, lowci.y),
         upci = ifelse(areatype=="Scot" & !is.na(rate.x), upci.x, upci.y),
         numerator = numerator.y, # there are no numerators in the dashboard extract
         source = ifelse(areatype=="Scot" & !is.na(rate.x), source.x, source.y)) %>%
  mutate(rate_diff = case_when(!is.na(rate.x) & !is.na(rate.y) ~ rate.x-rate.y, 
                               TRUE ~ as.numeric(NA)))
#shes_from_dashboard has 25,693 records
#shes_from_ukds has 612,347 records
#shes_combined has 614,689 records

# how do the indicator values compare?
shes_combined %>% 
  filter(!is.na(rate.x) & !is.na(rate.y)) %>%
  ggplot() +
  geom_point(aes(x=rate.x, y=rate.y)) +
  facet_wrap(~indicator)
# SHOWS VERY CLOSE AND LARGELY PERFECT MATCH BETWEEN UKDS AND DASHBOARD DATA, WHERE BOTH ARE AVAILABLE. 
# SLIGHT DISCREPANCIES APPARENT THAT PROBABLY ARISE FROM THE UKDS DATA BEING A MORE SUPPRESSED VERSION OF THE RAW DATA THE SHES TEAM USE

### 6. Check geographical availability: ----

# which Scotland-wide data to keep to match any lower geographies in the main_data (needed for trend chart and rank comparisons)?
availability <- shes_combined %>%
  mutate(years = case_when(nchar(trend_axis)==4 ~ "single",
                           nchar(trend_axis)>4 ~ "aggregated",
                           TRUE ~ as.character(NA))) %>%
  filter(split_value=="Total") %>%
  select(ind_id, indicator, areatype, years, split_name) %>%
  unique() %>%
  group_by(ind_id, indicator, areatype, split_name) %>%
  summarise(count = n()) %>% # whether available for single/aggregated years only (count == 1), or both (count==2)
  ungroup()
ftable(availability$indicator, availability$areatype, availability$split_name, availability$count)
# shows some splits are available at two levels of temporal aggregation: single year and 4y
# main_data needs to select data at the level of aggregation of any lower geographies, if present, so that trend charts use the same trend_axis labels

# make a list of the indicators this affects:
indicators_w_lower_geogs <- availability %>%
  filter(!areatype=="Scot") %>%
  select(indicator) %>%
  unique()
indicators_w_lower_geogs <- as.vector(indicators_w_lower_geogs$indicator)


### 7. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland and lower geogs
  main_data_final <- shes_combined %>% 
    filter(indicator == ind,
           split_name == "Sex",
           split_value == "Total") %>%
    # if lower geogs are present (4-y aggregated data only), the 4-y aggregated data should be selected for Scotland too, so that both can be plotted and compared
    {if (ind %in% indicators_w_lower_geogs) filter(., str_detect(def_period, "Aggregated")) #select the aggregated data
      else filter(., str_detect(def_period, "Survey year "))} %>% # select the un-aggregated data
    select(ind_id, year, code, numerator, rate, upci, lowci, trend_axis, def_period) %>%
    unique() %>%
    arrange(code, year)
  
  write.csv(main_data_final, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  
  # 2 - population groups data (i.e. data behind population groups tab)
  # Contains single year Scotland data and lower geog aggregated data 
  # Can use single year and aggregated data in the popgrp tab (as will only ever be comparing across data with the same geog)
  pop_grp_data <- shes_combined %>% 
    filter(indicator == ind) %>% 
    filter(split_name!="Deprivation (SIMD)") %>%
    filter((areatype=="Scot" & str_detect(def_period, "Survey year ")) |  #select the un-aggregated data for Scotland
             (areatype!="Scot" & str_detect(def_period, "Aggregated"))) %>%   # select the aggregated data for lower geogs
    select(ind_id, year, code, split_name, split_value, numerator, rate, upci, lowci, trend_axis, def_period) %>%
    mutate(split_value = factor(split_value, 
                                levels = c("Total", "0 to 4y","2 to 4y", "4 to 11y", "4 to 8y", "5 to 11y", "9 to 12y", "12 to 15y",  
                                           "16 to 64y", "65y and over", 
                                           "16-24", "25-34","35-44","45-54","55-64","65-74","75+",
                                           "1", "1 - highest income","2","3","4", "5", "5 - lowest income","Female","Male",            
                                           "No long-term illness", "Long-term illness"),
                                labels = c("Total", "0 to 4y","2 to 4y", "4 to 11y", "4 to 8y", "5 to 11y", "9 to 12y", "12 to 15y",  
                                           "16 to 64y", "65y and over", 
                                           "16-24y", "25-34y","35-44y","45-54y","55-64y","65-74y","75y+",
                                           "1", "1 - highest income","2","3","4", "5", "5 - lowest income","Female","Male",            
                                           "No long-term illness", "Long-term illness"))) %>%
    arrange(code, year, split_name, split_value)
  
  # Save
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  
  # Process SIMD data
  simd_data <- shes_combined %>% 
    filter(indicator == ind) %>% 
    filter(split_name=="Deprivation (SIMD)") %>%
    filter(!(areatype=="Scot" & str_detect(def_period, "Aggregated "))) %>% # for Scotland data, keep only the single year data
    select(ind_id, year, code, sex, split_name, split_value, numerator, rate, upci, lowci, trend_axis, def_period) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin") %>%
    select(-split_name) %>%
    arrange(code, year, quintile)
  
  # get arguments for the add_population_to_quintile_level_data() function: (done because the ind argument to the current function is not the same as the ind argument required by the next function)
  ind_name <- ind # dataset will already be filtered to a single indicator based on the parameter supplied to 'prepare final files' function
  ind_id <- unique(simd_data$ind_id) # identify the indicator number 
  
  # get the right age groups for the inequalities calculation:
  age_under16y <- c(30130, 30129) # all children included in "child with a parent with..." indicators
  age_4to12y <- c(99117, 30170, 30172, 30173, 30174, 30175) # all SDQ indicators pertain to 4-12 y olds
  age_5to15y <- c(14003, 14006, 14007, 14012) # these child PA indicators restricted to 5-15y olds
  age_2to15y <- c(30111) # child PA >1hr pertains to 2-15y olds
  
  agegp_pop <- ifelse(ind %in% age_under16y, "depr_pop_under16",
                      ifelse(ind %in% age_4to12y, "depr_pop_4to12",
                             ifelse(ind %in% age_2to15y, "depr_pop_2to15", 
                                    ifelse(ind %in% age_5to15y, "depr_pop_5to15", "depr_pop_16+")))) # default is adult (16+)  
  
  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop="depr_pop_16+",ind = ind_id,ind_name = ind_name) |>
    filter(!is.na(rate)) # some data biennial so not all years have data
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data_final
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
}

# Run function to create final files
prepare_final_files(ind = "self_assessed_health")
prepare_final_files(ind = "limiting_long_term_condition")
prepare_final_files(ind = "adult_healthy_weight")
prepare_final_files(ind = "food_insecurity")
prepare_final_files(ind = "fruit_veg_consumption")
prepare_final_files(ind = "common_mh_probs")
prepare_final_files(ind = "mental_wellbeing") 
prepare_final_files(ind = "physical_activity")
prepare_final_files(ind = "alc_binge_drinking")
prepare_final_files(ind = "haz_or_harmful_drinker")
prepare_final_files(ind = "alc_consumption_units")
prepare_final_files(ind = "unpaid_caring") 
prepare_final_files(ind = "life_satisfaction")  
prepare_final_files(ind = "cyp_parent_w_ghq4")    
prepare_final_files(ind = "cyp_parent_w_harmful_alc")
prepare_final_files(ind = "involved_locally")  
prepare_final_files(ind = "support_network") 
prepare_final_files(ind = "stress_at_work")
prepare_final_files(ind = "choice_at_work")   
prepare_final_files(ind = "line_manager") 
prepare_final_files(ind = "depression_symptoms")  
prepare_final_files(ind = "anxiety_symptoms")  
prepare_final_files(ind = "deliberate_selfharm")   
prepare_final_files(ind = "attempted_suicide")
prepare_final_files(ind = "work-life_balance")
prepare_final_files(ind = "cyp_pa_over_1h_per_day")
prepare_final_files(ind = "cyp_sdq_totaldiffs") #note SDQ = strengths and difficulties questionnaire, indicator name "Children's behavioural and emotional difficulties"
prepare_final_files(ind = "cyp_sdq_peer")
prepare_final_files(ind = "cyp_sdq_conduct")
prepare_final_files(ind = "cyp_sdq_hyperactivity")
prepare_final_files(ind = "cyp_sdq_emotional")
prepare_final_files(ind = "cyp_sdq_prosocial")
prepare_final_files(ind = "meeting_muscle_strengthening_recommendations")
prepare_final_files(ind = "adults_very_low_activity")
prepare_final_files(ind = "children_very_low_activity")
prepare_final_files(ind = "children_participating_sport")
prepare_final_files(ind = "children_active_play")
prepare_final_files(ind = "children_meet_pa_recs_excl_school")


# Run QA reports 
# main data
run_qa(type = "main", filename = "self_assessed_health", test_file = FALSE) 
run_qa(type = "main", filename = "limiting_long_term_condition", test_file = FALSE) # small diffs largely due to decimal places 
run_qa(type = "main", filename = "food_insecurity", test_file = FALSE) 
run_qa(type = "main", filename = "common_mh_probs", test_file = FALSE) 
run_qa(type = "main", filename = "mental_wellbeing", test_file = FALSE) 
run_qa(type = "main", filename = "physical_activity", test_file = FALSE) 
run_qa(type = "main", filename = "fruit_veg_consumption", test_file = FALSE) 
run_qa(type = "main", filename = "adult_healthy_weight", test_file = FALSE) 
run_qa(type = "main", filename = "alc_binge_drinking", test_file = FALSE)  
run_qa(type = "main", filename = "haz_or_harmful_drinker", test_file = FALSE)  
run_qa(type = "main", filename = "alc_consumption_units", test_file = FALSE)  
run_qa(type = "main", filename = "unpaid_caring", test_file = FALSE)  
run_qa(type = "main", filename = "life_satisfaction", test_file = FALSE)   
run_qa(type = "main", filename = "involved_locally", test_file = FALSE)   
run_qa(type = "main", filename = "support_network", test_file = FALSE)  
run_qa(type = "main", filename = "stress_at_work", test_file = FALSE) 
run_qa(type = "main", filename = "choice_at_work", test_file = FALSE)    
run_qa(type = "main", filename = "line_manager", test_file = FALSE)  
run_qa(type = "main", filename = "depression_symptoms", test_file = FALSE)   
run_qa(type = "main", filename = "anxiety_symptoms", test_file = FALSE)   
run_qa(type = "main", filename = "deliberate_selfharm", test_file = FALSE)    
run_qa(type = "main", filename = "attempted_suicide", test_file = FALSE) 
run_qa(type = "main", filename = "work-life_balance", test_file = FALSE) 
run_qa(type = "main", filename = "cyp_parent_w_ghq4", test_file = FALSE)     
run_qa(type = "main", filename = "cyp_parent_w_harmful_alc", test_file = FALSE) 
run_qa(type = "main", filename = "cyp_pa_over_1h_per_day", test_file = FALSE)
run_qa(type = "main", filename = "cyp_sdq_totaldiffs", test_file = FALSE) 
run_qa(type = "main", filename = "cyp_sdq_peer", test_file = FALSE)
run_qa(type = "main", filename = "cyp_sdq_conduct", test_file = FALSE)
run_qa(type = "main", filename = "cyp_sdq_hyperactivity", test_file = FALSE)
run_qa(type = "main", filename = "cyp_sdq_emotional", test_file = FALSE)
run_qa(type = "main", filename = "cyp_sdq_prosocial", test_file = FALSE)
run_qa(type = "main", filename = "meeting_muscle_strengthening_recommendations", test_file = FALSE)
run_qa(type = "main", filename = "adults_very_low_activity", test_file = FALSE)
run_qa(type = "main", filename = "children_very_low_activity", test_file = FALSE)
run_qa(type = "main", filename = "children_participating_sport", test_file = FALSE)
run_qa(type = "main", filename = "children_active_play", test_file = FALSE)
run_qa(type = "main", filename = "children_meet_pa_recs_excl_school", test_file = FALSE)


# ineq data: 
run_qa(type = "deprivation", filename = "self_assessed_health", test_file=FALSE)
run_qa(type = "deprivation", filename = "limiting_long_term_condition", test_file=FALSE)
run_qa(type = "deprivation", filename = "adult_healthy_weight", test_file=FALSE)
run_qa(type = "deprivation", filename = "food_insecurity", test_file=FALSE)
run_qa(type = "deprivation", filename = "fruit_veg_consumption", test_file=FALSE)
run_qa(type = "deprivation", filename = "common_mh_probs", test_file=FALSE) # no 2023 or 2024: why?
run_qa(type = "deprivation", filename = "mental_wellbeing", test_file=FALSE) 
run_qa(type = "deprivation", filename = "physical_activity", test_file=FALSE)
run_qa(type = "deprivation", filename = "alc_binge_drinking", test_file=FALSE)
run_qa(type = "deprivation", filename = "haz_or_harmful_drinker", test_file=FALSE) # PAF data for this indicator a bit peculiar since gradient isn't simple (quintile 4 highest) - consider excluding just the PAF dat afor this indicator on next update? 
run_qa(type = "deprivation", filename = "alc_consumption_units", test_file=FALSE)
run_qa(type = "deprivation", filename = "unpaid_caring", test_file = FALSE) 
run_qa(type = "deprivation", filename = "life_satisfaction", test_file = FALSE)  
run_qa(type = "deprivation", filename = "involved_locally", test_file = FALSE)  
run_qa(type = "deprivation", filename = "support_network", test_file = FALSE) 
run_qa(type = "deprivation", filename = "stress_at_work", test_file = FALSE)
run_qa(type = "deprivation", filename = "choice_at_work", test_file = FALSE)   
run_qa(type = "deprivation", filename = "line_manager", test_file = FALSE) 
run_qa(type = "deprivation", filename = "depression_symptoms", test_file = FALSE)  
run_qa(type = "deprivation", filename = "anxiety_symptoms", test_file = FALSE)  
run_qa(type = "deprivation", filename = "deliberate_selfharm", test_file = FALSE)   
run_qa(type = "deprivation", filename = "attempted_suicide", test_file = FALSE)
run_qa(type = "deprivation", filename = "work-life_balance", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_parent_w_ghq4", test_file = FALSE)    
run_qa(type = "deprivation", filename = "cyp_parent_w_harmful_alc", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_pa_over_1h_per_day", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_totaldiffs", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_peer", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_conduct", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_hyperactivity", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_emotional", test_file = FALSE)
run_qa(type = "deprivation", filename = "cyp_sdq_prosocial", test_file = FALSE)
run_qa(type = "deprivation", filename = "meeting_muscle_strengthening_recommendations", test_file = FALSE)
run_qa(type = "deprivation", filename = "adults_very_low_activity", test_file = FALSE)
run_qa(type = "deprivation", filename = "children_very_low_activity", test_file = FALSE)
run_qa(type = "deprivation", filename = "children_participating_sport", test_file = FALSE)
run_qa(type = "deprivation", filename = "children_active_play", test_file = FALSE)
run_qa(type = "deprivation", filename = "children_meet_pa_recs_excl_school", test_file = FALSE)

# popgrp data: 
run_qa(type = "popgrp", filename = "self_assessed_health", test_file=FALSE)
run_qa(type = "popgrp", filename = "limiting_long_term_condition", test_file=FALSE)
run_qa(type = "popgrp", filename = "adult_healthy_weight", test_file=FALSE)
run_qa(type = "popgrp", filename = "food_insecurity", test_file=FALSE)
run_qa(type = "popgrp", filename = "fruit_veg_consumption", test_file=FALSE)
run_qa(type = "popgrp", filename = "common_mh_probs", test_file=FALSE)
run_qa(type = "popgrp", filename = "mental_wellbeing", test_file=FALSE) 
run_qa(type = "popgrp", filename = "physical_activity", test_file=FALSE)
run_qa(type = "popgrp", filename = "alc_binge_drinking", test_file=FALSE)
run_qa(type = "popgrp", filename = "haz_or_harmful_drinker", test_file=FALSE) 
run_qa(type = "popgrp", filename = "alc_consumption_units", test_file=FALSE)
run_qa(type = "popgrp", filename = "unpaid_caring", test_file = FALSE) 
run_qa(type = "popgrp", filename = "life_satisfaction", test_file = FALSE)  
run_qa(type = "popgrp", filename = "involved_locally", test_file = FALSE)  
run_qa(type = "popgrp", filename = "support_network", test_file = FALSE) 
run_qa(type = "popgrp", filename = "stress_at_work", test_file = FALSE)
run_qa(type = "popgrp", filename = "choice_at_work", test_file = FALSE)   
run_qa(type = "popgrp", filename = "line_manager", test_file = FALSE) 
run_qa(type = "popgrp", filename = "depression_symptoms", test_file = FALSE)  
run_qa(type = "popgrp", filename = "anxiety_symptoms", test_file = FALSE)  
run_qa(type = "popgrp", filename = "deliberate_selfharm", test_file = FALSE)   
run_qa(type = "popgrp", filename = "attempted_suicide", test_file = FALSE)
run_qa(type = "popgrp", filename = "work-life_balance", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_parent_w_ghq4", test_file = FALSE)    
run_qa(type = "popgrp", filename = "cyp_parent_w_harmful_alc", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_pa_over_1h_per_day", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_totaldiffs", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_peer", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_conduct", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_hyperactivity", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_emotional", test_file = FALSE)
run_qa(type = "popgrp", filename = "cyp_sdq_prosocial", test_file = FALSE)
run_qa(type = "popgrp", filename = "meeting_muscle_strengthening_recommendations", test_file = FALSE)
run_qa(type = "popgrp", filename = "adults_very_low_activity", test_file = FALSE)
run_qa(type = "popgrp", filename = "children_very_low_activity", test_file = FALSE)
run_qa(type = "popgrp", filename = "children_participating_sport", test_file = FALSE)
run_qa(type = "popgrp", filename = "children_active_play", test_file = FALSE)
run_qa(type = "popgrp", filename = "children_meet_pa_recs_excl_school", test_file = FALSE)


#END







