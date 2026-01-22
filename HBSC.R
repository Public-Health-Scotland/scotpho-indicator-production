# To do:

# Get HBSC to provide more guidance on WHO5 mental wellbeing score, 2018 and 2022:
# score given in 2022, but only positive/low/depression risk in 2018 (and who_3cat and who_low in 2018 seem counter-intuitively coded: pos mood =risk or dep and vice versa...) How to use these comparably?

# Look into the Family Affluence Scale as possible split: not easily comparable across years as yet.





######################################################################
# Health Behaviour in School-aged Children (HBSC) survey data import
######################################################################

# Originally, 6 of the CYP MHIs were intended to be sourced from the HBSC, and 29 from the new HWB Census.
# In 2025 we decided that the HWB Census data would not be presented as there is only one data point, it is not complete for Scotland, and we don't know when it will be repeated, if it is.
# A further 18 of the 29 HWB Census indicators can be obtained from the HBSC. 
# This gives a time series, although data collection is only every 4 years, and is Scotland wide (no sub-national breakdowns).
# The survey is asked of pupils in P7, S2 and S4. 
# Scottish P7 pupils are approx. age 11 and in the final year of primary school. 
# S2 are approx. 13 years old and S4 approx. 15 years old.

# Data source:
# Requested from Joanna.Inchley@glasgow.ac.uk.
# Latest data (to 2022) transferred in Feb 2025.
# Next survey = 2026

# HBSC team advise:
# start at 1998 as prior to that were not full national samples
# no suppression needed
# weights started to be used from 2010 (or 2014 for weights that equated the contribution of the 3 grades, so that the CIs for whole sample estimates could reflect this)
# HBSC still present weighted and unweighted estimates together. I will check to see if this looks OK. Preliminary checks suggest it is OK. 
# adjust for clustered survey design: pupils within schools within strata. The team advised which variables to use in each year. 


### CYP mental health indicators: 

# 30100	Children's mental wellbeing =	Mean mental wellbeing score (WHO-5 wellbeing index). Derived from responses to five statements about how the respondent has been feeling during the past two weeks. Items = have felt cheerful and in good spirits / I have felt calm and relaxed / I have felt active and vigorous / I woke up feeling fresh and rested / My daily life has been filled with things that interest me. A score of 50 or less is classified as low mood.  (2018 and 2022 only)
# 30104	Children reporting high life satisfaction =	% of pupils reporting high life satisfaction (a score of 6 or more from 0 (worst possible life) to 10 (best possible life))
# 30112	Children getting sufficient sleep =	% of pupils getting at least 8 hours sleep on a school night, on average. Calculated from their reported usual bedtime and waking time on a schoolday.
# 30113	Children's sleep quality score =	Adolescent Sleep Wake Scale (ASWS) mean score. Derived from responses to 10 items covering bedtime behaviours, sleep efficiency, and morning wakefulness. 
# 30114	Children assessing their general health as good or excellent =	% of pupils who perceive their health in general to be good or excellent (as opposed to fair or poor).
# 30115	Children with limiting long-term conditions =	% of pupils reporting an illness, disability or medical condition that affects their attendance or participation in school.
# 30116	Children with problematic social media usage = % of pupils with problematic social media use, based on responses to 9 items. Problematic usage defined as scoring 6+ out of 9.
# During the past year, have you… regularly found that you can’t think
# of anything else but the moment that you will be able to use social media again? / ...regularly felt dissatisfied because you wanted to
# spend more time on social media? / ..often felt bad when you could not use social media? / ..tried to spend less time on social media
# but failed?/..regularly neglected other activities (e.g hobbies, sport) because you wanted to use social media? / ..regularly had arguments
# with others because of your social media use? / ..regularly lied to your parents or friends about the amount of time you spend on social
# media?/..often used social media to escape from negative feelings? /  ...had serious conflict with your parents, brother(s) or sister (s)
# because of your social media use?
# 30117	Children thinking they are overweight =	% of pupils that think their body is a bit overweight or very overweight. (NB. Pre-2022 the wording included 'fat' rather than 'overweight' so is not comparable.)
# 30124	Children finding it easy to talk to parents =	% of pupils reporting it is easy/very easy to at least one of their parents about things that really bother them. (combination of variables including step parents)
# 30126	Children reporting high level of family support =	% of pupils reporting having high level of family support. Based on the family subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My family really tries to help me / I get the emotional help and support I need from my family / I can talk about my problems with my family / My family is willing to help me make decisions). A score of 5.5+ classified as high family support. (original MHI was 'enjoy living with family')
# 30133	Children reporting high level of peer support =	% of pupils reporting having high level of peer support. Based on the peer subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My friends really try to help me / I can count on my friends when things go wrong / I have friends with whom I can share my joys and sorrows / I can talk about my problems with my friends.). A score of 5.5+ classified as high peer support.
# 30134	Children rarely feeling lonely =	% of pupils reporting never or rarely feeling lonely in past year.
# 30137	Children feeling accepted by classmates =	% of pupils who strongly agree or agree that other pupils accept them as they are.
# 30138	Children being bullied at school =	% of pupils reporting having been bullied at school at least 2-3 times a month in the past couple of months.
# 30139	Children being cyberbullied =	% of pupils reporting having been cyberbullied at least 2-3 times a month in the past couple of months. Examples given: someone sent mean instant messages, email or text messages
# about you; wall postings; created a website making fun of you; posted unflattering or inappropriate pictures of you online without permission or shared them with others)
# 30141	Children liking school =	% of pupils who like school a lot or a bit at present 
# 30142	Children reporting high level of teacher support =	% of pupils reporting they receive a high level of teacher support. Based on 3 items (I feel that my teachers accept me as I am / I feel that my teachers care about me as a person / I feel a lot of trust in my teachers.). Score of 10+ out of 12 classified as high teacher support. 
# 30144	Children feeling pressure of school work =	% of pupils reporting feeling a lot of schoolwork pressure.
# 30148	Children participating in clubs, groups or organisations each week =	% of pupils participating in organised leisure activities each week (i.e., activities carried out under the leadership of a coach, teacher, instructor or leader). 
# 30149	Children having a trusted adult =	% of pupils who have an adult they can trust and talk to about any personal problems (sometimes or always)
# 30150	Children feeling safe in their neighbourhood =	% of pupils who report always feeling safe in the area they live in. 
# 30163	Children experiencing discrimination from adults =	% of pupils who report often or very often being treated unfairly by adults because of their sex, ethnicity, or socioeconomic status. 
# 30164	Children thinking their neighbourhood is a good place to live =	% of pupils who think that the area in which they live is a good place to live

# now sourced from SHeS instead of HBSC (although percentages are quite different due to different info source and way in which questions answered):
# SHeS deemed to be a more comprehensive (covered more ages) sample and perhaps more reliable question on which to base activity levels on
# 30111	Children meeting physical activity guidelines =	% of pupils reporting doing 1 hour or more of physical activity every day


### functions/packages -----

source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA
library(haven) # for reading in .sav files
library(survey) # analysing data from a clustered survey design


### 1. Read in data ----

# Identify data folder
hbsc_data_folder <- paste0(profiles_data_folder, "/Received Data/Health Behaviour in School-Aged Children/data transfer Feb 2025/")

# Identify data files 
hbsc_data_files <- paste0(hbsc_data_folder, list.files(path = hbsc_data_folder, pattern = "hbscscot"))

# function to identify which vars are labelled (so we can use the labels as factor levels)
is.havenlab <- function(x) "haven_labelled" %in% class(x)

# Read in population groups data files
hbsc_data_list <- lapply(hbsc_data_files, # read in each file in turn
                         function(x) read_sav(x, # read in the .sav file (function from haven package)
                                              encoding = "latin1") %>% # encoding needs setting or the import throws an error
                           mutate(across(where(is.havenlab), haven::as_factor)) %>% # if the var has labels convert it to a factor (using the labels) 
                           mutate(trend_axis = str_extract(x, "([0-9]+)_phs_mh\\.sav$", group=1)) %>% # extracts the digits (the year) preceding the "_phs_mh.sav" suffix in the filename
                           setNames(tolower(names(.))) %>% # make all var names lower case
                           mutate(across(everything(), ~tolower(.)))) %>% # make all data lower case
  plyr::rbind.fill() %>%   # bind all the files together: copes with different columns (adds NA if that col not present): we can coalesce any cols that have same definition
  filter(!trend_axis %in% c("1990", "1994")) # not using before 1998


# cross tabulate years and variables, to see what's available when  
hbsc_vars_by_year <- hbsc_data_list %>%
  mutate(across(-trend_axis,  ~ !is.na(.x))) %>%
  unique() %>%
  pivot_longer(-trend_axis, names_to="var") %>%
  filter(value==TRUE) %>%
  unique() %>%
  pivot_wider(names_from = trend_axis, values_from = value) %>%
  arrange(var)
# save and open in excel to match with the indicator requirements, and work out which vars we need to keep
write.csv(hbsc_vars_by_year, paste0(hbsc_data_folder, "hbsc_vars_by_year.csv"))




###############################################
# Define lookups for recoding the variables:
###############################################


# Function for extracting responses to a single column data frame
get_responses <- function(var) {
  hbsc_data_list %>% select(all_of(var)) %>% arrange(var) %>% unique() %>% rename(responses = var)
}


# Sex
################################
sex_cols <- c("sex", "sexbirth_s", "gender", "gender_p", "gender_binary_s", "genderid_s")
sex_cats <- lapply(sex_cols, get_responses) %>% unlist(use.names=FALSE) %>% unique()
# [1] "boy"                         "girl"                        NA                            "female"                      "male"                       
# [6] "in another way"              "i identify myself as a girl" "i identify myself as a boy"  "other/s"                     "neither boy nor girl"      

# For recoding the sex vars:
lookup_sex <- list(
  "girl"="Female",
  "female"="Female",                      
  "i identify myself as a girl"="Female", 
  "boy"="Male",
  "male"="Male",                       
  "i identify myself as a boy"="Male",  
  "in another way"="Other",              
  "other/s"="Other",                     
  "neither boy nor girl"="Other"
)


# Ease of talking to parents:
################################

parental_comms <- c("talkfather", "talkmother", "talkstepfa", "talkstepmo", 
                    "talkstpf", "talkstpm", "talktoma", "talktopa")
parental_responses <- lapply(parental_comms, get_responses) %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                                    "easy"                                "dont have\\see this person"          "very easy"                          
# [5] "difficult"                           "very difficult"                      "don't have or don't see this person" "don't have or see this person"      
# [9] "don't have or see"                   "dont have or see"                    "no such person"                      "dont have/see this person"          

# For recoding any var in parental_comms:
lookup_parent_comms <- list(
  "difficult"="no",
  "very difficult"="no",
  "easy"="yes",
  "very easy"="yes"
)


# Experienced discrimination from adults:
############################################

adult_discrim <- c("discrborn_othad",
                   "discrborn_teacher",
                   "discrgender_othad",
                   "discrgender_teacher",
                   "discrses_othad",
                   "discrses_teacher")
discrim_responses <- lapply(adult_discrim, get_responses) %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                           "no discrimation"            "experienced descrimination"

# For recoding any var in adult_discrim:
lookup_discrim <- list(
  "no discrimation"="no",
  "experienced descrimination"="yes"
)

# Does organised leisure activities weekly:
############################################

leisure_activities <- c("ls_arts",
                        "ls_club",
                        "ls_indsport",
                        "ls_relig",
                        "ls_teamsp",
                        "ls_youth")
leisure_responses <- lapply(leisure_activities, get_responses) %>%
  unlist(use.names=FALSE) %>% unique()
# [1] NA                                             "twice a week or more"                         "about once or twice a month"                 
# [4] "once a week"                                  "i donâ\u0080\u0099t do this type of activity"

# For recoding any var in leisure_activities:
lookup_leisure <- list(
  "about once or twice a month"="no",
  "i donâ\u0080\u0099t do this type of activity"="no",
  "twice a week or more"="yes",
  "once a week"="yes"
)


# Life satisfaction:
###################################

lifesat_responses <- get_responses("lifesat") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                      "best possible life"    "6"                     "9"                     "7"                     "5"                    
# [7] "1"                     "worst possible life"   "8"                     "4"                     "3"                     "2"                    
# [13] "10 best possible life" "0 worst possible life"

# For recoding lifesat:
lookup_lifesat <- list(
  "0 worst possible life"="no",
  "worst possible life"="no",
  "1"="no",  
  "2"="no",  
  "3"="no",  
  "4"="no",  
  "5"="no",  
  "6"="yes",  
  "7"="yes",                 
  "8"="yes",  
  "9"="yes",                                                                              
  "10 best possible life"="yes", 
  "best possible life"="yes"
)



# General health good/excellent:
###################################

health_responses <- get_responses("health") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA          "excellent" "good"      "fair"      "poor"     

# For recoding health:
lookup_health <- list(
  "excellent"="yes", 
  "good"="yes",      
  "fair"="no",      
  "poor"="no"     
)


# Problematic social media usage:
###################################

d_emc_problem_responses <- get_responses("d_emc_problem") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA    "non-problematic social media user" "problematic social media user"         

# For recoding "d_emc_problem":
lookup_d_emc_problem <- list(
  "problematic social media user"="yes",      
  "non-problematic social media user"="no"     
)

# Body image:
###################################

thinkbody_1_responses <- get_responses("thinkbody_1") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA    "neither underweight nor overweight" "a bit overweight"                   "a bit underweight"                 
#  [5] "very overweight"                    "very underweight"     

# For recoding thinkbody_1:
lookup_thinkbody_1 <- list(
  "very overweight"="yes",                    
  "a bit overweight"="yes",                   
  "a bit underweight"="no",                 
  "neither underweight nor overweight"="no", 
  "very underweight" ="no"     
)

# Lonely:
###################################

lonely_responses <- get_responses("lonely") %>% unlist(use.names=FALSE) %>% unique()
#  "never"            "sometimes"        "often"            NA                 "rarely"           "most of the time" "always"       

# For recoding lonely:
lookup_lonely <- list(
  "never"="yes", 
  "rarely"="yes",             
  "sometimes"="no",        
  "often"="no",
  "most of the time"="no", 
  "always"="no"     
)


# Accepted by classmates:
###################################

studaccept_responses <- get_responses("studaccept") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA                           "agree a lot"                "agree a bit"                "disagree a lot"             "neither agree nor disagree"
#  [6] "disagree a bit"             "strongly agree"             "agree"                      "strongly disagree"          "disagree"             

# For recoding studaccept:
lookup_studaccept <- list(
  "strongly agree"="yes",             
  "agree a lot"="yes",                
  "agree a bit"="yes",                
  "agree"="yes",                      
  "neither agree nor disagree"="no",
  "disagree a lot"="no",             
  "disagree a bit"="no",             
  "strongly disagree"="no",          
  "disagree"="no"             
)


# Bullied/cyberbullied:
###################################

bullied_responses <- lapply(c("beenbullied", "cbeenbullied"), get_responses) %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                                                               "several times/week"                                            
# [3] "havent been bullied"                                            "once/twice"                                                    
# [5] "about once/week"                                                "sometimes"                                                     
# [7] "once\\twice"                                                    "about once\\week"                                              
# [9] "several times\\week"                                            "i haven't been bullied in the past couple of months"           
# [11] "about once a week"                                              "it has only happened once or twice"                            
# [13] "several times a week"                                           "2 or 3 times a month"                                          
# [15] "2-3 times a month"                                              "haven't"                                                       
# [17] "once or twice"                                                  "2 or 3 times per month"                                        
# [19] "once a week"                                                    "2-3 times per month"                                           
# [21] "once/week"                                                      "i have not been bullied at school in the past couple of months"
# [23] "it has happened once or twice"                                  "i have not been cyberbullied in the past couple of months"         

# For recoding "beenbullied" or "cbeenbullied":
lookup_bullied <- list(
  "several times/week"="yes",
  "several times a week"="yes",
  "several times\\week"="yes",  
  "once a week"="yes",
  "once/week"="yes",
  "about once/week"="yes",  
  "about once\\week"="yes",
  "about once a week"="yes",                                              
  "2-3 times a month"="yes", 
  "2 or 3 times a month"="yes",
  "2 or 3 times per month"="yes",
  "2-3 times per month"="yes",
  
  "sometimes"="no",
  "once/twice"="no",
  "once\\twice"="no",                                                    
  "it has only happened once or twice"="no",
  "once or twice"="no",
  "it has happened once or twice"="no",                                  
  "i have not been bullied at school in the past couple of months"="no",
  "haven't"="no",
  "havent been bullied"="no",                                            
  "i haven't been bullied in the past couple of months"="no",
  "i have not been cyberbullied in the past couple of months"="no"
)

# Like school:
###################################

likeschool_responses <- get_responses("likeschool") %>% unlist(use.names=FALSE) %>% unique()
# [1] "like it a bit"                         "like it a lot"                         "dont like it at all"                   "dont like it v. much"                 
# [5] NA                                      "dont like it v.much"                   "i like it a lot"                       "i like it a bit"                      
# [9] "i don't like it very much"             "i don't like it at all"                "like a bit"                            "not very much"                        
# [13] "not at all"                            "like a lot"                            "i donâ\u0080\u0099t like it very much" "i donâ\u0080\u0099t like it at all"   

# For recoding likeschool:
lookup_likeschool <- list(
  "like it a bit"="yes",
  "like it a lot"="yes",                         
  "i like it a lot"="yes",
  "i like it a bit"="yes",     
  "like a bit"="yes",
  "like a lot"="yes",
  "dont like it at all"="no",                   
  "dont like it v. much"="no",                 
  "dont like it v.much"="no",                                    
  "i don't like it very much"="no",             
  "i don't like it at all"="no",                                            
  "not very much"="no",
  "not at all"="no",
  "i donâ\u0080\u0099t like it very much"="no", 
  "i donâ\u0080\u0099t like it at all"="no"   )


# Schoolwork pressure:
###################################

schoolpressure_responses <- get_responses("schoolpressure") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA           "a little"   "some"       "not at all" "a lot"  

# For recoding schoolpressure:
lookup_schoolpressure <- list(
  "a little"="no",   
  "some"="no",       
  "not at all"="no", 
  "a lot"="yes"   )


# Trusted adult:
###################################

trusted_adult_responses <- get_responses("trusted_adult") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                        "yes, i always do"        "yes, i sometimes do"     "no, i don't"             "no, i donâ\u0080\u0099t"

# For recoding trusted_adult:
lookup_trusted_adult <- list(
  "yes, i always do"="yes",        
  "yes, i sometimes do"="yes",     
  "no, i don't"="no",             
  "no, i donâ\u0080\u0099t"="no"   )


# Feel safe in local area:
###################################

area_safe <- get_responses("area_safe") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                 "most of the time" "sometimes"        "all of the time"  "never"            "hardly ever"      "always"           "rarely or never" 

# For recoding area_safe:
lookup_area_safe <- list(
  "always"="yes",           
  "all of the time"="yes",  
  "most of the time"="no", 
  "sometimes"="no",        
  "never"="no",            
  "hardly ever"="no",      
  "rarely or never"="no"    )


# Feel area is a good place to live:
###################################

area_sat <- get_responses("area_sat") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                         "yes, it's really good"    "it's ok"                  "it's not very good"       "yes, it's good"           "no, it's not good at all"
# [7] "yes, really good"         "yes, good"                "not very good"            "ok"                       "not good at all"         

# For recoding area_sat:
lookup_area_sat <- list(
  "yes, it's really good"="yes",     
  "yes, really good"="yes",
  "yes, it's good"="yes",
  "yes, good"="yes",
  "it's ok"="no",                  
  "ok"="no",                       
  "it's not very good"="no",                   
  "not very good"="no",            
  "no, it's not good at all"="no",
  "not good at all"="no" 
)


# Limiting long-term conditions:
###################################
# identify children with a long-term condition affecting their school attendance/participation.
# Requires identifying those answering yes to having a long term condition (ccsqlong = yes) and to it affecting school (ccsqlong = yes AND ccsqschool = yes).
# Children are only asked to answer ccsqschool if they answer yes to ccsqlong.
# denominator will therefore be ccsqlong==no + ccsqschool!=yes

llti_responses <- get_responses("ccsqlong") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA    "no"  "yes"

llti_school_responses <- get_responses("ccsqschool") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA                                                                  
# [2] "do not have"                                                       
# [3] "yes"                                                               
# [4] "no"                                                                
# [5] "i do not have a long-term illness, disability or medical condition"

# No lookup required here.


# Variables that can be from their numbers (either apply a cut-off value, or present as a mean):

# # Physical activity: NOW SOURCED FROM SHES INSTEAD
# ###################################
# physact_responses <- get_responses("physact60") %>% unlist(use.names=FALSE) %>% unique()
# # [1] NA        "7 days"  "4 days"  "3 days"  "5 days"  "0 days"  "1 day"   "6 days"  "2 days"  " 6 days" " 7 days" " 5 days" " 4 days" " 2 days" " 3 days"


# Schoolday sleep hours:
###################################
sleepdur_responses <- get_responses("schooldays_sleep_hrs") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA     "6.5"  "9.5"  "7.5"  "8.5"  "10"   "8"    "6"    "7"    "4"    "5"    "9"    "4.5"  "5.5"  "10.5" "3.5"  "11"   "3"    "12"  


# Sleep quality:
###################################
sleepqual_responses <- get_responses("sleepqual_tot") %>% unlist(use.names=FALSE) %>% unique()
# lots of numbers stored as strings plus "high quality" and "poor quality"
# found out that 1 = "poor quality" and 6 = "high quality" so can recode these then convert to numeric





# Recode the variables
hbsc_data <- hbsc_data_list %>%  
  
  # Variables with categorical recoding:
  
  # one var at a time:
  mutate(area_safe = recode(area_safe, !!!lookup_area_safe, .default = as.character(NA))) %>%
  mutate(area_sat = recode(area_sat, !!!lookup_area_sat, .default = as.character(NA))) %>%
  mutate(d_emc_problem = recode(d_emc_problem, !!!lookup_d_emc_problem, .default = as.character(NA))) %>%
  mutate(health = recode(health, !!!lookup_health, .default = as.character(NA))) %>%
  mutate(lifesat = recode(lifesat, !!!lookup_lifesat, .default = as.character(NA))) %>%
  mutate(likeschool = recode(likeschool, !!!lookup_likeschool, .default = as.character(NA))) %>%
  mutate(lonely = recode(lonely, !!!lookup_lonely, .default = as.character(NA))) %>%
  mutate(schoolpressure = recode(schoolpressure, !!!lookup_schoolpressure, .default = as.character(NA))) %>%
  mutate(studaccept = recode(studaccept, !!!lookup_studaccept, .default = as.character(NA))) %>%
  mutate(thinkbody_1 = recode(thinkbody_1, !!!lookup_thinkbody_1, .default = as.character(NA))) %>%
  mutate(trusted_adult = recode(trusted_adult, !!!lookup_trusted_adult, .default = as.character(NA))) %>%
  
  # multiple vars that use the same lookup:
  mutate(across(c(beenbullied, cbeenbullied), ~recode(., !!!lookup_bullied, .default = as.character(NA)))) %>%
  mutate(across(any_of(sex_cols), ~recode(., !!!lookup_sex, .default = as.character(NA)))) %>%
  mutate(across(any_of(parental_comms), ~recode(., !!!lookup_parent_comms, .default = as.character(NA)))) %>%
  mutate(across(any_of(adult_discrim), ~recode(., !!!lookup_discrim, .default = as.character(NA)))) %>%
  mutate(across(any_of(leisure_activities), ~recode(., !!!lookup_leisure, .default = as.character(NA)))) %>%
  
  # variables with numeric recoding:
  # mutate(physact60 = parse_number(physact60)) %>% # extract the number (there are no responses with no number)
  # mutate(physact60 = case_when(physact60==7 ~ "yes", 
  #                              physact60<7 ~ "no",
  #                              TRUE ~ as.character(NA))) %>%
  mutate(schooldays_sleep_hrs = parse_number(schooldays_sleep_hrs)) %>% # extract the number (there are no responses with no number)
  mutate(schooldays_sleep_hrs = case_when(schooldays_sleep_hrs>=8 ~ "yes", 
                                          schooldays_sleep_hrs<8 ~ "no",
                                          TRUE ~ as.character(NA))) %>%
  
  # variables used for mean scores:
  mutate(sleepqual_tot = case_when(sleepqual_tot == "poor quality" ~ 1, # recoding the extremes of this numeric scale 
                                   sleepqual_tot == "high quality" ~ 6,
                                   TRUE ~ as.numeric(sleepqual_tot))) %>%
  mutate(sleepqual_tot = as.numeric(sleepqual_tot)) %>%
  mutate(across(c("d_family_support", "d_peer_support", "d_student_support", "d_teacher_support"), as.numeric)) %>%
  # family_support and peer_support are already mean scores on the original scale of 1 to 7.
  # student and teacher support scales need some adjustment as these are sums:
  mutate(student_support = (d_student_support + 3 / 3)) %>% # the var in the data is a sum of responses to 3 items that have been recoded from 0 to 4. We want a mean score on the original scale (1 to 5), hence we add 1 for each item, and divide by the number of items.
  mutate(teacher_support = (d_teacher_support + 3 / 3)) %>% # as above
  
  # create indicators based on multiple vars:
  
  ## gender/sex:
  ## sex_all takes the binary sex if reported, and fills in any missing with the non-binary response given.
  ## done because non-binary only became an option in 2022 data, and sometimes binary sex/gender is missing as a result. 
  mutate(sex_all = case_when(!is.na(sex) ~ sex, # most frequently provided, binary (asked since 1990, missing in 2010, 2014, 2022; Q = are you a girl or a boy?)
                             !is.na(gender) ~ gender, # next most frequently provided, binary (var name used in 2010 and 2014, despite question being same as the sex question: are you a girl or a boy?)
                             !is.na(sexbirth_s) ~ sexbirth_s, # next most frequently provided, binary (were you registered girl/boy at birth?)
                             !is.na(gender_binary_s) ~ gender_binary_s, # next most frequently provided, binary (2022: boy/girl?)
                             !is.na(genderid_s) ~ genderid_s, # non-binary, 2022
                             !is.na(gender_p) ~ gender_p, # non-binary, 2022
                             TRUE ~ as.character(NA))) %>% # leaves ~100 with NA, 
  # issue with sex_all = will have classed a small number of individuals as boys who were born girls, and vice versa, 
  # because this is how they identify (and were only given the chance to give this response in 2022)
  
  ## derive sex from only the binary questions: 
  mutate(sex_binary = case_when(!is.na(sex) ~ sex, # most frequently provided, binary (asked since 1990, missing in 2010, 2014, 2022; Q = are you a girl or a boy?)
                                !is.na(gender) ~ gender, # next most frequently provided, binary (var name used in 2010 and 2014, despite question being same as the sex question: are you a girl or a boy?)
                                !is.na(sexbirth_s) ~ sexbirth_s, # next most frequently provided, binary (were you registered girl/boy at birth?)
                                !is.na(gender_binary_s) ~ gender_binary_s, # next most frequently provided, binary (2022: boy/girl?)
                                TRUE ~ as.character(NA))) %>% # leaves ~1500 with NA, all from 2022 (over a third of responses that year).
  # ER: My preference would be using the binary variable while there's only one year of non-binary data. 
  # Values not split by sex can use all the data. Revisit after the next data update (2026).  
  
  ## standardise grade variable 
  mutate(grade = coalesce(grade, class), # class was used in 2002
         grade = case_when(grade %in% c("primary 7", "p7") ~ "Primary 7",
                           grade %in% c("secondary 2", "s2") ~ "Secondary 2",
                           grade %in% c("secondary 4", "s4") ~ "Secondary 4",
                           TRUE ~ as.character(NA))) %>%
  
  ## set parent_comms = "yes" if they said they found talking to any parent easy
  mutate(parent_comms = case_when(if_any(all_of(parental_comms), ~ . == "yes") ~ "yes", # yes if answered yes to at least one var. Applied first, so not overwritten by second clause
                                  if_any(all_of(parental_comms), ~ . == "no") ~ "no", # no if they've answered no to at least one var (and not answered yes to any var)
                                  TRUE ~ as.character(NA))) %>%
  
  ## set discrim = "yes" if they had experienced any discrimination from adults
  mutate(discrim = case_when(if_any(all_of(adult_discrim), ~ . == "yes") ~ "yes", # yes if answered yes to at least one var. Applied first, so not overwritten by second clause
                             if_any(all_of(adult_discrim), ~ . == "no") ~ "no", # no if they've answered no to at least one var (and not answered yes to any var)
                             TRUE ~ as.character(NA))) %>%
  
  ## set leisure = "yes" if they had participated in any of the organised leisure activities at least weekly
  mutate(leisure = case_when(if_any(all_of(leisure_activities), ~ . == "yes") ~ "yes", # yes if answered yes to at least one var. Applied first, so not overwritten by second clause
                             if_any(all_of(leisure_activities), ~ . == "no") ~ "no", # no if they've answered no to at least one var (and not answered yes to any var)
                             TRUE ~ as.character(NA))) %>%
  
  ## set llti = "yes" if they reported having a long term condition AND that it affected their attendance and participation at school
  mutate(llti = case_when(ccsqlong == "yes" & ccsqschool == "yes" ~ "yes",
                          ccsqlong == "yes" & ccsqschool != "yes" ~ "no",
                          ccsqlong == "no"  ~ "no",
                          TRUE ~ as.character(NA))) %>%
  
  ## get dataset_weights all called the same 
  mutate(dataset_weight = as.numeric(coalesce(dataset_weight, weighting)), # 'weighting' was used in 2010, HBSC team confirm this is same as 'dataset_weight'
         dataset_weight = ifelse(is.na(dataset_weight), 1, dataset_weight)) %>% # prior to 2010: no weights, so replace missings with 1 (equal weighting)
  # 1998, 2002 and 2006 won't have weights.
  
  ## whole sample weights (equal weights to the grades)
  mutate(dataset_weight_equating_grade = as.numeric(coalesce(dataset_weight_equating_grade, dataset_weight_equigrade)),
         dataset_weight_equating_grade = ifelse(is.na(dataset_weight_equating_grade), 1, dataset_weight_equating_grade)) %>% # prior to 2014: no weights, so replace missings with 1 (equal weighting)
  # available 2014, 2018 and 2022
  
  ## Survey design vars: pupils nested within schools nested with strata
  # HBSC team advised on which vars to use in each year
  mutate(id_pupil = as.numeric(coalesce(id, serial, id_pupil))) %>% 
  mutate(id_school = as.numeric(coalesce(school, id2, id_school))) %>% 
  mutate(id_strata = case_when(schooltype %in% c("state", "1") & trend_axis == "1998" ~ as.numeric(as.factor(id1)),
                               schooltype %in% c("state", "1") & trend_axis == "2002" ~ as.numeric(as.factor(region)),
                               schooltype %in% c("independent", "2") & trend_axis %in% c("2002", "1998") ~ 33,
                               TRUE ~ as.numeric(id_strata))) %>% 
  mutate(id_strata = ifelse(is.na(id_strata), 1, id_strata)) %>% # prior to 2014: no weights, so replace missings with 1 (equal weighting)
  
  ## keep the vars we need
  select(trend_axis, sex_all, sex = sex_binary, grade,
         area_safe, area_sat, d_emc_problem,
         health, lifesat, likeschool, lonely, schoolpressure, 
         studaccept, thinkbody_1, trusted_adult, beenbullied, cbeenbullied,
         #physact60, 
         schooldays_sleep_hrs, parent_comms, discrim, leisure, llti,
         sleepqual_tot, d_family_support, d_peer_support, student_support, teacher_support,
         dataset_weight, dataset_weight_equating_grade,
         id_pupil, psu=id_school, strata=id_strata)



##########################################################################################
# Calculate the indicators
##########################################################################################

# Functions to run the survey calcs (survey package) on the survey data, by various groupings

# These functions prep the data and perform calculations for percentage and score indicators from a microdata file from a survey with a complex survey design.
# Adapted from the functions used for the SHeS and Understanding Society data in the ScotPHO_survey_data repo

# Survey design vars: pupils (id_pupil) nested within schools (id_school) nested with strata (id_strata)




# Function to run the three shared functions for a single indicator

calc_single_breakdown <- function (df, var, wt, variables, type) {
  
  df1 <- prep_df_for_svy_calc(df, var, wt, variables, type)
  df2 <- run_svy_calc(df1, variables, var, type)
  df3 <- add_more_required_cols(df, var, df2, variables, type)
  
}

# Function to get the data ready for the survey calculation.
# The groupings required are passed as a vector to 'variables' 
# If the indicator is a % (type == "percent") then groupings with no positive cases are removed (step not needed for score indicators)

prep_df_for_svy_calc <- function(df, var, wt, variables, type) {
  
  svy_df <- df %>%
    # rename the wt to "wt" if not renamed that already:
    rename_with( ~ case_when(. == wt ~ "wt",
                             . == var ~ "var",
                             TRUE ~ .)) %>%
    filter(!is.na(var)) %>%
    filter(!is.na(wt)) %>%
    select(all_of(variables), var, wt, psu, strata)
  
  if(type == "percent") {
    
    svy_df <- svy_df %>%
      group_by(across(all_of(variables))) %>%
      mutate(count_n = sum(var=="yes", na.rm=TRUE)) %>%
      ungroup() %>%
      filter(count_n>0) %>%  # drop groups with no cases (N but no n) (breaks the survey calc otherwise)
      select(all_of(variables), var, wt, psu, strata)
  }
  
  svy_df
  
}



# Function to run the survey calculation for the indicator
# Specifies the survey design, and runs the right model, depending on whether the indicator type is percent or score. 

run_svy_calc <- function(df, variables, var, type) {
  
  # single-PSU strata are centred at the sample mean
  options(survey.lonely.psu="adjust") 
  
  # specify the complex survey design
  svy_design <- svydesign(id=~psu,
                          strata=~strata,
                          weights=~wt,
                          data=df,
                          nest=TRUE) #different strata might have same psu numbering (which are different psu)
  
  if (type == "percent") {
    
    # Calculate % and CIs 
    percents <- data.frame(svyby(~I(var=="yes"), 
                                 reformulate(termlabels = variables), # turns the variables vector c(x, y, z) into the rhs formula ~x+y+z
                                 svy_design, 
                                 svyciprop, ci=TRUE, vartype="ci", method="logit")) %>% # produces CIs appropriate for proportions, using the logit method 
      # (this fits a logistic regression model and computes a Wald-type interval on the log-odds scale, which is then transformed to the probability scale)
      mutate(rate = I.var.....yes.. * 100, #resulting estimate has very unwieldly name!
             lowci = ci_l * 100,
             upci = ci_u * 100,
             indicator = var) %>%
      select(all_of(variables), indicator, rate, lowci, upci)
    
  } else if (type == "score") {
    
    # Calculate mean scores and CIs
    scores <- data.frame(svyby(~var, 
                               reformulate(termlabels = variables), 
                               svy_design, svymean, 
                               ci=TRUE, vartype="ci")) %>% 
      rename(rate = var,
             lowci = ci_l,
             upci = ci_u) %>%
      mutate(indicator = var) %>%
      select(all_of(variables), indicator, rate, lowci, upci)
    
    
  } else {
    
    "Error: type should be score or percent"
    
  }
  
}

# Function to add numerators and denominators into the dataset
# Also adds spatial.unit, spatial.scale and quintile columns

add_more_required_cols <- function(df, var, svy_result, variables, type) {
  
  # Options:
  # variables <- c("trend_axis")
  # variables <- c("trend_axis", "sex)
  # variables <- c("trend_axis", "grade")
  
  # add numerators and denominators (including zeroes if present)
  
  if (type == "percent") {
    
    results <- df %>%
      filter(!is.na(var)) %>%
      group_by(across(all_of(variables))) %>%
      summarize(numerator = sum(.data[[var]]=="yes", na.rm=TRUE),
                denominator = sum(!is.na(.data[[var]]))) %>% # includes situations where no positive cases (these were dropped for the survey analysis, but need to be retained)
      ungroup() %>%
      merge(y = svy_result, by = variables) 
    
  } else if (type == "score") {
    
    results <- df %>%
      filter(!is.na(var)) %>%
      group_by(across(all_of(variables))) %>%
      summarize(denominator = sum(!is.na(.data[[var]]))) %>% 
      ungroup() %>%
      mutate(numerator = as.numeric(NA)) %>%
      merge(y = svy_result, by = variables) 
    
  } else {
    
    "Error: type should be score or percent"
    
  }
  
}  

# Function for calling the required functions:

calc_indicator_data <- function (df, var, ind_id, type) {
  
  # Scotland 
  results1 <- calc_single_breakdown(df, var, wt="dataset_weight_equating_grade", variables = c("trend_axis"), type) %>%
    mutate(split_value = "Total",
           split_name = "Total")
  # Scotland by sex
  results2 <- calc_single_breakdown(df, var, wt="dataset_weight_equating_grade", variables = c("trend_axis", "sex"), type) %>%
    rename(split_value = sex) %>%
    mutate(split_name = "Sex")
  # Scotland by grade
  results3 <- calc_single_breakdown(df, var, wt="dataset_weight", variables = c("trend_axis", "grade"), type) %>%
    rename(split_value = grade) %>%
    mutate(split_name = "School stage")
  
  
  results <- rbind(results1, results2, results3) %>%
    mutate(ind_id = ind_id) %>%
    # add year in
    mutate(year = as.numeric(trend_axis)) %>% 
    # add def_period
    mutate(def_period = paste0("Survey year (", trend_axis, ")")) %>%
    # add code for Scotland
    mutate(code = "S00000001") %>%
    # arrange so the points plot in right order in QA stage
    arrange(ind_id, code, split_name, split_value, year, trend_axis)
  
}

## Run the functions:

# Derive percentages:
cyp_lifesat <- calc_indicator_data(df = hbsc_data, var = "lifesat", ind_id = 30104, type = "percent")
#cyp_pa_guidelines <- calc_indicator_data(df = hbsc_data, var = "physact60", ind_id = 30111, type = "percent")
cyp_sufficient_sleep <- calc_indicator_data(df = hbsc_data, var = "schooldays_sleep_hrs", ind_id = 30112, type = "percent")
cyp_gen_health <- calc_indicator_data(df = hbsc_data, var = "health", ind_id = 30114, type = "percent")
cyp_llti <- calc_indicator_data(df = hbsc_data, var = "llti", ind_id = 30115, type = "percent")
cyp_socmedproblem <- calc_indicator_data(df = hbsc_data, var = "d_emc_problem", ind_id = 30116, type = "percent")
cyp_think_ovwt <- calc_indicator_data(df = hbsc_data, var = "thinkbody_1", ind_id = 30117, type = "percent")
cyp_talk_parents <- calc_indicator_data(df = hbsc_data, var = "parent_comms", ind_id = 30124, type = "percent")
cyp_lonely <- calc_indicator_data(df = hbsc_data, var = "lonely", ind_id = 30134, type = "percent")
cyp_classmate_acceptance <- calc_indicator_data(df = hbsc_data, var = "studaccept", ind_id = 30137, type = "percent")
cyp_bullied <- calc_indicator_data(df = hbsc_data, var = "beenbullied", ind_id = 30138, type = "percent")
cyp_cyberbullied <- calc_indicator_data(df = hbsc_data, var = "cbeenbullied", ind_id = 30139, type = "percent")
cyp_like_school <- calc_indicator_data(df = hbsc_data, var = "likeschool", ind_id = 30141, type = "percent")
cyp_school_pressure <- calc_indicator_data(df = hbsc_data, var = "schoolpressure", ind_id = 30144, type = "percent")
cyp_leisure_participation <- calc_indicator_data(df = hbsc_data, var = "leisure", ind_id = 30148, type = "percent")
cyp_trusted_adult <- calc_indicator_data(df = hbsc_data, var = "trusted_adult", ind_id = 30149, type = "percent")
cyp_nhood_safe <- calc_indicator_data(df = hbsc_data, var = "area_safe", ind_id = 30150, type = "percent")
cyp_discrimination <- calc_indicator_data(df = hbsc_data, var = "discrim", ind_id = 30163, type = "percent")
cyp_nhood_good <- calc_indicator_data(df = hbsc_data, var = "area_sat", ind_id = 30164, type = "percent")

# Derive mean scores:
cyp_sleep_qual <- calc_indicator_data(df = hbsc_data, var = "sleepqual_tot", ind_id = 30113, type = "score")
cyp_fam_support <- calc_indicator_data(df = hbsc_data, var = "d_family_support", ind_id = 30126, type = "score")
cyp_peer_support <- calc_indicator_data(df = hbsc_data, var = "d_peer_support", ind_id = 30133, type = "score")
cyp_teacher_support <- calc_indicator_data(df = hbsc_data, var = "teacher_support", ind_id = 30142, type = "score")



# 9. Combine all the resulting indicator data into a single file
###############################################################################

hbsc_results <- mget(ls(pattern = "^cyp_"), .GlobalEnv) %>% # finds all the dataframes produced by the functions above
  do.call(rbind.data.frame, .)  #rbinds them all together (appending the rows)
rownames(hbsc_results) <- NULL # drop the row names


# data checks:
table(hbsc_results$trend_axis, useNA = "always") # 1998 to 2022, na NA
table(hbsc_results$indicator, useNA = "always") # 23 vars , no NA
table(hbsc_results$year, useNA = "always") # 1998 to 2022, na NA
table(hbsc_results$def_period, useNA = "always") # Survey year (), no NA
table(hbsc_results$split_name, useNA = "always") # School stage, Sex, Total no NA
table(hbsc_results$split_value, useNA = "always") # M/F/Total, P7, S2, S4, no NA
# all good


# get indicator names into more informative names for using as filenames
hbsc_results <- hbsc_results %>%
  mutate(indicator = case_when( indicator == "lifesat"             ~ "cyp_lifesat",                          
                              #  indicator == "physact60"           ~ "cyp_pa_guidelines",                    
                                indicator == "schooldays_sleep_hrs"~ "cyp_sufficient_sleep",                 
                                indicator == "health"              ~ "cyp_gen_health",                       
                                indicator == "llti"                ~ "cyp_llti",                             
                                indicator == "d_emc_problem"       ~ "cyp_socmedproblem",                    
                                indicator == "thinkbody_1"         ~ "cyp_think_ovwt",                       
                                indicator == "parent_comms"        ~ "cyp_talk_parents",                     
                                indicator == "lonely"              ~ "cyp_lonely",                           
                                indicator == "studaccept"          ~ "cyp_classmate_acceptance",             
                                indicator == "beenbullied"         ~ "cyp_bullied",                          
                                indicator == "cbeenbullied"        ~ "cyp_cyberbullied",                     
                                indicator == "likeschool"          ~ "cyp_like_school",                      
                                indicator == "schoolpressure"      ~ "cyp_school_pressure",                  
                                indicator == "leisure"             ~ "cyp_leisure_participation",            
                                indicator == "trusted_adult"       ~ "cyp_trusted_adult",                    
                                indicator == "area_safe"           ~ "cyp_nhood_safe",                       
                                indicator == "discrim"             ~ "cyp_discrimination",                   
                                indicator == "area_sat"            ~ "cyp_nhood_good",  
                                indicator == "sleepqual_tot"       ~ "cyp_sleep_qual",                    
                                indicator == "d_family_support"    ~ "cyp_fam_support",                       
                                indicator == "d_peer_support"      ~ "cyp_peer_support",                   
                                indicator == "teacher_support"     ~ "cyp_teacher_support",  
                                TRUE ~ as.character(NA) )) %>%
  select(-denominator) 

# Add sex == Total 
sex_total <- hbsc_results %>%
  filter(split_name=="Total") %>%
  mutate(split_name="Sex")

# Add stage == Total 
stage_total <- hbsc_results %>%
  filter(split_name=="Total") %>%
  mutate(split_name="School stage")

all_data <- rbind(hbsc_results,
                  sex_total,
                  stage_total)

##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  
  main_data <- all_data %>% 
    filter(indicator == ind,
           split_name == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() 
  
  # Save:
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # 2 - population groups data (ie data behind population groups tab)
  
  pop_grp_data <- all_data %>% 
    filter(indicator == ind & !(split_name == "Total")) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) 
  
  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data

  
}


# Run function to create final files
prepare_final_files(ind = "cyp_lifesat")                          
#prepare_final_files(ind = "cyp_pa_guidelines")                    
prepare_final_files(ind = "cyp_sufficient_sleep")                 
prepare_final_files(ind = "cyp_gen_health")                       
prepare_final_files(ind = "cyp_llti")                             
prepare_final_files(ind = "cyp_socmedproblem")                    
prepare_final_files(ind = "cyp_think_ovwt")                       
prepare_final_files(ind = "cyp_talk_parents")                     
prepare_final_files(ind = "cyp_lonely")                           
prepare_final_files(ind = "cyp_classmate_acceptance")             
prepare_final_files(ind = "cyp_bullied")                          
prepare_final_files(ind = "cyp_cyberbullied")                     
prepare_final_files(ind = "cyp_like_school")                      
prepare_final_files(ind = "cyp_school_pressure")                  
prepare_final_files(ind = "cyp_leisure_participation")            
prepare_final_files(ind = "cyp_trusted_adult")                    
prepare_final_files(ind = "cyp_nhood_safe")                       
prepare_final_files(ind = "cyp_discrimination")                   
prepare_final_files(ind = "cyp_nhood_good")  
prepare_final_files(ind = "cyp_sleep_qual")                    
prepare_final_files(ind = "cyp_fam_support")                       
prepare_final_files(ind = "cyp_peer_support")                   
prepare_final_files(ind = "cyp_teacher_support")  

# # Run QA reports 

# # main data: 
run_qa(type ="main",filename="cyp_lifesat", test_file=FALSE)                          
#run_qa(type ="main",filename="cyp_pa_guidelines", test_file=FALSE)                    
run_qa(type ="main",filename="cyp_sufficient_sleep", test_file=FALSE)                 
run_qa(type ="main",filename="cyp_gen_health", test_file=FALSE)                       
run_qa(type ="main",filename="cyp_llti", test_file=FALSE)                             
run_qa(type ="main",filename="cyp_socmedproblem", test_file=FALSE)                    
run_qa(type ="main",filename="cyp_think_ovwt", test_file=FALSE)                       
run_qa(type ="main",filename="cyp_talk_parents", test_file=FALSE)                     
run_qa(type ="main",filename="cyp_lonely", test_file=FALSE)                           
run_qa(type ="main",filename="cyp_classmate_acceptance", test_file=FALSE)             
run_qa(type ="main",filename="cyp_bullied", test_file=FALSE)                          
run_qa(type ="main",filename="cyp_cyberbullied", test_file=FALSE)                     
run_qa(type ="main",filename="cyp_like_school", test_file=FALSE)                      
run_qa(type ="main",filename="cyp_school_pressure", test_file=FALSE)                  
run_qa(type ="main",filename="cyp_leisure_participation", test_file=FALSE)            
run_qa(type ="main",filename="cyp_trusted_adult", test_file=FALSE)                    
run_qa(type ="main",filename="cyp_nhood_safe", test_file=FALSE)                       
run_qa(type ="main",filename="cyp_discrimination", test_file=FALSE)                   
run_qa(type ="main",filename="cyp_nhood_good", test_file=FALSE)  
run_qa(type ="main",filename="cyp_sleep_qual", test_file=FALSE)                    
run_qa(type ="main",filename="cyp_fam_support", test_file=FALSE)                       
run_qa(type ="main",filename="cyp_peer_support", test_file=FALSE)                   
run_qa(type ="main",filename="cyp_teacher_support", test_file=FALSE)  

## END
