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


### CYP mental health indicators: 
# more info required: sleep qual score; llti; 

# 30100	Children's mental wellbeing =	Mean mental wellbeing score (WHO-5 wellbeing index). Derived from responses to five statements about how the respondent has been feeling during the past two weeks. Items = have felt cheerful and in good spirits / I have felt calm and relaxed / I have felt active and vigorous / I woke up feeling fresh and rested / My daily life has been filled with things that interest me. A score of 50 or less is classified as low mood.  (2018 and 2022 only)
# 30104	Children reporting high life satisfaction =	% of pupils reporting high life satisfaction (a score of 6 or more from 0 (worst possible life) to 10 (best possible life))
# 30111	Children meeting physical activity guidelines =	% of pupils reporting doing 1 hour or more of physical activity every day
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

# Other possibilities:
# Children reporting high level of classmate support = % of pupils reporting high levels of support from their classmates. Derived from 3 items (The pupils in my class(es) enjoy being together/ Most of the pupils in my class(es) are kind and helpful/ Other pupils accept me as I am.). Score of 10+ out of 12 classified as high classmate support. 


# Questions:
# Regions provided: can data be produced for these?
# wemwbs available? (for more than a single year?)
# which weights to use? 2014-2022 dataset_weight, 2010 weighting, 2002 and 2006 w_age, 1990-1998 w_agetyp??
# more info about FAS: is it usable over time?
# for comparability over time: use gender, sex, and gender_binary_s in 2022 (boy/girl): check this with HBSC team
# rather than continuous age, grade is better to use perhaps, tho missing in 2002. what is used in 2002?
# llti: 
# ccsqlong = yes/no/NA (2014, 2018, 2022)
# ccsqschool = Yes/No/Do not have (LT illness/disab/med cond) (2014 and 2018). 2022 just yes and no.
# What do they mean? How to use to extract 'limiting long-standing phys condition' or 'illness/disab/medical cond that affects their school attendance/pptn'?
# WHO5 mental wellbeing score, 2018 and 2022
# score given in 2022, but only positive/low/depression risk in 2018 (and who_3cat and who_low in 2018 seem counter-intuitively coded: pos mood =risk or dep and vice versa...) How to use these comparably?




### functions/packages -----

source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA
library(haven) # for reading in .sav files supplied by SG


### 1. Read in data ----

# Identify data folder
hbsc_data_folder <- paste0(profiles_data_folder, "/Received Data/Health Behaviour in School-Aged Children/data transfer Feb 2025/")

# Identify data files 
hbsc_data_files <- paste0(hbsc_data_folder, list.files(path = hbsc_data_folder, pattern = "hbscscot"))

# Read in population groups data files
hbsc_data_list <- lapply(hbsc_data_files, # read in each file in turn
                         function(x) read_sav(x, # read in the .sav file (function from haven package)
                                              encoding = "latin1") %>% # encoding needs setting or the import throws an error
                           mutate_if(~"haven_labelled" %in% class(.), as_factor) %>% # uses the SPSS labels for variables with labels
                           mutate(year = str_extract(x, "([0-9]+)_phs_mh\\.sav$", group=1)) %>% # extracts the digits (the year) preceding the "_phs_mh.sav" suffix in the filename
                           setNames(tolower(names(.))) %>% # make all var names lower case
                           mutate(across(everything(), ~tolower(.)))) %>% # make all data lower case
    plyr::rbind.fill() # bind all the files together: copes with different columns (adds NA if that col not present): we can coalesce any cols that have same definition


# cross tabulate years and variables, to see what's available when  
hbsc_vars_by_year <- hbsc_data_list %>%
  mutate(across(-year,  ~ !is.na(.x))) %>%
  unique() %>%
  pivot_longer(-year, names_to="var") %>%
  filter(value==TRUE) %>%
  unique() %>%
  pivot_wider(names_from = year, values_from = value) %>%
  arrange(var)
# save and open in excel to match with the indicator requirements, and work out which vars we need to keep
write.csv(hbsc_vars_by_year, paste0(hbsc_data_folder, "hbsc_vars_by_year.csv"))


# variable checks:

check_talk <- hbsc_data_list %>% select(year, starts_with("talk")) %>% unique()
# showed that talkfath and talkmoth are binary recodes of other vars: so not needed

check_llti <- hbsc_data_list %>% select(year, starts_with("ccsq")) %>% unique()
# ccsqlong = yes/no/NA (2014, 2018, 2022)
# ccsqschool = Yes/No/Do not have (LT illness/disab/med cond) (2014 and 2018). 2022 just yes and no.
# What do they mean? How to use to extract 'limiting long-standing phys condition' or 'illness/disab/medical cond that affects their school attendance/pptn'?

check_lonely <- hbsc_data_list %>% select(year, starts_with("lonel")) %>% unique()
# lonely (1990 and 2022) can be coerced to never/sometimes/often
# loneliness (1994, 1998) also: never/sometimes/often
# but v big gap, 1998 to 2022. Maybe don't use? (until an additional data point: 2026)

check_who <- hbsc_data_list %>% select(year, who_3cat, who_low, d_who5) %>% unique()
# WHO5 mental wellbeing score, 2018 and 2022
# score given in 2022, but only positive/low/depression risk in 2018 (and who_3cat and who_low in 2018 seem counter-intuitively coded: pos mood =risk or dep and vice versa...)
# how to use these comparably?


# how coded?


# age
check_age <- hbsc_data_list %>% select(year, w_agetyp, age, w_age, agecat) %>% unique()
# age available all but 1990. agecat gives 11/13/15y for 2014. other vars are weights.
hist(hbsc_data_list$age)
# shows the peaks at 11 (~P7), 13 (~S2) and 15y (~S4)
# grade is better to use perhaps, tho missing in 2002. what is used in 2002?

# gender and sex
check_sex <- hbsc_data_list %>% select(year, sex, sexbirth_s, gender, gender_p, gender_binary_s, genderid_s) %>% unique()
# gender 2010, 2014
# sex 1990 to 2006, 2018
# 2022 = gender_binary_s, sexbirth_s, genderid_s
# for comparability over time: use gender, sex, and gender_binary_s (boy/girl): check this with HBSC team

# region
check_region <- hbsc_data_list %>% select(year, region, la, id_region) %>% unique()
# not provided after 2002?



###############################################
# Define lookups for recoding the variables:
###############################################


# Function for extracting responses to a single column data frame
get_responses <- function(var) {
  hbsc_data_list %>% select(var) %>% arrange(var) %>% unique() %>% rename(responses = var)
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

# Variables that can be coded numerically:

# Physical activity:
###################################
physact_responses <- get_responses("physact60") %>% unlist(use.names=FALSE) %>% unique()
# [1] NA        "7 days"  "4 days"  "3 days"  "5 days"  "0 days"  "1 day"   "6 days"  "2 days"  " 6 days" " 7 days" " 5 days" " 4 days" " 2 days" " 3 days"


# Schoolday sleep hours:
###################################
sleepdur_responses <- get_responses("schooldays_sleep_hrs") %>% unlist(use.names=FALSE) %>% unique()
#  [1] NA     "6.5"  "9.5"  "7.5"  "8.5"  "10"   "8"    "6"    "7"    "4"    "5"    "9"    "4.5"  "5.5"  "10.5" "3.5"  "11"   "3"    "12"  




# need coding:

# 30100	Children's mental wellbeing =	Mean mental wellbeing score (WHO-5 wellbeing index). Derived from responses to five statements about how the respondent has been feeling during the past two weeks. Items = have felt cheerful and in good spirits / I have felt calm and relaxed / I have felt active and vigorous / I woke up feeling fresh and rested / My daily life has been filled with things that interest me. A score of 50 or less is classified as low mood.  (2018 and 2022 only)
# 30113	Children's sleep quality score =	Adolescent Sleep Wake Scale (ASWS) mean score. Derived from responses to 10 items covering bedtime behaviours, sleep efficiency, and morning wakefulness. 
# 30115	Children with limiting long-term conditions =	% of pupils reporting an illness, disability or medical condition that affects their attendance or participation in school.
# 30126	Children reporting high level of family support =	% of pupils reporting having high level of family support. Based on the family subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My family really tries to help me / I get the emotional help and support I need from my family / I can talk about my problems with my family / My family is willing to help me make decisions). A score of 5.5+ classified as high family support. (original MHI was 'enjoy living with family')
# 30133	Children reporting high level of peer support =	% of pupils reporting having high level of peer support. Based on the peer subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My friends really try to help me / I can count on my friends when things go wrong / I have friends with whom I can share my joys and sorrows / I can talk about my problems with my friends.). A score of 5.5+ classified as high peer support.
# 30142	Children reporting high level of teacher support =	% of pupils reporting they receive a high level of teacher support. Based on 3 items (I feel that my teachers accept me as I am / I feel that my teachers care about me as a person / I feel a lot of trust in my teachers.). Score of 10+ out of 12 classified as high teacher support. 
# Children reporting high level of classmate support = % of pupils reporting high levels of support from their classmates. Derived from 3 items (The pupils in my class(es) enjoy being together/ Most of the pupils in my class(es) are kind and helpful/ Other pupils accept me as I am.). Score of 10+ out of 12 classified as high classmate support. 



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
  mutate(physact60 = parse_number(physact60)) %>% # extract the number (there are no responses with no number)
  mutate(physact60 = case_when(physact60==7 ~ "yes", 
                               physact60<7 ~ "no",
                               TRUE ~ as.character(NA))) %>%
  mutate(schooldays_sleep_hrs = parse_number(schooldays_sleep_hrs)) %>% # extract the number (there are no responses with no number)
  mutate(schooldays_sleep_hrs = case_when(schooldays_sleep_hrs>=8 ~ "yes", 
                                          schooldays_sleep_hrs<8 ~ "no",
                                          TRUE ~ as.character(NA))) %>%
  
  # create indicators based on multiple vars:
  
  ## sex_r takes the binary sex if reported, and fills in any missing with the non-binary response given.
  ## done because non-binary only became an option in 2022 data, and sometimes binary sex/gender is missing as a result. 
  mutate(sex_r = case_when(!is.na(sex) ~ sex, # most frequently provided, binary
                           !is.na(gender) ~ gender, # next most frequently provided, binary
                           !is.na(sexbirth_s) ~ sexbirth_s, # next most frequently provided, binary
                           !is.na(gender_binary_s) ~ gender_binary_s, # next most frequently provided, binary
                           !is.na(genderid_s) ~ genderid_s, # non-binary
                           !is.na(gender_p) ~ gender_p, # non-binary
                           TRUE ~ as.character(NA))) %>% # leaves ~100 with NA
  
  ## standardise grade variable (and derive from age if not provided (i.e., in 2002))
  mutate(grade = case_when(grade %in% c("primary 7", "p7") ~ "Primary 7",
                           grade %in% c("secondary 2", "s2") ~ "Secondary 2",
                           grade %in% c("secondary 4", "s4") ~ "Secondary 4",
                           age<=12 ~ "Primary 7", # age is continuous but evident low counts at 12 and 14 (P7=approx 11, S2=approx 13, and S4=approx 15y)
                           age>12 & age<=14 ~ "Secondary 2",
                           age>14 ~ "Secondary 4",
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
  
  # keep the vars we need
  select(year, sex_r, grade,
         area_safe, area_sat, d_emc_problem,
         health, lifesat, likeschool, lonely, schoolpressure, 
         studaccept, thinkbody_1, trusted_adult, beenbullied, cbeenbullied,
         physact60, schooldays_sleep_hrs, parent_comms, discrim, leisure)
  ## add weights when known
  
  ## add more variables once coded:
# 30100	Children's mental wellbeing =	Mean mental wellbeing score (WHO-5 wellbeing index). Derived from responses to five statements about how the respondent has been feeling during the past two weeks. Items = have felt cheerful and in good spirits / I have felt calm and relaxed / I have felt active and vigorous / I woke up feeling fresh and rested / My daily life has been filled with things that interest me. A score of 50 or less is classified as low mood.  (2018 and 2022 only)
# 30113	Children's sleep quality score =	Adolescent Sleep Wake Scale (ASWS) mean score. Derived from responses to 10 items covering bedtime behaviours, sleep efficiency, and morning wakefulness. 
# 30115	Children with limiting long-term conditions =	% of pupils reporting an illness, disability or medical condition that affects their attendance or participation in school.
# 30126	Children reporting high level of family support =	% of pupils reporting having high level of family support. Based on the family subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My family really tries to help me / I get the emotional help and support I need from my family / I can talk about my problems with my family / My family is willing to help me make decisions). A score of 5.5+ classified as high family support. (original MHI was 'enjoy living with family')
# 30133	Children reporting high level of peer support =	% of pupils reporting having high level of peer support. Based on the peer subscale of the Multidimensional Scale of Perceived Social Support (MSPSS), and derived from 4 items (My friends really try to help me / I can count on my friends when things go wrong / I have friends with whom I can share my joys and sorrows / I can talk about my problems with my friends.). A score of 5.5+ classified as high peer support.
# 30142	Children reporting high level of teacher support =	% of pupils reporting they receive a high level of teacher support. Based on 3 items (I feel that my teachers accept me as I am / I feel that my teachers care about me as a person / I feel a lot of trust in my teachers.). Score of 10+ out of 12 classified as high teacher support. 
# Children reporting high level of classmate support = % of pupils reporting high levels of support from their classmates. Derived from 3 items (The pupils in my class(es) enjoy being together/ Most of the pupils in my class(es) are kind and helpful/ Other pupils accept me as I am.). Score of 10+ out of 12 classified as high classmate support. 

# summarise data by year, sex and grade

# Calculate the indicator
workless <- aps_data %>%
  mutate(sex="Total") %>% # recode all individuals to sex==total
  rbind(aps_data) %>% # add male and female data back in, so grouping produces values for m, f, and total.
  group_by(year, sex) %>%
  summarise(yes_grossed = sum(weight * inecac05 %in% yes_responses, na.rm = TRUE), #grossed up to population
            no_grossed = sum(weight * inecac05 %in% no_responses, na.rm = TRUE), #grossed up to population
            yes_unwted = sum(1 * inecac05 %in% yes_responses, na.rm = TRUE),
            no_unwted = sum(1 * inecac05 %in% no_responses, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(denominator = yes_unwted + no_unwted,
         proportion = yes_grossed / (yes_grossed + no_grossed),
         numerator = rnd(denominator * proportion), # calculate RESPONSE-weighted number of cases based on share of the unweighted base
         rate = 100 * proportion,
         lowci = 100 * (((2*numerator) + (1.96*1.96) - 1.96*sqrt((1.96*1.96) + (4*numerator*(1-proportion)))) / (2 * (denominator + (1.96*1.96)))), # Wilson's score method.
         upci = 100 * (((2*numerator) + (1.96*1.96) + 1.96*sqrt((1.96*1.96) + (4*numerator*(1-proportion)))) / (2 * (denominator + (1.96*1.96)))),
         year = 2000 + as.numeric(year),
         trend_axis = as.character(year),
         def_period = paste0(year, "survey year"),
         code = "S00000001",
         ind_id = 30032,
         split_name = "Sex") %>%
  rename(split_value = sex) %>%
  select(-c(proportion, no_unwted, yes_unwted, yes_grossed, no_grossed)) %>%
  arrange(ind_id, split_name, split_value, code, year)

# check on suppression requirements

