# ScotPHO indicators: Quit attempts from pregnant smokers
# Denominator data - Number of all women recoded as a 'current smoker' 
# at antenatal booking appointment (maternity team).
# 
#   Part 1 - Create basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
# Reading Quit attempts from pregnant smokers data provided by smoking team
quit_pregnant_num <- read.spss( paste0(data_folder, "Received Data/Smoking_Cessation_Pregnant_women_FY2009-FY2017.sav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% 
  rename(ca = ca2011, numerator = pregnant_quit_attempt) %>% 
  # formatting year as needed
  mutate(year = as.numeric(substr(finyear, 1, 4)),
  # recoding code to NA so it works fine through analysis functions
         ca = na_if(ca, "S12000099")) %>% select(-finyear)

# Reading population file provided by mataernity team (also used for smoking during pregnancies numerator). 
quit_pregnant_den <- read_csv(file=paste0(data_folder, 'Received Data/IR2019-00231_smoking_pregnancy.csv')) %>% 
  setNames(tolower(names(.))) %>% 
# the year variable refers to the year end e.g. 2004 = 2003/2004.
# Change this to fit the method used for profiles 
  mutate(year = finyear - 1) %>% select(-finyear, known_status)

# Bring ca lookup
ca_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_1.5.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(datazone2011, ca2011) %>% distinct() %>% 
  rename(c)

# Merging with CA lookup and aggregating denominator
quit_pregnant_den <- left_join(quit_pregnant_den, ca_lookup, 
                               by = "datazone2011") %>% 
  rename(datazone = datazone2011,  ca = ca2011) %>% 
  group_by(ca, year) %>% 
  summarise(denominator = sum(smoker)) %>% ungroup() %>% 
  subset(year > 2008)   #data for numerator only from 2009/10 onwards

# Merging denominator and numerator
quit_pregnant <- left_join(quit_pregnant_den, quit_pregnant_num, 
                           by = c("ca", "year")) 

saveRDS(quit_pregnant, file=paste0(data_folder, 'Prepared Data/quitattempts_pregnant_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "quitattempts_pregnant", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2017, time_agg = 3)

analyze_second(filename = "quitattempts_pregnant", measure = "percent", time_agg = 1, 
               ind_id = 1526, year_type = "financial", profile = "TB", min_opt = 105773)

##END