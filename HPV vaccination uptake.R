# ScotPHO indicators: HPV vaccination in S3 girls

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
# Reading data provided by immunisations team
# Data requested in more than one file 
# Data for 2009/10 to 2016/17
hpv_data_old <- read.spss(paste0(data_folder, "Received Data/HPV_raw_2009_2016.sav"), 
                          to.data.frame=TRUE, use.value.labels=FALSE)

# Data for 2017/18
hpv_data_17 <- read.spss(paste0(data_folder, "Received Data/2018_HPVuptakeAndRates.sav"), 
                      to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% rename(datazone = datazone2011) %>% 
  filter(substr(datazone,1,3) == "S01") %>% 
  mutate(year = 2017) %>% #so it follows standard in functions
  select(-rates, -schoolyear_ending) 

# Data for 2018/19
hpv_data_18 <- read.spss(paste0(data_folder, "Received Data/2019_HPVuptakeAndRates.zsav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% rename(datazone = geography) %>% 
  filter(substr(datazone,1,3) == "S01") %>% 
  mutate(year = 2018) %>% #so it follows standard in functions
  select(-rates, -schoolyear_ending) 

# Data for 2019/20
hpv_data_19 <- read_rds(paste0(data_folder, "Received Data/2020 - HPV uptake.rds")) %>% 
                         #to.data.frame=TRUE, use.value.labels=FALSE) 
  setNames(tolower(names(.))) %>% rename(datazone = geography) %>% 
  filter(substr(datazone,1,3) == "S01") %>% 
  mutate(year = 2019) %>% #so it follows standard in functions
  select(-rates, -schoolyear_ending) 

hpv_data <- rbind(hpv_data_19, hpv_data_18, hpv_data_17, hpv_data_old) #merging both together

saveRDS(hpv_data, file=paste0(data_folder, 'Prepared Data/hpv_uptake_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "hpv_uptake", geography = "datazone11", measure = "percent", 
              yearstart = 2009, yearend = 2019, time_agg = 3)

analyze_second(filename = "hpv_uptake", measure = "percent", time_agg = 3, 
               ind_id = 13032, year_type = "school")

#Deprivation analysis function
analyze_deprivation(filename="hpv_uptake", measure="percent", time_agg=3, 
                    yearstart= 2014, yearend=2019,   year_type = "school", 
                    ind_id = 13032)

##END