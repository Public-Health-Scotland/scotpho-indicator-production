# ScotPHO indicators: Quit attempts at 4 weeks (including quintile indicators)

#   Part 1 - Create basefile for general indicator
#   Part 2 - Create basefiles for quintile indicators
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile for general indicator ----
###############################################.

#Reading data extracted from table from Smoking cessation annual publication
quit_4weeks <- read_csv(paste0(data_folder, "Received Data/quit_attempts_4weeks_2020.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "numerator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

#the total number of quit attempts is the denominator 
quit_total <- read_csv(paste0(data_folder, "Received Data/quit_attempts_total_2020.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "denominator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

# merging numerator and denominator
quit_4weeks <- left_join(quit_4weeks, quit_total, by = c("year", "la_name"))

# converting council names into codes. First bring lookup.
ca_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds") %>% 
  setNames(tolower(names(.))) %>% rename(ca=code)

quit_4weeks <- left_join(quit_4weeks, ca_lookup, by = c("la_name" = "areaname")) %>% 
  select(-la_name)

saveRDS(quit_4weeks, file=paste0(data_folder, 'Prepared Data/quitattempts_4weeks_raw.rds'))

###############################################.
## Part 2 - Create basefiles for quintile indicators ----
###############################################.

# Reading council quintile data requested to smoking cessation team
quit4w_quint <- read.spss(paste0(data_folder, "Received Data/Smoking_Cessation_Council_SIMD_FY2009-10 to FY2019-20.sav"),
                          to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  filter(scsimdquintile != 99) %>% #excluding unknown values
  rename(ca = ca2011, numerator = four_week_quit, denominator = quit_attempt, year = finyear) %>% 
  mutate(year = substr(year,1,4))

for (quint in 1:5) { #creating files for each one of the quintiles
  quit4w_quint_raw <- quit4w_quint %>% filter(scsimdquintile == quint) %>% 
    select(-scsimdquintile) 
  
  saveRDS(quit4w_quint_raw, 
          paste0(data_folder, "Prepared Data/quitattempts_4weeks_quint", quint, "_raw.rds"))
  
}

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "quitattempts_4weeks", geography = "council", hscp = T,
              measure = "percent", yearstart = 2009, yearend = 2019, time_agg = 1)

analyze_second(filename = "quitattempts_4weeks", measure = "percent", time_agg = 1, 
               ind_id = 1536, year_type = "financial")

# For quintile indicators 
# Names of the files used in the next two functions
filenames <- c("quitattempts_4weeks_quint1", "quitattempts_4weeks_quint2",
               "quitattempts_4weeks_quint3","quitattempts_4weeks_quint4",
               "quitattempts_4weeks_quint5") 

mapply(analyze_first, filename = filenames, geography = "council", 
       measure = "percent", yearstart = 2009, yearend = 2019, time_agg = 1)

mapply(analyze_second, filename = filenames, measure = "percent", time_agg = 1, qa = F,
       ind_id = c(1539:1543), year_type = "financial")


##END