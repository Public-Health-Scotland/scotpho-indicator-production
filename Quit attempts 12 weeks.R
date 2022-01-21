# ScotPHO indicators: Quit attempts at 12 weeks

#   Part 1 - Create basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data extracted from table from Smoking cessation annual publication
quit_12weeks <- read_csv(paste0(data_folder, "Received Data/quit_attempts_12weeks_2020.csv")) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  gather("year", "numerator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

#the total number of quit attempts is the denominator 
quit_total <- read_csv(paste0(data_folder, "Received Data/quit_attempts_total_2020.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "denominator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

# merging numerator and denominator
quit_12weeks <- left_join(quit_12weeks, quit_total, by = c("year", "la_name"))

# converting council names into codes. First bring lookup.
ca_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds") %>% 
  setNames(tolower(names(.))) %>% rename(ca=code)

quit_12weeks <- left_join(quit_12weeks, ca_lookup, 
                          by = c("la_name" = "areaname")) %>% 
  select(-la_name)

saveRDS(quit_12weeks, file=paste0(data_folder, 'Prepared Data/quitattempts_12weeks_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "quitattempts_12weeks", geography = "council", hscp = T,
              measure = "percent", yearstart = 2009, yearend = 2019, time_agg = 1)

analyze_second(filename = "quitattempts_12weeks", measure = "percent", time_agg = 1, 
               ind_id = 1537, year_type = "financial")

##END