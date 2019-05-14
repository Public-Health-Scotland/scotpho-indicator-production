# ScotPHO indicators: Quit attempts at 4 weeks

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
quit_4weeks <- read_csv(paste0(data_folder, "Received Data/quit_attempts_4weeks_2018.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "numerator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

#the total number of quit attempts is the denominator 
quit_total <- read_csv(paste0(data_folder, "Received Data/quit_attempts_total_2018.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "denominator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

# merging numerator and denominator
quit_4weeks <- left_join(quit_4weeks, quit_total, by = c("year", "la_name"))

# converting council names into codes. First bring lookup.
ca_lookup <- read_csv("/conf/linkage/output/lookups/geography/Codes_and_Names/Council Area 2011 Lookup.csv") %>% 
  setNames(tolower(names(.))) %>% rename(ca=councilarea2011code)

quit_4weeks <- left_join(quit_4weeks, ca_lookup, 
                          by = c("la_name" = "councilarea2011name")) %>% 
  select(-la_name)

saveRDS(quit_4weeks, file=paste0(data_folder, 'Prepared Data/quitattempts_4weeks_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "quitattempts_4weeks", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2017, time_agg = 1)

analyze_second(filename = "quitattempts_4weeks", measure = "percent", time_agg = 1, 
               ind_id = 1536, year_type = "financial", profile = "TB", min_opt = 105773)

##END