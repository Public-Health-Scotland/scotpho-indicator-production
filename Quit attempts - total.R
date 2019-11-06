# ScotPHO indicators: Quit attempts total number

#   Part 1 - Create basefile
#   Part 2 - Run analysis function
#   Part 3 - Format the file for Shiny tool

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data extracted from table from Smoking cessation annual publication
quit_total <- read_csv(paste0(data_folder, "Received Data/quit_attempts_total_2019.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "numerator", -la_name) %>% #from wide to long format
  mutate(year = substr(year,1,4))

# converting council names into codes. First bring lookup.
ca_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds") %>% 
  setNames(tolower(names(.))) %>% rename(ca=code)

quit_total <- left_join(quit_total, ca_lookup, 
                          by = c("la_name" = "areaname")) %>% 
  select(-la_name) %>% 
  mutate(denominator = NA_integer_) # needed for the function

saveRDS(quit_total, file=paste0(data_folder, 'Prepared Data/quitattempts_total_raw.rds'))

###############################################.
## Part 2 - Run first analysis functions ----
###############################################.
# to generate geographical aggregations
analyze_first(filename = "quitattempts_total", geography = "council", 
              measure = "percent", yearstart = 2009, yearend = 2018, time_agg = 1)

###############################################.
## Part 3 - Format the file for Shiny tool ----
###############################################.
quit_total <- readRDS(file=paste0(data_folder, "Temporary/quitattempts_total_formatted.rds")) %>% 
  #creating labels and indicator id
  mutate(trend_axis = paste0(year, "/", substr(year+1, 3, 4)),
         def_period = paste0(trend_axis, " financial year"),
         ind_id = 1505) %>% 
  mutate_at(c("lowci", "upci", "rate"),  ~NA_real_) %>% #empty for tool
  select(-denominator) %>% 
  filter(!is.na(code)) #taking out empty rows

#Preparing data for Shiny tool. Including both rds and csv file for now
saveRDS(quit_total, paste0(data_folder, "Shiny Data/quitattempts_total_shiny.rds"))
write_csv(quit_total, paste0(data_folder, "Shiny Data/quitattempts_total_shiny.csv"))

##END