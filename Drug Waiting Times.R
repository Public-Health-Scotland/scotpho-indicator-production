# ScotPHO indicators: Drug Waiting Times

#   Part 1 - Create basefile
#   Part 2 - Format  Basefile for macro
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by DWT team
dwt_data_raw <- read_excel(paste0(data_folder, "Received Data/Drug_waiting_times_2019.xlsx"), 
                               sheet = "Drug_waiting_times") %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(-area) %>% #variable not needed
  mutate(year = as.numeric(year)) #converting to numeric as needed for functions

saveRDS(dwt_data_raw, file=paste0(data_folder, 'Prepared Data/Drug_waiting_times_raw.rds'))

###############################################.
## Part 2 - Format  Basefile for macro ----
###############################################.
# Compute Scotland numerator denominator
dwt_data_scotland <- dwt_data_raw %>% filter(substr(code, 1, 3) == "S08") %>% 
  mutate(code = "S00000001") %>% group_by(year, code) %>%
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) %>% ungroup() 
                               
dwt_data_formatted <- rbind(dwt_data_raw, dwt_data_scotland) 

saveRDS(dwt_data_formatted, file=paste0(data_folder, 'Temporary/Drug_waiting_times_formatted.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.
analyze_second(filename = "Drug_waiting_times", measure = "percent", 
               time_agg = 1, ind_id = 4136, year_type = "financial")


##END
