# ScotPHO indicators: Drug Waiting Times

#   Part 1 - Create basefile
#   Part 2 - Format  Basefile for macro
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
DWT_data_raw <- read_excel(paste0(data_folder, "Received Data/Drug_waiting_times_2019.xlsx"), 
                               sheet = "Drug_waiting_times") %>% 
  setNames(tolower(names(.)))   #variables to lower case

saveRDS(DWT_data_raw, file=paste0(data_folder, 'Prepared Data/Drug_waiting_times_raw.rds'))

###############################################.
## Part 2 - Format  Basefile for macro ----
###############################################.

# Compute Scotland numerator denominator
DWT_data_scotland <- DWT_data_raw %>% filter(str_detect(code, "S08")) %>% mutate(code_s = "S00000001") %>% 
  select(area, denominator, numerator, year, code_s) %>% mutate(area = "Scotland") %>% rename("code" = "code_s") %>%  group_by(area, year, code) %>%
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% ungroup() 
                               
DWT_data_formatted <- full_join(DWT_data_raw, DWT_data_scotland) %>% mutate(type = "percent", time = "single years")

saveRDS(DWT_data_formatted, file=paste0(data_folder, 'Temporary/Drug_waiting_times_formatted.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.

analyze_second(filename = "Drug_waiting_times", measure = "percent", time_agg = 1, 
               ind_id = 4136, year_type = "financial", profile = "DU", min_opt = 162970)

##END
