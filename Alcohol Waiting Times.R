# ScotPHO indicators: Alcohol Waiting Times

#   Part 1 - Create basefile
#   Part 2 - Format  Basefile for macro
#   Part 3 - Call analysis macros
library(tidyverse)
###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
AWT_data_raw <- read_excel(paste0(data_folder, "Received Data/Alcohol_waiting_times_2019.xlsx"), 
                               sheet = "Alcohol_waiting_times") %>% 
  setNames(tolower(names(.)))   #variables to lower case

saveRDS(AWT_data_raw, file=paste0(data_folder, 'Prepared Data/Alcohol_waiting_times_raw.rds'))

###############################################.
## Part 2 - Format  Basefile for macro ----
###############################################.

# Compute Scotland numerator denominator
AWT_data_scotland <- AWT_data_raw %>% filter(str_detect(code, "S08")) %>% mutate(code_s = "S00000001") %>% 
  select(area, denominator, numerator, year, code_s) %>% mutate(area = "Scotland") %>% rename("code" = "code_s") %>%  group_by(area, year, code) %>%
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% ungroup() 

AWT_data_formatted <- full_join(AWT_data_raw, AWT_data_scotland) %>% mutate(type = "percent", time = "single years")

saveRDS(AWT_data_formatted, file=paste0(data_folder, 'Temporary/Alcohol_waiting_times_formatted.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.

#analyze_second(filename = "Alcohol_waiting_times", measure = "percent", time_agg = 1, 
#               ind_id = 4119, year_type = "financial", profile = "AL", min_opt = 1005872)


analyze_second(filename = "Alcohol_waiting_times", measure = "percent", 
               time_agg = 1, ind_id = 4119, year_type = "financial")

##END
