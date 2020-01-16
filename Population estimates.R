# ScotPHO indicators: 
# Population all ages. 
# Population 0-15 years 
# Population 16-64 years 
# Population 65-74 years 
# Population 75+ 
# Population 85+ 
# Population 16+ 
# Population 18+ 
# Population <18
# Population 16-39 
# Population 40-64 
# Population 65+ 
# Population 0-25 
# Population under 1 
# Population 16-25 
# Population 1-4 
# Population 5-15 
# 
# It uses the files created by the population lookups code in the lookups repo

# Part 1 - Formatting population files
# Part 2 - Calling the analysis macro
# Part 3 - All ages indicator

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

# Function to format population files as indicators
# Brings the selected numerator and denominator files, merge them together and save it
format_pops <- function(filename) {
  
  numer <- readRDS(paste0(data_folder, "Lookups/Population/DZ11_", filename, ".rds")) %>% 
    rename(numerator = denominator) 
    
  denom <- readRDS(paste0(data_folder, "Lookups/Population/DZ11_pop_allages.rds"))
  
  pop <- left_join(numer, denom, by = c("year", "code"))
  
  saveRDS(pop, paste0(data_folder, "Temporary/", filename, "_formatted.rds"))

}

###############################################.
## Part 1 - Creating files for each indicator ----
###############################################.
# defining list of indicators
file_list <- c("pop_under16", "pop_under18", "pop_16to64", "pop_65to74", 
               "pop_65+", "pop_75+", "pop_85+", "pop_16+", "pop_18+", 
               "pop_under26", "pop_16to25", "pop_under1", "pop_1to4", 
               "pop_5to15", "pop_16to39", "pop_40to64")

mapply(format_pops, file_list) #formatting all files

###############################################.
## Part 2 - Running analysis function for each indicator ----
###############################################.
mapply(analyze_second, filename = file_list, measure = "percent", 
       year_type = "calendar", time_agg = 1, qa = F,
       ind_id = c(20002, 4162, 20003, 20005, 1504, 20006, 20007, 20004, 4161, 
                  13101, 13105, 13102, 13103, 13104, 1502, 1503))

###############################################.
## Part 3 - Preparing data for all ages indicator ----
###############################################.
allages_pop <- readRDS(paste0(data_folder, "Lookups/Population/DZ11_pop_allages.rds")) %>% 
  rename(numerator = denominator) %>% 
  mutate(ind_id = 20001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, rate = NA)   # blank variables are needed

#Including both rds and csv file for now
saveRDS(allages_pop, file = paste0(data_folder, "Data to be checked/pop_allages_shiny.rds"))
write_csv(allages_pop, path = paste0(data_folder, "Data to be checked/pop_allages_shiny.csv"))

##END
