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
  
  saveRDS(pop_data, paste0(data_folder, "Temporary/", filename, "_formatted.rds"))

}

###############################################.
## Part 1 - Creating files for each indicator ----
###############################################.
format_pops("pop_under16")
format_pops("pop_under18")
format_pops("pop_16to64")
format_pops("pop_65to74")
format_pops("pop_65+")
format_pops("pop_75+")
format_pops("pop_85+")
format_pops("pop_16+")
format_pops("pop_18+")
format_pops("pop_under26")
format_pops("pop_16to25")
format_pops("pop_under1")
format_pops("pop_1to4")
format_pops("pop_5to15")
format_pops("pop_16to39")
format_pops("pop_40to64")

###############################################.
## Part 2 - Running analysis function for each indicator ----
###############################################.
# Population 0-15.
analyze_second(filename = "pop_under16", measure = "percent", year_type = "calendar",
              time_agg = 1, ind_id = 20002, profile = "PO", min_opt = 1006439)
# Population 0-17 (under 18).
analyze_second(filename = "pop_under18", measure = "percent", year_type = "calendar",
              time_agg = 1, ind_id = 4162, profile = "PO", min_opt = 1006439)
# Population 0-25.
analyze_second(filename = "pop_under26", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 13101, profile = "PO", min_opt = 1006439)
# Population 16-25.
analyze_second(filename = "pop_16to25", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 13105, profile = "PO", min_opt = 1006439)
# Population under 1.
analyze_second(filename = "pop_under1", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 13102, profile = "PO", min_opt = 1006439)
# Population 1-4.
analyze_second(filename = "pop_1to4", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 13103, profile = "PO", min_opt = 1006439)
# Population 5-15.
analyze_second(filename = "pop_5to15", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 13104, profile = "PO", min_opt = 1006439)
# Population 18 plus.
analyze_second(filename = "pop_18+", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 4161, profile = "PO", min_opt = 1006439)
# Population 16 plus.
analyze_second(filename = "pop_16+", measure = "percent", year_type = "calendar",
              time_agg = 1, ind_id = 20004, profile = "PO", min_opt = 1006439)
# Population 16-39.
analyze_second(filename = "pop_16to39", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 1502, profile = "PO", min_opt = 1006439)
# Population 40-64.
analyze_second(filename = "pop_40to64", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 1503, profile = "PO", min_opt = 1006439)
# Population 16-64.
analyze_second(filename = "pop_16to64", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 20003, profile = "PO", min_opt = 1006439)
# Population 65 to 74.
analyze_second(filename = "pop_65to74", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 20005, profile = "PO", min_opt = 1006439)
# Population 65 plus.
analyze_second(filename = "pop_65+", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 1504, profile = "PO", min_opt = 1006439)
# Population 75 plus.
analyze_second(filename = "pop_75+", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 20006, profile = "PO", min_opt = 1006439)
# Population 85 plus.
analyze_second(filename = "pop_85+", measure = "percent", year_type = "calendar",
               time_agg = 1, ind_id = 20007, profile = "PO", min_opt = 1006439)

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

