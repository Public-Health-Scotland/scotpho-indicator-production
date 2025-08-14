## notes ----

# this script produces data for indicator 13030 - school leavers from publicly funded secondary schools in positive destinations, 9 months after leaving school
# Summary Statistics for Attainment and Initial Leaver Destinations, 2025 Edition, released on 25th Feb 2025
# https://www.gov.scot/news/school-leaver-attainment-and-destinations-8/
# Script updated in Nov 2024 (by ER) to import more population group splits from the data file (previously only Scotland and CA data were imported)
# Script updated in March 2025 (by ER) to use the new indicator production paths and functions.
# Script updated Aug 2025 (by ER) to aggregate to additional geogs


###############################################.
## Part 1 - Setup ----
###############################################.


### Load functions/dependencies ----

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA function
library(openxlsx) 


### Paths ----

# Identify data folder
schdest_data_folder <- paste0(profiles_data_folder, "/Received Data/School leaver positive destinations/")
file <- "SSAILD+2025+supplementary+tables+L1.6+correction.xlsx"


### Lookups ----

# council area lookup
ca <- readRDS(paste0(profiles_lookups,"/Geography/CAdictionary.rds")) 


###############################################.
## Part 2 - Import and process Scotland data ----
###############################################.

# Scotland data, totals
scot_series <- read.xlsx(paste0(schdest_data_folder, file),
                         sheet = "L1.1",
                         colNames = TRUE,
                         startRow = 6
                         ) %>%
  rename(trend_axis = "Year.[note.19][note.20]") %>%
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         across(contains(c("Voluntary", "Activity", "Personal")), as.numeric),
         rate = rowSums(pick("Higher.Education":"Personal.Skills.Development.[note.13]"), na.rm = T),
         denominator = `Number.of.Leavers`,
         numerator = round((rate * `Number.of.Leavers`)/100)) %>%
  select(trend_axis, numerator, denominator, rate) %>%
  mutate(split_name = "Total",
         split_value = "Total")

# Scotland data, by population groups

## Function to read in population group data
import_schdest_split_data <- function(sheet, split_name) {
  
  df <- read.xlsx(paste0(schdest_data_folder, file),
                  sheet = sheet,
                  colNames = TRUE,
                  startRow = 6) %>%
    mutate(Initial.Destination = str_trim(Initial.Destination)) %>%
    filter(Initial.Destination %in% c("All Positive Destinations", "Number of Leavers")) %>%
    pivot_longer(-c(Year, Initial.Destination), names_to = "split_value", values_to = "value") %>%
    mutate(across(value, ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
           across(value, as.numeric),
           split_value = str_replace_all(split_value, "\\.", " "),
           split_value = str_to_sentence(split_value)) %>%
    pivot_wider(names_from = Initial.Destination, values_from = value) %>%
    mutate(numerator = round(`Number of Leavers` * `All Positive Destinations`/100),
           split_name = split_name) %>%
    rename(rate = `All Positive Destinations`,
           trend_axis = Year,
           denominator = `Number of Leavers`)
  
}

# Run the function to import the data for the various splits available:
# note the rows will need changed each year to ensure latest years data included
schdest_sex <- import_schdest_split_data(sheet = "L1.2", split_name = "Sex")
schdest_urbrur <- import_schdest_split_data(sheet = "L1.3", split_name = "Urban/Rural status")
schdest_ethnicity <- import_schdest_split_data(sheet = "L1.4", split_name = "Ethnicity") %>%
  mutate(split_value = str_replace_all(split_value, "/ ", "/"), # extra tidying of values needed
         split_value = str_replace(split_value, " \\[.*\\]", ""),
         split_value = str_to_title(split_value))
schdest_disabled <- import_schdest_split_data(sheet = "L1.6", split_name = "Disabled (declared or assessed)") %>%
  mutate(split_value = str_replace(split_value, "Declared or assessed disabled: ", ""),
         split_value = str_to_title(split_value))
schdest_simd <- import_schdest_split_data(sheet = "Table_2", split_name = "Deprivation (SIMD)") %>%
  mutate(split_value = case_when(split_value=="Quintile 0-20% (most deprived)"  ~ "1", # recode the splits
                                 split_value=="Quintile 20-40%" ~ "2",
                                 split_value=="Quintile 40-60%" ~ "3",
                                 split_value=="Quintile 60-80%" ~ "4",
                                 split_value=="Quintile 80-100% (least deprived)" ~ "5",
                                 split_value=="Total" ~ "Total")) %>%
  filter(!is.na(split_value))

# combine Scotland data
scot_all <- rbind(scot_series, 
                  schdest_disabled,
                  schdest_ethnicity,
                  schdest_sex,
                  schdest_urbrur,
                  schdest_simd) %>%
  mutate(code = "S00000001", # add code for Scotland
         ind_id = 13010,
         year = as.numeric(substr(trend_axis, 1, 4)),
         trend_axis = gsub("-", "/", trend_axis),
         def_period = paste0("School year (", trend_axis, ")")) %>%
  rename(rate_orig = rate) %>% #checked and it's a perfect match with calculated one, so can drop
  calculate_percent() %>%
  select(-c(denominator, rate_orig))

# make population file for school leavers by year and SIMD quintile, for inequalities metrics:
depr_pop_schoolleavers <- schdest_simd %>%
  mutate(year = as.numeric(substr(trend_axis, 1, 4))) %>%
  select(year, quintile=split_value, denominator) %>%
  mutate(code = "S00000001",
         quint_type = "sc_quin") 

saveRDS(depr_pop_schoolleavers, paste0(profiles_lookups, "/Population/depr_pop_schoolleavers.rds"))

###############################################.
## Part 3 - Import and process CA data ----
###############################################.

# Council area data, totals
ca_series <- read.xlsx(paste0(schdest_data_folder, file),
                       sheet = "L2.1",
                       colNames = TRUE,
                       startRow = 6) %>%
  filter(LA.Name!="Scotland") %>% # longer time series in the scot_series data
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         year = as.numeric(substr(Year, 1, 4)),
         rate = as.numeric(`Positive.Destination`),
         numerator = round((rate * `Number.of.leavers`)/100),
         areaname = str_replace(LA.Name, "Edinburgh, City of","City of Edinburgh"),
         areaname = str_replace(areaname, "&","and")) %>%
  merge(ca, by = "areaname") %>% # join with council area lookup
  select(year, code, numerator, denominator = `Number.of.leavers`) 

# Save ready for the main analysis function
saveRDS(ca_series, file=paste0(profiles_data_folder, '/Prepared Data/positive_destinations_ca_raw.rds'))

# Run main analysis function to aggregate and calculate rates (CA to higher geogs)
main_analysis(filename = "positive_destinations_ca", ind_id = 13010, 
              geography = "council", measure = "percent", 
              yearstart = 2009, yearend = 2023,
              time_agg = 1, year_type = "school")

# Aggregated Scotland data do not equal the original Scotland data from the spreadsheet (likely due to some suppression), so drop the aggregated data for Scotland:
ca_data <- readRDS(file.path(profiles_data_folder, "Data to be checked", "positive_destinations_ca_shiny.rds") ) %>%
  filter(code != "S00000001")

# Also, NA (suppressed) in original data have been replaced by zeroes during the main_analysis function, so need to revert to NA:
ca_data <- ca_data %>%
  mutate(upci = ifelse(numerator==0, as.numeric(NA), upci),
         lowci = ifelse(numerator==0, as.numeric(NA), lowci),
         rate = ifelse(numerator==0, as.numeric(NA), rate),
         numerator = ifelse(numerator==0, as.numeric(NA), numerator))

##########################################################
### Part 3 - Prepare final files -----
##########################################################

# 1 - main data: (combines Scotland data (since 1992/93) with data derived from CA level (from 2009). The mismatch is OK for the dashboard)
main_data <- scot_all %>%
  filter(split_name == "Total") %>%
  select(-starts_with("split")) %>%
  rbind(ca_data) %>%
  arrange(code, year)

# save to folder that QA script accesses:
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/school_leaver_destinations_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/school_leaver_destinations_shiny.csv"), row.names = FALSE) 

# 2 - population groups data (ie data behind population groups tab)
pop_grp_data <- scot_all %>%
  filter(!(split_name %in% c("Total", "Deprivation (SIMD)"))) %>%
  select(code, ind_id, year, numerator, rate, upci,
         lowci, def_period, trend_axis, split_name, split_value) %>%
  arrange(code, year)

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_leaver_destinations_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/school_leaver_destinations_shiny_popgrp.csv"), row.names = FALSE) 

# 3 - SIMD data (ie data behind deprivation tab)

# Process SIMD data
simd_data <- scot_all %>%
  filter(split_name == "Deprivation (SIMD)") %>%
  unique() %>%
  select(-split_name) %>%
  rename(quintile = split_value) %>%
  mutate(quint_type = "sc_quin") %>%
  arrange(code, year, quintile)

# add population data (quintile level) so that inequalities can be calculated
simd_data <-  simd_data|>
  add_population_to_quintile_level_data(pop="depr_pop_schoolleavers", # the population file created above
                                        ind = 13010, ind_name = "school_leaver_destinations") |>
  filter(!is.na(rate)) # not all years have data

# calculate the inequality measures
simd_data <- simd_data |>
  calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
  select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields

# save the data as RDS file
saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/school_leaver_destinations_ineq.rds"))

  
# Run QA reports
####################

# main data:
run_qa(type = "main", filename = "school_leaver_destinations", test_file = FALSE) # no historic file
# Orkney has no data for latest year: plots as zero in QA file, but won't plot in the app I think

# popgrp data:
run_qa(type = "popgrp", filename = "school_leaver_destinations", test_file=FALSE)

# ineq data:
run_qa(type = "deprivation", filename = "school_leaver_destinations", test_file=FALSE)


#END





