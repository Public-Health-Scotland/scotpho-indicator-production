## notes ----

# this script produces data for indicator 13030 - school leavers from publicly funded secondary schools in positive destinations, 9 months after leaving school
# Summary Statistics for Attainment and Initial Leaver Destinations, No. 6: 2024 Edition, released on 27th February 2024
# https://www.gov.scot/news/school-leaver-attainment-and-destinations-8/
# Script updated in Nov 2024 (by ER) to import more population group splits from the data file (previously only Scotland and CA data were imported)
# Script updated in March 2025 (by ER) to use the new indicator production paths and functions.


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

# ca to hb lookup
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) |>
  select(ca2019, hb2019)
geo_lookup %<>% distinct %>% rename(ca = ca2019, hb = hb2019)


###############################################.
## Part 2 - Import data ----
###############################################.



# National:
###############

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
  mutate(split_name = "None",
         split_value = "None")

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
  mutate(code = "S00000001") # add code for Scotland

# make population file for school leavers by year and SIMD quintile, for inequalities metrics:
depr_pop_schoolleavers <- schdest_simd %>%
  mutate(year = as.numeric(substr(trend_axis, 1, 4))) %>%
  select(year, quintile=split_value, denominator) %>%
  mutate(code = "S00000001",
         quint_type = "sc_quin") 

saveRDS(depr_pop_schoolleavers, paste0(profiles_lookups, "/Population/depr_pop_schoolleavers.rds"))

# Council area:
##################

# Council area data, totals
ca_series <- read.xlsx(paste0(schdest_data_folder, file),
                       sheet = "L2.1",
                       colNames = TRUE,
                       startRow = 6) %>%
  rename(trend_axis = Year) %>%
  filter(LA.Name!="Scotland") %>% # longer time series in the scot_series data
  mutate(across(everything(), ~replace(., . %in% c("[c]", "[z]", "[low]", "S"), NA)), #replace suppression symbols with NA
         rate = as.numeric(`Positive.Destination`),
         numerator = round((rate * `Number.of.leavers`)/100),
         areaname = str_replace(LA.Name, "Edinburgh, City of","City of Edinburgh"),
         areaname = str_replace(areaname, "&","and")) %>%
  merge(ca, by = "areaname") %>% # join with council area lookup
  select(trend_axis, code, numerator, denominator = `Number.of.leavers`, rate) %>%
  mutate(split_name = "None",
         split_value = "None")

# aggregate to HB level
hb_series <- ca_series %>%
  merge(y=geo_lookup, by.x="code", by.y="ca") %>%
  group_by(hb, trend_axis) %>%
  summarise(numerator = sum(numerator, na.rm=TRUE),
            denominator = sum(denominator, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(rate = 100 * numerator/denominator,
         split_name = "None",
         split_value = "None") %>%
  rename(code = hb)


# Combine all:
###################

all_data <- rbind(scot_all, ca_series, hb_series) %>%
  mutate(ind_id = 13010,
         year = substr(trend_axis, 1, 4),
         def_period = paste0("School year (", trend_axis, ")")) %>%
  # confidence intervals
  mutate(ci_wald = 100 * (1.96*sqrt(((rate/100)*(1-(rate/100)))/denominator)), # Wald method. 
         lowci = rate - ci_wald,
         upci = rate + ci_wald) %>%
  select(-ci_wald, -denominator)



##########################################################
### Part 3 - Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- all_data %>%
    filter(split_name == "None") %>%
    select(code, ind_id, year,
           numerator, rate, upci, lowci,
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE) 
  
  # 2 - population groups data (ie data behind population groups tab)
  pop_grp_data <- all_data %>%
    filter(!(split_name %in% c("None", "Deprivation (SIMD)"))) %>%
    select(code, ind_id, year, numerator, rate, upci,
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year)
  
  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE) 
  
  # 3 - SIMD data (ie data behind deprivation tab)
  
  # Process SIMD data
  simd_data <- all_data %>%
    filter(split_name == "Deprivation (SIMD)") %>%
    unique() %>%
    select(-split_name) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type = "sc_quin") %>%
    arrange(code, year, quintile)
  
  # get arguments for the add_population_to_quintile_level_data() function: (done because the ind argument to the current function is not the same as the ind argument required by the next function)
  ind_name <- ind # dataset will already be filtered to a single indicator based on the parameter supplied to 'prepare final files' function
  ind_id <- unique(simd_data$ind_id) # identify the indicator number 
  
  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop="depr_pop_schoolleavers", # the population file created above
                                          ind = ind_id, ind_name = ind_name) |>
    filter(!is.na(rate)) # not all years have data
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
  
}


# Run function to create final files
prepare_final_files(ind = "school_leaver_destinations")


# Run QA reports
####################

# main data:
run_qa(type = "main", filename = "school_leaver_destinations", test_file = FALSE) # no historic file
# Orkney has no data for latest year: plots as zero in QA file, but won't plot in the app I think

# ineq data:
run_qa(type = "deprivation", filename = "school_leaver_destinations", test_file=FALSE)


#END





