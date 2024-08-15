###   Update ScotPHO Care and Wellbeing indicators sourced from Scottish Health Survey: 
#   99105: Food insecurity
#   99106: Healthy Weight adults
#   99108: Self-assessed health of adults (age 16+)
#   99109: Limiting long-term conditions (age 16+)

# all indicators available as male/female/all splits

# Data source is the Scottish Health Survey - received dashboard files from SHeS team (scottishhealthsurvey@gov.scot)
# data supplied in .sav file format (this is file format used by SPSS - requires haven package to open in R).

### functions/packages -----
source("1.indicator_analysis.R")
library(haven) # for reading in .sav files 


### 1. Read in data ----

# Identify data folder
shes_data_folder <- paste0(data_folder, "Received Data/Scottish Health Survey/")


## Main data  -----
## This is dataset behind summary/trend/rank tabs of profiles tool

# Identify main data file with geographic breakdowns and aggregated Scotland data (create list of data files)
main_data_files <- paste0(shes_data_folder, list.files(path = shes_data_folder, pattern = "rank"))

# Read in main data file
main_data_raw <- read_spss(main_data_files)


## Pop groups data -----
## This is dataset behind population group tab of profiles tool

# Identify data files with population group breakdowns at Scotland level (for single years)
pop_grp_data_files <- paste0(shes_data_folder, list.files(path = shes_data_folder, pattern = "trend"))

# Read in population groups data files
pop_grp_data_list <- lapply(pop_grp_data_files, read_spss)

# Name each file (for later use in the split_name column)
names(pop_grp_data_list) <- c("Age", "Equivalised income", "Long-term conditions", 
                              "Sex", "Scottish Index of Multiple Deprivation")

# Rename columns so they're consistent across files and rows can be combined
pop_gpr_col_names <- c("year", "areaname", "topic", "indicator", "categories", 
                       "split_value", "unweightedbases", "mean", "lowci", "upci", "rate")
pop_grp_data_list <- lapply(pop_grp_data_list, setNames, pop_gpr_col_names)

# Save as single data frame
pop_grp_data_raw <- bind_rows(pop_grp_data_list, .id = "split_name")


## Geography lookup -----

# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))



### 2. Prepare data  -----

## Prepare data sets so they can be combined (to reduce duplicate wrangling) ----

# Rename, add and remove relevant columns for main data
main_data <- main_data_raw %>% 
  clean_names() %>% 
  rename(areaname = location,
         areatype = geographylevel,
         rate = percent,
         lowci = lower_ci,
         upci = upper_ci,
         split_value = sex) %>% 
  mutate(split_name = "Sex") %>% 
  select(!significance_scot)

# Add new area type column to pop groups data
pop_grp_data <- pop_grp_data_raw %>% 
  mutate(areatype = "Scotland")

# Combine to single data frame
all_data <- bind_rows(main_data, pop_grp_data)



## Wrangle single data frame ----

data <- all_data %>% 
  
  # Filter for relevant indicators
  filter(indicator %in% c("Self-assessed general health",
                          "Long-term conditions",
                          "Healthy weight",
                          "Food insecurity (worried would run out of food)"),
         
         # Filter for category of interest for each indicator
         categories %in% c("Healthy weight", # Healthy weight
                           "Very good/Good", # Self-assessed health
                           "Limiting long-term conditions",
                           "Yes")) %>% # Food insecurity
  
        # Tidy area type and names
  mutate(areatype = str_to_sentence(areatype),
         areatype = if_else(areatype == "Local authority", "Council area", areatype),
         areaname = str_replace_all(areaname, c(" and " = " & ")),
         areaname = str_replace_all(areaname, c("Edinburgh City" = "City of Edinburgh")),
         areaname = if_else(areatype == "Health board", paste0("NHS ", areaname), areaname),
         
         # Tidy indicator names
         indicator = case_when(indicator == "Self-assessed general health" ~ "self_assessed_health",
                               indicator == "Long-term conditions" ~ "limiting_long_term_condition",
                               indicator == "Healthy weight" ~ "healthy_weight",
                               indicator == "Food insecurity (worried would run out of food)" ~ "food_insecurity"),
         
         # Round confidence intervals to 0 dp to match main estimate
         lowci = round(lowci),
         upci = round(upci),
         
         # Create new indicator id and date columns
         ind_id = case_when(indicator == "self_assessed_health" ~ 99108,
                            indicator == "limiting_long_term_condition" ~ 99109,
                            indicator == "healthy_weight" ~ 99106,
                            indicator == "food_insecurity" ~ 99105),
         trend_axis = year,
         year = if_else(str_detect(trend_axis, "-"), as.numeric(str_sub(year, start = 1, end = 4))+2, as.numeric(year)),
         def_period = if_else(str_detect(trend_axis, "-"), paste0("4-year aggregate"," (", trend_axis, ")"), paste0(year, " survey year")),
         numerator = NA) %>% 
  
  # Join geography codes
  left_join(dictionary, by = c("areatype", "areaname")) %>%
  
  # Select relevant columns
  select(ind_id, indicator, code, year, trend_axis, def_period, split_name, split_value, rate, lowci, upci, numerator)



### 3. Prepare final files -----

# Function to prepare final files
prepare_final_files <- function(ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data (4-year aggregate)
  main_data_final <- data %>% 
    filter(indicator == ind,
           split_value == "All",
           str_detect(def_period, "aggregate")) %>% 
    select(!c(split_name, split_value, indicator)) %>% 
    unique() 
  
  write.csv(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  
  # 2 - population groups data (ie data behind population groups tab)
  # Contains LA/HB data by sex (4-year aggregate) and Scotland data by sex/age/condition/income/simd (single year)
  pop_grp_data_final <- data %>% 
    filter(indicator == ind) %>% 
    select(!indicator)

  write.csv(pop_grp_data_final, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data_final, paste0(data_folder, "Test Shiny Data/", ind, "_shiny_popgrp.rds"))
  

  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data_final
  pop_grp_data_result <<- pop_grp_data_final
  
}
  

# Run function to create final files
prepare_final_files(ind = "food_insecurity")
prepare_final_files(ind = "self_assessed_health")
prepare_final_files(ind = "limiting_long_term_condition")
prepare_final_files(ind = "healthy_weight")

  
# Run QA reports
run_qa(filename = "food_insecurity")
run_qa(filename = "self_assessed_health")
run_qa(filename = "limiting_long_term_condition")
run_qa(filename = "healthy_weight")


#END

