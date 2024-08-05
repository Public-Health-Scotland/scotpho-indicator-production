###   Update ScotPHO Care and Wellbeing indicators sourced from Scottish Health Survey: 
#   99105: Food insecurity
#   99106: Healthy Weight adults
#   99108: Self-assessed health of adults (age 16+)
#   99109: Limiting long-term conditions (age 16+)

# all indicators available as male/female/all splits

# Data source is the Scottish Health Survey - received dashboard files from SHeS team (scottishhealthsurvey@gov.scot)


### functions/packages -----
source("1.indicator_analysis.R")



### 1. Read in data ----

# Identify data folder
shes_data_folder <- paste0(data_folder, "Received Data/Scottish Health Survey/")

# Identify data file with geographic breakdowns and aggregated Scotland data
geog_data_files <- paste0(shes_data_folder, list.files(path = shes_data_folder, pattern = "rank"))

# Read in data file
geog_data_raw <- read_spss(geog_data_files)


# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


### 2. Prepare data  -----

geog_data <- geog_data_raw %>% 
  
  # Clean variable names
  clean_names() %>% 
  
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
  
  # Rename variables
  rename(areaname = location,
         areatype = geographylevel,
         rate = percent,
         lowci = lower_ci,
         upci = upper_ci) %>% 
  
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
         
         # Create new columns
         ind_id = case_when(indicator == "self_assessed_health" ~ 99108,
                            indicator == "limiting_long_term_condition" ~ 99109,
                            indicator == "healthy_weight" ~ 99106,
                            indicator == "food_insecurity" ~ 99105),
         
         trend_axis = year,
         year = as.numeric(str_sub(year, start= 1, end = 4))+2,
         def_period = paste0("4-year aggregate"," (", trend_axis, ")"),
         numerator = NA) %>% 
  
  # Join geography codes
  left_join(dictionary, by = c("areatype", "areaname"))



### 3. Prepare final files -----

# Function to prepare main data files
prepare_final_files <- function(ind){
  
  # Filter for main data
  # (ie dataset behind scotland and/or sub national summary data that populates summary/trend/rank tab)
  maindata <- geog_data %>% 
    filter(indicator == ind,
           sex == "All") %>% 
    select(ind_id, code, year, trend_axis, def_period, rate, lowci, upci, numerator)

  # Save files in folder to be checked
  write.csv(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(maindata, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))

  # Make data created available outside of function so it can be visually inspected if required
  maindata_result <<- maindata
  
}
  

# Create final files
prepare_final_files(ind = "food_insecurity")
prepare_final_files(ind = "self_assessed_health")
prepare_final_files(ind = "limiting_long_term_condition")
prepare_final_files(ind = "healthy_weight_adults")

  
# Run QA reports
run_qa(filename = "food_insecurity")
run_qa(filename = "self_assessed_health")
run_qa(filename = "limiting_long_term_condition")
run_qa(filename = "healthy_weight_adults")


#END

