###   Update ScotPHO Care and Wellbeing indicators: 
#   Food insecurity
#   Healthy Weight adults
#   Physical Activity
#   Self-assessed health of adults (age 16+)
#   Limiting long-term conditions (age 16+)



# Data source is the Scottish Health Survey open data on statistics.gov.scot
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data



### functions/packages -----
source("1.indicator_analysis.R") 
library(devtools)
library(janitor)
library(opendatascot)



### 1. Read in SHeS open data using opendatascot package ----

# Read in data
data <- opendatascot::ods_dataset("scottish-health-survey-local-area-level-data",
                                  scottishHealthSurveyIndicator = c("food-insecurity-worried-would-run-out-of-food-yes",
                                                                    "healthy-weight-healthy-weight",
                                                                    "summary-activity-levels-meets-recommendations",
                                                                    "good", # self-assessed health
                                                                    "long-term-illness-limiting-long-term-illness")) %>%
                                  clean_names()



### 2. Prepare data  -----

data <- data %>%

  # Rename columns
  rename("trend_axis" = ref_period,
         "code" = ref_area,
         "measure" = measure_type,
         "indicator" = scottish_health_survey_indicator) %>%
  
  # Rename indicators and create new columns
  mutate(indicator = str_replace(indicator, "food-insecurity-worried-would-run-out-of-food-yes", "food_insecurity"),
         indicator = str_replace(indicator, "healthy-weight-healthy-weight", "healthy_weight_adults"),
         indicator = str_replace(indicator, "summary-activity-levels-meets-recommendations", "physical_activity"),
         indicator = str_replace(indicator, "good", "self_assessed_health"),
         indicator = str_replace(indicator, "long-term-illness-limiting-long-term-illness", "limiting_long_term_condition"),
         measure = str_replace(measure, "95-lower-confidence-limit", "lowci"),
         measure = str_replace(measure, "95-upper-confidence-limit", "upci"),
         year = as.numeric(str_sub(trend_axis, start= 1, end = 4))+2,
         def_period = paste0("4-year aggregate (",trend_axis,")"),
         code = ifelse(code == "S92000003", "S00000001", code),
         numerator = "",
         ind_id = case_when(indicator == "food_insecurity" ~ 99105,
                            indicator == "healthy_weight_adults" ~ 99106,
                            indicator == "physical_activity" ~ 99107,
                            indicator == "self_assessed_health" ~ 99108,
                            indicator == "limiting_long_term_condition" ~ 99109)) %>%
  
  # Change sex from long to wide format
  pivot_wider(names_from = sex, values_from = value)




### 3. Prepare final files -----

# Create function to prepare final shiny outputs
prepare_shiny_file <- function(ind, sex_grp) {

  #  Select relevant data and change format
  dat <- data %>%
    filter(indicator == ind) %>%
    select(ind_id, sex_grp, code, year, measure, numerator, def_period, trend_axis) %>%
    pivot_wider(names_from = "measure", values_from = sex_grp) %>%
    arrange(year)
  
  # Save files in folder to be checked
  write.csv(dat, paste0(data_folder, "Data to be checked/", ind, "_", sex_grp, "_shiny.csv"), row.names = FALSE)
  write_rds(dat, paste0(data_folder, "Data to be checked/", ind, "_", sex_grp, "_shiny.rds"))
  
}


# Create files for each indicator and sex
for (i in unique(data$indicator)){
  for(j in c("all", "male", "female")){
    
    prepare_shiny_file(ind = i, sex_grp = j)
    
  }
}


