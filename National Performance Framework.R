###   Update ScotPHO Care and Wellbeing indicators: 
#   99116: Persistent poverty
#   99117: Child wellbeing and happiness
#   99118: Child material deprivation
#   99121: Health risk behaviours
#   99122: Quality of care experience
#   99123: Gender balance in organisations (for minority ethnic population)


# Data source is the National Performance Framework open data on statistics.gov.scot
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fnational-performance-framework


### functions/packages ----
source("1.indicator_analysis.R") 



### 1 - Read in data -----

# Read in NPF data 
data <- read_xlsx(paste0(data_folder, "Received Data/NPF Database - 01.11.23.xlsx")) %>%
  clean_names()


# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>%
  select(!c(parent_area, areaname_full))



### 2. Prepare data  -----

data <- data %>%
  
  # Select relevant indicators
  filter(indicator %in% c("Persistent Poverty",
                          "Child Wellbeing and Happiness",
                          "Child material deprivation",
                          "Health risk behaviours",
                          "Quality of care experience",
                          "Gender balance in organisations")) %>%
  
         # Rename indicators
  mutate(indicator = str_replace_all(tolower(indicator), " ", "_"),
         
         # Ensure breakdowns are named consistently
         disaggregation = str_replace_all(disaggregation, "Health and Social Care Partnership", "HSC partnership"),
         disaggregation = str_replace_all(disaggregation, "Health Board", "Health board"),
         disaggregation = str_replace_all(disaggregation, "SIMD", "Scottish Index of Multiple Deprivation"),
         disaggregation = str_replace_all(disaggregation, "Total Difficulties Score X ", ""),
         disaggregation = str_replace_all(disaggregation, "Gender", "Sex"),
         disaggregation = if_else(indicator == "persistent_poverty" & breakdown == "Total", "Total", disaggregation),
         disaggregation = if_else(str_detect(breakdown, "Ethnic minority"), "Total", disaggregation),

         breakdown = if_else(str_detect(breakdown, "1|(?i)most"), "1 - most deprived", breakdown),
         breakdown = if_else(str_detect(breakdown, "2$|2nd"), "2", breakdown),
         breakdown = if_else(str_detect(breakdown, "3"), "3", breakdown),
         breakdown = if_else(str_detect(breakdown, "4"), "4", breakdown),
         breakdown = if_else(str_detect(breakdown, "5|(?i)least"), "5 - least deprived", breakdown),
         
         breakdown = str_replace_all(breakdown, "Gender", "Sex"),
         breakdown = str_replace_all(breakdown, "Male", "Men"),
         breakdown = str_replace_all(breakdown, "Female", "Women"),
         
         breakdown = if_else(disaggregation == "Health board" & !grepl("NHS", breakdown), paste0("NHS ", breakdown), breakdown),
         breakdown = str_replace_all(breakdown, "\\band\\b", "&"),
         breakdown = str_replace_all(breakdown, "Edinburgh, City of", "City of Edinburgh"),
         
         year = if_else(str_detect(year, "(excl. 2020)"), "2017-2021", year),
         
         # Add indicator ids
         ind_id = case_when(indicator == "persistent_poverty" ~ 99116,
                            indicator == "child_wellbeing_and_happiness" ~ 99117,
                            indicator == "child_material_deprivation" ~ 99118,
                            indicator == "health_risk_behaviours" ~ 99121,
                            indicator == "quality_of_care_experience" ~ 99122,
                            indicator == "gender_balance_in_organisations" ~ 99123),
         
         # Create new columns
         areaname = if_else(str_detect(disaggregation, "Local Authority|Health board|HSC partnership"), breakdown, "Scotland"),
         numerator = "",
         rate = as.numeric(figure)) %>%
  
  # Select breakdowns of interest
  filter(disaggregation %in% c("Total",
                               "Scottish Index of Multiple Deprivation",
                               "SIMD",
                               "Local Authority",
                               "HSC partnership",
                               "Health board",
                               "Gender",
                               "Total Difficulties Score",
                               "Total Difficulties Score X Sex",
                               "Total Difficulties Score X SIMD"),
         
         # Remove duplicate rows
         !(breakdown == "Total" & disaggregation == "Scottish Index of Multiple Deprivation"),
         !(indicator == "gender_balance_in_organisations" & breakdown == "Total")) %>%
  
  # Also remove inequalities breakdowns for now until it's decided how they'll be presented on OPT
  filter(disaggregation != "Scottish Index of Multiple Deprivation") %>%
  
  # Create new area type variable from disaggregation
  mutate(areatype = if_else(str_detect(disaggregation, "Total"), "Scotland", disaggregation)) %>%
  
  # Join area codes
  left_join(dictionary, by = c("areaname", "areatype")) %>%
  
  # Select relevant variables
  select(c(ind_id, indicator, code, year, rate, numerator)) %>%
  
  # Reorder data frame
  arrange(indicator, code, year)
  


### 3. Prepare final files -----

# Create function to prepare final shiny outputs
prepare_shiny_file <- function(ind) {
  
  #  Select indicator data
  dat <- data %>% filter(indicator == ind)
  
  # Create different date variables depending on what indicator it is:
  
  # Single survey year
  if (ind == "health_risk_behaviours") {
    
    dat <- dat %>%
      mutate(trend_axis = year,
             def_period = paste0(year, " survey year"))
    
  # Single calendar year
  } else  if (ind == "gender_balance_in_organisations") {
      
      dat <- dat %>%
        mutate(trend_axis = year,
               def_period = paste0(year, " calendar year"))
  
  # Combined survey years  
  } else if (ind == "quality_of_care_experience") {
    
    dat <- dat %>% 
      mutate(trend_axis = year,
             def_period = paste0(trend_axis, " survey year"),
             year = as.numeric(str_sub(trend_axis, start= 1, end = 4)))
  
  # 5-year aggregate  
  } else if (ind == "persistent_poverty") {
    
    dat <- dat %>%
      mutate(trend_axis = year,
             def_period = paste0("5-year aggregate (",trend_axis,")"),
             year = as.numeric(str_sub(trend_axis, start= 1, end = 4))+2)
  
  # 4-year aggregate  
  } else {
    
    dat <- dat %>% 
      mutate(trend_axis = year,
             def_period = paste0("4-year aggregate (",trend_axis,")"),
             year = as.numeric(str_sub(trend_axis, start= 1, end = 4))+2)
  }
  
  # Save files in folder to be checked
  write.csv(dat, paste0(data_folder, "Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  write_rds(dat, paste0(data_folder, "Data to be checked/", ind, "_shiny.rds"))
  
  # Make data file created available outside of function so it can be visually inspected if required
  indicator_result <<- dat 
  
}


# Create files for each indicator and run QA reports
for (i in unique(data$indicator)){
    
    prepare_shiny_file(ind = i)
  
    run_qa(filename = i)
    
}


#END

