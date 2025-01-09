# To-do contains indicators saved in test output location- this should be updated to non-test folder when new profile designed is published. 

#  ScotPHO indicators: 2 indicator outputs from this script 
#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE can only be generated at Scotland, NHS board and CA level.  It is NOT possibly to generate at smaller geographies as robust data on SAH (self assessed health is not available)

# Population splits by SIMD quintile and Urban/rural split for male/female HLE available at Scotland level only.

# HLE data published annually by NRS - usually in December - check website to see if new data has been published
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland
# it may take some time before this data is then available in statistics.gov 

# stats.gov data also seems to have missing years for some of the population groups e.g. rural/urban split missing for 2017-2019
# rural urban splits also only available at Scotland level not for NHS board or council area.

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") #Normal indicator functions

# Data queried directly from statistics.gov 
# install the dev tools/opendata scotland r package which communicates with the statistics.gov website api - if you don't already have them.
# install.packages("devtools") #commented out as only needs to be run once and you may already have the packages installed.
# devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov

# Extracts for Life Expectancy data saved in left expectancy network folder.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
} else {
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
}

###############################################.
# Extract Healthy life expectancy data ----
## by CA & NHS board
###############################################.

# see structure and variables of this dataset
ods_structure("healthy-life-expectancy") 

# Set filter parameters to use in open data extraction
# reminder: there is a limit on number of rows that can be extracted using this particular method - in case the extracted data doesn't look complete.
age_select <- c("0-years") #include filter in a extraction as it is needed to ensure number of rows to extract is within limits however (annoyingly initial extratction returns '0 years' and '90 years' so need to filter again)
#urban_rural <- c("all")

# extract data
hle_data_raw <- ods_dataset("healthy-life-expectancy", 
                            #urbanRuralClassification = urban_rural,
                            age = age_select)

# prepare data
hle <- hle_data_raw %>% 
  
  # clean column names
  clean_names() %>%
  
  # filter again as this will select correctly filter only '0 years' (ie healthy life expectancy at birth) otherwise you have '90 years' 
  filter(age == age_select) %>%  
  
  # pivot measure types to wide format
  pivot_wider(names_from = measure_type, values_from = value) %>%
  
  # rename columns
  rename("code" = ref_area, 
         "trend_axis" = ref_period,
         "rate" = count,
         "lowci" = `95-lower-confidence-limit`,
         "upci" = `95-upper-confidence-limit`) %>% 

         # create single split name column
  mutate(split_name = case_when(urban_rural_classification != "all" ~ "Urban/Rural",
                                simd_quintiles != "all" ~ "Scottish Index of Multiple Deprivation",
                                urban_rural_classification == "all" & simd_quintiles == "all" ~ "Total"),
         
         # create single split value column
         split_value = case_when(split_name == "Urban/Rural" ~ urban_rural_classification,
                                 split_name == "Scottish Index of Multiple Deprivation" ~ simd_quintiles,
                                 split_name == "Total" ~ "Total"),
         
         # tidy split values
         split_value = str_to_sentence(split_value), # capitalises first letter
         split_value = str_replace_all(split_value, c("-" = " ",
                                                      "1 most deprived" = "1 - most deprived",
                                                      "5 least deprived" = "5 - least deprived",
                                                      "Large urban areas" = "1 Large urban areas",
                                                      "Other urban areas" = "2 Other urban areas",
                                                      "Accessible small towns" = "3 Acessible small towns",
                                                      "Remote small towns" = "4 Remote small towns",
                                                      "Accessible rural" = "5 Accessible rural",
                                                      "Remote rural" = "6 Remote rural")),
         # recode standard geo code for scotland to the ScotPHO dictionary where S00000001 is Scotland
         code = ifelse(code == "S92000003", "S00000001", code),
         
         # Create new columns
         numerator = NA, # insert column where numerator would ordinarily be - there is no numerator for LE
         ind_id = case_when(sex == "female" ~ 99101,
                            sex == "male" ~ 99102),
         def_period = paste0(trend_axis, " (3 year aggregate)"),
         year = as.numeric(substr(trend_axis, 1, 4)) +1) %>% # its 3 year average so take first year of time period then add 1 to find mid-year

  # remove irrelevant columns
  select(!c(age, simd_quintiles, urban_rural_classification)) %>% 
  
  arrange(ind_id, code, year, split_name, split_value)


##########################################################################.
##1. Generate Main Data files for healthy life expectancy shiny files ----
##########################################################################.

hle_main_file <- function(indicator, sex_filter){
  
maindata_df <- hle %>%
  filter(split_name == "Total",
         sex==sex_filter) %>%
  select(-c(sex, split_name, split_value))

if (sex_filter=="female"){
  hle_main_file_female <<- maindata_df #save sex specific dataframe as this will be overwritten by next function call
} else if (sex_filter=="male") {
  hle_main_file_male <<- maindata_df #save sex specific dataframe as this will be overwritten by next function call
}

write_csv(maindata_df, file = paste0(data_folder, "Data to be checked/", indicator, "_shiny.csv"))
write_rds(maindata_df, file = paste0(data_folder, "Data to be checked/", indicator, "_shiny.rds"))

}

# run the function for each of the sexes:
hle_main_file(indicator="healthy_life_expectancy_female", sex_filter ="female")
hle_main_file(indicator="healthy_life_expectancy_male", sex_filter="male")

#run QA reports for main data
run_qa("healthy_life_expectancy_female")
run_qa("healthy_life_expectancy_male")



###################################################################################.
##2. Generate Deprivation Data files for healthy life expectancy shiny files ----
###################################################################################.

hle_depr_file <- function(indicator, sex_filter ){

  # filter data to include simd and total split
  depr_df <- hle %>%
    filter(split_name %in% c("Scottish Index of Multiple Deprivation","Total")) |>
    filter(code=="S00000001") |> # deprivation split for HLE only available at scotland level so filter for this geo
    rename(quintile = split_value) %>%
    mutate(quint_type="sc_quin",
           quintile = case_when(quintile=="Total" ~ "Total", TRUE ~ substr(quintile, 1, 1)))|>
    select(-split_name)
  
    # apply sex filtering
    depr_df <- depr_df |>
      filter(year !="2018")|> #exclude period 2017-2019 since there is only scotland data but no deprivation split
      filter(sex== sex_filter) |>
      select(-sex)
  
    # Get ind_id argument for the analysis function 
    ind_id <- unique(simd_df$ind_id)
    
  # Save intermediate SIMD file so that files can be run through deprivation function to calculate SII/RII/PAF
  write_rds(depr_df, file = paste0(data_folder, "Prepared Data/", indicator, "_shiny_depr_raw.rds"))
  write.csv(depr_df, file = paste0(data_folder, "Prepared Data/", indicator, "_shiny_depr_raw.csv"), row.names = FALSE)

  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(indicator, "_shiny_depr"), 
                                 pop = "depr_pop_allages", # these are all-age indicators, with no sex split for SIMD
                                 ind_id,
                                 indicator)
  
  # allow visual inspection of sex specific final results
  if (sex_filter=="female"){
    hle_dep_final_female <<- final_result #save sex specific dataframe as this will be overwritten by next function call
  } else if (sex_filter=="male") {
    hle_dep_final_male <<- final_result #save sex specific dataframe as this will be overwritten by next function call
  }
  
}

# run the function for each of the sexes:
hle_depr_file(indicator="healthy_life_expectancy_female", sex_filter ="female")
hle_depr_file(indicator="healthy_life_expectancy_male", sex_filter ="male")

  
###################################################################################.
##3. Generate Population Group files for healthy life expectancy shiny files ----
###################################################################################.

# filter data to include simd and total split
pop_df <- hle %>%
    filter(split_name!="Scottish Index of Multiple Deprivation") |>
    filter(code=="S00000001") # deprivation split for HLE only available at scotland level so filter for this geo

  #create a female population group (so we can display a sex comparison for both male and female indicators)
 female_pop_df <-pop_df |>
 filter(sex=="female",
        split_name=="Total") |>
   mutate(split_name=(case_when(split_name=="Total" ~ "Sex", TRUE~"Other")),
          split_value=(case_when(split_value=="Total" ~ "Female", TRUE~"Other")))

 #create a male population group (so we can display a sex comparison for both male and female indicators)
 male_pop_df <-pop_df |>
   filter(sex=="male",
          split_name=="Total") |>
   mutate(split_name=(case_when(split_name=="Total" ~ "Sex", TRUE~"Other")),
          split_value=(case_when(split_value=="Total" ~ "Male", TRUE~"Other")))

 pop_df <-pop_df |>
   filter(year !="2018")|> #exclude period 2017-2019 since there is only scotland data but no rural/urban deprivation split
   mutate(split_name=(case_when(split_name=="Total" ~ "Urban/Rural", TRUE~split_name))) # keep a total column in urban rural split
  
 pop_final <-bind_rows(pop_df,female_pop_df,male_pop_df)

 # function to prepare male and female file files  
  
 hle_pop_file <- function(indicator, sex_filter, ind_id ){


  if (sex_filter=="female"){
    
    pop_final <-  pop_final|>
      mutate(ind_id=case_when(split_value=="Male" ~ 99101, TRUE ~ind_id)) |>
      filter(ind_id==99101) |>
      select(-sex)
    
    hle_pop_final_female <<- pop_final #save sex specific dataframe as this will be overwritten by next function call
  
    } else if (sex_filter=="male") {
      
    pop_final <-  pop_final|>
      mutate(ind_id=case_when(split_value=="Female" ~ 99102, TRUE ~ind_id)) |>
      filter(ind_id==99102)|>
      select(-sex)
    
    hle_pop_final_male <<- pop_final #save sex specific dataframe as this will be overwritten by next function call
  }
 
  write_csv(pop_final, file = paste0(data_folder, "Data to be checked/", indicator, "_shiny_popgrp.csv"))
  write_rds(pop_final, file = paste0(data_folder, "Data to be checked/", indicator, "_shiny_popgrp.rds"))
   
}

# run the function for each of the sexes:
hle_pop_file(indicator="healthy_life_expectancy_female", sex_filter ="female", ind_id=99101)
hle_pop_file(indicator="healthy_life_expectancy_male", sex_filter ="male", ind_id=99102)



#END
