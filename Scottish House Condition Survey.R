#########################################################
# Scottish House Condition Survey - data import
#########################################################

### Update ScotPHO indicators sourced from SHCS data: 
### Author: Liz Richardson, 12 Nov 2024

### Adult mental health indicators:
# 30048 - Households in dwellings with critical, extensive and/or urgent disrepair =
#         Percentage of households with any disrepair to critical elements, considered urgent disrepair, or extensive disrepair to their dwelling. The variables used were critany, extanyVP, and urgany. 

### CYP mental health indicators:
# 30166 - Children in dwellings with critical, extensive and/or urgent disrepair = 
#         Proportion of households with children aged 17 years or under with any disrepair to critical elements, considered urgent disrepair, or extensive disrepair to their dwelling

### Notes on the data source:
# Data obtained from the UK Data Service (https://ukdataservice.ac.uk/), on an End User Licence. 
# The raw survey microdata are stored in the MHI folders (Liz has access).
# The script ukds_shcs_processing.R in Liz's mental_health_indicators.proj repo processes the raw data and saves it to Received Data, so it can be processed here.
# This involves linking with the Scottish Household Survey microdata to get the SIMD quintile for each household. 
# We calculate 3-y rolling averages for all breakdowns, national and local. This limits the latest data point to 2019, as no 2020 data, and 2021 data weren't comparable.
# 2022 was released in 2024, but will require another 2 years of data to make a 3-y average. 
# Next update = therefore 2026, unless we add custom rolling averages, e.g., 2018, 2019 & 2022... Not ideal.


### functions/packages -----
source("1.indicator_analysis.R")
source("2.deprivation_analysis.R")

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
shcs_data_folder <- paste0(data_folder, "Received Data/Scottish House Condition Survey/")
file <- "ukda_shcs_data.csv"

## Geography lookup -----

# Read in geography lookup
geo_lookup <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))


## Read in pre-processed SHCS data
all_shcs <- read.csv(paste0(shcs_data_folder, file)) %>%
  select(year, la, hb, simd5, any_kids, criturgext_or, ts_wght_p_n, la_wght_p) 

la_data <- all_shcs %>%
  select(year, spatial.unit = la, criturgext_or, weight = la_wght_p, any_kids) %>%
  mutate(spatial.scale = "Council area") %>%
  mutate(spatial.unit = gsub(" and ", " & ", spatial.unit),
         spatial.unit = ifelse(spatial.unit=="Edinburgh, City of", "City of Edinburgh", spatial.unit)) %>%
  mutate(split_name = "None",
         split_value = "None")

scot_data <- all_shcs %>%
  select(year, criturgext_or, weight = ts_wght_p_n, any_kids) %>%
  mutate(spatial.unit = "Scotland",
         spatial.scale = "Scotland") %>%
  mutate(split_name = "None",
         split_value = "None")

simd_data <- all_shcs %>%
  filter(!is.na(simd5)) %>%
  select(year, criturgext_or, weight = ts_wght_p_n, simd5, any_kids) %>%
  mutate(spatial.unit = "Scotland",
         spatial.scale = "Scotland") %>%
  mutate(split_name = "Deprivation (SIMD)",
         split_value = substr(simd5, 1, 1)) %>%
  select(-simd5)
simd_data2 <- scot_data %>% # repeat to give Scotland totals (includes households with no SIMD given )
  mutate(split_name = "Deprivation (SIMD)", 
         split_value = "Total") %>%
  rbind(simd_data)
  
#combine
all_shcs2 <- rbind(la_data,
                   scot_data,
                   simd_data2)



##########################################################
### 3. Prepare final files -----
##########################################################


# Function to prepare final files: main_data and simd_data
prepare_final_files <- function(ind){
  
  ind_name <- ifelse(ind==30166, "disrepair_cyp",
                      ifelse(ind==30048, "disrepair_all",
                             "ERROR"))
  

  if(ind==30166) {
    all_shcs2 <- all_shcs2 %>%
      filter(any_kids=="Yes")
  }
  
  disrepair_3y <- all_shcs2 %>%
    group_by(year, spatial.unit, spatial.scale, split_name, split_value, criturgext_or) %>%
    summarise(n = n(),
              wt = sum(weight)) %>%
    ungroup() %>%
    pivot_wider(names_from = criturgext_or, values_from = c(n, wt)) %>%
    mutate(n_0 = if_else(is.na(n_0), 0, n_0),
           wt_0 = if_else(is.na(wt_0), 0, wt_0)) %>%
    group_by(spatial.unit, spatial.scale, split_name, split_value) %>%
    dplyr::mutate(across(c(starts_with("n"), starts_with("wt")), # rolling sum of counts and weights over 3 year windows (centred)
                         ~ RcppRoll::roll_sum(., 3,  align = "center", fill = NA), .names = '{col}_rolling')) %>% 
    ungroup() %>%
    filter(!is.na(n_0_rolling)) %>% # drops ends of the series
    mutate(trend_axis = paste0(year-1, "-", year+1),
           Nuw = n_0_rolling + n_1_rolling,
           Nw = wt_0_rolling + wt_1_rolling) %>% # grossed up
    rename(nuw = n_1_rolling,
           nw = wt_1_rolling) %>% # grossed up
    mutate(rate = 100 * nw / Nw) %>% 
    mutate(proportion = nw/Nw,
           rate = 100 * proportion,
           ci_wald = 100 * (1.96*sqrt((proportion*(1-proportion))/Nuw)), # Wald method 
           lowci = rate - ci_wald,
           upci = rate + ci_wald) %>%
    select(-proportion, -starts_with("n_"), -starts_with("wt_"), -ci_wald) %>%
    mutate(ind_id = ind,
           spatial.unit = as.character(spatial.unit)) %>%
    # add the geog codes
    merge(y=geo_lookup, by.x=c("spatial.unit", "spatial.scale"), by.y=c("areaname", "areatype")) %>% 
    # add def_period
    mutate(def_period = paste0("Aggregated survey years (", trend_axis, ")")) %>%
    rename(numerator = nuw) %>%
    select(-c(starts_with("spatial"), nw, Nuw, Nw)) 
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- disrepair_3y %>% 
    filter(split_name == "None") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code,year)

  # Save the indicator data
  write.csv(main_data, paste0(data_folder, "Test Shiny Data/", ind_name, "_shiny.csv"), row.names = FALSE) #once indicator is live saving to test can be deleted
  write_rds(main_data, paste0(data_folder, "Test Shiny Data/", ind_name, "_shiny.rds"))#once indicator is live saving to test can be deleted
  # save to folder that QA script accesses:
  write_rds(main_data, paste0(data_folder, "Data to be checked/", ind_name, "_shiny.rds"))
  

  # 3 - SIMD data (ie data behind deprivation tab)

  # Process SIMD data
  simd_data <- disrepair_3y %>% 
    filter(split_name == "Deprivation (SIMD)") %>% 
    unique() %>%
    select(-split_name) %>%
    rename(quintile = split_value) %>%
    mutate(quint_type = "sc_quin")
  
  # Save intermediate SIMD file
  write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", ind_name, "_shiny_depr_raw.rds"))
  write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", ind_name, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(ind_name, "_shiny_depr"), 
                                 pop = "depr_pop_16+", # these are adult (16+) indicators, with no sex split for SIMD
                                 ind, 
                                 ind_name
  )
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  simd_data_result <<- simd_data
  
  
}


# Run function to create final files

# CYP indicator:
prepare_final_files(ind = 30166)

# Adult indicator:
prepare_final_files(ind = 30048)



# Run QA reports

# main data:
run_qa(filename = "disrepair_cyp")
run_qa(filename = "disrepair_all")

# ineq data:
run_ineq_qa(filename = "disrepair_cyp")
run_ineq_qa(filename = "disrepair_all")


# Manually check the data instead:

# Plot the indicator(s)
# =================================================================================================================
# Let's now see what the series look like:

# total
main_data_result %>%
  filter(code=="S00000001") %>%
  ggplot(aes(year, rate, group = ind_id, colour = ind_id, shape = ind_id)) + 
  geom_point() + geom_line() 

# councils
main_data_result %>%
  filter(code!="S00000001") %>%
  ggplot(aes(year, rate, group = code, colour = code, shape = code)) + 
  geom_point() + geom_line() +
  facet_wrap(~ind_id, scales = "free_y") 

# by SIMD 
simd_data_result %>%
  ggplot(aes(year, rate, group = quintile, colour = quintile, shape = quintile)) + 
  geom_point() + geom_line() +
  facet_wrap(~ind_id, scales = "free_y") 



#END

