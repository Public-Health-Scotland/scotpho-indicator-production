#########################################################
# Scottish Household Survey data import
#########################################################

# Update ScotPHO indicators for mental health profile:

# 30022	Adults agreeing they can influence local decisions
# 30043	Households managing well financially (household level: sex==Total)
# 30024	Adults feeling they belong to their local neighbourhood
# 30046	Adults rating neighbourhood as a very good place to live
# 30027	Adults trusting most people in their neighbourhood
# 30047	Adults regularly using or passing through local open space
# 30020	Adults volunteering in past year
# 30038	Adults experiencing discrimination in past year (use CWB ind_id of 99134)
# 30041	Adults experiencing harassment in past year (use CWB ind_id of 99135)
# 30025	Adults feeling lonely in past week
# 30049	Adults experiencing noisy neighbours in past year
# 30045	Adults using high-risk loans in past year (household level: sex==Total)

# Source: bespoke analysis from SHoS team, SG. 

# NOTES:
# Data for 2020 and 2021 are not included in the timeseries as they are not comparable 
# with other years due to changes in the mode and delivery of the survey for those years because of the pandemic. 
# Latest year available is 2023.
# Where the base is under 50, the associated figures have been left blank.
# For the credit tables specifically, the percentages are very low as the actual 
# numbers for the credit309 options are low. As such, they will likely not be useful for your 
# purposes and it may be worth excluding them from your final dashboards.

#   Part 1 - Read in data
#   Part 2 - Prepare data for shiny
#   Part 3 - Prepare final files for checking

### functions/packages ----

#source("1.indicator_analysis.R") 
#source("2.deprivation_analysis.R") 
source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA
library(stringr) #for string manipulation
library(rio) #for importing data
library(readr) # for writing/reading rds files


### filepaths ----

#scotpho_folder renamed
profiles_data_folder <- "/PHI_conf/ScotPHO/Profiles/Data"

# paths to ScotPHOs lookup folders - can be used to shorten files paths indicator generation scripts
profiles_lookups <- file.path(profiles_data_folder, "Lookups")



### Lookups ----


# Read in geography lookup
area_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))



### 1 - Read in SHoS data from spreadsheet ----

file <- "/Scottish Household Survey/Tables_2025.xlsx"

vars <- c("SERV1H", "HK2", "COMMBEL", "RB1", "SOCIAL32", 
          "GREENUSE13", "VOLUN", "SOCIAL2",
          "ASB2A", "CREDIT", "DISCRIM", "HARASS")


# Function to read in worksheets and perform common processing
import_shos_xlsx <- function(suffix, filename) {
  import_list(paste0(profiles_data_folder,"/Received Data", filename),  
              setclass = "tbl", rbind = TRUE, 
              which = paste0(vars, suffix), 
              rbind_label = "indicator") %>%
    #rename fields to match expected names for scotpho app
    rename(year = F_DYEAR,
           rate = RowPercent,
           lowci = RowLowerCL,
           upci = RowUpperCL) %>%
    mutate(indicator = gsub(suffix, "", indicator),
           numerator=as.numeric(NA),
           def_period=paste(year, "survey year"),
           trend_axis=year) %>%
    mutate(year = as.integer(substr(trend_axis, 1, 4))) %>%
    # Create new indicator id column
    mutate(ind_id = case_when(indicator == "SERV1H" ~ 30022,
                              indicator == "HK2" ~ 30043,
                              indicator == "COMMBEL" ~ 30024,
                              indicator == "RB1" ~ 30046,
                              indicator == "SOCIAL32" ~ 30027, 
                              indicator == "GREENUSE13" ~ 30047, 
                              indicator == "VOLUN" ~ 30020, 
                              indicator == "SOCIAL2" ~ 30025,
                              indicator == "ASB2A" ~ 30049,
                              indicator == "CREDIT" ~ 30045, 
                              indicator == "DISCRIM" ~ 99134, 
                              indicator == "HARASS" ~ 99135)) %>%
    mutate(ind_name = case_when(ind_id == 30022  ~	"influence_local_decisions",
                                ind_id == 30043	~	"managing_well_financially",
                                ind_id == 30024	~	"neighbourhood_belonging",
                                ind_id == 30046	~	"neighbourhood_good_place",
                                ind_id == 30027	~	"neighbourhood_trust",
                                ind_id == 30047	~	"open_space_use",
                                ind_id == 30020	~	"volunteering",
                                ind_id == 99134	~	"discrimination",
                                ind_id == 99135	~	"harassment",
                                ind_id == 30025	~	"feeling_lonely",
                                ind_id == 30049	~	"noisy_neighbours",
                                ind_id == 30045	~	"high_risk_loans")) %>%
    select(-Base, -RowStdErr, -indicator) 
}

# Get the Scotland data (tab suffix _1)
shos_scot <- import_shos_xlsx("_1", file) %>%
  mutate(areatype = "Scotland",
         areaname = "Scotland",
         split_name = "Sex",
         split_value = "Total") 

# Get the CA data (tab suffix _2)
shos_ca <- import_shos_xlsx("_2", file) %>%
  rename(areaname = council) %>%
  mutate(areatype = "Council area",
         split_name = "Sex",
         split_value = "Total")

# Get the HB data (tab suffix _3)
shos_hb <- import_shos_xlsx("_3", file) %>%
  rename(areaname = hb2014) %>%
  mutate(areatype = "Health board",
         split_name = "Sex",
         split_value = "Total")

# Get the SIMD data (tab suffix _4)
shos_simd <- import_shos_xlsx("_4", file) %>%
  mutate(areatype = "Scotland", # Scotland only
         areaname = "Scotland",
         split_name = "Deprivation (SIMD)") %>%
  rename(split_value = mdquin_ts) 

# Get the sex data (tab suffix _5)
shos_sex <- import_shos_xlsx("_5", file) %>%
  mutate(areatype = "Scotland", # Scotland only
         areaname = "Scotland",
         split_name = "Sex") %>%
  rename(split_value = randgender_ts) %>%
  filter(is.na(hihgender_ts)) %>% # drop the indicators measured at hhd level, as sex/gender is meaningless for these (i.e., those indicators giving the sex of the householder (hihgender_ts)) 
  select(-hihgender_ts)


### 2 - Prepare data for shiny ----

# Combine 
shos_df <- bind_rows(shos_ca, shos_hb, shos_scot, shos_simd, shos_sex) %>%
  
  # get areanames sorted before adding codes
  mutate(areaname = gsub(" and ", " & ", areaname)) %>%
  mutate(areaname = ifelse(areaname=="Edinburgh, City of", "City of Edinburgh", areaname)) %>%
  mutate(areaname = case_when(areatype=="Health board" ~ paste("NHS", areaname),
                              TRUE ~ areaname)) %>%
  mutate(areaname = ifelse(areaname=="NHS Orkney Islands", "NHS Orkney", areaname)) %>%
  mutate(areaname = ifelse(areaname=="NHS Shetland Islands", "NHS Shetland", areaname)) %>%
  left_join(area_lookup, by=c("areatype","areaname")) %>%
  select(-areatype, -areaname) %>%
  
  # convert values to numeric (which sets missing/suppressed data to NA)
  mutate(rate = as.numeric(rate),
         lowci = as.numeric(lowci),
         upci = as.numeric(upci)) %>%
  
  # recode SIMD data:
  mutate(split_value = case_when(split_value=="Quintile 1- 20% most deprived" ~ "1",
                              split_value=="Quintile 2" ~ "2",
                              split_value=="Quintile 3" ~ "3",
                              split_value=="Quintile 4" ~ "4",
                              split_value=="Quintile 5 - 20% least deprived" ~ "5",
                              TRUE ~ split_value)) %>%
  
  
  # keep M and F sex only
  filter(!(split_value %in% c("Identify in another way", "Prefer not to say"))) %>%
  
  # keep only Scotland data for unmanageable loans
  filter(!(ind_id==30045 & code!="S00000001"))  # loans = too few cases for splits (many are suppressed)
  

# get totals for the splits (because no 'all' in the sex/SIMD data)
shos_needing_totals <- shos_df %>%
  filter(code=="S00000001") %>%
  select(ind_id, ind_name, code, trend_axis, year, def_period, split_name) %>% 
  distinct() # the groups we need totals for

shos_totals_to_add <- shos_df %>%
  filter(code=="S00000001" & split_name=="Sex" & split_value=="Total") %>%
  select(-split_name) # the Scotland totals 
  
shos_totals_for_splits <- shos_needing_totals %>%
  merge(y=shos_totals_to_add, by=c("ind_id", "ind_name", "code", "trend_axis", "year", "def_period"))  

# Add totals in
shos_df <- bind_rows(shos_df, shos_totals_for_splits) 



### 3. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(indicator_name){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- shos_df %>% 
    filter(ind_name == indicator_name,
           split_name=="Sex",
           split_value=="Total") %>% 
    unique() %>%
    select(-ind_name, -split_name, -split_value) %>%
    arrange(code, year)
  
  # Save files
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", indicator_name, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", indicator_name, "_shiny.rds"))

  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data

  
  if(indicator_name!="high_risk_loans") { # don't run for the loans indicator, as too few obs
    
  # 2 - population groups data (ie data behind population groups tab)
    pop_grp_data <- shos_df %>% 
      filter(ind_name == indicator_name,
             code=="S00000001",
             split_name == "Sex") %>% 
      unique() %>%
      select(-ind_name) %>%
      arrange(code, year, split_name, split_value)

  # Save
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", indicator_name, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", indicator_name, "_shiny_popgrp.rds"))

  # 3 - SIMD data (ie data behind deprivation tab)

  # Process SIMD data
  simd_data <- shos_df %>% 
    filter(ind_name == indicator_name,
           split_name == "Deprivation (SIMD)") %>% 
    unique() %>%
    mutate(quint_type = "sc_quin",
           quintile = split_value) %>%
    select(-ind_name, -split_value, -split_name) %>%
    arrange(code, year, quintile)
  
  # get ind_id argument for the add_population_to_quintile_level_data() function: 
  ind_id <- unique(simd_data$ind_id)

  # add population data (quintile level) so that inequalities can be calculated
  simd_data <-  simd_data|>
    add_population_to_quintile_level_data(pop="depr_pop_16+", ind = ind_id, ind_name = indicator_name) |>
    filter(!is.na(rate)) # drop any years without data (that have been given population data)
  
  simd_data$numerator[is.na(simd_data$numerator)] <- 0 # Converting any NAs to 0s (all are NA in SHoS because not provided)
  
  # calculate the inequality measures
  simd_data <- simd_data |>
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(overall_rate, total_pop, proportion_pop, most_rate, least_rate, par_rr, count)) #delete unwanted fields
  
  # save the data as RDS file
  saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_ineq.rds"))
  
  
  
  # Make data created available outside of function so it can be visually inspected if required
  pop_grp_data_result <<- pop_grp_data
  simd_data_result <<- simd_data
  
  
  }
  
}



# Prepare the final files 
prepare_final_files(indicator_name = "influence_local_decisions")
prepare_final_files(indicator_name = "managing_well_financially")
prepare_final_files(indicator_name = "neighbourhood_belonging")
prepare_final_files(indicator_name = "neighbourhood_good_place")
prepare_final_files(indicator_name = "neighbourhood_trust")
prepare_final_files(indicator_name = "open_space_use")
prepare_final_files(indicator_name = "volunteering")
prepare_final_files(indicator_name = "discrimination")
prepare_final_files(indicator_name = "harassment")
prepare_final_files(indicator_name = "feeling_lonely")
prepare_final_files(indicator_name = "noisy_neighbours")
prepare_final_files(indicator_name = "high_risk_loans") 


# Run QA reports 
# main data
run_qa(filename = "influence_local_decisions", test_file = FALSE)
run_qa(filename = "managing_well_financially", test_file = FALSE)
run_qa(filename = "neighbourhood_belonging", test_file = FALSE)
run_qa(filename = "neighbourhood_good_place", test_file = FALSE)
run_qa(filename = "neighbourhood_trust", test_file = FALSE)
run_qa(filename = "open_space_use", test_file = FALSE)
run_qa(filename = "volunteering", test_file = FALSE)
run_qa(filename = "discrimination", test_file = FALSE)
run_qa(filename = "harassment", test_file = FALSE)
run_qa(filename = "feeling_lonely", test_file = FALSE)
run_qa(filename = "noisy_neighbours", test_file = FALSE)
run_qa(filename = "high_risk_loans", test_file = FALSE) # "Warning: Error in eval: object 'S08' not found"


# ineq data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_qa(type = "deprivation", filename = "influence_local_decisions", test_file=FALSE) # Warning: Error in eval: object 'S08' not found"
run_qa(type = "deprivation", filename = "managing_well_financially", test_file=FALSE)
run_qa(type = "deprivation", filename = "neighbourhood_belonging", test_file=FALSE)
run_qa(type = "deprivation", filename = "neighbourhood_good_place", test_file=FALSE)
run_qa(type = "deprivation", filename = "neighbourhood_trust", test_file=FALSE)
run_qa(type = "deprivation", filename = "open_space_use", test_file=FALSE)
run_qa(type = "deprivation", filename = "volunteering", test_file=FALSE)
run_qa(type = "deprivation", filename = "discrimination", test_file=FALSE)
run_qa(type = "deprivation", filename = "harassment", test_file=FALSE)
run_qa(type = "deprivation", filename = "feeling_lonely", test_file=FALSE)
run_qa(type = "deprivation", filename = "noisy_neighbours", test_file=FALSE)
run_qa(type = "deprivation", filename = "high_risk_loans", test_file=FALSE) 

