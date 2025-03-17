
# ============================================================================
# ===== Processing SG HOMELESSNESS STATS =====
# ============================================================================

# NOTES ON THE INDICATORS

# 2 indicators: 
# 30034 = "Rate of homelessness applications assessed as homeless or potentially homeless in the past year, per 1000 population."
# 30161 = "Number of children per 1,000 in temporary accommodation" 

# xlsx received from Kelechi.Agwu@gov.scot on behalf of homelessness_statistics_inbox@gov.scot, Nov 2024
# Data also available online (main tables at https://www.gov.scot/publications/homelessness-in-scotland-2023-24/documents/), but not by gender (these have to be requested)
# Data are counts of adults or children (not counts of applications or households)

# Producer
# Scottish Government, Directorate For Tackling Child Poverty And Social Justice, Communities Analysis Division: Housing, Homelessness & Regeneration Analysis, Homelessness Statistics & Analysis team
# 
# Data sources
# The statistics in this supporting workbook are based on administrative data collected by local authorities in the course of processing homelessness applications - submitted on a quarterly basis. 
# 
# Definitions
# A household is homeless if they have no accommodation in the UK or elsewhere, or have accommodation but cannot reasonably occupy it. 
# A household is threatened with homelessness if it is likely they will become homeless within two months. 
# An adult is defined as being over 18 years old
# BUT: There must always be at least one adult in the applicant household - therefore, where a person aged 16, 17 or 18 is the only household member they would always be considered an adult. 
# We will use the 19+ population as the denominator for the adult MHI rates 
# A child is defined as 0-15y: use under 16 population as denominator for CYP indicator. 
#
# There are three stages involved in a homeless application in Scotland:
# 1) The Application stage where the household first presents to the local authority.
# 2) The Assessment stage which determines:
# (a) If the household is eligible for assistance. Households with no recourse to public funds are not eligible for homelessness assistance, though they may be provided temporary accommodation while their status is assessed.
# (b) whether the household is homeless or threatened with homelessness;
# (c) if the household is homeless, whether this is ‘unintentional’ or ‘intentional’; and
# (d) if unintentionally homeless, whether there is a connection to the local authority to which the application was made and/or to any other (Scottish) local authority.
# 3) The Outcome stage. A case can be closed only once the local authority has fulfilled its statutory duty or contact has been lost for 28 days.
# 
# Correspondence and enquiries
# Homelessness Statistics home page
# Telephone: 0131 244 8502
# E-mail: homelessness_statistics_inbox@gov.scot



###############################################.
## 1 - Prepare data ----
###############################################.


### functions/packages -----
source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA function (and path to lookups)

# Load additional packages
library(openxlsx)

### 1. Read in data ----

# Identify data folder
homeless_data_folder <- paste0(profiles_data_folder, "/Received Data/Homelessness/")
file <- "Adhoc - 2024.11.13 - Homeless Adults gender breakdown & children in TA - PHS.xlsx"

# Read in geography lookups:

# names to codes
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# LAs to HBs lookup
hb <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)

# Read in population lookups:

# Get adult populations 
la_pops <- read_rds("/conf/linkage/output/lookups/Unicode/Populations/Estimates/CA2019_pop_est_1981_2023.rds") %>%
  filter(age>=19) %>% #homeless application data: adults = 19+
  group_by(year, ca2019, sex) %>%
  summarise(pop = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  rename(code = ca2019) 

hb_pops <- la_pops %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code) %>%
  rename(code = hb2019) %>%
  group_by(code, year, sex) |>
  summarise(pop = sum(pop, na.rm=T)) %>%
  ungroup()

scot_pops <- la_pops %>%
  group_by(year, sex) %>%
  summarise(pop = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(code = "S00000001")

pops19plus <- rbind(la_pops, hb_pops, scot_pops) %>%
  mutate(sex=3) %>% #repeats all the rows to give for total pop
  rbind(la_pops, hb_pops, scot_pops) %>% # add M and F rows back in 
  group_by(year, sex, code) %>%
  summarise(pop = sum(pop, na.rm=T)) %>%
  ungroup() %>%
  mutate(sex = case_when(sex==1 ~ "Male",
                         sex==2 ~ "Female",
                         sex==3 ~ "Total"))

# Get child populations: (use file already produced for ScotPHO use)
child_pops <- readRDS(paste0(profiles_lookups, "/Population/CA_pop_under16.rds"))


#########################################
# 2 - Read in homelessness HL1 data 
#########################################

## Function to read in data

get_data <- function(sheetnum, gender, ind_id) {

df <- read.xlsx(paste0(homeless_data_folder, file),
                      sheet = sheetnum,
                      startRow = 4,
                      colNames = TRUE) %>%
  rename(areaname = X1) %>%
  
  # reshape the data 
  pivot_longer(-areaname, values_to="numerator", names_to = "trend_axis") %>%
  mutate(trend_axis = gsub("-", "/", trend_axis),
         year = as.numeric(substr(trend_axis, 1, 4)),
         split_name = "Gender",
         split_value = gender) %>%
  
  mutate(areaname = case_when(areaname=="Edinburgh" ~ "City of Edinburgh",  # to ensure matches geo_lookup OK
                          areaname=="Eilean Siar" ~ "Na h-Eileanan Siar",
                          areaname=="Shetland" ~ "Shetland Islands",
                          areaname=="Orkney" ~ "Orkney Islands",
                          TRUE ~ areaname)) %>% 
  mutate(areatype = ifelse(areaname=="Scotland", "Scotland", "Council area")) %>%
  
  # add ind_id column
  mutate(ind_id = ind_id) 
  
}


# Run the function to extract the data:

# Adult homelessness indicator:
homeless_male <- get_data("T1", "Male", 30034)
homeless_female <- get_data("T2", "Female", 30034)
homeless_total <- get_data("T3", "Total", 30034)

# CYP temporary accom indicator:
tempaccom_total <- get_data("T4", "Total", 30161)


# Create data for HBs by aggregating LAs:

# Adult homelessness indicator:

# Append extracted data and add geog codes
homeless_la_scot <- rbind(homeless_male,
                     homeless_female,
                     homeless_total) %>%
  merge(y=geo_lookup, by = c("areatype", "areaname")) %>%
  select(-areatype, -areaname)

# Add HB codes and aggregate
homeless_hb <- homeless_la_scot %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code) %>%
  rename(code = hb2019) %>%
  group_by(code, trend_axis, year, split_name, split_value, ind_id) |>
  summarise_all(sum) %>% 
  ungroup()

# Combine
homeless <- rbind(homeless_la_scot, homeless_hb)


# CYP temporary accom indicator:

# Append extracted data and add geog codes
tempaccom_la_scot <- tempaccom_total %>%
  merge(y=geo_lookup, by = c("areatype", "areaname")) %>%
  select(-areatype, -areaname)

# Add HB codes and aggregate
tempaccom_hb <- tempaccom_la_scot %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code) %>%
  rename(code = hb2019) %>%
  group_by(code, trend_axis, year, split_name, split_value, ind_id) |>
  summarise_all(sum) %>% 
  ungroup()

# Combine
tempaccom <- rbind(tempaccom_la_scot, tempaccom_hb)

###############################################.
## 3 - Computing rates and adding labels ----
###############################################.

# adult homelessness
homeless <- homeless %>%
  merge(y=pops19plus, by.x = c("code", "year", "split_value"), by.y = c("code", "year", "sex")) %>% 
  rename(denominator = pop) %>%
  # calculate the rate and the confidence intervals (Byars method)
  mutate(rate = numerator/denominator*1000,
         o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
         o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
         lowci = o_lower/(denominator)*1000,
         upci = o_upper/(denominator)*1000) %>% 
  select(-o_upper,- o_lower, -denominator) %>% 
  # add in the definition period label.
  mutate(def_period = paste0(trend_axis, " financial year"))


# children in temporary accommodation
tempaccom <- tempaccom %>%
  merge(y=child_pops, by.x = c("code", "year"), by.y = c("code", "year")) %>% 
  # calculate the rate and the confidence intervals (Byars method)
  mutate(rate = numerator/denominator*1000,
         o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
         o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
         lowci = o_lower/(denominator)*1000,
         upci = o_upper/(denominator)*1000) %>% 
  mutate(lowci = ifelse(is.nan(lowci), 0, lowci)) %>% # replaces NaN with 0 for the lowcis when rate==0
  select(-o_upper,- o_lower, -denominator) %>% 
  # add in the definition period label.
  mutate(def_period = paste0("Yearly snapshot (", trend_axis, ")"))



##########################################################
### 4 - Prepare final files -----
##########################################################


# Function to prepare final files: main_data and popgroup
prepare_final_files <- function(input_file, ind){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  main_data <- input_file %>% 
    filter(split_value == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  # Save
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE) 
  
  # 2 - population groups data (ie data behind population groups tab)
  
  if("Male" %in% unique(input_file$split_value)) {
    
  pop_grp_data <- input_file %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year)
  
  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)
  
  }
  
}


# Run function to create final files
prepare_final_files(ind = "adults_homeless", input_file = homeless)
prepare_final_files(ind = "cyp_temporary_accommodation", input_file = tempaccom)

# Run QA reports 
run_qa(type = "main", filename = "adults_homeless", test_file = FALSE) 
run_qa(type = "main", filename = "cyp_temporary_accommodation", test_file = FALSE) 


##END
