

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

# Path to population lookups 
pop_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"

# Make 19y+ denominator files for crude rates by sex:
CA_pop_M_19plus <- readRDS(file=paste0(pop_lookup, "CA_pop_19+_SR.rds")) %>% 
  filter(sex_grp==1) %>% 
  group_by(year, code, sex_grp) %>% summarise(denominator=sum(denominator)) %>% ungroup()
saveRDS(CA_pop_M_19plus, file=paste0(pop_lookup, 'CA_pop_M_19+.rds'))

CA_pop_F_19plus <- readRDS(file=paste0(pop_lookup, "CA_pop_19+_SR.rds")) %>% 
  filter(sex_grp==2) %>% 
  group_by(year, code, sex_grp) %>% summarise(denominator=sum(denominator)) %>% ungroup()
saveRDS(CA_pop_F_19plus, file=paste0(pop_lookup, 'CA_pop_F_19+.rds'))


#########################################
# 2 - Read in homelessness HL1 data 
#########################################

## Function to read in data

get_data <- function(sheetnum, gender) {
  
  df <- read.xlsx(paste0(homeless_data_folder, file),
                  sheet = sheetnum,
                  startRow = 4,
                  colNames = TRUE) %>%
    rename(areaname = X1) %>%
    
    # reshape the data 
    pivot_longer(-areaname, values_to="numerator", names_to = "trend_axis") %>%
    mutate(year = as.numeric(substr(trend_axis, 1, 4)),
           split_name = "Gender",
           split_value = gender) %>%
    
    mutate(areaname = case_when(areaname=="Edinburgh" ~ "City of Edinburgh",  # to ensure matches geo_lookup OK
                                areaname=="Eilean Siar" ~ "Na h-Eileanan Siar",
                                areaname=="Shetland" ~ "Shetland Islands",
                                areaname=="Orkney" ~ "Orkney Islands",
                                TRUE ~ areaname)) %>% 
    mutate(areatype = ifelse(areaname=="Scotland", "Scotland", "Council area")) %>%
    merge(y=geo_lookup, by = c("areatype", "areaname")) %>% # add geog codes
    filter(code!="S00000001") %>% # drop Scotland data as these are calculated in the aggregation
    select(-areatype, -areaname, -trend_axis) 
  
}


# Run the function to extract the data:

# Adult homelessness indicator:
homeless_total <- get_data("T3", "Total" )
homeless_male <- get_data("T1", "Male")
homeless_female <- get_data("T2", "Female")

# CYP temporary accom indicator: (30161)
tempaccom_total <- get_data("T4", "Total")

# Save ready for the main analysis function
saveRDS(homeless_total, file=paste0(profiles_data_folder, '/Prepared Data/homeless_total_raw.rds'))
saveRDS(homeless_male, file=paste0(profiles_data_folder, '/Prepared Data/homeless_male_raw.rds'))
saveRDS(homeless_female, file=paste0(profiles_data_folder, '/Prepared Data/homeless_female_raw.rds'))
saveRDS(tempaccom_total, file=paste0(profiles_data_folder, '/Prepared Data/tempaccom_total_raw.rds'))


#########################################
# 3 - Run main analysis to aggregate and calculate rates ---- 
#########################################

# Added NA_means_suppressed = TRUE: no suppression in data currently, but adding to future-proof the code

# Run main analysis function
main_analysis(filename = "homeless_total", ind_id = 30034, geography = "council", measure = "crude", 
              pop = "CA_pop_19+", yearstart = 2002, yearend = 2023,
              time_agg = 1, crude_rate = 1000, year_type = "financial", police_div=TRUE, NA_means_suppressed = TRUE)

#saved to 'to be checked folder' but is not standalone indicator - only required to generate popgrp data
main_analysis(filename = "homeless_male", ind_id = 30034, geography = "council", measure = "crude", 
              pop = "CA_pop_M_19+", yearstart = 2002, yearend = 2023,
              time_agg = 1, crude_rate = 1000, year_type = "financial", police_div=TRUE, NA_means_suppressed = TRUE)

#saved to 'to be checked folder' but is not standalone indicator - only required to generate popgrp data
main_analysis(filename = "homeless_female_X", ind_id = 30034, geography = "council", measure = "crude", 
              pop = "CA_pop_F_19+", yearstart = 2002, yearend = 2023,
              time_agg = 1, crude_rate = 1000, year_type = "financial",police_div=TRUE, NA_means_suppressed = TRUE)

main_analysis(filename = "tempaccom_total", ind_id = 30161, geography = "council", measure = "crude", 
              pop = "CA_pop_under16", yearstart = 2002, yearend = 2024, # requires the 2024 MYE, due Aug 2025.
              time_agg = 1, crude_rate = 1000, year_type = "snapshot",police_div=TRUE, NA_means_suppressed = TRUE)




##########################################################
### 4 - Prepare final files (required for popgroups only) -----
##########################################################

homeless_male <- readRDS(file.path(profiles_data_folder, "Data to be checked", "homeless_male_shiny.rds") ) %>% mutate(split_name="Gender", split_value="Male")
homeless_female <- readRDS(file.path(profiles_data_folder, "Data to be checked", "homeless_female_shiny.rds") ) %>% mutate(split_name="Gender", split_value="Femle")
homeless_total <- readRDS(file.path(profiles_data_folder, "Data to be checked", "homeless_total_shiny.rds") ) %>% mutate(split_name="Gender", split_value="Total")
homeless_popgroup <- rbind(homeless_male, homeless_female, homeless_total)

# Save
write_rds(homeless_popgroup, paste0(profiles_data_folder, "/Data to be checked/homeless_shiny_popgrp.rds"))
write.csv(homeless_popgroup, paste0(profiles_data_folder, "/Data to be checked/homeless_shiny_popgrp.csv"), row.names = FALSE)

# QA popgrp data:
run_qa(type = "popgrp", filename = "homeless", test_file=FALSE)



##END
