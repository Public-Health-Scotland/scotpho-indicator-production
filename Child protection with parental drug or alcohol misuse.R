# Child protection with parental drug or alcohol misuse.R

# 4 indicators generated by this script:
# 1. Child protection with parental drug misuse (ind_id = 4130)
# 2. Child protection with parental alcohol misuse (ind_id = 4110)
# 3. Child protection with parental drug or alcohol misuse (ind_id = 4153)
# 4. Children on the child protection register (ind_id = 13035)

# Full definition: (indicators 1-3)
# Rate of Child Protection Case Conference where parental drug, alcohol or substance misuse (ie drug &/or alcohol)
# has been identified for children on the register at 31st July, per 10,000 population aged under 18.


# Full definition: (indicator 4)
# Children on the child protection register; number and rate per 1,000 children under 18 years.
# N.B. Prior to 2025 the definition used on the site was under 16 years, but the data used were 0-17 year olds (mistakenly I think). 
# The same numerators are being used from now, but the denominators used to calculate the rates will be larger (0-17 year olds rather than 0-15 year olds)
# so the rates may be slightly smaller than previously presented. 


###############################################################################. 
## Analyst Notes on generating this indicator data ----

# This script analyses Scottish Government data on the number of children protection parental alcohol, drugs, substance misuse by local authority 

## Data are downloaded from the childrens social work publication on SG website:
## https://www.gov.scot/collections/childrens-social-work/

# Figures are published for Scotland and local authorities only. (ScotPHO derive NHS board and ADP figures by aggregating council area figures)

# Each year (around March) the SG publish an additional year of data.

###############################################################################.

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

### Load functions/dependencies ----

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA function
library(readxl) # imports xls and xlsx 
library(hablar) # sum_ function from hablar keeps NA when there should be NA

### Lookups ----

# Read in geography profiles_lookups
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# LAs to HBs lookup
hb <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)

# LAs to ADPs lookup
adp <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, adp) %>%
  distinct(.)


# Make a pop lookup (all need 0-17y, with age groups and sex splits for Scotland)
# in thepry could use scotpho population lookup under18 files as pop source but simpler to code from phs lookups
# but not worth recoding

popfile <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/CA2019_pop_est_1981_2023.rds") %>%
  filter(age<18) %>%
  mutate(agegroup = case_when(age %in% c(0:4) ~ "0-4 years",
                                 age %in% c(5:10) ~ "5-10 years",
                                 age %in% c(11:15) ~ "11-15 years",
                                 age %in% c(16:17) ~ "16-17 years")) %>%
  mutate(sex = case_when(sex_name == "M" ~ "Male",
                         sex_name == "F" ~ "Female")) %>%
  mutate(trend_axis = as.character(year)) %>%
  select(trend_axis, code = ca2019, agegroup, sex, denominator = pop)
  
pop_la <- popfile %>%
  mutate(split_name = "Total", 
         split_value = "Total") %>%
  select(-agegroup, -sex)

pop_scot <- popfile %>%
  mutate(code = "S00000001",
         split_name = "Total", 
         split_value = "Total") %>%
  select(-agegroup, -sex)

pop_scot_sex <- popfile %>%
  mutate(code = "S00000001",
         split_name = "Sex", 
         split_value = sex) %>%
  select(-agegroup, -sex)

pop_scot_agegp <- popfile %>%
  mutate(code = "S00000001",
         split_name = "Age group", 
         split_value = agegroup) %>%
  select(-agegroup, -sex)

pop_scot_sex_total <- pop_scot_sex %>%
  mutate(split_value = "Total") 

pop_scot_agegp_total <- pop_scot_agegp %>%
  mutate(split_value = "Total") 

pop_lookup <- rbind(pop_la, 
                    pop_scot,
                    pop_scot_agegp, 
                    pop_scot_agegp_total,
                    pop_scot_sex,
                    pop_scot_sex_total) %>%
  group_by(trend_axis, code, split_name, split_value) %>%
  summarise(denominator = sum(denominator)) %>%
  ungroup()

# repeat 2023 data for 2024: (have added this to the caveats column)
# when indicator is updated in future the correc 2024 population should be applied and historic rates corrected
pop_lookup_incl_2024 <- pop_lookup %>%
  filter(trend_axis == 2023) %>%
  mutate(trend_axis = 2024) %>%
  rbind(pop_lookup)


### Paths ----

# Identify data folder
chprot_data_folder <- paste0(profiles_data_folder, "/Received Data/SG CYP Social Work stats/")
file <- "Children%27s+Social+Work+Statistics+-+Child+Protection+2022-23+-+Additional+Tables.xlsx"

# Identify data files 
chprot_data_files <- list.files(path = chprot_data_folder)

publication_2023 <- "Child+Protection+Statistics+2023-24+Publication+Tables.xlsx"                             
additional_2023 <- "Child+Protection+Statistics+2023-24+Additional+Tables.xlsx"                              
additional_2022 <- "Children%27s+Social+Work+Statistics+-+Child+Protection+2022-23+-+Additional+Tables.xlsx" 
additional_2021 <- "Children%27s+Social+Work+Statistics+2021-22+-+Additional+Tables.xlsx"                
additional_2020 <- "childrens-social-work-statistics-2020-21-additional-tables.xlsx"                         
additional_2019 <- "childrens-social-work-statistics-2019-20-additional-tables.xlsx"                         
additional_2018 <- "childrens-social-work-statistics-2018-19-revised-additional-tables.xlsx"                 
additional_2017 <- "additional-tables_2017to2018.xlsx"                                                       
additional_2016 <- "00543716_2016to17.xlsx"                                                                  
additional_2015 <- "CSWS+Additional+Tables+2015-16.xlsx"      

### Bespoke functions ----

# Function to import data from a wide format tab, and replace suppressed with NA
import_wide_data <- function(filename, sheetnum, range, non_num_cols) {
  df <- read_excel(paste0(chprot_data_folder, filename),
                   sheet = paste0("Table ", sheetnum),
                   range = range) %>%
    mutate(across(everything(), ~str_replace(., "\\*", "NA"))) %>%
    mutate(across(-all_of(non_num_cols), ~as.numeric(.)))  
}  


# Function to import LA data from a wide format tab, replace suppressed with NA, make long, and standardise var names
import_wide_LA_data <- function(filename, sheetnum, range, year) {
  df <- read_excel(paste0(chprot_data_folder, filename),
                   sheet = paste0("Table ", sheetnum),
                   range = range) %>%
    mutate(across(everything(), ~str_replace(., "\\*", "NA"))) %>%
    mutate(across(everything(), ~str_replace(., "-", "NA"))) %>%
    mutate(across(-1, ~as.numeric(.))) %>%
    pivot_longer(-(1), names_to = "indicator", values_to = "numerator") %>%
    mutate(trend_axis = year,
           areatype = "Council area") %>%
    rename(areaname = 1) %>%
    select(areaname, areatype, trend_axis, indicator, numerator)   
}  


################################################################################. 
## Part 1 - Read in data downloaded from SG and format for functions ----
################################################################################.

# CHILD PROTECTION REGISTER DATA

# Import the data: 
# Scotland
register_scot_trend_count <- import_wide_data(filename = publication_2023, sheetnum = "1.1", range = "A18:N39", non_num_cols = c(1:2)) %>%
  pivot_longer(-(1:2), names_to = "trend_axis", values_to = "count") %>%
  mutate(sex = ifelse(Category == "Total", as.character(NA), Category)) %>%
  fill(sex) %>% # replaces Total with the relevant sex/all 
  mutate(areaname = "Scotland", areatype = "Scotland") %>%
  select(areaname, areatype, trend_axis, sex, agegp = Subcategory, count) # can only use total age group when split by sex, and total sex when split by age group

register_scot_trend_total <- register_scot_trend_count %>%
  filter(agegp=="Total" & sex=="All") %>%
  mutate(split_name = "Total",
         split_value = "Total") %>%
  select(areaname, areatype, trend_axis, split_value, split_name, numerator = count)

register_scot_trend_sex <- register_scot_trend_count %>%
  filter(agegp=="Total") %>%
  mutate(split_name = "Sex",
         split_value = ifelse(sex=="All", "Total", sex)) %>%
  select(areaname, areatype, trend_axis, split_value, split_name, numerator = count)

register_scot_trend_agegp <- register_scot_trend_count %>%
  filter(sex=="All") %>%
  mutate(split_name = "Age group") %>%
  mutate(agegp = case_when(agegp %in% c("0-4", "11-15", "5-10") ~ paste0(agegp, " years"),
                           agegp == "16+" ~ "16-17 years",
                           TRUE ~ agegp)) %>%
  filter(agegp != "Unknown") %>%
  select(areaname, areatype, trend_axis, split_value = agegp, split_name, numerator = count) 

# LA
register_la_trend <- import_wide_data(filename = publication_2023, sheetnum = "1.2", range = "A40:Y72", non_num_cols = c(1)) %>%
  pivot_longer(-`Local authority`, names_to = c("trend_axis", "metric"), 
               names_pattern = "(\\d{4}) (\\w*).*?", # (\\d{4}) extracts a 4-digit number, (\\w*) extracts a word after a space after the number
               values_to = "value") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(areatype = "Council area",
         split_value = "Total",
         split_name = "Total") %>%
  select(areaname = `Local authority`, areatype, trend_axis, split_value, split_name, numerator = Number)

# # Too many 'not knowns' to use the splits
# register_scot_trend_count_splits <- import_wide_data(filename = additional_2023, sheetnum = "1.1", range = "A5:N27", non_num_cols = c(1:2)) %>%
#   pivot_longer(-(1:2), names_to = "trend_axis", values_to = "count") %>%
#   mutate(code = "S00000001") %>%
#   select(code, trend_axis, split_name = Category, split_value = Subcategory, count) 

all_register_data <- rbind(register_la_trend, 
                           register_scot_trend_total,
                           register_scot_trend_sex, 
                           register_scot_trend_agegp) %>%
  mutate(indicator = "child_protection_all")




# CHILD PROTECTION CASES WITH CONCERNS RECORDED AS ALCOHOL/DRUGS/SUBSTANCE ABUSE

# Import the data

# Scotland
concerns_scot_trend_count <- import_wide_data(filename = additional_2023, sheetnum = "1.2", range = "A5:M9", non_num_cols = c(1)) %>%
  pivot_longer(-(1), names_to = "trend_axis", values_to = "numerator") %>%
  mutate(areaname = "Scotland", areatype = "Scotland") %>%
  filter(`Type of Concern` != "Emotional abuse") %>%
  select(areaname, areatype, trend_axis, indicator = `Type of Concern`, numerator) 

# LA
concerns_la_count_2024 <- import_wide_LA_data(filename = additional_2023, sheetnum = "1.10", range = "A5:D37", year = "2024") 
concerns_la_count_2023 <- import_wide_LA_data(filename = additional_2022, sheetnum = "1.10", range = "A5:D37", year = "2023") 
concerns_la_count_2022 <- import_wide_LA_data(filename = additional_2021, sheetnum = "1.10", range = "A4:D36", year = "2022") 
concerns_la_count_2021 <- import_wide_LA_data(filename = additional_2020, sheetnum = "4.6", range = "A2:D34", year = "2021") 
concerns_la_count_2020 <- import_wide_LA_data(filename = additional_2019, sheetnum = "4.6", range = "A3:D35", year = "2020") 
concerns_la_count_2019 <- import_wide_LA_data(filename = additional_2018, sheetnum = "4.5", range = "A3:D35", year = "2019") 
concerns_la_count_2018 <- import_wide_LA_data(filename = additional_2017, sheetnum = "4.5", range = "A3:D35", year = "2018") 
concerns_la_count_2017 <- import_wide_LA_data(filename = additional_2016, sheetnum = "4.5", range = "A3:D35", year = "2017") 
concerns_la_count_2016 <- import_wide_LA_data(filename = additional_2015, sheetnum = "4.5", range = "A3:D35", year = "2016") 


all_concerns <- do.call("bind_rows", mget(ls(pattern="^concerns_"))) %>%
  mutate(indicator = case_when(str_detect(indicator, "drug|Drug") ~ "child_protection_parental_drug",
                               str_detect(indicator, "alcohol|Alcohol") ~ "child_protection_parental_alcohol",
                               str_detect(indicator, "substance|Substance") ~ "child_protection_parental_substance_misuse")) %>%
  mutate(split_value = "Total",
         split_name = "Total")


# Combine 
all_data_scot_la <- rbind(all_concerns, all_register_data) %>%
  mutate(areaname = gsub(" and ", " & ", areaname)) %>%
  mutate(areaname = case_when(areaname == "Edinburgh, City of" ~ "City of Edinburgh",
                              areaname %in% c("Glasgow", "Glasgow City(7)") ~ "Glasgow City", 
                              areaname == "Na hNAEileanan Siar" ~ "Na h-Eileanan Siar",
                              TRUE ~ areaname)) %>%
  merge(y = geo_lookup, by=c("areaname", "areatype"), all.x=T) %>%
  merge(y = pop_lookup_incl_2024, by = c("trend_axis", "code", "split_value", "split_name"), all.x=T) %>%
  select(-areaname, -areatype) %>%
  filter(!is.na(denominator)) # this drops the unborn children counts, as no pop denominators for these. What could be used here?
  
# aggregate to HB 
all_data_hb <- all_data_scot_la %>%
  merge(y=hb, by.x="code", by.y = "ca2019") %>%
  group_by(trend_axis, indicator, split_value, split_name, hb2019) %>%
  summarise(numerator = sum_(numerator), # sum_function from hablar keeps true NAs rather than converting to 0
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  rename(code = hb2019)

# aggregate to ADP 
all_data_adp <- all_data_scot_la %>%
  merge(y=adp, by.x="code", by.y = "ca2019") %>%
  group_by(trend_axis, indicator, split_value, split_name, adp) %>%
  summarise(numerator = sum_(numerator), # sum_function from hablar keeps true NAs rather than converting to 0
            denominator = sum_(denominator)) %>%
  ungroup() %>%
  rename(code = adp)


# Combine all data:
all_data <- rbind(all_data_scot_la, all_data_hb, all_data_adp)



# Calculate the indicator data

# calculate crude rates
all_data <- all_data %>%
  # add the per x pop value (differs by indicator) 
  mutate(crude_rate = ifelse(indicator == "child_protection_all" , 1000, 10000)) %>%
  # calculate crude rate
  mutate(rate = numerator/denominator * crude_rate) %>%
  # calculate CIS
  mutate(lowci = (numerator * (1-1/9/numerator - 1.96/3/sqrt(numerator))^3) / (denominator) * crude_rate,
         upci = ((numerator + 1) *(1 - 1/9/(numerator + 1) + 1.96/3/sqrt(numerator + 1))^3) / (denominator) * crude_rate) %>%
  # add required columns
  # create indicator id column 
  mutate(ind_id = case_when(indicator == "child_protection_all" ~ 13035,
                            indicator == "child_protection_parental_drug" ~ 4130,
                            indicator == "child_protection_parental_alcohol" ~ 4110,
                            indicator == "child_protection_parental_substance_misuse" ~ 4153)) |>
  # create trend axis column (used to create axis labels on trend charts)
  mutate(year = as.numeric(trend_axis)) |>
  # create definition period column (used to show time period for charts looking at a single year)
  mutate(def_period = paste0("Snapshot 31 July, ", trend_axis))





# Function to prepare main data file: 
prepare_main_data <- function(ind){
  
  main_data <- all_data %>% 
    filter(indicator == ind,
           split_name == "Total") %>% 
    select(code, ind_id, year, 
           numerator, rate, upci, lowci, 
           def_period, trend_axis) %>%
    unique() %>%
    arrange(code, year)
  
  # save 
  write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.rds"))
  write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny.csv"), row.names = FALSE)
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data
  
}
  
# Function to prepare popgroup data file: 
prepare_popgrp_data <- function(ind){
  
  pop_grp_data <- all_data %>% 
    filter(indicator == ind & !(split_name %in% c("Total"))) %>% 
    select(code, ind_id, year, numerator, rate, upci, 
           lowci, def_period, trend_axis, split_name, split_value) %>%
    arrange(code, year)
  
  # Save
  write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.rds"))
  write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/", ind, "_shiny_popgrp.csv"), row.names = FALSE)

  # Make data created available outside of function so it can be visually inspected if required
  pop_grp_data_result <<- pop_grp_data
  
}


# Run function to create final files
prepare_main_data(ind = "child_protection_all")
prepare_popgrp_data(ind = "child_protection_all")

prepare_main_data(ind = "child_protection_parental_drug")
prepare_main_data(ind = "child_protection_parental_alcohol")
prepare_main_data(ind = "child_protection_parental_substance_misuse")

# Run QA reports 

# main data: 
run_qa(type = "main", filename = "child_protection_all", test_file = FALSE) # highlights that most/all rates lower than previously, due to correcting the denominator from under 16 to under 18
run_qa(type = "main", filename = "child_protection_parental_drug", test_file = FALSE) # differences found: investigated below
run_qa(type = "main", filename = "child_protection_parental_alcohol", test_file = FALSE) # differences found: investigated below
run_qa(type = "main", filename = "child_protection_parental_substance_misuse", test_file = FALSE) # differences found: investigated below


# The rate differences for child_protection_all due to higher denominators now is understandable and to be expected.
# Other differences highlighted needed checking out. 
# Different numerators were found for some areas.
# The old and new data are compared below. This found that 84% of the comparisons were identical.
# The biggest difference was for Angus, 2021, current data says 45 on child protection register, tho the previous data said 25. 
# The raw data confirmed this. I would trust the latest data as more accurate (numbers may have been amended in the data source since the last update).
# Also the previous data was a spreadsheet that just got added to with the latest year's data, without making updates to any previous data that may have been corrected.
# In conclusion: I'm happy with the current data as is. 

# import older data for comparison:
ca <- readRDS(paste0(profiles_lookups,"/Geography/CAdictionary.rds")) %>%
  rename("ca"="code")

old_data <- read_excel("/PHI_conf/ScotPHO/Profiles/Data/Received Data/SG CYP Social Work stats/archive/CP CSWS 2012_22 revised 19_20.xlsx") %>%
  mutate(la = str_replace(la, "Glasgow","Glasgow City"),
         la = str_replace(la, "Edinburgh, City of","City of Edinburgh"),
         la = str_replace(la, "Eilean Siar","Na h-Eileanan Siar"),
         la = str_replace(la, "&","and"),
         across(contains(c("drug", "alcohol", "substance_misuse", "all")), as.numeric)) %>% #convert data columns to numeric (suppressed)
  left_join(ca, by = c("la" = "areaname")) %>% # join with council area lookup
  mutate(code = ifelse(la == "Scotland", "S00000001", ca)) %>%
  select(-ca) %>%
  pivot_longer(cols = c(drug, alcohol, substance_misuse, all_on_register), names_to = "indicator", values_to = "old_numerators") %>%
  mutate(indicator = case_when(indicator == "drug" ~ "child_protection_parental_drug", 
                               indicator == "alcohol" ~ "child_protection_parental_alcohol", 
                               indicator == "substance_misuse" ~ "child_protection_parental_substance_misuse", 
                               indicator == "all_on_register" ~ "child_protection_all"))

new_data <- all_data_scot_la %>%
  filter(split_value=="Total" & split_name == "Total") %>%
  mutate(year = as.numeric(trend_axis)) %>%
  select(year, code, indicator, numerator)

compare <- new_data %>%
  merge(y = old_data, by=c("year", "code", "indicator")) %>%
  mutate(diff_pc = 100 * (old_numerators - numerator) / old_numerators )
# 84% (833 of 996) have identical numerators. 


###END ----

