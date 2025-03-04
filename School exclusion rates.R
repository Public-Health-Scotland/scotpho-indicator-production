
### 1. notes -----

# this script updates the following indicator: 13016 - School exclusion rate

# numerator source (no. of exclusions): https://www.gov.scot/publications/school-exclusion-statistics/ INCLUDES: PUBLICLY-FUNDED LOCAL AUTHORITY SCHOOLS ONLY. NOT INCLUDING GRANT-AIDED SCHOOLS. 
# denominator source (no. of pupils):   https://www.gov.scot/publications/pupil-census-supplementary-statistics/  (Table 5.1: Schools by local authority)
# update file names and ranges below when new data from links above are saved in "data received" folder.
# Latest data (2022/23) published March 2024

#################################################################################
### 1. Required packages/functions -----
#################################################################################

source("functions/main_analysis.R") # needed for the QA
source("functions/deprivation_analysis.R") # needed for the QA
library(hablar) # for sums that ignore NA only if other values are present, otherwise it returns NA
library(readxl) # for reading in excel files

# Read in geography lookup
geo_lookup <- readRDS(paste0(profiles_lookups, "/Geography/opt_geo_lookup.rds")) %>% 
  select(!c(parent_area, areaname_full))

# health board lookup
hb <- readRDS(paste0(profiles_lookups, "/Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)


#################################################################################
### 2. Read in data -----
#################################################################################

# file paths:

# the folder where the data are saved
exclusions_folder <- paste0(profiles_data_folder, "/Received Data/School Exclusion Rates/")
# counts of exclusions (numerators)
exclusions2022 <- paste0(exclusions_folder, "Exclusions_202223.xlsx") # Numerators for Scotland and LAs
exclusions2022_simd <- paste0(exclusions_folder, "Exclusions+2022-23+supplementary+statistics+-+March.xlsx") # Numerators for SIMD in 2022
exclusions2020_simd <- paste0(exclusions_folder, "Exclusion+statistics+202021.xlsx") # Numerators for SIMD in 2020
# pupil counts (denominators)
census2022 <- paste0(exclusions_folder, "Pupils_Census_2022.xlsx") # Denominators for Scotland, LAs, and SIMD 2022 
census2020 <- paste0(exclusions_folder, "Pupils_in_Scotland_2020.xlsx") # For SIMD denominators for 2020


#################################################################################
# Get Scotland and CA data (numerators and denominators), 
# ...then aggregate to HB and calculate the indicator.
#################################################################################


# Numerators:

# exclusions by council area (time series from 2007/08)
ca_exclusions <- read_xlsx(exclusions2022, sheet = "Table 2.1", skip = 3) |> 
  head(32) %>% #select first 32 rows (excludes all LA totals, as we get these for a longer time series from sheet Table 1.1)
  mutate_at(c(2:11), as.numeric) %>% # some suppressed values converted to NA
  pivot_longer(-c("Local Authority"), names_to = "trend_axis", values_to = "numerator") 

# exclusions scotland level (time series from 2002/03)
scotland_exclusions <- read_xlsx(exclusions2022, sheet = "Table 1.1", skip = 3) |> 
  filter(`Exclusion type` == "All cases of exclusion") |> #excludes breakdown of exclusion types
  mutate_at(c(2:16), as.numeric) %>% 
  select(-`Exclusion type`) |> 
  mutate("Local Authority" = "All local authorities") %>%
  pivot_longer(-c("Local Authority"), names_to = "trend_axis", values_to = "numerator") 


# Denominators:

# total pupils by council area 
total_pupils <- read_xlsx(census2022, sheet = "Table 5.2", skip = 4) |>  
  head(33) |> # we want 'All local authorities' rather than 'Scotland' here, as the Scotland figure includes Grant-aided schools that are not included in the exclusions stats.
  mutate_at(c(2:23), as.numeric) %>% 
  pivot_longer(-c(`Local Authority`), names_to = "year", values_to = "denominator") %>%
  mutate(year = as.numeric(substr(year, 1, 4)))


# Combine
exclusions_ca_scot <- rbind(ca_exclusions, scotland_exclusions) %>%
  mutate(trend_axis = substr(trend_axis, 1, 7),
         year = as.numeric(substr(trend_axis, 1, 4))) %>%
  mutate(`Local Authority` = gsub(" and ", " & ", `Local Authority`)) %>% # for matching with denoms
  merge(y = total_pupils, by=c("Local Authority", "year"), all.x=T) %>% # add denoms
  rename(areaname = `Local Authority`) %>%
  mutate(areatype = ifelse(areaname == "All local authorities", "Scotland", "Council area"),
         areaname = ifelse(areaname == "All local authorities", "Scotland", areaname)) %>%
  # add the geog codes, 
  merge(y=geo_lookup, by=c("areaname", "areatype"), all.x=TRUE) %>%  # kept all.x to check the match... produced a full match...
  select(-starts_with("area"))

# Add in HB codes and aggregate counts to this geog
exclusions_hb <- exclusions_ca_scot %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code) %>%
  rename(code = hb2019) %>%
  group_by(code, trend_axis, year) |>
  summarise_all(sum_) %>% # sum function from hablar package
  ungroup()

# Process the indicator: calc rates and CIs and add required columns
exclusions_all <- rbind(exclusions_ca_scot, exclusions_hb) %>%
  mutate(ind_id = 13016,
         def_period = paste0("School year (", trend_axis, ")")) %>%
  # calculate the rate and the confidence intervals (Byars method)
  mutate(rate = numerator/denominator*1000,
         o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
         o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
         lowci = o_lower/(denominator)*1000,
         upci = o_upper/(denominator)*1000) %>% 
  mutate(lowci = ifelse(is.nan(lowci), 0, lowci)) %>% # sorts out NaNs where lowci couldn't be calculated for a rate of 0
  select(-o_lower, -o_upper) 



#################################################################################
# Get the SIMD level data: 
#################################################################################

# Available only since 2020/21

# Read in the data:

# Crude rates (cases per 1000 pupils)
#######################################

## LAs, 2020:
la_simd_2020 <- read_xlsx(exclusions2020_simd, sheet = "Table 2.10", range = "A72:F104") %>% # no total row (this is obtained below)
  mutate(trend_axis = "2020/21",
         stat = "rate")
## Scotland, 2020:
scot_simd_2020 <- read_xlsx(exclusions2020_simd, sheet = "Table 1.11", range = "A4:F7") %>% 
  rename(stat = 'Exclusion statistic') %>%
  mutate(trend_axis = "2020/21",
         "Local Authority" = "All local authorities",
         stat = case_when(stat=="Cases of exclusion" ~ "numerator",
                          stat=="Cases of exclusion per 1,000 pupils [note 4]" ~ "rate")) %>%
  filter(!is.na(stat))
## LAs and Scotland, 2022:
la_simd_2022 <- read_xlsx(exclusions2022_simd, sheet = "Table 2.10", range = "A74:F107") %>% # includes a total row
  mutate(trend_axis = "2022/23",
         stat = "rate")

# Numerators
#######################################

## LAs, 2020:
la_simd_2020_nums <- read_xlsx(exclusions2020_simd, sheet = "Table 2.10", range = "A4:F36") %>% # no total row (this is obtained above)
  mutate(trend_axis = "2020/21",
         stat = "numerator")
## LAs and Scotland, 2022
la_simd_2022_nums <- read_xlsx(exclusions2022_simd, sheet = "Table 2.10", range = "A4:F37") %>% # includes a total row
  mutate(trend_axis = "2022/23",
         stat = "numerator")


# Denominators: (pupil counts, in LA schools)
#######################################

# (Available as deciles: need to sum to get quintile counts)

## LAs, 2020
la_simd_2020_denoms <- read_xlsx(census2020, sheet = "Table 5.10", range = "A5:K37") %>% #no total row (the Scotland value includes Grant-aided, so needs to be excluded)
  mutate_at(c(2:11), as.numeric) %>% # some suppressed values converted to NA
  rename(`Local Authority` = `...1`) %>%
  group_by(`Local Authority`) %>%
  mutate("SIMD Quintile 1" = sum_(c(`1`, `2`)),
         "SIMD Quintile 2" = sum_(c(`3`, `4`)),
         "SIMD Quintile 3" = sum_(c(`5`, `6`)),
         "SIMD Quintile 4" = sum_(c(`7`, `8`)),
         "SIMD Quintile 5" = sum_(c(`9`, `10`))) %>% # includes some zero denominators for the islands
  ungroup() %>%
  mutate(trend_axis = "2020/21",
         stat = "denominator") %>%
  select(-c(`1`:`10`))

## LAs, 2022
la_simd_2022_denoms <- read_xlsx(census2022, sheet = "Table 5.10", range = "A5:K37") %>% #no total row (the Scotland value includes Grant-aided, so needs to be excluded)
  mutate_at(c(2:11), as.numeric) %>% # some suppressed values converted to NA
  group_by(`Local Authority`) %>%
  mutate("SIMD Quintile 1" = sum_(c(`SIMD 1`, `SIMD 2`)),
         "SIMD Quintile 2" = sum_(c(`SIMD 3`, `SIMD 4`)),
         "SIMD Quintile 3" = sum_(c(`SIMD 5`, `SIMD 6`)),
         "SIMD Quintile 4" = sum_(c(`SIMD 7`, `SIMD 8`)),
         "SIMD Quintile 5" = sum_(c(`SIMD 9`, `SIMD 10`))) %>% # includes some zero denominators for the islands
  ungroup() %>%
  mutate(trend_axis = "2022/23",
         stat = "denominator") %>%
  select(`Local Authority`, trend_axis, stat, starts_with("SIMD Q"))



# Combine LA data
la_simd_denoms <- rbind(la_simd_2020_denoms, la_simd_2022_denoms)

# Aggregate to give Scotland level data
scot_simd_denoms <- la_simd_denoms %>%
  mutate(`Local Authority` = "All local authorities") %>%
  group_by(`Local Authority`, trend_axis, stat) %>%
  summarise_all(sum_) %>%
  ungroup()


# Combine the data and process
exclusions_la_scot_simd <- rbind(la_simd_2020, scot_simd_2020, la_simd_2022, # rates
                                 la_simd_2020_nums, la_simd_2022_nums, # numerators
                                 la_simd_denoms, scot_simd_denoms) %>% # denominators (required for deriving HB data)
  rename(areaname = "Local Authority") %>%
  mutate(areatype = ifelse(areaname == "All local authorities", "Scotland", "Council area")) %>%
  mutate(areaname = ifelse(areaname == "All local authorities", "Scotland", areaname)) %>%
  # amend the spatial unit col for merging with lookup
  mutate(areaname = case_when(areaname == "Edinburgh City" ~ "City of Edinburgh",
                              areaname == "Eilean Siar" ~ "Na h-Eileanan Siar",
                              TRUE ~ gsub(" and ", " & ", areaname))) %>%
  # add the geog codes, 
  merge(y=geo_lookup, by=c("areaname", "areatype"), all.x=TRUE) %>% # kept all.x to check the match... produced a full match...
  # keep required columns
  select(trend_axis, code, stat, starts_with("SIMD Q")) %>%
  # reshape
  pivot_longer(-c(trend_axis, code, stat), names_to = "quintile", names_prefix = "SIMD Quintile ", values_to = "value") %>%
  mutate(value = as.numeric(value))  %>% # some suppressed values converted to NA
  pivot_wider(names_from = stat, values_from = value) 

# Add in HB codes and aggregate counts to this geog
exclusions_hb_simd <- exclusions_la_scot_simd %>%
  merge(y=hb, by.x="code", by.y= "ca2019") %>%
  select(-code, -rate) %>%
  rename(code = hb2019) %>%
  group_by(code, trend_axis, quintile) |>
  summarise_all(sum_) %>%
  ungroup() %>%
  mutate(rate = 1000 * numerator/denominator)

# combine all SIMD data  
exclusions_all_simd <- rbind(exclusions_hb_simd, exclusions_la_scot_simd) %>%
  mutate(year = as.numeric(substr(trend_axis, 1, 4)),
         ind_id = 13016,
         def_period = paste0("School year (", trend_axis, ")")) %>%
  # calculate the confidence intervals (Byars method)
  mutate(o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
         o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
         lowci = o_lower/(denominator)*1000,
         upci = o_upper/(denominator)*1000) %>% 
  mutate(lowci = ifelse(is.nan(lowci), 0, lowci)) %>% # sorts out NaNs where lowci couldn't be calculated for a rate of 0
  mutate(rate = ifelse(numerator==0 & denominator==0 & is.nan(rate), 0, rate), # fix some NaN/Inf where rate and CIs should be 0
         lowci = ifelse(numerator==0 & denominator==0  & is.nan(lowci), 0, lowci),
         upci = ifelse(numerator==0 & denominator==0  & is.infinite(upci), 0, upci)) %>%
  select(-o_lower, -o_upper)

simd_totals <- exclusions_all %>%
  filter(year %in% exclusions_all_simd$year) %>% # gives 94 = 2 years x (47 areanames (=1 + 32 + 14))
  mutate(quintile = "Total") 

exclusions_all_simd <- rbind(exclusions_all_simd, simd_totals) %>%
  # drop records where a geog doesn't have the specific SIMD2020 quintile: Orkney, Shetland and Western Isles. 
  # These have 0 numerators and rates in the data files, but should be NA
  filter(!(code %in% c("S12000027",	"S08000026", "S12000013",	"S08000028", "S12000023",	"S08000025") & quintile=="1")) %>%
  filter(!(code %in% c("S12000027",	"S08000026", "S12000013",	"S08000028") & quintile=="5")) %>%
  filter(!(code %in% c("S12000013",	"S08000028") & quintile=="4")) %>%
  filter(!is.na(denominator)) #remove suppressed denominators
# No denominators should be zero now.




#################################################################################
# make population file for pupils by year and SIMD quintile, for inequalities metrics:
#################################################################################
# (I checked the existing file depr_pop_schoolpupils.rds but this doesn't have HB)
depr_pop_pupils_HB_CA_2020_2022 <- exclusions_all_simd %>%
  select(year, code, quintile, denominator) %>%
  mutate(quint_type = "sc_quin") 

saveRDS(depr_pop_pupils_HB_CA_2020_2022, paste0(profiles_lookups, "/Population/depr_pop_pupils_HB_CA_2020_2022.rds"))




##########################################################
### 3. Prepare final files -----
##########################################################


# 1 - main data (ie data behind summary/trend/rank tab)
main_data <- exclusions_all %>% 
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  unique() %>%
  arrange(code, year)

# Save
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_shiny.csv"), row.names = FALSE) 


# 3 - SIMD data (ie data behind deprivation tab)

# add population data (quintile level) so that inequalities can be calculated
simd_data <- exclusions_all_simd %>%
  mutate(quint_type = "sc_quin") %>%
  select(-denominator) %>%
  arrange(code, year, quintile) %>%
  add_population_to_quintile_level_data(pop="depr_pop_pupils_HB_CA_2020_2022",
                                        ind = 13016, ind_name = "school_exclusions") %>%
  filter(!is.na(rate)) # not all years have data

# calculate the inequality measures
simd_data <- simd_data |>
  calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
  select(-c(overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields

# save the data as RDS file
saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_ineq.rds"))


##########################################################
### 4. Run QA reports -----
##########################################################

# # Run QA reports 
run_main_analysis_qa(filename="school_exclusions", test_file=FALSE)
# Flagged big differences between this and the last version of the indicator.
# These occurred for Clackmannanshire and Orkney in 2020. 
# The values were suppressed in the original dataset, but were coded erroneously as 0 in the last version of the indicator. 
# In the current version suppressed data are presented as NA, hence the differences observed.

run_qa(type = "deprivation", filename="school_exclusions", test_file=FALSE)
# Shows lots of missings (due to suppression) at LA level. Much less of a problem at HB level.
# Restrict deprivation analysis to HBs and Scotland:
simd_data <- simd_data %>%
  filter(!(substr(code, 1, 3)=="S12"))

# save the data as RDS file
saveRDS(simd_data, paste0(profiles_data_folder, "/Data to be checked/school_exclusions_ineq.rds"))

# run QA report again
run_qa(type = "deprivation", filename="school_exclusions", test_file=FALSE)
# Orkney HB missing 2020/21 deprivation data. 



##END
