# ============================================================================
# ===== Processing PHS HOSPITAL STATS (SMR01 AND SMR04) =====
# ============================================================================

# NOTES ON THE INDICATOR

# 1 adult MH indicator: 
# ind_id = 30006 = "Rate of alcohol-related hospitalisation from mental and behavioural disorders due to alcohol use, per 100,000 adults."

# Whether rate of patients or stays was not specified in MHI spec. 
# I opted to use stays, as this is the headline stat the ARHS team use in their publications. 

# Changes in 2025: the alcohol team now provide the raw counts by DZ in an rds file:

# SEPT 2025: 
# Data provided by Scott Kilgariff, mailto:phs.alcohol@phs.scot
# rds file at DZ11 level: "IR2025-00668-output.RDS"
# Patients/stays from 16 years and older.
# Only Scottish residents have been included
# Only those patients/stays where the sex is known have been included

# OCT 2025: 
# Alc team confirmed that age_grp 4 = 16-19 years (not 15-19, as labelled) and provided a new rds file "IR2025-00668-output_updated.RDS" with DZ01 for 1998 to 2011 and DZ11 for 2012 onwards.
# Use updated file for SIMD analysis, but original file for main analysis.
# DZ01 have been supplied up to and including 2011, because the alcohol team have been using SIMD 2016 for 2012 and 2013 data (a departure from the ISD GPD guidance which uses SIMD 2012 for these years)
# This has been noted in the indicator metadata.

# OCT 2026:
# Data to 2025/26 provided: IR2026-00790-output.RDS
# 
# FUTURE DATA:
# I queried the use of SIMD 2016 rather than SIMD 2012 with the alcohol team, and their analysis will be reviewed in future. Response received:
# "Thanks for bringing the updated guidance to our attention. 
# I’ve looked in to our existing code and we use SIMD2016 for 2012 & 2013 because we use the 2011 Data Zones for our population estimates. 
# The current guidance states that “Pre-SIMD 2016 versions cannot be used with 2011 Data Zone” which is quoted in the commentary for the script calculating decile populations.
# However, this was decided based on the 3.3 version of the guidance so we will be looking into this further for future updates."

# statistical disclosure control applied to numerator after rate calculations : CHECK FINAL OUTPUT WITH ALCOHOL TEAM BEFORE PUBLISHING.
# Method used in disclosure control should align with that used in total alcohol related admissions indicator & alcohol team publication not be published. 


##########################################################
### Functions/packages -----
##########################################################

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA
source("//PHI_conf/ScotPHO/Profiles/Code/stat_disclosure_alcohol_stays.R") # statistical disclosure methodology - confidential - do not share

##########################################################
### Read in data -----
##########################################################

# File location
arhs_data_folder <- paste0(profiles_data_folder, "/Received Data/Alc-related hosp stays - mental and behavioural disorders/")
file <- "IR2026-00790-output.RDS"

# lookups
geography_lookups <- file.path(profiles_data_folder, "Lookups", "Geography")
population_lookups <- file.path(profiles_data_folder, "Lookups", "Population")
simd_lookup <- readRDS(file.path(geography_lookups, "simd_datazone_lookup.rds"))

# DZ to higher geog lookup
dz_to_higher_geogs <- simd_lookup %>%
  select(datazone, ca, hb, hscp, pd, scotland) %>%
  unique() # n=13481, i.e., n DZ2001 + n DZ2011; So that's correct.

# DZ to SIMD lookup
# The ARHS data and publications use SIMD 2012 for 2010-11 and SIMD 2016 for 2012-16
# This differs from the simd_lookup used in the deprivation_analysis process, where SIMD 2012 is used up to 2013.
# Here we adjust the simd_lookup file to match the data, and run the other functions used in deprivation_analysis
# data for 2012 and 2013 need to be replaced with a copy of the data for 2014
dz_to_simd <- simd_lookup %>%
  select(datazone, year, ends_with("quin")) 
simd_2014 <- dz_to_simd %>% filter(year == 2014)
simd_2012 <- simd_2014 %>% mutate(year = 2012)
simd_2013 <- simd_2014 %>% mutate(year = 2013)
dz_to_simd <- dz_to_simd %>%
  filter(!year %in% c(2012, 2013)) %>%
  rbind(simd_2012, simd_2013)

# Population lookups:
# use the pops for calculating std rates, as these include age group and sex splits
# choose 16+ SR files as these restrict the pop in agegrp 4 to be 16-19y rather than 15-19y.
# lowest geogs are CA, so merge into the numerator data after aggregating
depr_pop_16plus <- readRDS(paste0(population_lookups, "/depr_pop_16+_SR.rds")) %>% filter(str_detect(quint_type, "quin")) # drop deciles for now
CA_pop_16plus <- readRDS(paste0(population_lookups, "/CA_pop_16+_SR.rds"))

# Numerator data
arhs_dz <- readRDS(paste0(arhs_data_folder, file)) %>%
  mutate(sex_grp = ifelse(sex=="Female", 2, 1),
         year = as.numeric(substr(financial_year, 1, 4))) %>%
  select(year, datazone, sex_grp, age_grp, numerator = stays) 

# check if there any NAs in the geography code column - even though these cannot
# be mapped to any local areas, they are used within the Scotland totals.
# assign them the scotland geography code to ensure they're included when data is summarised by geography 
na_vals <- arhs_dz |>
  filter(is.na(datazone)) |>
  select(-datazone) |>
  mutate(code = "S00000001")

# add simd quintiles, by year:
arhs_simd <- arhs_dz %>%
  merge(y=dz_to_simd, by=c("year", "datazone")) # those without a dz (those in na_vals) are dropped automatically

# join the geography lookup to the data so there is a column for each geography level
arhs_higher <- arhs_simd %>%
  merge(y=dz_to_higher_geogs) 

# pivot the data into a 'longer' format so there is just one geography column called 'code'
arhs_higher <- arhs_higher |>
  select(-contains("datazone")) |> # remove datazone column if this was the base geography - we don't publish at this level
  tidyr::pivot_longer(cols = c(ca, hb, hscp, pd, scotland), values_to = "code", names_to = NULL) %>%
  # add NA values back into the dataset  
  bind_rows(na_vals)

# For main and popgroup data: 
# aggregate by each geography code
arhs_agg <- arhs_higher |>
  select(-ends_with("quin")) %>%
  group_by(across(any_of(c("code", "year", "age_grp", "sex_grp")))) |>
  summarise_all(sum, na.rm = T) |>
  ungroup() 

# then add populations
arhs_agg_pop <- arhs_agg %>%
  filter(year >= 2002 & year <= 2024) %>% # match the pop data (2002-2025) and arhs data (1998-2024)
  merge(y=CA_pop_16plus, by=c("year", "code", "sex_grp", "age_grp"), all=TRUE) %>%
  mutate(numerator = replace_na(numerator, 0))
  
# and calculate easrs:
arhs_main_rates <- calculate_easr(data=arhs_agg_pop, 
                                  epop_total = 165800, # 82900 x 2 sexes (because age grp 4 starts at 16y)
                                  epop_age = "16+") # calculate standardised rate
arhs_main_rates_F <- calculate_easr(data = arhs_agg_pop %>% filter(sex_grp==2), 
                                  epop_total = 82900, # because starts at 16y
                                  epop_age = "16+") # calculate standardised rate
arhs_main_rates_M <- calculate_easr(data = arhs_agg_pop %>% filter(sex_grp==1), 
                                    epop_total = 82900, # because starts at 16y
                                    epop_age = "16+") # calculate standardised rate



# For simd data: aggregate by each geography code & quintile type
arhs_simd_agg <- arhs_higher |>
  tidyr::pivot_longer(cols = ends_with("quin"), values_to = "quintile", names_to = "quint_type") %>%
  filter((code == "S00000001" & quint_type == "sc_quin")
         |(str_detect(code, "S08") & quint_type %in% c("sc_quin", "hb_quin")) # select the right quintile types for the geography
         |(str_detect(code, "S12") & quint_type %in% c("sc_quin", "ca_quin"))
         |(str_detect(code, "S37") & quint_type %in% c("sc_quin", "hscp_quin"))
         |(str_detect(code, "S32") & quint_type %in% c("sc_quin"))) %>%
  group_by(across(any_of(c("code", "year", "age_grp", "sex_grp", "quint_type", "quintile")))) |>
  summarise_all(sum, na.rm = T) |>
  ungroup() %>%
  filter(!is.na(quintile)) # drop those with no datazone to start (the na_vals)

# then add populations
arhs_simd_agg_pop <- arhs_simd_agg %>%
  group_by(code, year, age_grp, sex_grp, quint_type) %>%
  summarise(numerator = sum(numerator, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(quintile="Total") %>%
  rbind(arhs_simd_agg) %>%
  filter(year >= 2002 & year <= 2024) %>% # match the pop data (2002-2024) and arhs data (1998-2024)
  merge(y=depr_pop_16plus, by=c("year", "code", "sex_grp", "age_grp", "quintile", "quint_type"), all=TRUE) %>%
  mutate(numerator = replace_na(numerator, 0))

# and calculate easrs:
arhs_simd_rates <- calculate_easr(data=arhs_simd_agg_pop, 
                                  epop_total = 165800, # 82900 x 2 sexes (because age grp 4 starts at 16y)
                                  epop_age = "16+") # calculate standardised rate
arhs_simd_rates_F <- calculate_easr(data = arhs_simd_agg_pop %>% filter(sex_grp==2), 
                                    epop_total = 82900, # because starts at 16y
                                    epop_age = "16+") # calculate standardised rate
arhs_simd_rates_M <- calculate_easr(data = arhs_simd_agg_pop %>% filter(sex_grp==1), 
                                    epop_total = 82900, # because starts at 16y
                                    epop_age = "16+") # calculate standardised rate


# Calculate measures of inequality  ----
arhs_simd_rates_2 <- calculate_inequality_measures(
  calculate_easr(data=arhs_simd_agg_pop, 
                 epop_total = 165800, # 82900 x 2 sexes (because age grp 4 starts at 16y)
                 epop_age = "16+") # calculate standardised rate
  )

# check the rates: look ok?
# compare with previous
# drop small areas x simd (or don't calc in first place

)
# Save the raw data for feeding to the processing functions
# Total population
saveRDS(arhs, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_raw.rds"))
# Subsets by sex
arhs_m <- arhs %>% filter(sex_grp==1)
saveRDS(arhs_m, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_M_raw.rds"))
arhs_f <- arhs %>% filter(sex_grp==2)
saveRDS(arhs_f, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_F_raw.rds"))

##########################################################
### Main analysis (used for pop group files too) -----
##########################################################

# Run main_analysis to produce total population rates
main_analysis(filename="ARHS_mental_and_behav",
                            measure = "stdrate",
                            geography = "datazone11",
                            year_type = "calendar",
                            ind_id = 30006, 
                            time_agg = 1, 
                            yearstart=2002, 
                            yearend=2023, 
                            pop = "DZ11_pop_16+", 
                            epop_total = 165800, # 82900 x 2 sexes (because age grp 4 starts at 16y)
                            epop_age = "16+", 
                            test_file = FALSE, 
                            QA = TRUE, 
                            police_div = TRUE)

popgrp_total <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Total")

# Run main_analysis to produce rates by sex
main_analysis(filename="ARHS_mental_and_behav_M",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2023, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # because starts at 16y
              epop_age = "16+", 
              test_file = FALSE, 
              QA = TRUE, 
              police_div = TRUE)

popgrp_m <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Male")

main_analysis(filename="ARHS_mental_and_behav_F",
              measure = "stdrate",
              geography = "datazone11",
              year_type = "calendar",
              ind_id = 30006, 
              time_agg = 1, 
              yearstart=2002, 
              yearend=2023, 
              pop = "DZ11_pop_16+", 
              epop_total = 82900, # because starts at 16y
              epop_age = "16+", 
              test_file = FALSE, 
              QA = TRUE, 
              police_div = TRUE)

popgrp_f <- main_analysis_result %>%
  mutate(split_name = "Sex",
         split_value = "Female")


# Combine data
stays <- rbind(popgrp_total,
               popgrp_f,
               popgrp_m) |>
  apply_stats_disc_mandb() |> # statistical disclosure applied to final values
  # remove small geographies
  filter(!(substr(code, 1, 3) %in% c("S02", "S99"))) #IZs and localities, these have some counts < 5
# now smallest numerator is 15



##########################################################
### Deprivation analysis -----
##########################################################

# The ARHS data and publications use SIMD 2012 for 2010-11 and SIMD 2016 for 2012-16
# This differs from the simd_lookup used in the deprivation_analysis process, where SIMD 2012 is used up to 2013.
# Here we adjust the simd_lookup file to match the data, and run the other functions used in deprivation_analysis

# Lookups
geography_lookups <- file.path(profiles_data_folder, "Lookups", "Geography")
population_lookups <- file.path(profiles_data_folder, "Lookups", "Population")

# get simd lookup
simd_lookup <- readRDS(file.path(geography_lookups, "simd_datazone_lookup.rds"))
# # data for 2012 and 2013 need to be replaced with a copy of the data for 2014
# simd_2014 <- simd_lookup %>% filter(year == 2014)
# simd_2012 <- simd_2014 %>% mutate(year = 2012)
# simd_2013 <- simd_2014 %>% mutate(year = 2013)
# simd_lookup <- simd_lookup %>%
#   filter(!year %in% c(2012, 2013)) %>%
#   rbind(simd_2012, simd_2013)


# Function to run the deprivation_analysis:
arhs_deprivation_analysis <- function(filename, yearstart, yearend, pop_sex, epop_total) {
  
  # Read in data
  data <- readRDS(file.path(profiles_data_folder, "Prepared Data", paste0(filename, "_raw.rds"))) |>
    janitor::clean_names()
  
  # Assign each geography to an SIMD quintile/decile  ----
  
  # join data with lookup
  data <- left_join(data, simd_lookup, by = c("datazone", "year"))
  
  # Aggregate by geography and SIMD quintiles/deciles  ----
  # keep only Scotland data (small numbers in smaller geographies)
  simd_data <- rbind(
    aggregate_by_simd(data, geo_col = "scotland", simd_col = "sc_decile"), # scotland data, split by scotland deciles
    aggregate_by_simd(data, geo_col = "scotland", simd_col = "sc_quin") # scotland data, split by scotland quintiles
  )

  # create overall totals
  totals <- simd_data |>
    mutate(quintile = "Total") |>
    group_by(across(any_of(c("year", "code", "quint_type", "quintile", "sex_grp", "age_grp")))) |>
    summarise_all(sum) |>
    ungroup()
  
  # combine totals with SIMD splits
  simd_data <- simd_data |>
    rbind(totals) |> # remove any data that wasn't assigned to a datazone in the prepared datafile, that therefore couldn't be assigned to any SIMD quintiles/deciles
    filter(!is.na(code))

  # Add population figures  ----

    #convert specified sex to code for lookup - this is used to filter population lookup
  if(pop_sex == "male") {
    pop_sex = 1
  } else if(pop_sex == "female") {
    pop_sex = 2
  } else {
    pop_sex = NULL  # No filtering on sex if "all"
  }
  
  # read in the simd population lookup, filter by age group and summarise
  population_lookup <- readRDS(file.path(population_lookups, "basefile_deprivation.rds")) |> #new file with PD, HSCP in it (revert to simd_population_lookup once that file has these geogs in)
    filter(year >= yearstart & year <=yearend)  |> 
    filter(age >= 16 & age <= 150) |> # filter by age range
    filter(code == "S00000001")

  # if indicator is for a single sex, filter pop lookup by that sex. Otherwise do not filter
  if (!is.null(pop_sex)) {
    population_lookup <- population_lookup |>
      filter(sex_grp == pop_sex)
  }
  
  grouping_vars <- c("year", "code", "quintile", "quint_type", "age_grp", "sex_grp")

  population_lookup <- population_lookup |>
    select(-age) |>
    group_by(across(grouping_vars)) |>
    summarise(denominator = sum(denominator), .groups = "drop")
  
  # join data with population lookup to add population column to use as denominator (when 2024 populations added double check that right join is most appropriate)
  simd_data <- right_join(simd_data, population_lookup, by = grouping_vars)
 
  # Aggregate by time period  ----

  # filter years
  simd_data <- simd_data |>
    filter(year >= yearstart & year <= yearend)
  
  # replace NAs with 0 before aggregating data by time period
  simd_data <- simd_data |>
    tidyr::replace_na(list(numerator = 0,
                           denominator = 0))
  
  # Calculate rate ----
  simd_data <- calculate_easr(simd_data, epop_total, epop_age="16+") # calculate standarised rate

  # Calculate measures of inequality  ----
  simd_data <- calculate_inequality_measures(simd_data)
  
  # Add some metadata columns  ----
  simd_data <- simd_data |>
    # create indicator id column - whatever id has been passed to the 'ind_id' argument should match
    # the id assigned to the indicator in our teams technical document
    mutate(ind_id = 30006) |>
    # create trend axis column (used to create axis labels on trend charts)
    create_trend_axis_column(year_type="calendar", agg=1) |>
    # create definition period column (used to show time period for charts looking at a single year)
    create_def_period_column(year_type="calendar", agg=1)
  
  # Save final file  ----

  # select final columns
  simd_data <- simd_data |>
    select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
              least_rate, par_rr, count))

  # save the data as an RDS file
  saveRDS(simd_data, file.path(profiles_data_folder, "Data to be checked", paste0(filename, "_ineq.rds")))
  
  # make results available in global environment
  deprivation_analysis_result <<- simd_data
  
  # Run QA  ----
  run_qa(type = "deprivation", filename={{filename}}, test_file=FALSE)

}



# Total population
arhs_deprivation_analysis(filename="ARHS_mental_and_behav", 
                          yearstart=1998, 
                          yearend=2024,
                          pop_sex = "all", 
                          epop_total = 165800) # because starts at 16y

arhs_simd_total <- deprivation_analysis_result %>%
  mutate(sex = "Total")

# Male population
arhs_deprivation_analysis(filename="ARHS_mental_and_behav_M", 
                          yearstart=2002, 
                          yearend=2023,
                          pop_sex = "male", 
                          epop_total = 82900) # because starts at 16y

arhs_simd_m <- deprivation_analysis_result %>%
  mutate(sex = "Male")

# Female population
arhs_deprivation_analysis(filename="ARHS_mental_and_behav_F", 
                          yearstart=2002, 
                          yearend=2023,
                          pop_sex = "female", 
                          epop_total = 82900) # because starts at 16y

arhs_simd_f <- deprivation_analysis_result %>%
  mutate(sex = "Female")


# Combine data
stays_simd<- rbind(arhs_simd_total,
                    arhs_simd_f,
                    arhs_simd_m) |>
  apply_stats_disc_mandb() # statistical disclosure applied to final values
# Smallest numerator is 207 for deciles (468 for quintiles)
  

##########################################################
### Prepare final files -----
##########################################################


# 1 - main data (ie data behind summary/trend/rank tab)

main_data <- stays %>% 
  filter(split_value == "Total") %>% 
  select(code, ind_id, year, 
         numerator, rate, upci, lowci, 
         def_period, trend_axis) %>%
  unique() 
# SMALLEST NUMERATOR = 75

# Save 
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny.csv"), row.names = FALSE)


# 2 - population groups data (ie data behind population groups tab)

pop_grp_data <- stays %>% # includes the contents of main data, as the 'total' for each split
  select(code, ind_id, year, numerator, rate, upci, 
         lowci, def_period, trend_axis, split_name, split_value,) 
# SMALLEST NUMERATOR = 15

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_shiny_popgrp.csv"), row.names = FALSE)


# 3 - SIMD data (ie data behind deprivation tab)
# SMALLEST NUMERATOR = 207

# Save
write_rds(stays_simd, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.rds"))
write.csv(stays_simd, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.csv"), row.names = FALSE)



##########################################################
### QA -----
##########################################################

run_qa(type = "main", filename = "ARHS_mental_and_behav", test_file = FALSE) # any differences across nested geogs are <=3 (explained by the rounding)
run_qa(type = "popgrp", filename = "ARHS_mental_and_behav", test_file = FALSE)
run_qa(type = "deprivation", filename = "ARHS_mental_and_behav", test_file = FALSE) 
