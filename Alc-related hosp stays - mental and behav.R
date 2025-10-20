# ============================================================================
# ===== Processing PHS HOSPITAL STATS (SMR01 AND SMR04) =====
# ============================================================================

# NOTES ON THE INDICATOR

# 1 adult MH indicator: 
# ind_id = 30006 = "Rate of alcohol-related hospitalisation from mental and behavioural disorders due to alcohol use, per 100,000 adults."

# Whether rate of patients or stays was not specified in MHI spec. 
# I opted to use stays, as this is the headline stat the ARHS team use in their publications. 

# Data provided by Scott Kilgariff, mailto:phs.alcohol@phs.scot, in Sept 2025
# rds file at DZ11 level: "IR2025-00668-output.RDS"
# No suppression applied: CHECK FINAL OUTPUT WITH SCOTT'S TEAM BEFORE PUBLISHING.
# Patients/stays from 16 years and older.
# •	For the SIMD quintile figures I have used a top age group of 85+ whilst for the rest of the figures I have used a top age group of 90+ when calculating EASR.
# •	SIMD has been applied as follows:
# o	1996 to 2003 – SIMD 2004 has been used
# o	2004 to 2006 – SIMD 2006 has been used
# o	2007 to 2009 – SIMD 2009(v2) has been used
# o	2010 to 2011 – SIMD 2012 has been used (NB differs from the ISD GPD guidance which applies SIMD2012 to 2010-2013 data)
# o	2012 to 2016 – SIMD 2016 has been used
# o	2017+ – SIMD 2020(v2) has been used
# •	Only Scottish residents have been included
# •	Only those patients/stays where the sex is known have been included
# •	You’ll see in the attached that some of the population figures per group have decimal values. This is due to removing 0.2 of the 15-19 age group to create the 16-19 age group.

# UPDATE Oct 2025: Alc team confirmed that age_grp 4 = 16-19 years (not 15-19) and provided a new rds file "IR2025-00668-output_updated.RDS" with DZ01 for back to 1998.
#                  Use updated file for SIMD analysis, but original file for main analysis.
#                  Need to apply SIMD2012 to 2010 and 2011, then SIMD2016 to 2012 to 2016. 2012 and 2013 are the differences from ScotPHO protocol here. 
#                  Note this departure in the metadata.

##########################################################
### Functions/packages -----
##########################################################

source("functions/main_analysis.R") # for packages and QA function 
source("functions/deprivation_analysis.R") # for packages and QA


##########################################################
### Read in data -----
##########################################################

# File location
arhs_data_folder <- paste0(profiles_data_folder, "/Received Data/Alc-related hosp stays - mental and behavioural disorders/")
file1 <- "IR2025-00668-output.RDS"
file2 <- "IR2025-00668-output_updated.RDS"

arhs <- readRDS(paste0(arhs_data_folder, file1)) %>%
  mutate(sex_grp = ifelse(sex=="Female", 2, 1)) %>%
  select(year, datazone2011, sex_grp, age_grp, numerator = stays)

arhs_simd <- readRDS(paste0(arhs_data_folder, file2)) %>%
  mutate(sex_grp = ifelse(sex=="Female", 2, 1)) %>%
  select(year, datazone, sex_grp, age_grp, numerator = stays)

# Save the raw data for feeding to the processing functions

# File 1

# Total population
saveRDS(arhs, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_raw.rds"))

# Subsets by sex
arhs_m <- arhs %>% filter(sex_grp==1)
saveRDS(arhs_m, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_M_raw.rds"))

arhs_f <- arhs %>% filter(sex_grp==2)
saveRDS(arhs_f, file.path(profiles_data_folder, "Prepared Data", "ARHS_mental_and_behav_F_raw.rds"))

# File 2 (SIMD analysis)

# Total population
saveRDS(arhs_simd, file.path(profiles_data_folder, "Prepared Data", "ARHS_MBD_SIMD_raw.rds"))

# Subsets by sex
arhs_simd_m <- arhs_simd %>% filter(sex_grp==1)
saveRDS(arhs_simd_m, file.path(profiles_data_folder, "Prepared Data", "ARHS_MBD_SIMD_M_raw.rds"))

arhs_simd_f <- arhs_simd %>% filter(sex_grp==2)
saveRDS(arhs_simd_f, file.path(profiles_data_folder, "Prepared Data", "ARHS_MBD_SIMD_F_raw.rds"))




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
               popgrp_m) %>%
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
# data for 2012 and 2013 need to be replaced with a copy of the data for 2014
simd_2014 <- simd_lookup %>% filter(year == 2014)
simd_2012 <- simd_2014 %>% mutate(year = 2012)
simd_2013 <- simd_2014 %>% mutate(year = 2013)
simd_lookup <- simd_lookup %>%
  filter(!year %in% c(2012, 2013)) %>%
  rbind(simd_2012, simd_2013)


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
arhs_deprivation_analysis(filename="ARHS_MBD_SIMD", 
                          yearstart=2002, 
                          yearend=2023,
                          pop_sex = "all", 
                          epop_total = 165800)

arhs_simd_total <- deprivation_analysis_result %>%
  mutate(sex = "Total")

# Male population
arhs_deprivation_analysis(filename="ARHS_MBD_SIMD_M", 
                          yearstart=2002, 
                          yearend=2023,
                          pop_sex = "male", 
                          epop_total = 82900)

arhs_simd_m <- deprivation_analysis_result %>%
  mutate(sex = "Male")

# Female population
arhs_deprivation_analysis(filename="ARHS_MBD_SIMD_F", 
                          yearstart=2002, 
                          yearend=2023,
                          pop_sex = "female", 
                          epop_total = 82900)

arhs_simd_f <- deprivation_analysis_result %>%
  mutate(sex = "Female")


# Combine data
stays_simd <- rbind(arhs_simd_total,
                    arhs_simd_f,
                    arhs_simd_m) 
# Smallest numerator is 206 for deciles (469 for quintiles)


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
# SMALLEST NUMERATOR = 206

# Save
write_rds(stays_simd, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.rds"))
write.csv(stays_simd, paste0(profiles_data_folder, "/Data to be checked/ARHS_mental_and_behav_ineq.csv"), row.names = FALSE)



##########################################################
### QA -----
##########################################################

run_qa(type = "main", filename = "ARHS_mental_and_behav", test_file = FALSE) 
run_qa(type = "popgrp", filename = "ARHS_mental_and_behav", test_file = FALSE)
run_qa(type = "deprivation", filename = "ARHS_mental_and_behav", test_file = FALSE) 
