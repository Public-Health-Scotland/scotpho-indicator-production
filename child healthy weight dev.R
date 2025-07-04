# ~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# data requested from PHS child health team each year following the release of P1 BMI publication (typically published Dec):
# https://publichealthscotland.scot/publications/primary-1-body-mass-index-bmi-statistics-scotland/

# The team provide 2 extracts:
# data based on 2011 datazones - for main and deprivation analysis (to match post 2013 data to later versions of SIMD that are DZ01 based)
# data based on 2001 datazones - for deprivation analysis only (to match pre-2013 data to early versions of SIMD that are DZ11 based)

# Data is provided at DZ level but need to cross-reference with published data and remove some larger geographies. 
# Coverage is very low/non existent in some areas meaning the published data has some boards/councils suppressed. 
# but we have been provided data for datazones that fall within these suppressed areas.


# files produced:
# main_dataset: Y
# deprivation_dataset: Y
# popgroups_dataset: Y (sex splits)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 - load packages / source functions  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("functions/main_analysis.R") # for main analysis
source("functions/deprivation_analysis.R") # for deprivation analysis 
library(phsopendata) # for extracting published opendata to identify areas to suppress


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2 - read raw data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# path to folder where raw data saved once team send over
folder <- file.path(profiles_data_folder, "Received Data", "Child Healthy Weight")

# read in data files (update each year to reflect new IR number)
dz_01 <- readRDS(file.path(folder, "IR2024-00930_DZ2001.rds")) # 2001 datazones 
dz_11 <- readRDS(file.path(folder, "IR2024-00930_DZ2011.rds")) # 2011 datazones


# create numeric year cols using starting year of school year
dz_01$year <- as.numeric(paste0("20", substr(dz_01$schlyr_exam, 1, 2)))
dz_11$year <- as.numeric(paste0("20", substr(dz_11$schlyr_exam, 1, 2)))

# rename dz code columns
colnames(dz_01)[1] <- "datazone"
colnames(dz_11)[1] <- "datazone" 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3 - prepare main dataset file  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create temp data
# calculating the numerator (total with healthy BMI)
# and denominator (total reviewed) for each datazone and year
main_temp <- dz_11 |>
  group_by(datazone, year) |>
  summarise(numerator = sum(Healthy_Weight),
            denominator = sum(tot), 
            .groups = "drop")

# save temp file to pass to analysis function
saveRDS(main_temp, file.path(profiles_data_folder, "Prepared Data", "child_healthyweight_raw.rds"))

# run function to prepare final file

# note this will produce the following warning:
#   There was 1 warning in `mutate()`.
# ℹ In argument: `pcf = case_when(...)`.
# Caused by warning in `sqrt()`:
#   ! NaNs produced 

# this occurs when calculating the population correction factor (PCF) when the estimated population is 
# larger than the denominator e.g more P1 children reviewed in an area than 
# the estimated no. of 5 year olds in that area resulting in a PCF of less than 0
# Warning can be ignored. When this happens the confidence intervals
# will be made the same as the rate on assumption that the whole population has been reviewed

main_analysis(filename = "child_healthyweight", geography = "datazone11", ind_id = 21106,
              yearstart = 2002, yearend = 2023, time_agg = 1, year_type = "school",
              measure = "perc_pcf", pop="DZ11_pop_5", QA = FALSE, test_file = FALSE)

# remove objects from global env
rm(main_temp, main_analysis_result)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4 - prepare popgroups file  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare temp dataset
# calculating numerators for denominators for each datazone, year and sex
popgroups_temp <- dz_11 |>
  group_by(datazone, year, sex) |>
  summarise(numerator = sum(Healthy_Weight),
            denominator = sum(tot), 
            .groups = "drop")

# create list of dfs split using sex column
popgroups_temp <- split(popgroups_temp, popgroups_temp$sex, drop = TRUE)


# save temp files - one for males and one for females
saveRDS(popgroups_temp$M, file.path(profiles_data_folder, "Prepared Data", "child_healthyweight_males_raw.rds"))
saveRDS(popgroups_temp$`F`, file.path(profiles_data_folder, "Prepared Data", "child_healthyweight_females_raw.rds"))


## temporary step - create male and female population files for under 5s
# as we don't currently create a lookup that splits males/females under 5
# pop_file <- readRDS(file.path(profiles_data_folder, "Lookups", "Population", "DZ11_pop_5_SR.rds"))
# 
# males <- pop_file |>
#   filter(sex_grp == 1) |>
#   select(year, code, denominator)
# 
# 
# females <- pop_file |>
#   filter(sex_grp == 2) |>
#   select(year, code, denominator)
# 
# 
# saveRDS(males, file.path(profiles_data_folder, "Lookups", "Population", "DZ11_pop_5_male.rds"))
# saveRDS(females, file.path(profiles_data_folder, "Lookups", "Population", "DZ11_pop_5_female.rds"))


# calculate rates for males
main_analysis(filename = "child_healthyweight_males", geography = "datazone11", ind_id = 21106,
              yearstart = 2002, yearend = 2023, time_agg = 1, year_type = "school",
              measure = "perc_pcf", pop="DZ11_pop_5_male", QA = FALSE)

# calculate rates for females
main_analysis(filename = "child_healthyweight_females", geography = "datazone11", ind_id = 21106,
              yearstart = 2002, yearend = 2023, time_agg = 1, year_type = "school",
              measure = "perc_pcf", pop="DZ11_pop_5_female", QA = FALSE)

# read males results back in 
males <- readRDS(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_males_shiny.rds")) |>
  mutate(split_value = "Males")

# read females results back in 
females <- readRDS(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_females_shiny.rds")) |>
  mutate(split_value = "Females")

# read totals results back in (i.e. result from main analysis)
all <- readRDS(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_shiny.rds")) |>
  mutate(split_value = "All")

# combine into one dataset
# filter on Scotland, council, board and HSCP only
# dont want to report IZ/HSC locality level sex splits as too granular
popgroups_data <- rbind(males, females, all) |>
  mutate(split_name = "Sex") |>
  filter(grepl("S00|S12|S18|S11", code))

# save final file 
saveRDS(popgroups_data, file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_shiny_popgrp.rds"))
write.csv(popgroups_data, file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_shiny_popgrp.csv"), row.names = FALSE)


# remove objects from global env
rm(males, females, all, popgroups_data, popgroups_temp, main_analysis_result)

# delete individual male/female files from the data to be checked folder
file.remove(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_females_shiny.rds"))
file.remove(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_females_shiny.csv"))
file.remove(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_males_shiny.rds"))
file.remove(file.path(profiles_data_folder, "Data to be checked", "child_healthyweight_males_shiny.csv"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5 - prepare deprivation file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# filter 2001 dz data (for mapping to earlier versions of SIMD that are based on 2001 dzs)
depr_dz01 <- dz_01 |>
  filter(year <= 2013)

# filter 2011 dz data (for mapping to later versions of SIMD that are based on 2011 dzs)
depr_dz11 <- dz_11 |>
  filter(year >= 2014)

# combine into one temp file 
depr_temp <- rbind(depr_dz01, depr_dz11) |>
  group_by(datazone, year) |>
  summarise(numerator = sum(Healthy_Weight),
            denominator = sum(tot), 
            .groups = "drop")

# save temp file to use in analysis function 
saveRDS(depr_temp, file.path(profiles_data_folder, "Prepared Data", "child_healthyweight_depr_raw.rds"))


# create final deprivation file
deprivation_analysis(filename = "child_healthyweight_depr", ind_id = 21106,
                     yearstart = 2011, yearend = 2023, time_agg = 1, year_type = "school",
                     measure = "perc_pcf", pop_age = c(5,5), QA = FALSE, test_file = FALSE)


# remove objects from global env
rm(depr_dz01, depr_dz11, depr_temp, deprivation_analysis_result)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6 - identify geographies to be suppressed ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There are several boards and councils for which we only have incomplete data 
# for certain years for various reasons such as joining Child Health Systems Programme (CHSP) School System later on, COVID-19 etc.
# We need to exclude incomplete periods/areas as they're also excluded from the published tables

# this function finds geographies/years excluded from published data
# note that we need to look at councils and boards separately
# because whilst some councils are suppressed, the board they fall within might not be
# e.g. NHS Ayrshire & Arran is comprised of 3 council areas
# in 2019/20, one of these councils was suppressed, but the NHS A&A figure is still reported, despite lower completeness


find_na <- function(geography = c("council", "board")){
  
  # id of table to read from P1 BMI open data collection:
  # https://www.opendata.nhs.scot/dataset/primary-1-body-mass-index-bmi-statistics/ 
  res_id <- ifelse(
    geography == "council", 
    "e9f8d10c-9c06-4e77-a0f5-70ff14af25a4", # Epidemiological BMI at Council Level
    "2cb9d907-7149-4bbd-904a-174f15344585" # Epidemiological BMI at Health board Level
  )
  
  # geography column name
  geo_col <- ifelse(geography == "council", "CA", "HBR")
  
  # extract open data and find geographies/years where no reviews were carried out
  # i.e. where there are rows missing in the data for particular years/geographies
  # as the open data doesn't have the rows with suppressed values like their excel publication tables on the PHS website do
  # instead the rows of data are entirely missing
  results <- phsopendata::get_resource(res_id = res_id) |>
    complete(SchoolYear, !!sym(geo_col)) |> # create rows and populate figures with NA where there are geography/year combinations missing 
    mutate(year = as.numeric(substr(SchoolYear, 1, 4))) |>
    select(SchoolYear, year, code = !!sym(geo_col), ValidReviews) |>
    filter(is.na(ValidReviews)) |> # filter on those NA rows created indicating suppressed data
    filter(year > 2001)
  
  return(results)
  
}


# boards that need suppressed for particular years 
hb_no_reviews <- find_na(geography = "board") |>
  mutate(suppress = "Y")


# councils that need suppressed for particular years
ca_no_reviews <- find_na(geography = "council")


# read in geography lookup 
geo_lookup <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "DataZone11_All_Geographies_Lookup.rds")) |>
  select(ca2019, hscp2019, adp) |>
  unique()

# identify which other geographies map to councils to also be suppressed
# i.e. HSCPs and ADPs
affected_geos <- geo_lookup |>
  filter(if_any(everything(), ~ .x %in% unique(ca_no_reviews$code)))


# identify which councils, HSCPs and ADPS need suppressed for which years
# (we have already handled health boards separately as described above)
ca_no_reviews <- ca_no_reviews |>
  left_join(affected_geos, by = c("code" = "ca2019")) |>
  pivot_longer(c("code", "hscp2019", "adp"), values_to = "code", names_to = NULL) |>
  mutate(suppress = "Y") |>
  unique()

# find all vals that need suppressed 
all_suppression_required <- rbind(ca_no_reviews, hb_no_reviews)

# tidy up 
rm(geo_lookup, ca_no_reviews, hb_no_reviews, affected_geos)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7 -  suppress datasets ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

suppress_data <- function(file = c("main", "deprivation", "popgrp")){
  
  # filename 
  filename <- switch(file,
                     "main" = "child_healthyweight_shiny",
                     "deprivation" = "child_healthyweight_depr_ineq",
                     "popgrp" = "child_healthyweight_shiny_popgrp"
                     )
  
  # read in data 
  dataset <- readRDS(file.path(profiles_data_folder, "Data to be checked", paste0(filename, ".rds")))

  # suppress data
  dataset_suppressed <- dataset |>
  left_join(all_suppression_required, by = c("code", "year")) |>
  mutate(across(contains(c("numerator", "denominator", "rate", "upci", "lowci", "par", "sii", "rii", "abs", "rel")), ~ case_when(suppress == "Y" ~ NA_real_ , TRUE ~ .))) |>
  select(-c(ValidReviews, SchoolYear, suppress))
  
  # remove rows of suppressed data
  dataset_suppressed <- dataset_suppressed |>
    filter(!is.na(rate))

  # re-save file in data to be checked folder 
  saveRDS(dataset_suppressed, file.path(profiles_data_folder, "Data to be checked", paste0(filename, ".rds")))
  write.csv(dataset_suppressed, file.path(profiles_data_folder, "Data to be checked", paste0(filename, ".csv")), row.names = FALSE)
  cli::cli_alert(paste0(filename, " files re-saved"))
}


# suppress datasets
suppress_data(file = "main")
suppress_data(file = "deprivation")
suppress_data(file = "popgrp")


# ~~~~~~~~~~~~~~~~~~~
# 8 - QA datasets ----
# ~~~~~~~~~~~~~~~~~~~~

# note these checks will flag that there are some geographies missing for some years
# mostly earliers years (due to not being part of the (CHSP) School System)
# and covid years (2019/20 and 2020/21)
run_qa(filename = "child_healthyweight", type = "main", test_file = FALSE)
run_qa(filename = "child_healthyweight_depr", type = "deprivation", test_file = FALSE)
run_qa(filename = "child_healthyweight", type = "popgrp", test_file = FALSE)


## END