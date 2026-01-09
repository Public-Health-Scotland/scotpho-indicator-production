#############################.
# Analyst notes ----
#############################.

# ScotPHO indicator: Single Adult Dwellings

# Data extracted from statistics.gov.scot website at datazone, council and Scotland level. 
# The DZ level data is published by different DZ versions, depending on the year (DZ01 for 2007-2013, DZ11 for 2014-2023 and DZ22 for 2024) 
# This means that whilst CA level and above figures can be produced for all years (since already pre-aggregated)
# small area figures (IZ/HSC locality) can only be produced from 2014-2023 for main indicator data (using 2011 DZs) 
# and from 2007-2023 for deprivation data (using combination of 2001 and 2011 DZs)
# 2022 DZ data can not yet be used for main analysis or deprivation analysis.

# Files produced:
# Main: Y
# Deprivation: Y
# Popgroups: N


#   Part 1 - Create basefiles
#     1a) Extract data from SG website
#     1b) Create files for DZ01
#     1c) Create files for DZ11
#     1d) Create files for DZ22
#     1e) Create main deprivation file
#   Part 2 - Aggregate files from different dz periods

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Create basefile ----
###############################################.

#########################.
# 1a - Extract data ----
#########################.
#Note for 2026 update - try to use opendatascot package. Couldn't do historically do to file size or VPN issues

col_names_n <- c("datazone", "name", 2006:2024)
col_names_d <- c("datazone", "name", 2001:2024)

#read data in direct from source
sad_data_extract <- bind_rows(read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Fwith-single-adult-discounts",
                                       skip=8, col_names = col_names_n) |>   mutate(type = "numerator"),
                              read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Ftotal-dwellings", 
                                       skip = 8, col_names = col_names_d) |>  mutate(type = "denominator")) |> 
  select(-name, -any_of(as.character(2001:2006))) |>  #drop cols with no data
  mutate(datazone = str_sub(datazone, -9)) |>   #keep only the last 9 characters i.e. the s-code
  tidyr::pivot_longer(cols = c(`2007`:`2024`), names_to = "year", values_to = "count") |> 
  tidyr::pivot_wider(names_from = type, values_from = count) |>  
  filter(!is.na(denominator))

################################.
# 1b - Create DZ01 file ----
################################.
#Main analysis function does not currently accommodate DZ01 data
#If we use CA figures instead we can produce all geographies other than intermediate zones and localities

dz01_sad <- sad_data_extract |> 
  filter(year <= 2013 & str_detect(datazone, "S12"))

saveRDS(dz01_sad, file.path(profiles_data_folder, "Prepared Data/single_adult_dwellings_dz01_raw.rds"))

main_analysis("single_adult_dwellings_dz01", measure = "percent", geography = "council",
              year_type = "calendar", ind_id = 20504, time_agg = 1, yearstart = 2007, yearend = 2013,
              QA = FALSE)

################################.
# 1c - Create DZ11 file ----
################################.

dz11_sad <- sad_data_extract |> 
  filter(year >= 2014 & year <= 2023 & str_detect(datazone, "S01")) #filter for relevant years and datazone only

# note that 2014 data is published by both 2001 and 2011 datazones. Therefore removing
# 2001 based DZs (i.e all DZs between S01000001 and S01006505) before passing to the main_analysis function to avoid NAs being produced
# 2011 based DZ codes start where 2001 ones end (i.e. from S01006506 onwards)
dz11_sad <- dz11_sad |>
  mutate(dz_number = str_sub(datazone, start = 2)) |>
  filter(!(year == 2014 & dz_number <= 01006505)) |> 
  select(-dz_number)
  

saveRDS(dz11_sad, file.path(profiles_data_folder, "Prepared Data/single_adult_dwellings_dz11_raw.rds"))

main_analysis(filename = "single_adult_dwellings_dz11", measure = "percent", geography = "datazone11",
              year_type = "calendar", ind_id = 20504, time_agg = 1, yearstart = 2014, yearend = 2023,
              QA = FALSE)

################################.
# 1d - Create DZ22 file ----
################################.
#Due to the lack of available lookups for DZ22 as of December 2025, we can't use the 2024 datazone figures
#Repeating method from 1b (DZ01), so IZs and localities will not be available for 2024 right now
#For 2026 update, ideally change this section to resemble section 1c and run functions at datazone22 level

dz22_sad <- sad_data_extract |> 
  filter(year == 2024 &str_detect(datazone, "S12")) 

saveRDS(dz22_sad, file.path(profiles_data_folder, "Prepared Data/single_adult_dwellings_dz22_raw.rds"))

main_analysis("single_adult_dwellings_dz22", measure = "percent", geography = "council",
              year_type = "calendar", ind_id = 20504, time_agg = 1, yearstart = 2024, yearend = 2024,
              QA = FALSE)


####################################.
# 1e - Create deprivation file ----
####################################.
#Cannot include 2024 data as SIMD for dz22 not released yet

sad_data_depr <- sad_data_extract |> 
  filter(str_detect(datazone, "S01") & year < 2024)  #keep datazones only)

saveRDS(sad_data_depr, file.path(profiles_data_folder, "Prepared Data/single_adult_dwellings_raw.rds"))

deprivation_analysis(filename = "single_adult_dwellings", yearstart = 2007, yearend = 2023,
                     time_agg = 1, year_type = "calendar", measure = "percent", ind_id = 20504)

###########################################################.
# Part 2 - Aggregate files from different dz periods ----
##########################################################.

# Read in results from main analysis 
dz01_sad <- readRDS(file.path(profiles_data_folder, "Test Shiny Data/single_adult_dwellings_dz01_shiny.rds"))
dz11_sad <- readRDS(file.path(profiles_data_folder, "Test Shiny Data/single_adult_dwellings_dz11_shiny.rds"))
dz22_sad <- readRDS(file.path(profiles_data_folder, "Test Shiny Data/single_adult_dwellings_dz22_shiny.rds"))

# combine into one single file 
single_adult_dwellings_final <- bind_rows(dz01_sad, dz11_sad, dz22_sad)

# delete the 6 individual files produced from main_analysis from data to be checked folder - no longer needed
list.files(
  path = file.path(profiles_data_folder, "Data to be checked"), 
  pattern = "single_adult_dwellings_dz[01-22]+_shiny",
  full.name = TRUE
  ) |>
  walk(~ file.remove(.x))

# save combined file 
write.csv(single_adult_dwellings_final, file.path(profiles_data_folder, "Data to be checked/single_adult_dwellings_shiny.csv"), row.names = FALSE)
saveRDS(single_adult_dwellings_final, file.path(profiles_data_folder, "Data to be checked/single_adult_dwellings_shiny.rds"))


# QA final file
run_qa(filename = "single_adult_dwellings", type = "main")

## end