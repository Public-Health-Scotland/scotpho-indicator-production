# ~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# Population living in the 15% most access deprived datazones in Scotland

# We apply population weighting i.e. ranking the DZs according to their access 
# deprivation score alongside a cumulative total of DZ populations. 
# The cut-off for the most deprived DZs is the point at which 15% of the population has been included.
# For a more thorough explanation of methodology, refer to the metadata in the techdoc.

# Each year when updating this indicator, the 'simd_list' below needs reviewed to 
# ensure there is a version of SIMD assigned to the year you are trying to update. 
# If there are no new versions of SIMD (they're only released every few years), 
# then simply extend the period in 'trends_from' for the most recent version of SIMD.


# ~~~~~~~~~~~~~~~~~~~~~~~~
# Dependencies ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(phslookups) # for get_simd_datazone() function to read in SIMD versions from cl-out

# uncomment and run below to install phslookups package:
# remotes::install_github("Public-Health-Scotland/phslookups")


# ~~~~~~~~~~~~~~~~~~~~~~~
# SIMD versions ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Details about each version of SIMD - matches Table 4 of PHS deprivation guidance:
# https://publichealthscotland.scot/media/24056/2023-12-phs-deprivation-guidance-v35.pdf

# dz_year = whether datazones are 2001/2011 based
# pop_year = year of population used in the index
# trend_years = period each version of SIMD should be applied to

simd_info <- list(
  # SIMD 2004 pop_year should be 2001 but our pop lookups only start at 2002
  "2004" = list(dz_year = "2001", pop_year = 2002, trend_years = 2002:2003),
  "2006" = list(dz_year = "2001", pop_year = 2004, trend_years =  2004:2006),
  "2009v2" = list(dz_year = "2001", pop_year = 2007, trend_years = 2007:2009),
  "2012" = list(dz_year = "2001", pop_year = 2010, trend_years = 2010:2013),
  "2016" = list(dz_year = "2011", pop_year = 2014, trend_years = 2014:2016),
  "2020v2" = list(dz_year = "2011", pop_year = 2017, trend_years = 2017:2023)
)


# The purrr::imap() function is used to iterate over this list and prepare
# data for SIMD version. When using this function:
# '.y' refers to the name of the simd version e.g. "2004"
# '.x' refers to an element in the list for that particular simd version e.g. dz_year

# e.g. when .y is "2020v2":
# .x$pop_year = 2017
# .x$dz_year = 2011
# .x$trends_years = 2017:2023



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population estimates data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Early versions of SIMD are based on 2001 DZs and more recent versions are based on 2011 DZs
# Therefore use pop estimates based on both 2001 and 2011 DZs for different years of this indicators time series
pop_data <- list("2001" = "DZ01_pop_basefile.rds", 
                 "2011" = "DZ11_pop_basefile.rds")


# read in 2 x pop data files and aggregate estimates for each year and datazone
pop_data <- map(pop_data, ~ 
                  readRDS(file.path(profiles_data_folder, "Lookups", "Population", .x)) |>
                  group_by(across(contains("datazone")), year) |>
                  summarise(pop = sum(denominator), .groups = "drop") |>
                  rename(datazone = 1))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Find most deprived datazones -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For each SIMD version, get pop estimates for the year that SIMD is based on 
# e.g. for SIMD2020v2, take 2011 pop estimates and filter on 2017
simd_pop_index <- imap(simd_info, ~ {
  pop_data[[.x$dz_year]] |>
    filter(year == .x$pop_year)
})


# Calculate 15% of the population for the year each SIMD is based on
pop_15 <- map(simd_pop_index, ~ sum(.x$pop) * 0.15)



# For each SIMD, read in the lookup from cl-out and select access domain rank and any geography cols
# Note versions of SIMD that are DZ01 based won't have an intzone2011 col
# This means indicator time series will only include IZ/locality trends from 2014 onwards
simd_data <- imap(simd_info, ~ {
  
  # name of domain col e.g. "simd2020v2_access_rank"
  domain_col <- paste0("simd", .y, "_access_rank")
  
  phslookups::get_simd_datazone(simd_version = .y) |>
    select(
      datazone = 1,
      rank = {{domain_col}},
      any_of(c(
        "ca2019" = matches("^CA2019$", ignore.case = TRUE),
        "hscp2019" = matches("^HSCP2019$", ignore.case = TRUE),
        "intzone2011" = matches("^intzone2011$", ignore.case = TRUE)
      ))
    )
})



# For each SIMD, join with pop index year data and arrange dzs from most (no 1) to least access deprived
# calculate a cumulative total of the population - when 15% of pop is reached, label DZs as 'most deprived'
simd_data <- imap(simd_info, ~ {
  
  simd_data[[.y]] |>
    left_join(simd_pop_index[[.y]], by = "datazone") |>
    arrange(rank) |>
    mutate(cumsum = cumsum(pop)) |>
    mutate(depr_status = if_else(cumsum <= pop_15[[.y]], "15% most deprived", "85% least deprived")) |>
    select(any_of(c("datazone", "intzone2011", "ca2019", "hscp2019", "rank", "depr_status")))
})



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create trend data -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


# For each SIMD, filter population data for the 
# years applicable to that version and join with SIMD lookup
# e.g. for SIMD2020v2, take DZ11 pop estimates and filter on years 2017-2023
# and join with SIMD2020v2 to get deprivation status for each dz/year
pop_data <- imap(simd_info, ~ {
  pop_data[[.x$dz]] |>
    filter(year %in% .x$trend_years) |>
    left_join(simd_data[[.y]], by = "datazone")
})


# unlist into single df
result <- bind_rows(pop_data) |>
  arrange(year) |>
  mutate(scotland = "S00000001")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate to different geography levels -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# add HSCP localities in - these are not an official geography
# with set of standard codes so are not part of any of the SIMD lookups
localities <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "DataZone11_HSCLocality_Lookup.rds")) |>
  select(datazone2011, hscp_locality)

result <- result |>
  left_join(localities, by = c("datazone" = "datazone2011"))


# aggregate by geography level and remove IZ/locality data pre-2017
result <- result |>
  select(-datazone) |>
  pivot_longer(
    cols = -c(pop, year, depr_status, rank), 
    names_to = "geo_level", 
    values_to = "code"
  ) |>
  filter(!is.na(code)) |>
  group_by(year, code, depr_status) |>
  summarise(denominator = sum(pop), .groups = "drop")



# create numerator and denominator data 
result <- result |>
  mutate(numerator = if_else(depr_status == "15% most deprived", denominator, 0)) |>
  select(-depr_status) |>
  group_by(year, code) |>
  summarise_all(sum) |>
  ungroup()


# save temp file for analysis function 
saveRDS(result, file.path(profiles_data_folder, "Prepared Data", "20902_pop_access_deprived_raw.rds"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run analysis function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A few points when looking at QA results:

# 1. Results should confirm that IZ/locality data only available for 2014 onwards

# 2. Scotland trend will always be around 15% since the 15% most deprived areas hold approx 15% of the scottish pop 
# (although rate may rise slightly in between pop index years before resetting again).

# 3.IZ/locality trends may produce odd looking trends - this is to be expected e.g. where an IZ is comprised of a small
# number of DZs where they may fall within the 15% most deprived in one version of SIMD but not another.

main_analysis(ind_id = 20902, filename = "20902_pop_access_deprived", measure = "percent", 
              geography = "multiple", year_type = "calendar", time_agg = 1, yearstart = 2002, yearend = 2023)


## END
