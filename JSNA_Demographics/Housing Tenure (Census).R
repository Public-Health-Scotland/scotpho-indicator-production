# Households by Tenure

# Script generates number/% households by Housing Tenure sourced from 2022 Census
# The output files produced form datafiles that are used in the demographics profile within profiles tool


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./functions/main_analysis.R") #Normal indicator functions
#library(nanoparquet) #needed for opening cl-out lookups
library(openxlsx) #for opening xl files

## Different geography level aggregation available from census website however disclosure measures applied meaning smaller geographies can't reliably be added to generate
## consistent higher geographies
# DZ : https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1d4bd-49c2-75ae-b08d-1674c6ef106f (dz used to generate HSCP localities - although this might be a bit of a fudge)
# IZ : https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1d4f5-f014-7687-bd26-4c4dfe1e62ea
# CA : https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1d4ec-0680-7d18-8ea0-6a90a1399259
# HB : https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1d4f1-bc49-7b78-978a-ddccc8716e62
# HSCP :https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1d4f3-314f-77d2-89be-3d346fc52191
# Scotland (included in HB table as total)
################################################################################################.


## Read in larger geographies ----

tenure_hb <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census households by tenure and 2011 hb.xlsx"), sheet = "hb")|>
  clean_names()|>
  rename(code =health_board_area_2019)|>
  mutate(code = case_when(code=="Total" ~ "S00000001", TRUE ~ code))

tenure_ca <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census households by tenure and 2011 council.xlsx"), sheet = "ca")|>
  clean_names()|>
  rename(code =council_area_2019)

tenure_hscp <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census households by tenure and 2011 hscp.xlsx"), sheet = "hscp")|>
  clean_names()|>
  rename(code =integration_authority_2019)

tenure_iz <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census households by tenure and 2011 iz.xlsx"), sheet = "iz")|>
  clean_names()|>
  rename(code =intermediate_zone_2011)

# Coombine
tenure_data <-rbind(tenure_hb,tenure_ca,tenure_hscp,tenure_iz)
# tidy
rm(tenure_hb,tenure_ca,tenure_hscp,tenure_iz)


### Derive  HSCP Locality ----
#read in datazone data to match to locality lookup - best way to generate locality data although disclosure control applied by census will mean these might not be totally accurate
tenure_dz <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census households by tenure and 2011 datazone.xlsx"), sheet = "dz_tenure") |>
  clean_names() |> # converts column names to lowercase
  rename(datazone2011 = data_zone_2011) 

# open geo lookup that can match 2011 dz to any higher geography
geography_lookup <- readRDS(file.path(profiles_data_folder, "Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")) 

# match to geo lookup 
tenure_locality <- left_join(x = tenure_dz, y = geography_lookup, by = "datazone2011")|>
  select(hscp_locality,owned_owned_outright:total) |>
  rename(code = hscp_locality) |>
  group_by(code) |>
  summarise_all(sum, na.rm = T) |>
  ungroup()


## Combine larger geographies with locality data
tenure_data <- rbind(tenure_data, tenure_locality)

#tidy
rm(tenure_locality,tenure_dz, geography_lookup)


# manipulate file and calculate %
tenure_data2 <- tenure_data |>
  mutate(percentage_owned_outright = round(owned_owned_outright/total*100,2),
         percentage_owned_with_mortgage=round(owned_owns_with_a_mortgage_or_loan_or_shared_ownership/total*100,2),
         percentage_social_rented = round(social_rented_council_la_or_housing_association_registered_social_landlord/total*100,2),
         percentage_private_rented = round(private_rented/total*100,2),
         percentage_other = round(living_rent_free/total*100,2))|>
  select(-total)

tenure_data2 <- tenure_data2 |>
  rename(
    households_owned_outright = owned_owned_outright,
    households_owned_with_mortgage = owned_owns_with_a_mortgage_or_loan_or_shared_ownership,
    households_social_rented = social_rented_council_la_or_housing_association_registered_social_landlord,
    households_private_rented = private_rented,
    households_other = living_rent_free) %>%
  #reshape to long format with one column labeling tenure type
  pivot_longer(
    cols = -code,
    names_to = c(".value", "tenure"),
    names_pattern = "(households|percentage)_(.*)")


## Save output file which can be picked up in shiny app data prep steps 
write_parquet(ethnicity_populations, paste0(profiles_data_folder, "/Data to be checked/ethnicity_populations.parquet"))

