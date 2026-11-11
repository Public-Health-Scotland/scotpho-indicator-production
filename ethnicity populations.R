# Ethnicity populations

# Script generates populations data file containing census based populations by ethnicity


# The output files produced form datafiles that are used in the demographics profile within profiles tool


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./functions/main_analysis.R") #Normal indicator functions
library(nanoparquet) #needed for opening cl-out lookups
library(openxlsx) #for opening xl files

#cl-out folderwith population projections
cl_out <- "/conf/linkage/output/lookups/Unicode/" 

######################################################################.
## Census Populations by ethnicity ----

## Available for Scotland, NHS board, council from PHS cl-out
######################################################################.

# source scotland, nhs board, council populations by ethnicity for 2022
ethnicity_pops_scot <- read_parquet(file.path(cl_out, "/Populations/Ethnicity/sc_ethnicity_census_2022.parquet"))|>
  filter(sex=="All people" & age_group=="Total") |>
  mutate(area_code="S00000001")
ethnicity_pops_hb <- read_parquet(file.path(cl_out, "/Populations/Ethnicity/hb_ethnicity_census_2022.parquet")) |>
  filter(sex=="All people" & age_group=="Total")
ethnicity_pops_ca <- read_parquet(file.path(cl_out, "/Populations/Ethnicity/ca_ethnicity_census_2022.parquet")) |>
  filter(sex=="All people" & age_group=="Total")

# bind 3 geographies 
ethnicity_pops <- bind_rows(ethnicity_pops_scot, ethnicity_pops_hb, ethnicity_pops_ca) |>
  rename(code=area_code) |>
  select(-area)

# manipulate to generate a split for white ethnicities and other groups (note that smaller geographies dtaa is sourced from census
# website which applied disclosure control and prevents more detailed analysis of ethnicity splits - for this reason all geography levels show white/other)
ethnicity_pops <- ethnicity_pops |>
  mutate(ethnicity = case_when(substr(ethnic_group_code,1,1) =="1" ~ "white",
                               TRUE ~ "other_ethnic_group")) |>
  group_by(code, ethnicity) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  pivot_wider(names_from=ethnicity, values_from = population)|>
  mutate(tot_pop=white+other_ethnic_group,
         percentage_other=other_ethnic_group/tot_pop*100) #only calculate % other ethnic group

#tidy
rm(ethnicity_pops_ca,ethnicity_pops_hb,ethnicity_pops_scot)

## Datazone level aggregation available from census website
# https://www.scotlandscensus.gov.uk/webapi/opentable?id=01a1c607-1a14-7fab-ac6b-3f85638acab1

# small area populations are only available by 2 broad ethnicity groups - white and other.
################################################################################################.

ethnicity_dz <- read.xlsx(paste0(profiles_data_folder,"/Received Data/census/2022 census populations by ethnicity and 2011 datazone.xlsx"), sheet = "dz_pop") |>
  clean_names() |> # converts column names to lowercase
  mutate(area_type = "Scotland")|> # add in column for area type
  rename(datazone2011 = data_zone_2011)

# open geo lookup that can match 2011 dz to any higher geography
geography_lookup <- readRDS(file.path(profiles_data_folder, "Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")) 

# match to geo lookup 
ethnicity_dz <- left_join(x = ethnicity_dz, y = geography_lookup, by = "datazone2011") |>
  select(intzone2011, hscp2019, hscp_locality, white, other_ethnic_group, total)

ethnicity_dz <- ethnicity_dz |>
  pivot_longer(cols = intzone2011:hscp_locality, values_to = "code", names_to = NULL) |>
  group_by(code) |>
  summarise_all(sum, na.rm = T) |>
  ungroup() |>
  mutate(percentage_other=other_ethnic_group/total*100) |>
  rename(tot_pop=total)

#tidy
rm(geography_lookup)


######################################################################.
## add all geographies ----


ethnicity_populations <- rbind(ethnicity_pops, ethnicity_dz)


## Save output file which can be picked up in shiny app data prep steps 
write_parquet(ethnicity_populations, paste0(profiles_data_folder, "/Data to be checked/ethnicity_populations.parquet"))



