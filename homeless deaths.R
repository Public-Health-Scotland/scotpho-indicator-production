# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following indicator:
# 99143 - Homeless deaths

# Data is sourced from the NRS homeless deaths publication:
# https://www.nrscotland.gov.uk/publications/homeless-deaths-2023/

# Published data table to be download each year and saved here:
# \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data\Homelessness deaths

# We use 3 tabs from published tables:
# Table_1 - deaths by local authority
# Table_2 - deaths by age and sex (Scotland level only)
# Data_Figure_5 - deaths by selected causes of death (Alcohol, drugs, suicide) (Scotland level only)

# NRS publish both estimated deaths and crude rate (estimated deaths per 1 million population aged 15-74)
# However, they only publish estimated deaths for the breakdowns by cause of death
# We therefore calculate our own crude rates for this particular split

# Note they publish council and scotland estimates. However, council estimates
# combined do not match their published Scotland total. Therefore, choosing 
# not to calculate our own HB/HSCP/ADP figures using estimated deaths
# and our own population file incase would be innacurate


# Files produced:
# Main: Y
# Popgroups: Y (age/sex and select causes of death)
# Deprivation: N 



# ~~~~~~~~~~~~~~~~~~~~~~
# Dependencies ----- 
# ~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(readxl)


# function to prepare cols required for final files
prepare_final_cols <- function(data) {
  data |>
    # select columns 
    select(any_of(
      c("code","year","split_name", 
        "split_value", "numerator", "rate"))
      ) |>
    # create columns 
    mutate(
      trend_axis = year,
      def_period = year,
      upci = NA,
      lowci = NA,
      ind_id = 99143
    )
}


# ~~~~~~~~~~~~~~~~~~~~~
# Read data -----
# ~~~~~~~~~~~~~~~~~~~~~

# excel file details
folder <- file.path(profiles_data_folder, "Received Data", "Homelessness deaths")
file <- "homeless-deaths-2023-data.xlsx"
path <- file.path(folder, file)

# read in data 
la_splits <- clean_names(read_excel(path, skip = 4, sheet = "Table_1")) # deaths by local authority 
age_sex_splits <- clean_names(read_excel(path, skip = 4, sheet = "Table_2")) # deaths by age and sex
select_causes_splits <- clean_names(read_excel(path, skip = 4, sheet = "Data_Figure_5")) # deaths drug/acl/suicide


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create main indicator dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in lookup with council area codes
ca_lookup <- readRDS(file.path(profiles_data_folder,"Lookups","Geography","CAdictionary.rds")) |>
  add_row(areaname = "Scotland", code = "S00000001") # add scotland row


# join with dataset to add area codes column
main <- la_splits |>
  mutate(
    area = gsub("&", "and", area),
    area = gsub("Edinburgh, City of", "City of Edinburgh", area)
  ) |>
  left_join(ca_lookup, by = c("area" = "areaname"))


# select and create cols required for final file 
main <- main |>
  rename(
    numerator = estimated_deaths,
    rate = estimated_death_rate_per_million_population
    ) |>
  prepare_final_cols()

# save final files
saveRDS(main, file.path(profiles_data_folder, "Data to be checked", "99143_homeless_deaths_shiny.rds"))
write.csv(main, file.path(profiles_data_folder, "Data to be checked", "99143_homeless_deaths_shiny.csv"), row.names = FALSE)


# remove objects from global env.
rm(main, ca_lookup, la_splits)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create population dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# format age/sex splits data
age_sex_formatted <- age_sex_splits |>
  mutate(
    # create split_name col 
    split_name = case_when(
      age_group == "All Ages" &sex %in% c("Persons", "Males", "Females") ~ "Sex",
      age_group != "All Ages" & sex == "Persons" ~ "Age",
      age_group != "All Ages" & sex == "Females" ~ "Age (females)",
      age_group != "All Ages" & sex == "Males" ~ "Age (males)",
      TRUE ~ "Other"
    ),
    # create split_value col 
    split_value = if_else(split_name == "Sex", sex, age_group),
    split_value = if_else(grepl("Persons|All", split_value), "All", split_value),
    
    # create code col
    code = "S00000001"
  ) |>
  rename(
    numerator = estimated_deaths,
    rate = estimated_death_rate_per_million_population
  ) |>
  # select/create cols for final file 
  prepare_final_cols()


# format select causes data (drugs, alcohol suicides)
select_causes_formatted <- select_causes_splits |>
  pivot_longer(
    cols = -year,
    names_to = "split_value",
    values_to = "numerator",
    names_transform = list(split_value = ~ str_replace_all(.x, "_", " "))
  ) |>
  mutate(split_name = "Select causes of death")


# get population lookup (pop estimates aged 15 to 74 to match NRS publication)
pop_lookup <- readRDS(file.path(profiles_data_folder, "Lookups/Population/CA_pop_15to74.rds")) |>
  filter(code == "S00000001") # filter on Scotland 


#calculate crude rate and prepare final columns 
select_causes_formatted <- select_causes_formatted |>
  left_join(pop_lookup, by = "year") |>
  calculate_crude_rate(1000000) |>
  prepare_final_cols()


# combine all splits
popgroups <- rbind(age_sex_formatted, select_causes_formatted)

# save final files
saveRDS(popgroups, file.path(profiles_data_folder, "Data to be checked", "99143_homeless_deaths_shiny_popgrp.rds"))
write.csv(popgroups, file.path(profiles_data_folder, "Data to be checked", "99143_homeless_deaths_shiny_popgrp.csv"), row.names = FALSE)

# ~~~~~~~~~~~~~~~~~
# QA files ----
# ~~~~~~~~~~~~~~~~
run_qa(file ="99143_homeless_deaths", type = "main")
run_qa(file ="99143_homeless_deaths", type = "popgrp")


## END

