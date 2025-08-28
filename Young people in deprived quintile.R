# ScotPHO indicators: 
# Young people living in the most access deprived quintile, aged 0-25 years
# Young people living in the most crime deprived quintile, aged 0-25 years
# Young people living in the most income deprived quintile, aged 0-25 years

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
population_lookups <- file.path(profiles_data_folder, "Lookups", "Population")

source("./functions/main_analysis.R") #Normal indicator functions


#Small function to standardize each years info. Function parameters:
#Data is for what basefile to use, list_pos is for the position of the data frame
#simd for which simd variables-year to look at, year for what year is the data created.
  prepare_file <- function(dz_list14, dz_list17) {
    pop <- readRDS(file.path(population_lookups, "DZ11_pop_basefile.rds")) |>  
      filter(age<26 & year>2010) |>  group_by(year, datazone2011) |> 
      summarise(numerator = sum(denominator, na.rm= T), .groups = "drop") |>  
      rename(datazone = datazone2011)
  
  raw_data <<- rbind(
    pop |>  filter((datazone %in% dz_list14 & between(year, 2011, 2016))),
    pop |>  filter((datazone %in% dz_list17 & year > 2016)))
  
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.

simd_data14 <- readRDS(file.path(cl_out_depr, 'DataZone2011_simd2016.rds')) |>  
  janitor::clean_names() |> 
  select(data_zone2011, simd2016_crime_rank, simd2016_access_rank, simd2016_inc_rank) |> 
  rename(datazone2011 = data_zone2011)

simd_data17 <- readRDS(file.path(cl_out_depr, 'DataZone2011_simd2020v2.rds')) |>  
  janitor::clean_names() |> 
  select(datazone2011, simd2020v2_crime_rank, simd2020v2_access_rank, simd2020v2_inc_rank)

# Population 25 or under
# Selecting pop years used to create each simd version 2014 for SIMD2016, 2017 for SIMD2020
pop <- readRDS(file.path(population_lookups, "DZ11_pop_basefile.rds")) |>  
  filter(year %in% c("2014", "2017") & age <26) |>  
  group_by(year, datazone2011) |>  
  summarise(pop = sum(denominator, na.rm= T), .groups = "drop")

# Population totals for each year
pop14 <- pop |>  filter(year == "2014") 
pop17 <- pop |>  filter(year == "2017") 

pop_total14 <- pop14 |> group_by(year) |>  
  summarise(pop = sum(pop, na.rm= T), .groups = "drop") |> pull(pop)

pop_total17 <- pop17 |> group_by(year) |> 
  summarise(pop = sum(pop, na.rm= T), .groups = "drop") |> pull(pop)

# Creating the population thresholds for each quintile
cut_breaks14 <- c(0, pop_total14/5, pop_total14/5*2, pop_total14/5*3, pop_total14/5*4, pop_total14)
cut_breaks17 <- c(0, pop_total17/5, pop_total17/5*2, pop_total17/5*3, pop_total17/5*4, pop_total17)

# Preparing files for simd 2016
simd_data14 <- left_join(simd_data14, pop14, by = c("datazone2011")) |>  
  arrange(simd2016_crime_rank) |>  # crime pop weighted quintile
  mutate(cum_pop_crime=cumsum(pop),
         crime_quintile = as.numeric(paste(cut(cum_pop_crime, cut_breaks14, include.lowest=TRUE,
                                        labels=c("5", "4", "3", "2", "1"))))) |> 
  arrange(simd2016_access_rank) |>  # access pop weighted quintile
  mutate(cum_pop_access=cumsum(pop),
         access_quintile = as.numeric(paste(cut(cum_pop_access, cut_breaks14, include.lowest=TRUE,
                                               labels=c("5", "4", "3", "2", "1"))))) |>  
  arrange(simd2016_inc_rank) |>  # income pop weighted quintile
  mutate(cum_pop_inc=cumsum(pop),
         inc_quintile = as.numeric(paste(cut(cum_pop_inc, cut_breaks14, include.lowest=TRUE,
                                               labels=c("5", "4", "3", "2", "1"))))) |>  
  select(-starts_with("cum_pop"), -starts_with("simd"))

# Preparing files for simd 2020
simd_data17 <- left_join(simd_data17, pop17, by = c("datazone2011")) |>  
  arrange(simd2020v2_crime_rank) |>  # crime pop weighted quintile
  mutate(cum_pop_crime=cumsum(pop),
         crime_quintile = as.numeric(paste(cut(cum_pop_crime, cut_breaks17, include.lowest=TRUE,
                                               labels=c("5", "4", "3", "2", "1"))))) |> 
  arrange(simd2020v2_access_rank) |>  # access pop weighted quintile
  mutate(cum_pop_access=cumsum(pop),
         access_quintile = as.numeric(paste(cut(cum_pop_access, cut_breaks17, include.lowest=TRUE,
                                                labels=c("5", "4", "3", "2", "1"))))) |>  
  arrange(simd2020v2_inc_rank)  |>  # income pop weighted quintile
  mutate(cum_pop_inc=cumsum(pop),
         inc_quintile = as.numeric(paste(cut(cum_pop_inc, cut_breaks17, include.lowest=TRUE,
                                             labels=c("5", "4", "3", "2", "1")))))  |>  
  select(-starts_with("cum_pop"), -starts_with("simd"))

# Creating lists of datazones in most topic deprived quintile
crime_dz14 <- simd_data14 |>  filter(crime_quintile == "5") |>  pull(datazone2011)
inc_dz14 <- simd_data14 |>  filter(inc_quintile == "5") |>  pull(datazone2011)
access_dz14 <- simd_data14 |>  filter(access_quintile == "5") |>  pull(datazone2011)
crime_dz17 <- simd_data17 |>  filter(crime_quintile == "5") |>  pull(datazone2011)
inc_dz17 <- simd_data17 |>  filter(inc_quintile == "5") |>  pull(datazone2011)
access_dz17 <- simd_data17 |>  filter(access_quintile == "5") |>  pull(datazone2011)

# Preparing files, joining with population by dz and saving files
saveRDS(prepare_file(inc_dz14, inc_dz17), 
        file.path(profiles_data_folder, "/Prepared Data/young_people_income_raw.rds"))
saveRDS(prepare_file(crime_dz14, crime_dz17), 
        file.path(profiles_data_folder, "/Prepared Data/young_people_crime_raw.rds"))
saveRDS(prepare_file(access_dz14, access_dz17), 
        file.path(profiles_data_folder, "/Prepared Data/young_people_access_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
###############################################.

main_analysis(filename = "young_people_crime", measure = "percent", geography = "datazone11", 
              year_type = "calendar", ind_id = 13005, time_agg = 1, yearstart = 2011,
              yearend = 2024, pop = "DZ11_pop_under26")

main_analysis(filename = "young_people_access", measure = "percent", geography = "datazone11", 
                year_type = "calendar", ind_id = 13003, time_agg = 1, yearstart = 2011,
                yearend = 2024, pop = "DZ11_pop_under26")

main_analysis(filename = "young_people_income", measure = "percent", geography = "datazone11", 
                year_type = "calendar", ind_id = 13004, time_agg = 1, yearstart = 2011,
                yearend = 2024, pop = "DZ11_pop_under26")

