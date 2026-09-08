
###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions

library(nanoparquet)

cl_out <- "/conf/linkage/output/lookups/Unicode/" #cl-out folderwith population projections



###############################################.
## Population projections ----
###############################################.


projections_scot <- read_parquet(file.path(cl_out, "/Populations/Projections/scot_pop_proj_5year_agegroups_2024_2049.parquet")) |>
  mutate(code="S00000001") |>
  select(year, code, sex, age_group, age_group_name, pop)

projections_hscp <- read_parquet(file.path(cl_out, "/Populations/Projections/HSCP2019_pop_proj_5year_agegroups_2022_2047.parquet")) |>
  rename(code=hscp2019) |>
  select(year, code, sex, age_group, age_group_name, pop)

projections_hb <- read_parquet(file.path(cl_out, "/Populations/Projections/HB2019_pop_proj_5year_agegroups_2022_2047.parquet")) |>
  rename(code=hb2019)|>
  select(year, code, sex, age_group, age_group_name, pop)

projections_ca <- read_parquet(file.path(cl_out, "/Populations/Projections/CA2019_pop_proj_5year_agegroups_2022_2047.parquet"))|>
  rename(code=ca2019)|>
  select(year, code, sex, age_group, age_group_name, pop)

# bind the files
projections_pop <- rbind(projections_scot, projections_hb, projections_ca, projections_hscp)

projections_pop <-projections_pop |>
rename(age = age_group_name,
       age_grp = age_group,
       sex_grp = sex,
       population= pop)|>
  #combine the agebands so that under 1s are grouped with 1-4 - creates 19 agebands 
  mutate(age_grp= case_when(age_grp==0 ~ 1, TRUE ~ age_grp),
         age= case_when(age=="0" ~"0-4", age=="1-4" ~"0-4",  TRUE ~ age),
         sex_grp=case_when(sex_grp=="1" ~ "Male",sex_grp=="2" ~"Female", TRUE ~"other"))|>
  group_by(year, code,sex_grp,age,age_grp) |>
  summarise(population=sum(population))|>
  ungroup()


projections_pop2 <-projections_pop |>
  # not all geography levels available for same time period
  # projections come out at different time to new MYE so possible duplicate years
  # national 2024-2049
  # subnational 2022-2047
  # useful years to keep 2025,2030,2035,2040,2045,2049
  filter(year %in% c("2025","2030","2035","2040","2045","2049")) |>
  # derive total population for each area and year (this is so we can 
  group_by(code,year)|>
  mutate(pop_sum=sum(population))|> #add total population for a particular geography code & year 
  ungroup()|> #ungroup to allow % with male/female within geocode
  mutate(percentage=population/pop_sum*100)|>
  select(-pop_sum) |>
  pivot_wider(names_from = sex_grp, values_from = c(percentage,population))|>
  mutate(percentage_Male=0-percentage_Male,
         source = "Projections",
         year = as.numeric(year)) |> #convert male percentage to negative value which is required in population pyramid
arrange (code, year, age_grp)

rm(projections_ca,projections_hb,projections_hscp, projections_scot)
  

#saveRDS(population_breakdown, file=paste0(pop_lookup, 'population_age_sex_breakdown_wide.rds'))




demographics_dataset <- readRDS(file=paste0(profiles_data_folder, '/Lookups/Population/population_age_sex_breakdown_wide.rds')) |>
  # Add age desriptions to population data (should this be done in indicator production?)
  mutate(age = case_when(age_grp == 1 ~ "0-4",
                         age_grp == 2   ~ "5-9",
                         age_grp == 3   ~ "10-14",
                         age_grp == 4   ~ "15-19",
                         age_grp == 5   ~ "20-24",
                         age_grp == 6   ~ "25-29",
                         age_grp == 7   ~ "30-34",
                         age_grp == 8   ~ "35-39",
                         age_grp == 9   ~ "40-44",
                         age_grp == 10  ~ "45-49",
                         age_grp == 11  ~ "50-54",
                         age_grp == 12  ~ "55-59",
                         age_grp == 13  ~ "60-64",
                         age_grp == 14  ~ "65-69",
                         age_grp == 15  ~ "70-74",
                         age_grp == 16  ~ "75-79",
                         age_grp == 17  ~ "80-84",
                         age_grp == 18  ~ "85-89",
                         age_grp == 19  ~ "90+", TRUE ~"otheer"),
         #make the source specific so if its small area or mye or not based on geo type
         source="Mid-year Estimates")


all<- rbind(demographics_dataset,projections_pop2)

