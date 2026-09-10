
###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions

library(nanoparquet) #needed for opening cl-out lookups

#cl-out folderwith population projections
cl_out <- "/conf/linkage/output/lookups/Unicode/" 


## functions ----


# small function to create 5 year age groups
create_broad_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp_broad = as.character(case_when(between(age, 0, 17) ~ 1, between(age, 18, 44) ~ 2,
                                                      between(age, 45, 64) ~ 3, between(age, 65, 74) ~ 4, 
                                                      between(age, 75, 84) ~ 5, between(age, 85, 200) ~ 6)))
}

label_agegroups <- function(dataset, age_col=c("age_grp5","age_grp_broad")) {
  
  if(age_col== "age_grp_broad") {
  dataset %>% mutate(age_label = case_when({{age_col}} == "1" ~ "0-17", {{age_col}} == "2"   ~ "18-44",
                                     {{age_col}} == "3"   ~ "45-64", {{age_col}} == "4"   ~ "65-74",
                                     {{age_col}} == "5"   ~ "75-84", {{age_col}} == "6"   ~ "85+", TRUE ~"other")) 
  }else if (age_col=="age_grp5") {
   dataset %>% mutate(age_label = case_when({{age_col}}  == 1 ~ "0-4",{{age_col}}  == 2   ~ "5-9",
                                            {{age_col}}  == 3   ~ "10-14", {{age_col}}  == 4   ~ "15-19",
                                            {{age_col}}  == 5   ~ "20-24", {{age_col}}  == 6   ~ "25-29",
                                            {{age_col}}  == 7   ~ "30-34", {{age_col}}  == 8   ~ "35-39",
                                            {{age_col}}  == 9   ~ "40-44", {{age_col}}  == 10  ~ "45-49",
                                            {{age_col}}  == 11  ~ "50-54", {{age_col}}  == 12  ~ "55-59",
                                            {{age_col}}  == 13  ~ "60-64", {{age_col}}  == 14  ~ "65-69",
                                            {{age_col}}  == 15  ~ "70-74", {{age_col}}  == 16  ~ "75-79",
                                            {{age_col}}  == 17  ~ "80-84", {{age_col}}  == 18  ~ "85-89",
                                            {{age_col}}  == 19  ~ "90+", TRUE ~"other")) }
  }

  
  
  
#   if(age_col== age_grp5) 
#   dataset %>% mutate(age_label = case_when(age_grp == 1 ~ "0-4",
#                          age_grp == 2   ~ "5-9",
#                          age_grp == 3   ~ "10-14",
#                          age_grp == 4   ~ "15-19",
#                          age_grp == 5   ~ "20-24",
#                          age_grp == 6   ~ "25-29",
#                          age_grp == 7   ~ "30-34",
#                          age_grp == 8   ~ "35-39",
#                          age_grp == 9   ~ "40-44",
#                          age_grp == 10  ~ "45-49",
#                          age_grp == 11  ~ "50-54",
#                          age_grp == 12  ~ "55-59",
#                          age_grp == 13  ~ "60-64",
#                          age_grp == 14  ~ "65-69",
#                          age_grp == 15  ~ "70-74",
#                          age_grp == 16  ~ "75-79",
#                          age_grp == 17  ~ "80-84",
#                          age_grp == 18  ~ "85-89",
#                          age_grp == 19  ~ "90+", TRUE ~"other"),
#   
#   
#   
# }









###############################################.
## Population projections ----
## Available for scotland, nhs board, council and hscp only
###############################################.


projections_scot <- read_parquet(file.path(cl_out, "/Populations/Projections/scot_pop_proj_2024_2049.parquet")) |>
  mutate(code="S00000001") |>
  select(year, code, sex, age, pop)

projections_hscp <- read_parquet(file.path(cl_out, "/Populations/Projections/HSCP2019_pop_proj_2022_2047.parquet")) |>
  rename(code=hscp2019) |>
  select(year, code, sex, age, pop)

projections_hb <- read_parquet(file.path(cl_out, "/Populations/Projections/HB2019_pop_proj_2022_2047.parquet")) |>
  rename(code=hb2019)|>
  select(year, code, sex, age, pop)

projections_ca <- read_parquet(file.path(cl_out, "/Populations/Projections/CA2019_pop_proj_2022_2047.parquet"))|>
  rename(code=ca2019)|>
  select(year, code, sex, age, pop)

# bind the files
projections_pop <- rbind(projections_scot, projections_hb, projections_ca, projections_hscp)

# remove separate files and keep only combined version
rm(projections_ca,projections_hb,projections_hscp, projections_scot)


#####################################################.
## Dependency ----
## Create file that can generate dependency ratios (0-16 plus 65+ versus working age)
projections_dependency <-projections_pop |>
  mutate(dependency_cat = case_when (age <=15 ~ "young&old", age >=65 ~ "young&old", TRUE ~"working")) |>
  group_by(year, code, sex, dependency_cat) |>
  summarise (pop=sum(pop)) |>
  ungroup()


#####################################################.
## Projections for population pyramids ----
projections_populations <-projections_pop |>
  create_agegroups()|>
  create_broad_agegroups()

  # not all geography levels available for same time period
  # projections come out at different time to new MYE so possible duplicate years
  # national 2024-2049
  # subnational 2022-2047
  # useful years to keep 2025,2030,2035,2040,2045,2049
  filter(year %in% c("2025","2030","2035","2040","2045","2049"))
         

projections_5y <-projections_populations |>
  group_by(code, year, sex, age_grp)|>
  summarise(population=sum(pop),
            age_category = "5 year agebands")|>
  ungroup()|>
  rename(age_grp5=age_grp)

projections_5y<- projections_5y |>
  label_agegroups(age_col="age_grp5")
  
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
                         age_grp == 19  ~ "90+", TRUE ~"other")) |>
  arrange(year, code, sex, as.numeric(age_grp))


projections_broadage <-projections_populations |>
  group_by(code, year, sex, age_grp_broad)|>
  summarise(population=sum(pop),
            age_category = "Broad agebands")|>
  ungroup()|>
#  rename(age_grp=age_grp_broad)|>
  label_agegroups(age_col=age_grp_broad)
  
  rm(projections_broadage)
  
  mutate(age = case_when(age_grp == 1 ~ "0-17",
                         age_grp == 2   ~ "18-44",
                         age_grp == 3   ~ "45-64",
                         age_grp == 4   ~ "65-74",
                         age_grp == 5   ~ "75-84",
                         age_grp == 6   ~ "85+", TRUE ~"other")) |>
  arrange(year, code, sex, as.numeric(age_grp))

#bind the two data frames
projections_populations2 <- rbind(projections_5y, projections_broadage)

#tidy
rm(projections_5y ,projections_broadage)

  
## Calculate total sum of populations to allow derivation of the percentage of total population (which are used to generate population pyramids)

projections_populations3 <-projections_populations2 |>         
  #aggregate populations by age band
  group_by(year, code, age_category) |>
  mutate(pop_sum=sum(population))|> #add total population for a particular geography code & year 
  ungroup() |> #ungroup to allow % with male/female within geocode
  mutate(percentage=population/pop_sum*100)|>
  select(-pop_sum) |>
  # create sex field with description - these become fieldnames so need character description not numeric values
  mutate(sex_grp=case_when(sex=="1" ~ "Male",sex=="2" ~"Female", TRUE ~"other"))|>
  pivot_wider(names_from = sex_grp, values_from = c(percentage,population))|>
  mutate(percentage_Male=0-percentage_Male,
         source = "Projections",
         year = as.numeric(year)) |> #convert male percentage to negative value which is required in population pyramid
  arrange (code, year, age_category, age_grp)


projections_5y <-projections_populations |>
  # derive total population for each area and year (this is so we can 
  group_by(code, year)|>
  mutate(population_sum=sum(population))|> #add total population for a particular geography code & year 
  ungroup() |> #ungroup to allow % with male/female within geocode
  mutate(percentage=population/population_sum*100)|>
  select(-pop_sum) |>
  pivot_wider(names_from = sex_grp, values_from = c(percentage,population))|>
  mutate(percentage_Male=0-percentage_Male,
         source = "Projections",
         year = as.numeric(year)) |> #convert male percentage to negative value which is required in population pyramid
  arrange (code, year, age_grp)


,
  # rename(age = age_group_name,
  #      age_grp = age_group,
  #      sex_grp = sex,
  #      population= pop)|>
  #combine the agebands so that under 1s are grouped with 1-4 - creates 19 agebands 
  # mutate(age_grp= case_when(age_grp==0 ~ 1, TRUE ~ age_grp),
  #        age= case_when(age=="0" ~"0-4", age=="1-4" ~"0-4",  TRUE ~ age),
  #        sex_grp=case_when(sex_grp=="1" ~ "Male",sex_grp=="2" ~"Female", TRUE ~"other"))|>
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


################################################.
## Mid Year/SAPE Populations ----
################################################.


## Populations derived from ScotPHO population lookups
## Script where file generated can be found "https://github.com/Public-Health-Scotland/scotpho-lookups/blob/master/population_lookup.R"

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
                         age_grp == 19  ~ "90+", TRUE ~"other"),
         #make the source specific so if its small area or mye or not based on geo type
         source=case_when(substr(code, 1, 3) %in% c("S02","S99")~ "Small area population estimates (SAPE)", TRUE ~ "Mid-year Estimates (MYE)"))


################################################.
## Add projections and populations estimates ----
################################################.

all<- rbind(demographics_dataset,projections_pop2)


depend_ratio <- all |>
  mutate(category = case_when(age %in% c("0-4","5-9","10-14",))
  group_by(year, code
  summarise(dependents = sum())





