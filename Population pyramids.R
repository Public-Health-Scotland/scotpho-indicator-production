# Population pyramids.R

# Script generates populations data file containing projections and mid-year estimates 
# Also population_dependency_ratio (working age population (16-64) as a percentage of total pop

# The output files produced form datafiles that are used in the demographics profile within profiles tool


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./functions/main_analysis.R") #Normal indicator functions
library(nanoparquet) #needed for opening cl-out lookups

#cl-out folderwith population projections
cl_out <- "/conf/linkage/output/lookups/Unicode/" 

# Function to create broad age groups ----
create_broad_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp_broad = as.character(case_when(between(age, 0, 17) ~ 1, between(age, 18, 44) ~ 2,
                                                      between(age, 45, 64) ~ 3, between(age, 65, 74) ~ 4, 
                                                      between(age, 75, 84) ~ 5, between(age, 85, 200) ~ 6)))
}


# Function to flexibly label agebands depending on what age band type is (5 year or broad ageband) ----
label_agegroups <- function(dataset,age_type = c("5 year agebands","Broad agebands"), age_col=c("age_grp5","age_grp_broad")) {
  
  if(age_type== "Broad agebands") {
  dataset %>% mutate(age_label = case_when({{age_col}} == "1" ~ "0-17", {{age_col}} == "2"   ~ "18-44",
                                     {{age_col}} == "3"   ~ "45-64", {{age_col}} == "4"   ~ "65-74",
                                     {{age_col}} == "5"   ~ "75-84", {{age_col}} == "6"   ~ "85+", TRUE ~"other")) 
  
  }else if(age_type == "5 year agebands") {
   dataset %>% mutate(age_label = case_when({{age_col}}  == 1 ~ "0-4",{{age_col}}  == 2   ~ "5-9",
                                            {{age_col}}  == 3   ~ "10-14", {{age_col}}  == 4   ~ "15-19",
                                            {{age_col}}  == 5   ~ "20-24", {{age_col}}  == 6   ~ "25-29",
                                            {{age_col}}  == 7   ~ "30-34", {{age_col}}  == 8   ~ "35-39",
                                            {{age_col}}  == 9   ~ "40-44", {{age_col}}  == 10  ~ "45-49",
                                            {{age_col}}  == 11  ~ "50-54", {{age_col}}  == 12  ~ "55-59",
                                            {{age_col}}  == 13  ~ "60-64", {{age_col}}  == 14  ~ "65-69",
                                            {{age_col}}  == 15  ~ "70-74", {{age_col}}  == 16  ~ "75-79",
                                            {{age_col}}  == 17  ~ "80-84", {{age_col}}  == 18  ~ "85-89",
                                            {{age_col}}  == 19  ~ "90+", TRUE ~"other")) }}


######################################################################.
## Population projections ----
## Available for Scotland, NHS board, council and hscp only
######################################################################.

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
projections <- rbind(projections_scot, projections_hb, projections_ca, projections_hscp)

# remove separate files and keep only combined version
rm(projections_ca,projections_hb,projections_hscp, projections_scot)


#####################################################.
## Dependency Ratio ----
## Create file that can be used to generate projected dependency ratios (0-16 plus 65+ versus working age)
projections_dependency <-projections |>
  mutate(dependency_cat = case_when (age <=15 ~ "young&old", age >=65 ~ "young&old", TRUE ~"working")) |>
  group_by(year, code, sex, dependency_cat) |>
  summarise (pop=sum(pop)) |>
  ungroup()

# this file is picked up again later in syntax when creating dependency ratio indicator


########################################################################################.
## Creating datset containg population projections for use in population pyramids ----

projections <-projections |>
  create_agegroups()|>
  create_broad_agegroups() |>
  # not all geography levels available for same time period
  # projections are published at a differnt point in the year to new MYE (so need to look out for possible duplicate years i.e. years which are in available through MYE but that might also 
  # be contained in the projections if they haven't been updated - this is done when projections and mye dataframes are combined).
  # Also note that Scotland level and subnation projections may cover different period since these are also published at different times of the year.
  # national 2024-2049
  # subnational 2022-2047
  # We don't need every single year of porjected population - filter for a selection of useful date e.g. ?every 5 years for example- 2025,2030,2035,2040,2045,2049
  filter(year %in% c("2025","2030","2035","2040","2045","2049"))
       
## 5 YEAR AGE BAND PROJECTIONS ----
projections_5y <-projections |>
  group_by(code, year, sex, age_grp)|>
  summarise(population=sum(pop),
            age_category = "5 year agebands")|>
  ungroup()|>
  rename(age_grp5="age_grp") |> # naming agegrp field explicitly to reduce chance of mislabelling 
  label_agegroups(age_type="5 year agebands", age_col=age_grp5) |>
  rename(age_grp=age_grp5) #revert to original name to allow binding of df


## BROAD AGE BAND PROJECTIONS ----
projections_broadage <-projections |>
  group_by(code, year, sex, age_grp_broad)|>
  summarise(population=sum(pop),
            age_category = "Broad agebands")|>
  ungroup()|>
  label_agegroups(age_col=age_grp_broad, age_type="Broad agebands") |>
  rename(age_grp=age_grp_broad)


#bind the two data frames
pop_projections <- rbind(projections_5y, projections_broadage) |>
  arrange(year, code, sex, age_category, as.numeric(age_grp)) |>
  mutate(geotype=substr(code,1,3), #create a geo type field as we need to avoid where years are present in both projections and population estimates
         pop_type="Population Projection")|> #label source of this population 
 rename(sex_grp=sex)
  
#tabulate which years are present for which geographies
xtabs(~pop_projections$geotype+pop_projections$year)

# #OPTIONAL remove any duplicated years if there are any
# pop_projections <-pop_projections |>
#   filter(year != XXXX)

#tidy
rm(projections_5y ,projections_broadage, projections)


################################################.
## Mid Year/SAPE Populations ----
################################################.

## Populations derived from ScotPHO population lookups (all geographies, single year of age, all years) - note this is a large file 
## Script where file generated can be found "https://github.com/Public-Health-Scotland/scotpho-lookups/blob/master/population_lookup.R"

estimates_dataset <- readRDS(file=paste0(profiles_data_folder, '/Lookups/Population/basefile_DZ11.rds')) |>
  filter(age >= 0 & age <= 200) |> #ensure selecting age reasonable range
  filter(!(substr(code,1,3) %in% c("S11","S32"))) |> #exclude police divisions (s32) and ADP (s11)level geographies for this data
  create_agegroups()|>
  create_broad_agegroups()

## 5 YEAR AGE BAND ESTIMATES----
estimates_5y <- estimates_dataset|>
  group_by(year,code,age_grp,sex_grp) |>
  summarise(population=sum(denominator),
            age_category = "5 year agebands")|>
  ungroup() |>
  rename(age_grp5="age_grp") |> # naming agegrp field explicitly to reduce chance of mislabelling 
  label_agegroups(age_type="5 year agebands", age_col=age_grp5) |>
  rename(age_grp=age_grp5) #revert to original name to allow binding of df


## BROAD AGE BAND ESTIMATES ----
estimates_broadage <- estimates_dataset|>
  group_by(code, year, sex_grp, age_grp_broad)|>
  summarise(population=sum(denominator),
            age_category = "Broad agebands")|>
  ungroup()|>
  label_agegroups(age_col=age_grp_broad, age_type="Broad agebands") |>
  rename(age_grp=age_grp_broad)  
  

#bind the two data frames
pop_estimates <- rbind(estimates_5y, estimates_broadage) |>
  arrange(year, code, sex_grp, age_category, as.numeric(age_grp)) |>
  mutate(geotype=substr(code,1,3),#create a geo type field as we need to avoid where years are present in both projections and population estimates
         pop_type="Population Estimates") #label source of this population

#tabulate which years are present for which geographies
xtabs(~geotype+year, data=pop_estimates)

# adjust years to include 
pop_estimates <- pop_estimates |>
filter(year %in% c(2024,2023,2022,2020,2015,2011,2002)) 

#tidy
rm(estimates_5y ,estimates_broadage, estimates_dataset)

################################################.
## Bind Projection and Estimates dataframes ----
################################################.

# bind the projection & estimate files
demographics_populations <- rbind(pop_estimates, pop_projections) |>

 # group to add total population for a geography by year
  group_by(year,code, age_category) |>
  mutate(pop_sum=sum(population), #add total population for a particular geography code & year 
         sex_grp=case_when(sex_grp=="1" ~ "Male",sex_grp=="2" ~"Female", TRUE ~"other")) |>
  ungroup()|> #ungroup to allow % with male/female within geocode
  mutate(percentage=population/pop_sum*100) |>
  select(-pop_sum) |>
  pivot_wider(names_from = sex_grp, values_from = c(percentage,population))|>
  mutate(percentage_Male=0-percentage_Male,#convert male percentage to negative value which is required in population pyramid
         tot_pop= population_Male+population_Female) 
  
## Save output file which can be picked up in shiny app data prep steps 
write_parquet(demographics_populations, paste0(profiles_data_folder, "/Data to be checked/demographics_populations.parquet"))
  

  

################################################.
## Dependency ratio ----
################################################.

all<- rbind(demographics_dataset,projections_pop2)


depend_ratio <- all |>
  mutate(category = case_when(age %in% c("0-4","5-9","10-14",))
  group_by(year, code
  summarise(dependents = sum())





