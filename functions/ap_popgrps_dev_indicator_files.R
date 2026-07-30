#Still to think about
#Restrict list of acceptable splits? Want to try and standardise where possible but how? E.g. don't want age, age group, age band etc
#Would there be times when it's really inconvenient to input the data in wide format? What should you happen if you input data with split totals?

#4 main measure types

#Percentage - Bowel screening
#Crude rate - Teenage pregnancies
#Standardised rate - Alcohol-related hospital admissions
#Percentages with population correction factor (pcf) - Child healthy weight

#First, create data files for each measure type to be input into the new function
source("./functions/data cleaning functions/fix_fin_year.R")


source("./functions/popgrps_analysis.R") #Sourcing new function for profiles data folder filepath
################################################################################
#Percentage
bowel_screening <- readRDS(file.path(profiles_data_folder, "Received Data/Bowel Screening Uptake/scotPHO_bowel2023.rds")) |> 
  mutate(sex = case_when(sex == "1" ~ "Male", 
                         sex == "2" ~ "Female",
                         TRUE ~ NA_character_))

saveRDS(bowel_screening, file.path(profiles_data_folder, "Prepared Data/bowel_screening_popgrps_raw.rds"))

#Testing
splits_bs <- list(
  age_group= c("50-54", "55-59", "60-64", "65-69", "70-74"),
  sex = c("Male", "Female"))

source("./functions/popgrps_analysis.R")

popgrps_analysis(filename = "bowel_screening", measure = "percent", geography = "datazone11",
                         year_type = "calendar", ind_id = 21102, time_agg = 3, yearstart = 2008,
                         yearend = 2023, test_file = TRUE, QA = TRUE, police_div = FALSE,
                         NA_means_suppressed = FALSE, splits = splits_bs)

#Checking against published data
bowel_screening_published <- readRDS(file.path(profiles_data_folder, "Shiny Data", "bowel_screening_shiny.rds"))  |> 
  mutate(rate = round(rate, digits = 2))

bowel_screening_new <- readRDS(file.path(profiles_data_folder, "Test Shiny Data", "bowel_screening_shiny_popgrp.rds")) |> 
  filter(split_name == "Age group" & split_value == "All")

bs_comparison <- left_join(bowel_screening_published, bowel_screening_new, by = c("code", "year", "trend_axis", "def_period", "ind_id")) |> 
  mutate(perc_diff = (rate.y - rate.x) / rate.x) #Check if rates for overlapping rows are the same between functions


################################################################################
#Crude rate

#To do - finish fixing issues with creating custom populations for user specified age brackets

teen_preg <- read_csv(file.path(profiles_data_folder, "Received Data/Teenage pregnancies/IR2026-00003_TeenPregs.csv")) %>% 
  clean_names()  |>  #set names to lower case
  rename(datazone = datazone2011, numerator = tp, year = yearcon)  |>  
  mutate(datazone = dplyr::na_if(datazone, "Unknown"),  #convert unknown datazones to NA so they're still included in Scotland total to align with births in Scotland publication which included non-residents. 
         age_group = case_when(agecon < 16 ~ "15",
                             agecon >= 16 & agecon < 18 ~ "16-17",
                             agecon >= 18 ~ "18-19",
                             TRUE ~ NA_character_)) #Creating an age group column which will be a split

saveRDS(teen_preg, file.path(profiles_data_folder, "Prepared Data/teen_preg_popgrps_raw.rds"))

#Testing
splits_tp <- list(
  age_group = c("15", "16-17", "18-19"))

source("./functions/popgrps_analysis.R")

popgrps_analysis(filename = "teen_preg", measure = "crude", geography = "datazone11",
                 year_type = "calendar", ind_id = 21001, time_agg = 3, yearstart = 2002,
                 yearend = 2023, 
                 #pop = "DZ11_pop_fem15to19", 
                 pop_sex = "female",
                 crude_rate = 1000, test_file = TRUE,
                 QA = TRUE, police_div = FALSE, NA_means_suppressed = FALSE, splits = splits_tp)

#Checking against published data
teen_preg_published <- readRDS(file.path(profiles_data_folder, "Shiny Data", "teen_preg_shiny.rds"))  |> 
  mutate(rate = round(rate, digits = 2))

teen_preg_new <- readRDS(file.path(profiles_data_folder, "Test Shiny Data", "teen_preg_shiny_popgrp.rds")) |> 
  filter(split_name == "Age group" & split_value == "All")

teen_preg_comparison <- left_join(teen_preg_published, teen_preg_new, by = c("code", "year", "trend_axis", "def_period", "ind_id")) |> 
  mutate(perc_diff = (rate.y - rate.x) / rate.x) #Check if rates for overlapping rows are the same between functions

################################################################################
#Percentages with population correction factor

healthy_weight <- readRDS(file.path(profiles_data_folder, "Received Data/Child Healthy Weight/IR2026-00049_DZ2011.rds")) |> 
  mutate(year = as.numeric(schlyr_exam),
         year = paste0("20", substr(schlyr_exam, 1, 2)),
         sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         TRUE ~ sex)) |> 
  group_by(datazone2011, year, sex) |> 
  summarise(numerator = sum(Healthy_Weight), denominator = sum(tot), .groups = "drop")

saveRDS(healthy_weight, file.path(profiles_data_folder, "Prepared Data/healthy_weight_popgrps_raw.rds"))


#Testing
splits_chw <- list(
  sex = c("Male", "Female"))

source("./functions/popgrps_analysis.R")

popgrps_analysis(filename = "healthy_weight", measure = "perc_pcf", geography = "datazone11",
                         year_type = "school", ind_id = 21106, time_agg = 1, yearstart = 2009,
                         yearend = 2024, test_file = TRUE, splits = splits_chw, QA = FALSE,
                         police_div = FALSE, NA_means_suppressed = FALSE, pop = "DZ11_pop_5")

#Checking against published data
healthy_weight_published <- readRDS(file.path(profiles_data_folder, "Shiny Data", "healthy_weight_shiny.rds"))  |> 
  mutate(rate = round(rate, digits = 2))

healthy_weight_new <- readRDS(file.path(profiles_data_folder, "Test Shiny Data", "healthy_weight_shiny_popgrp.rds")) |> 
  filter(split_name == "Sex" & split_value == "All")

teen_preg_comparison <- left_join(teen_preg_published, teen_preg_new, by = c("code", "year", "trend_axis", "def_period", "ind_id")) |> 
  mutate(perc_diff = (rate.y - rate.x) / rate.x) #Check if rates for overlapping rows are the same between functions



#Double check on NAs produced by square rooting in the pop percf calc

################################################################################
#Standardised rate - Alcohol-related hospital admissions

#Where on earth is "trace 3" coming from? Some NAs in the data?
#Fortunately think whatever population lookup system is in place is functioning as it should

#Reading in males and females existing pop groups prepared data files
alcohol_stays <- rbind(
readRDS(file.path(profiles_data_folder, "Prepared Data/alcohol_stays_females_raw.rds")),
readRDS(file.path(profiles_data_folder, "Prepared Data/alcohol_stays_males_raw.rds"))) |> 
  mutate(sex = case_when(sex_grp == "1" ~ "Male",
                                 sex_grp == "2" ~ "Female",
                                 TRUE ~ sex_grp))

saveRDS(alcohol_stays, file.path(profiles_data_folder, "Prepared Data/alcohol_stays_popgrps_raw.rds"))

#Testing
splits_alc <- list(
  sex = c("Male", "Female"))

source("./functions/popgrps_analysis.R")

popgrps_analysis(filename = "alcohol_stays", geography = "council", measure = "stdrate",
              pop = "CA_pop_allages", yearstart = 2002, yearend = 2024,
              time_agg = 1, epop_age = "normal", epop_total = 100000, ind_id = 20203,
              year_type = "financial", QA = TRUE, police_div = FALSE,
              NA_means_suppressed = FALSE, splits = splits_alc, pop_sex = NULL)


alcohol_stays_published <- readRDS(file.path(profiles_data_folder, "Shiny Data", "alcohol_stays_dz11_shiny.rds"))  |> 
  mutate(rate = round(rate, digits = 2))

alcohol_stays_new <- readRDS(file.path(profiles_data_folder, "Test Shiny Data", "alcohol_stays_shiny_popgrp.rds")) |> 
  filter(split_name == "Sex" & split_value == "All")

alcohol_stays_comparison <- left_join(alcohol_stays_published, alcohol_stays_new, by = c("code", "year", "trend_axis", "def_period", "ind_id")) |> 
  mutate(perc_diff = (numerator.x - numerator.y) / numerator.y) #Check if rates for overlapping rows are the same between functions




#splits variable testing
#the validate_popgrps_columns checks:
#1) That all split names specified in the splits argument are actually present in the data
#2) That there are no split values in the data that do not match an acceptable value from the arguments. 
#It will not check that all split values specified are actually in the data as they may not be for legitimate reasons

#For testing, consider
#Adding a new list element for a split not in the data e.g. SIMD
#Changing the name of a split e.g. age_grp -> age_group
#Changing the name of a split value e.g. "Under 16 years" -> "Under 16 year"
#These should all throw errors


