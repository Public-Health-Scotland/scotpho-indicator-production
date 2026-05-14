#Still to think about
#Restrict list of acceptable splits? Want to try and standardised where possible but how? E.g. don't want age, age group, age band etc

#4 main measure types

#Percentage - Bowel screening
#Crude rate - Teenage pregnancies
#Standardised rate - Alcohol-related hospital admissions
#Percentages with population correction factor (pcf) - Child healthy weight

#First, create data files for each measure type to be input into the new function

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

data <- popgrps_analysis(filename = "bowel_screening", measure = "percent", geography = "datazone11",
                         year_type = "calendar", ind_id = 21102, time_agg = 3, yearstart = 2008,
                         yearend = 2023, test_file = TRUE, splits = splits_bs)

################################################################################
#Crude rate
teen_preg <- read_csv(file.path(profiles_data_folder, "Received Data/Teenage pregnancies/IR2026-00003_TeenPregs.csv")) %>% 
  clean_names()  |>  #set names to lower case
  rename(datazone = datazone2011, numerator = tp, year = yearcon)  |>  
  mutate(datazone = dplyr::na_if(datazone, "Unknown"),  #convert unknown datazones to NA so they're still included in Scotland total to align with births in Scotland publication which included non-residents. 
         age_grp = case_when(agecon < 16 ~ "Under 16 years",
                             agecon >= 16 & agecon < 18 ~ "16-17 years",
                             agecon >= 18 ~ "Under 20 years",
                             TRUE ~ NA_character_), #Creating an age group column which will be a split
         dummy_group = sample(c("A", "B"), size = n(), replace = TRUE)) #Assigning all rows randomly to A or B to make another dummy split


saveRDS(teen_preg, file.path(profiles_data_folder, "Prepared Data/teen_preg_popgrps_raw.rds"))
################################################################################

splits_tp <- list(
  age_grp = c("Under 16 years", "16-17 years", "Under 20 years"),
  dummy_group = c("A", "B"))

source("./functions/popgrps_analysis.R")

data <- popgrps_analysis(filename = "teen_preg", measure = "crude", geography = "datazone11",
                 year_type = "calendar", ind_id = 21001, time_agg = 3, yearstart = 2002,
                 yearend = 2023, pop = "DZ11_pop_fem15to19", crude_rate = 1000, test_file = TRUE,
                 splits = splits_tp)


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




