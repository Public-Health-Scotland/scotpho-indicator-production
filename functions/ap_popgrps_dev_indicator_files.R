#4 main measure types

#Percentage - 
#Crude rate - Teenage pregnancies
#Standardised rate - Alcohol-related hospital admissions
#Percentages with population correction factor (pcf) - Child healthy weight

#First, create data files for each measure type to be input into the new function

source("./functions/popgrps_analysis.R")



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

splits_tp <- list(
  age_grp = c("Under 16 year", "16-17 years", "Under 20 years"),
  dummy_group = c("A", "B"))



popgrps_analysis(filename = "teen_preg", measure = "crude", geography = "datazone11",
                 year_type = "calendar", ind_id = 21001, time_agg = 3, yearstart = 2002,
                 yearend = 2023, pop = "DZ11_pop_fem15to19", crude_rate = 1000, test_file = TRUE,
                 splits = splits_tp)

