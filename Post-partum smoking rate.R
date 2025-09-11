# ScotPHO indicator: Post-partum smoking rate 1552

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") #Deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#Data comes from child health team
postpartum <- read.csv(file.path(profiles_data_folder, '/Received Data/Post-partum Smoking/IR2025-00008.csv')) |>  
  janitor::clean_names() |> 
  mutate(year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                   TRUE ~ paste0("20",substr(fin_year,1,2)))) #format year to display financial year

postpartum <- postpartum |> 
  rename(code = datazone2011, 
         numerator = smoker,
         denominator = total_valid_status) |> 
  select(-fin_year) |> 
  mutate(year = as.numeric(year))

#Aggregating over three-year period
postpartum_test <- postpartum |> 
  group_by(code) |> 
  mutate(year2 = lead(year, 1),
         year3 = lead(year, 2),
         num2 = lead(numerator, 1),
         num3 = lead(numerator, 2),
         denom2 = lead(denominator, 1),
         denom3 = lead(denominator, 2)) |> 
  filter(!is.na(year3)) |> 
  reframe(numerator = numerator + num2 + num3,
            denominator = denominator + denom2 + denom3,
          year = year2)


saveRDS(postpartum_test, file.path(profiles_data_folder, '/Prepared Data/postpartum_smoking_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "postpartum_smoking", geography = "datazone11", measure = "percent",
              yearstart = 2002, yearend = 2022, time_agg = 1, ind_id = 1552, year_type = "financial")

deprivation_analysis(filename = "postpartum_smoking", yearstart = 2002, yearend = 2022,
                       time_agg = 1, year_type = "financial", measure = "percent", pop_sex = "all",
                       ind_id = 1552)

##END