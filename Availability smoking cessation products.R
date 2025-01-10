# ScotPHO indicators: Availability of smoking cessation products

#   Part 1 - Create basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
data_products <- readRDS(paste0(data_folder, "Received Data/Availability of smoking cessation products/scotpho_smoking_ddds_20241127.rds")) |> 
  clean_names() |> 
  rename(datazone = datazone2011,
         numerator = ddds,
         year = financial_year) |> 
  ungroup() |>
  select(c(1,4:5))


saveRDS(data_products, file=paste0(data_folder, 'Prepared Data/cessation_products_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "cessation_products", geography = "datazone11", measure = "crude", hscp = T, 
              pop="DZ11_pop_12+", adp = F,
              yearstart = 2002, yearend = 2023, time_agg = 1)

analyze_second(filename = "cessation_products", measure = "crude", time_agg = 1,
               crude_rate = 1000, ind_id = 1544, year_type = "financial")

##END

