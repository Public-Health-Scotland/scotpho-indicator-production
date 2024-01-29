### SCRIPT UNDER DEVELOPMENT!!!

# To do:
# Find correct format to present monthly data - has this been done anywhere else in OPT?


###   Update ScotPHO Care and Wellbeing indicators: 
#   99127: COVID-19 vaccination uptake
#   99128: Influenza vaccination uptake


# Data source is PHS Flu & COVID vaccinations open data

# COVID - eligible uptake:
# https://www.opendata.nhs.scot/dataset/flu-covid-vaccinations/resource/831b8008-7635-4774-8bf1-495985965546?inner_span=True

# Flu - eligible uptake:
# https://www.opendata.nhs.scot/dataset/flu-covid-vaccinations/resource/ae1cd4fd-5658-47b5-8e0a-2274627682d5?inner_span=True


### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

# Access COVID data via API
ckan <- src_ckan("https://www.opendata.nhs.scot")
covid_res_id <- "831b8008-7635-4774-8bf1-495985965546"

covid_data <- dplyr::tbl(src = ckan$con, from = covid_res_id) %>%
  as_tibble() %>%
  clean_names()

# Access flu data via API
flu_res_id <- "ae1cd4fd-5658-47b5-8e0a-2274627682d5"

flu_data <- dplyr::tbl(src = ckan$con, from = flu_res_id) %>%
  as_tibble() %>%
  clean_names()



### 2. Prepare data  -----

# Prepare COVID data
covid_data <- covid_data %>%
  
        # Select Winter 2023 booster programme
  filter(dose == "Winter Booster 2023",
         
         # Remove data with unknown residence
         hb_name != "Unknown") %>%
  
  # Select relevant variables
  select(hb, date, cumulative_number_vaccinated, population, cumulative_percent_coverage) %>%
  
  # Rename variables
  rename(code = hb,
         numerator = cumulative_number_vaccinated,
         denominator = population,
         rate = cumulative_percent_coverage) %>%
  
        # Change Scotland area code
  mutate(code = ifelse(code == "S92000003", "S00000001", code),
         
        # Create new columns
        year = as.numeric(str_sub(date, start= 1, end = 4)),
        def_period = as.Date(as.character(date), format = "%Y%m%d"),
        ind_id = 99127)


# Prepare flu data
flu_data <- flu_data %>%
  
  # Select Winter 2023 booster programme
  filter(dose == "Flu 2023",
         
         # Remove data with unknown residence
         hb_name != "Unknown") %>%
  
  # Select relevant variables
  select(hb, date, cumulative_number_vaccinated, population, cumulative_percent_coverage) %>%
  
  # Rename variables
  rename(code = hb,
         numerator = cumulative_number_vaccinated,
         denominator = population,
         rate = cumulative_percent_coverage) %>%
  
  # Change Scotland area code
  mutate(code = ifelse(code == "S92000003", "S00000001", code),
         
         # Create new columns
         year = as.numeric(str_sub(date, start= 1, end = 4)),
         def_period = as.Date(as.character(date), format = "%Y%m%d"),
         ind_id = 99128)



### 3. Prepare final files -----

# Save COVID files in folder to be checked
write.csv(covid_data, paste0(data_folder, "Data to be checked/covid_vaccination_uptake_shiny.csv"), row.names = FALSE)
write_rds(covid_data, paste0(data_folder, "Data to be checked/covid_vaccination_uptake_shiny.rds"))

# Save flu files in folder to be checked
write.csv(flu_data, paste0(data_folder, "Data to be checked/flu_vaccination_uptake_shiny.csv"), row.names = FALSE)
write_rds(flu_data, paste0(data_folder, "Data to be checked/flu_vaccination_uptake_shiny.rds"))


# Run QA reports for each indicator check the output files
run_qa(filename="covid_vaccination_uptake")
run_qa(filename="flu_vaccination_uptake")



#END
  
  