### SCRIPT UNDER DEVELOPMENT!!!

# To do:
# Add ethnincity data once published at the end of 2023/2024 programme


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
  filter(dose %in% c("Winter Booster 2022", "Winter Booster 2023"),
         
         # Remove data with unknown residence
         hb_name != "Unknown") %>%
  
  # Rename variables
  rename(code = hb,
         numerator = cumulative_number_vaccinated,
         denominator = population) %>%
  
        # Change Scotland area code
  mutate(code = ifelse(code == "S92000003", "S00000001", code),
         
        # Create new year variable from dose variable
        year = case_when(dose == "Winter Booster 2022" ~ 2022,
                         dose == "Winter Booster 2023" ~ 2023)) %>% 
  
  # Group by area code and year
  group_by(code, year) %>% 
  
  # Sum for vaccination programme totals
  summarise(numerator = sum(numerator),
            denominator = sum(denominator)) %>% 
  
  # Calculate rate and create new columns
  mutate(rate = numerator / denominator,
        trend_axis = paste0(year, "/", year+1),
        def_period = paste0(year, "/", year+1, " Winter vaccination programme"),
        ind_id = 99127,
        lowci = NA, upci = NA) %>% 
  
  # Select relevant variables
  select(code, ind_id, year, trend_axis, def_period, numerator, denominator, rate, lowci, upci)



# Prepare flu data
flu_data <- flu_data %>%
  
  # Select Winter 2023 booster programme
  filter(dose %in% c("Flu 2022", "Flu 2023"),
         
         # Remove data with unknown residence
         hb_name != "Unknown") %>%
  
  # Rename variables
  rename(code = hb,
         numerator = cumulative_number_vaccinated,
         denominator = population) %>%
  
  # Change Scotland area code
  mutate(code = ifelse(code == "S92000003", "S00000001", code),
         
         # Create new year variable from dose variable
         year = case_when(dose == "Flu 2022" ~ 2022,
                          dose == "Flu 2023" ~ 2023)) %>% 
  
  # Group by area code and year
  group_by(code, year) %>% 
  
  # Sum for vaccination programme totals
  summarise(numerator = sum(numerator),
            denominator = sum(denominator)) %>% 
  
  # Calculate rate and create new columns
  mutate(rate = numerator / denominator,
         trend_axis = paste0(year, "/", year+1),
         def_period = paste0(year, "/", year+1, " Winter vaccination programme"),
         ind_id = 99128,
         lowci = NA, upci = NA) %>% 
  
  # Select relevant variables
  select(code, ind_id, year, trend_axis, def_period, numerator, denominator, rate, lowci, upci)



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
  
  