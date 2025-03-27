## ScotPHO Profiles Tool - Domestic Abuse (20804)

# Description: Rate of incidents of domestic abuse recorded by the police per 10,000 population
# Data is downloaded from SG website in Table 5:
#https://www.gov.scot/publications/domestic-abuse-statistics-recorded-police-scotland-2022-23/documents/

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

#sourcing analysis function
source("./functions/main_analysis.R")

#council area lookup
ca_lookup <- readRDS(paste0(profiles_data_folder, "/Lookups/Geography/CAdictionary.rds"))

###############################################.
## Read in data  ----
###############################################.

data <- readxl::read_excel(path = paste0(profiles_data_folder, "/Received Data/Domestic Abuse/domestic_abuse.xlsx"),
                         sheet = "Table 5") |> 

###############################################.
## Tidy up data  ----
###############################################.

  janitor::clean_names() |> #variables to lower case
  janitor::row_to_names(row_number = 2) |> #cut off metadata at the top and set top remaining row to col headings
  slice(1:(n() - 2)) |>  #cut off last two empty rows
  
  #pivot longer
  tidyr::pivot_longer(cols = c(2:11), names_to = "year", values_to = "rate") |> #create 1 year column instead of 1 per year
  mutate(rate = as.numeric(rate), #convert rate from character to numeric
         rate = round(rate, digits = 1)) |> #round to 1dp

  #amend council names to match lookup
  rename(areaname = `Local authority`) |> #rename LA to areaname
  mutate(areaname = stringr::str_replace(areaname,"Edinburgh City", "City of Edinburgh"), #replace Edi City w/ City of E
         areaname = stringr::str_replace(areaname, "&", "and"))  #replace & with and
  
#join data to lookup 
data_cleaned <-  left_join(data, ca_lookup, by = "areaname") |> 
  mutate(code = case_when(areaname == "Scotland" ~ "S00000001", #add in Scotland code
                          TRUE ~ code)) |> 
  mutate(year = substr(year, 1, 4), #truncate fin year to first calendar year
         year = as.numeric(year)) |> #convert year to numeric
  select(4, 2:3) #select final columns
  
###############################################.
## Add metadata ----
###############################################.

data_cleaned <- create_def_period_column(data = data_cleaned, year_type = "financial", agg = 1)  #helper functions in main_analysis.R
data_cleaned <- create_trend_axis_column(data_cleaned, year_type = "financial", agg = 1) #helper functions in main_analysis.R
data_final <- data_cleaned |> 
  mutate(ind_id = "20804",
         numerator = NA,
         upci = NA, 
         lowci = NA)

###############################################.
## Save final files ----
###############################################.
write.csv(data_final, file = paste0(profiles_data_folder, "/Data to be checked/domestic_abuse_shiny.csv"))
saveRDS(data_final, file = paste0(profiles_data_folder, "/Data to be checked/domestic_abuse_shiny.rds"))

###############################################.
## Run QA ----
###############################################.
run_qa(filename = "domestic_abuse", type = "main", test_file = FALSE)


