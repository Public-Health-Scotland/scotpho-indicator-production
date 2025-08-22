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

# # bringing lookup to match with council
# ca_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
#   select(datazone2011, ca2019) %>% distinct()
# 
# postpartum <- left_join(postpartum, ca_lookup, by = "datazone2011") %>% 
#   rename(ca = ca2019 ) %>% group_by(ca, year) %>% 
#   summarise(numerator = sum(smoker), denominator = sum(total_valid_status)) %>% 
#   ungroup() %>% 
#   # Selecting out a few cases from early years in Highland CA before the system was 
#   # properly in place that would cause confusion
#   filter(!(ca == "S12000017" & year<2007))


postpartum <- postpartum |> 
  rename(code = datazone2011, 
         numerator = smoker,
         denominator = total_valid_status) |> 
  select(-fin_year) |> 
  mutate(year = as.numeric(year))

saveRDS(postpartum, file.path(profiles_data_folder, '/Prepared Data/postpartum_smoking_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "postpartum_smoking", geography = "datazone11", measure = "percent",
              yearstart = 2002, yearend = 2023, time_agg = 3, ind_id = 1552, year_type = "financial")

deprivation_analysis(filename = "postpartum_smoking", yearstart = 2002, yearend = 2023,
                       time_agg = 3, year_type = "financial", measure = "percent", pop_sex = "all",
                       ind_id = 1552)

##END
