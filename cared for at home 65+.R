### notes ----

# indicator: 20502 - People aged 65+ with high levels of care needs who are cared for at home
# source: https://publichealthscotland.scot/publications/show-all-releases?id=168140
# data downloaded from dashboard. Navigate to "people supported" and "balance of care - trend by age group"
# although there is a 65+ tab it contains only percentages so can't be used to calculate CIs


# required functions/packages
source("./functions/main_analysis.R")

###############################################.
## Part 1 - Prepare data ----
###############################################.

### read in data file saved in "Data received" folder -----
dat <- read.csv(paste0(profiles_data_folder, "/Received Data/High Care Needs/highcare65.csv")) |> 
  janitor::clean_names()

### read in council lookup -----
ca_lookup <- readRDS(paste0(profiles_data_folder, "/Lookups/Geography/CAdictionary.rds")) # council area lookup 


### prepare data to be used in analysis functions -----
dat_cleansed <- dat |> 
  
  #filter to only over 65s then drop age col
  filter(age_group == "65+") |> 
  select(-c(age_group)) |> 

  #exclude Scotland figures - to be readded later in analysis function
  filter(location != "Scotland (Estimated)") |> 
  
  #pivot wider to sum care types to get denominator, then select only care at home
  tidyr::pivot_wider(id_cols = c("financial_year", "location"), names_from = category, values_from = number_of_people) 


dat_cleansed <- dat_cleansed |> 
  mutate(denominator = rowSums(select(dat_cleansed, c(3:5)))) |> 
  select(-c(4:5)) |> 

  #tidy up col names
  rename(year = financial_year, 
         numerator = `Care at Home 10+ Hours`,
         areaname = location) |> 
  
  #remove second year from financial year 
  mutate(year = substr(year, 1, 4),
         year = as.numeric(year)) |> 

  #tweak council names to match lookup then join
  mutate(areaname = str_replace(areaname, "Comhairle nan Eilean Siar","Na h-Eileanan Siar")) |> 
  left_join(ca_lookup, by = "areaname") |>
  
  #select final cols
  select(1,5,3:4)
  

#save final file
saveRDS(dat_cleansed, paste0(profiles_data_folder, "/Prepared Data/high_care_needs_raw.rds"))
  
  
###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "high_care_needs", geography = "council", measure = "percent",
              year_type = "financial", ind_id = 20502, time_agg = 1, yearstart = 2008, 
              yearend = 2023)

# ### add in 2006/7 - 2008/9 data currently in tool (not published)
# old_update <- read.csv(paste0(data_folder, "Shiny Data/high_care_needs_shiny.csv")) %>%
#   filter(year >= 2006 & year <= 2008)
# 
# final_result <- final_result %>%
#   select(-denominator) %>%
#   rbind(old_update)
# 
# # save files again    
# saveRDS(final_result, paste0(data_folder, "Data to be checked/high_care_needs_shiny.rds"))  
# write_csv(final_result, paste0(data_folder, "Data to be checked/high_care_needs_shiny.csv"))   

