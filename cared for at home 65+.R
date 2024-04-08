### notes ----

# indicator: 20502 - People aged 65+ with high levels of care needs who are cared for at home
# source: https://publichealthscotland.scot/publications/people-supported-through-social-care-services/people-supported-through-social-care-services-support-provided-or-funded-by-health-and-social-care-partnerships-in-scotland-202223/


# required functions/packages
source("1.indicator_analysis.R")

###############################################.
## Part 1 - Prepare data ----
###############################################.

### read in data file saved in "Data received" folder -----

dat <- read_excel(paste0(data_folder, "Received Data/High Care Needs/2024-balance-of-care.xlsm"), 
                  sheet = 'T2 Data', range='A1:S133') 


ca_lookup <- readRDS(paste0(lookups, "Geography/CAdictionary.rds")) # council area lookup 



### prepare data to be used in analysis functions -----


dat_cleansed <- dat %>%
  
  #rename columns with no name
  rename_at(2, ~ "care_type") %>%
  rename_at(4, ~ "areaname") %>%
  
  #selecting required columns
  select(contains(c("care_type", "areaname", "20"))) %>%
  
  # remove rates and scotland totals for now - will re-calculate later
  filter(care_type != "Percentage" & areaname != "Scotland") %>% 
  
  # reshape data from wide to long to create year column
  pivot_longer(cols = -c("care_type", "areaname"), names_to = "year") %>%
  
  # split care_type column into 3 seperate columns (to calculate numerator and denominator)
  pivot_wider(names_from = "care_type", values_from = "value") %>%
  setNames(tolower(names(.))) %>%
  
  # calculate denominator
  mutate(denominator = `care homes` + `home care` + `cc census`) %>%
  
  # include council area codes
  mutate(areaname = str_replace(areaname, "&","and"),
         areaname = str_replace(areaname, "Edinburgh, City of","City of Edinburgh"),
         areaname = str_replace(areaname, "Eilean Siar","Na h-Eileanan Siar")) %>% 
  left_join(ca_lookup, by = "areaname") %>%
  
  #select final columns
  select("ca" = "code", year, "numerator" = "home care", denominator) %>%
  mutate(across(-"ca", as.numeric)) %>%
  mutate(year = year - 1) # change year to beginning of FY 
  

saveRDS(dat_cleansed, paste0(data_folder, "Prepared Data/high_care_needs_raw.rds"))
  
  
###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "high_care_needs", geography = "council", measure = "percent", 
              yearstart = 2009,  yearend = 2022, time_agg = 1)


analyze_second(filename = "high_care_needs", measure = "percent", time_agg = 1, 
               ind_id = 20502, year_type = "financial")


### add in 2006/7 - 2008/9 data currently in tool (not published)
old_update <- read.csv(paste0(data_folder, "Shiny Data/high_care_needs_shiny.csv")) %>%
  filter(year >= 2006 & year <= 2008)

final_result <- final_result %>%
  select(-denominator) %>%
  rbind(old_update)

# save files again    
saveRDS(final_result, paste0(data_folder, "Data to be checked/high_care_needs_shiny.rds"))  
write_csv(final_result, paste0(data_folder, "Data to be checked/high_care_needs_shiny.csv"))   

