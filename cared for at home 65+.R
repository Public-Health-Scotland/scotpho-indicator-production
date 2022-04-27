### notes ----

# indicator: 20502 - People aged 65+ with high levels of care needs who are cared for at home
# source: https://publichealthscotland.scot/publications/insights-in-social-care-statistics-for-scotland/ 


# required functions/packages
source("1.indicator_analysis.R") 
library(xlsx)
library(stringr)



###############################################.
## Part 1 - Prepare data ----
###############################################.

### read in data file saved in "Data received" folder -----

dat <- read_excel(paste0(data_folder, "Received Data/2022-04-26-balance-of-care.xlsx"), # change filename
                  sheet = "T2 Data")


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
  left_join(ca_lookup, by = "areaname", all.x = TRUE) %>%
  
  #select final columns
  select("ca" = "code", year, "numerator" = "home care", denominator) %>%
  mutate(across(-"ca", as.numeric)) %>%
  mutate(year = year - 1) # change year to beginning of FY 
  

saveRDS(dat_cleansed, paste0(data_folder, "Prepared Data/high_care_needs_raw.rds"))
  
  
###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "high_care_needs", geography = "council", measure = "percent", 
              yearstart = 2009,  yearend = 2021, time_agg = 1)


analyze_second(filename = "high_care_needs", measure = "percent", time_agg = 1, 
               ind_id = 20502, year_type = "financial")



