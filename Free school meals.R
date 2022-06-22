### 1. notes ------

# indictor: 13012 - Children registered for free school meals
# source: https://www.gov.scot/publications/school-healthy-living-survey-statistics-2020/ 
# change filename and update year below: 

data_filename <- "healthy-living-survey-supplementary_statistics_2020.xlsx" # name of the excel file saved in "data received" folder
data_update_year <- 2020 # year of data that has been published



### 2. read in data ------

source("1.indicator_analysis.R") 

# primary 1-4 pupils
primary <- read_xlsx(paste0(data_folder, "Received Data/", data_filename), sheet = "Table 7", skip = 6) %>%
  head(32) %>%
  select("areaname" = 1, "denominator" = 6, "numerator" = 7)

# secondary pupils
secondary <- read_xlsx(paste0(data_folder, "Received Data/", data_filename), sheet = "Table 13", skip = 5) %>%
  head(32) %>%
  select("areaname" = 1, "denominator" = 2, "numerator" = 3)

# council area lookup
ca_lookup <- readRDS(paste0(lookups,"Geography/CAdictionary.rds"))



### 4. clean/format data  ------

# combine primary and secondary school data
all <- rbind(primary, secondary)

all <- all %>%
  mutate(areaname = gsub("^[1-9]|[1-9]$", "", areaname)) %>% # remove any reference to notes in ca names
  left_join(ca_lookup, by = "areaname", all.x = TRUE) %>%  # include council area codes
  select("ca" = "code", "numerator", "denominator") %>%
  group_by(ca) %>%
  summarise_all(sum) %>% ungroup() %>% # summing primary and secondary figures
  mutate(year = data_update_year) # add year column


write_rds(all, paste0(data_folder, "Prepared Data/free_school_meals_raw.rds"))



### 5. analyse data ------

analyze_first(filename = "free_school_meals", geography = "council", 
              measure = "percent", yearstart = 2015, yearend = 2020, 
              time_agg = 1)


analyze_second(filename = "free_school_meals", measure = "percent", 
               time_agg = 1, ind_id = "13012",year_type = "calendar")



### 5. combine new data with previous update to get all years -----

previous_update <- readRDS(paste0(data_folder, "Shiny Data/free_school_meals_shiny.rds"))

current_update <- readRDS(paste0(data_folder, "Data to be checked/free_school_meals_shiny.rds")) %>%
  rbind(previous_update)


write_rds(current_update, paste0(data_folder, "Data to be checked/free_school_meals_shiny.rds"))
write_csv(current_update, paste0(data_folder, "Data to be checked/free_school_meals_shiny.csv"))



### 6. QA the data -----

run_qa <- function(filename, old_filename="default", check_extras=c()){
  run("Data Quality Checks.Rmd")
}  

run_qa(filename = "free_school_meals_shiny", old_filename="default", check_extras=c())





