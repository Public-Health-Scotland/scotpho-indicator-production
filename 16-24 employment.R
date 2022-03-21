
### 1 - notes ----

# this script updates the following indicator: 13018 Employment rate for 16-24 year olds
# data source: https://www.nomisweb.co.uk/ 


### 2  - functions/packages ----

source("1.indicator_analysis.R") 
library(stringr)
library(rio)


### 3 - read in data and clean/format ----

  #read in the data
  data <- import_list(paste0(data_folder,"Received Data/nomis_16-24_employment.xlsx"),  setclass = "tbl",  
                           rbind = TRUE, skip = 5, rbind_label = "excel tab") %>% 
 
  #rename columns
  rename_at(2, ~"code") %>%
  rename_at(1, ~"ca name") %>%

  #add in Scotland
  mutate(code = ifelse(`ca name` == 'Column Total', "S00000001", code)) %>%
  
  #filter rows and columns
  filter_all(any_vars(grepl("S12|S00", .))) %>%  #remove unwanted rows 
  select(-contains("...")) %>% # remove unwanted cols
  
  #dealing with suppressed values
  mutate(across(everything(), ~replace(., . %in% c("!", "~", "*"), 0))) %>%
  mutate(across(-c("code", "ca name"), as.numeric)) %>%
  
  #combine ages and group together
  mutate(variable = ifelse(`excel tab` %in% c(1,3), "denominator", "numerator")) %>%
  select(-`excel tab`, -`ca name`) %>%
  group_by(code, variable) %>%
  summarise_all(sum) %>%
  ungroup() %>%

  #reshape the data
  pivot_longer(cols = -c("code", "variable"), names_to = "year") %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  
  #replace names of year
  mutate(year = str_sub(year, start= -4))
  
  
  # read in geography lookup to get health boards
  geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
    select(ca2019, hb2019) %>%
    distinct(.)
  
  #include health board figures
  final <- data %>%
   filter(code != "S00000001") %>% 
      left_join(geo_lookup, by = c("code" = "ca2019")) %>%
      select(-code) %>%
      rename("code" = "hb2019") %>%
      group_by(year, code) %>%
      summarise_all(sum) %>% ungroup() %>%
      rbind(data_list)
    
 #save files to be used in analyze_second() function
  saveRDS(final, file=paste0(data_folder, "Temporary/1308_16-24_employment_formatted.rds"))
  
  
  
# 4 - use analyze_second() function to create final shiny app file  
  
  analyze_second(filename = "1308_16-24_employment", measure = "percent", 
                 time_agg = 1, ind_id = "13018",year_type = "calendar")
  
  

  