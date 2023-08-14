#   Update ScotPHO indicator ID 13018: 
#   Employment rate for 16-24 year olds (percent)

 
# Data source is the Annual Population Survey (APS) within NOMIS 
# https://www.nomisweb.co.uk/
# To find data table behind the data update follow these selections in the NOMIS data download tool found at the url above

#  - Data downloads
# 	-Query		(https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)
# 	-Annual population service (APS)
# 	-Geography  (select countries : Scotland ,  Local authorities (within Scotland) then tick all within Scotland)
#   -Date (12 months to December) – pick time periods required – full series if refreshing all or single year to add one year to existing dataset)
# 	-Cell: Select ‘Change analysis’ option located near title “annual population survey” – then select table T01 “economic activity by age”
# 	-Select option to download data for those aged 16-19 amd 20-24 in the “All People” (so no need for sex breakdown) and “All” plus “In employment” options [This means 4 options should be ticked in total]
# 	-Summary of selections should allow you to review the choices
# 	-Download Data – should present option to download data as an excel file

# Data down load contains raw population total and numbers of 16-19 and 19-24 employed for scotland and local authorities, these datasets need to be manipulated then summed to give
# totals for 16-24 years.

#   Part 1 - Read in NOMIS extract data
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions 



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
  
  

  