#   Update ScotPHO indicator ID 13018: 
#   Employment rate for 16-24 year olds (percent)

 
# Data source is the Annual Population Survey (APS) within NOMIS 
# https://www.nomisweb.co.uk/
# To find data table behind the data update follow these selections in the NOMIS data download tool found at the url above

#  - Data downloads
# 	-Query		(https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)
# 	-Annual population service (APS)
# 	-Geography  (select countries : Scotland ,  Local authorities - District (within Scotland) then tick all within Scotland)
#   -Date (12 months to December) – pick time periods required – full series if refreshing all or single year to add one year to existing dataset)
# 	-Cell: Select ‘Change analysis’ option located near title “annual population survey” – then select table T01 “economic activity by age”
# 	-Select option to download data for those aged 16-19 amd 20-24 in the “All People” (so no need for sex breakdown) and “All” plus “In employment” options [This means 4 options should be ticked in total]
# 	-Summary of selections should allow you to review the choices (select one table per worksheet and include area codes)
# 	-Download Data – should present option to download data as an excel file

# Data down load contains raw population total and numbers of 16-19 and 19-24 employed for scotland and local authorities, these datasets need to be manipulated then summed to give
# totals for 16-24 years.

#   Part 1 - Read in NOMIS extract data
#   Part 2 - Run analysis functions 



### functions/packages ----

source("1.indicator_analysis.R") 
library(stringr)
library(rio)


### 1 - Read in NOMIS extract data ----

  #read in the data
  data <- import_list(paste0(data_folder,"Received Data/NOMIS Data Downloads/nomis_16_to_24_in_employment.xlsx"),  setclass = "tbl",  
                           rbind = TRUE, skip = 5, rbind_label = "excel tab") %>% 
 
  #rename columns
  rename_at(2, ~"ca") %>%
  rename_at(1, ~"ca name") %>%

  #add in Scotland
  mutate(ca = ifelse(`ca name` == 'Column Total', "S00000001", ca)) %>%
  
  #filter rows and columns
  filter_all(any_vars(grepl("S12|S00", .))) %>%  #remove unwanted rows 
  select(-contains("...")) %>% # remove unwanted cols
  
  #dealing with suppressed values
  mutate(across(everything(), ~replace(., . %in% c("!", "~", "*"), NA))) %>%
  mutate(across(-c("ca", "ca name"), as.numeric)) %>%
  
  #combine ages and group together
  mutate(variable = ifelse(`excel tab` %in% c(1,3), "denominator", "numerator")) %>%
  select(-`excel tab`, -`ca name`) %>%
  group_by(ca, variable) %>%
  summarise_all(sum) %>%
  ungroup() %>%

  #reshape the data
  pivot_longer(cols = -c("ca", "variable"), names_to = "year") %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  
  #replace names of year
  mutate(year = as.numeric(str_sub(year, start= -4)))
  

 #save files to be used in analysis functions
  saveRDS(data, file=paste0(data_folder, "Prepared Data/16-24_employment_raw.rds"))
  
  
  
# 2. Run analysis functions ----
  
  analyze_first(filename = "16-24_employment", measure = "percent",  geography = "council",
                 time_agg = 1, source_suppressed = TRUE, yearstart = 2004, yearend = 2022)
  

  analyze_second(filename = "16-24_employment", measure = "percent", 
                 time_agg = 1, ind_id = "13018",year_type = "calendar")
  
  

  