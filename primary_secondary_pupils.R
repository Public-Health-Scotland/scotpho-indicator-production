###  notes
# ScotPHO indicators
# 13107: Primaryschool children
# 13108: Secondary school children
# dataset downloaded from:  https://www.gov.scot/publications/pupil-census-supplementary-statistics/

source("1.indicator_analysis.R") 

###############################################.
# function to extract the required data from excel file,
# check to see if new data file setup is consistent with structure of previous years
# filepath is the filepath in the data folder. file must be in the data folder
# sheet name is sheet in excel file that contains needed data
# heading row number is the row number that contains the years i.e to be used as column names
# the column name for data for most recent year is usually titled "Total",  most_recent_year argument takes care of renaming this
extr_pupil <- function(filepath, sheet_name, heading_row_num, most_recent_year) {
  library("openxlsx")
  library("stringr")
  result <- read.xlsx(paste0(data_folder, filepath), sheet = sheet_name, 
                      startRow = heading_row_num , skipEmptyCols = T) %>% 
    rename("council_area"="X1",{{ most_recent_year}}:="Total")%>% 
    select(-c("Female","Male")) %>% 
    mutate(council_area=str_replace(council_area,pattern = "&",replacement = "and")) %>% 
    filter(`council_area`!="Grant aided",`council_area`!="Scotland",`council_area`!="All local authorities") %>% 
    pivot_longer(!council_area,names_to = "year",values_to = "numerator")
  
  return(result)
}
################################################ extracting data from file
# just update the filepath (file should be in Scotpho Data folder), sheet name within the excel file and row number to use as column headers

sec_pupils = extr_pupil("Received Data/Pupils+Census+Supplementary+Statistics+2021+V3.xlsx","Table 7.2",3,"2021")
pri_pupils = extr_pupil("Received Data/Pupils+Census+Supplementary+Statistics+2021+V3.xlsx","Table 6.2",3,"2021")

###################### merging with council area lookup to retrieve council area codes
ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) 

sec_pupils =  sec_pupils %>%  left_join(ca, by = c("council_area" = "areaname"), all.x = TRUE) %>% 
  select(year,code,numerator) %>% rename("ca"="code") %>% 
  mutate(across(c("numerator", "year"), ~ as.numeric(.))) # formatting to the right data type so analyze functions work

pri_pupils = pri_pupils %>%  left_join(ca, by = c("council_area" = "areaname"), all.x = TRUE) %>% 
  select(year,code,numerator) %>% rename("ca"="code")%>% 
  mutate(across(c("numerator", "year"), ~ as.numeric(.))) # formatting to the right data type so analyze functions work


#################################################### saving to prepared data folder
saveRDS(sec_pupils, file=paste0(data_folder, 'Prepared Data/cyp_secondary_raw.rds')) 
saveRDS(pri_pupils, file=paste0(data_folder, 'Prepared Data/cyp_primary_raw.rds')) 


#####running analysis functions to first create gography aggregations and then create shiny files 
analyze_first(filename = "cyp_secondary", geography = "council", measure = "percent", yearstart = 2005,
              yearend = 2021, time_agg = 1, pop = "CA_pop_allages")

analyze_first(filename = "cyp_primary", geography = "council", measure = "percent", yearstart = 2006,
              yearend = 2021, time_agg = 1, pop = "CA_pop_allages")

analyze_second(filename = "cyp_secondary", measure = "percent", time_agg = 1,
               ind_id = 13108, year_type = "calendar") # check if QA analysis shows some years missing

analyze_second(filename = "cyp_primary", measure = "percent", time_agg = 1,
               ind_id = 13107, year_type = "calendar") # check if QA analysis shows some years missing


###########################################################################
# only run if years are missing (historic data missing)
# extracting missing years from shiny data file already published in last update because 2002-2006 data unavailable
# missing years are 2002-2005 for primary and 2002-2004 for secondary

# create function that retrieves missing years
# filename is name of file in the shiny data folder
# retrieve_year is the year up to which you want to retrieve data for
merger <- function(filename,retrieve_year){
  years_needed <- read_csv(paste0(data_folder, "Shiny Data/", filename, "_shiny.csv")) %>% subset(year<=retrieve_year)
  new_file <- readRDS(paste0(data_folder, "Data to be checked/", filename, "_shiny.rds"))
  new_complete_file <- rbind(years_needed,new_file) %>% arrange(code,year)
  
  saveRDS(new_complete_file, file = paste0(data_folder, "Data to be checked/", filename, "_shiny.rds"))
  write_csv(new_complete_file, path = paste0(data_folder, "Data to be checked/", filename, "_shiny.csv"))
}

merger("cyp_primary",2005)# retrieve and create total data for primary indicator
run_qa("cyp_primary") # run QA on new data file to ensure no missing years anymore

merger("cyp_secondary",2004) # retrieve and create total data for secondary indicator
run_qa("cyp_secondary") # run QA on new data file to ensure no missing years anymore



