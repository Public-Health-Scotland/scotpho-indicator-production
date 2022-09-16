################################################################################
################################################################################
#########                                                              #########
#####            Population within 500 metres of a derelict site           #####
#########                                                              #########
################################################################################
################################################################################


## This script prepares SG Population within 500 metres of a derelict site indicator.
## Data formost recent update requested from SG contact (see technical document).  


source("1.indicator_analysis.R") 


################################################################################
#####  Part 1)  format received data --------------------------------
################################################################################

derelict_site_data = read.csv(paste0(data_folder, "Received Data/Population within 500 metres of a derelict site.csv"))%>% 
  select(c("datazone"="DataZone_Code","year","numerator"="u500pop","denominator"="totpop")) %>%
  mutate_at(vars(numerator), ~replace(., is.na(.), 0))


saveRDS(derelict_site_data, file=paste0(data_folder, 'Prepared Data/derelict_site_raw.rds')) 


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

analyze_first(filename = "derelict_site", geography = "datazone11",
              measure = "percent", yearstart = 2016, yearend = 2021,
              time_agg = 1)

analyze_second(filename = "derelict_site", measure = "percent",
               time_agg = 1, ind_id = "20901", year_type = "calendar")

###############################################.
## Part 3 - Retrieve missing years ----
###############################################.
# only run if years are missing (historic data missing)
# extracting missing years from shiny data file already published in last 
# because 2007-2015 data unavailable from contact at datazone11 


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

merger("derelict_site",2015)# retrieve and create total data for indicator
run_qa("derelict_site") # run QA on new data file to ensure no missing years anymore

