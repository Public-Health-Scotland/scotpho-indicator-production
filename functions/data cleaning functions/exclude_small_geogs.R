#exclude geog codes function

#This function reads in the file produced by the analysis function,
#removes intermediate zones, localities and/or other codes as specified, and then saves it again
#This function can be used when we receive a datazone-level extract but do not intend to publish 
#at such a granular level or when certain areas must be removed due to data quality issues

#filename - the same as used in the analysis functions
#iz - if set to true removes intermediate zones from datafile (no action if argument unspecified)
#locality - if set to true removes localities from datafile (no action if argument unspecified)
#test - if the indicators are in the test folder reads in from there, otherwise reads in from data to be checked
#codes - specific codes can be listed to be removed from the data 
#iz_years, locality_years and codes_years - these three arguments allow specific combinations of year and geog to be filtered out e.g izs before 2014. By default all years filtered out

exclude_geog_codes <- function(filename, iz = FALSE, locality = FALSE, 
                               test = FALSE, codes = NULL, iz_years = NULL, 
                               locality_years = NULL, codes_years = NULL){

library(dplyr)
library(stringr)
profiles_data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
  
#Read in the data
  if(test == TRUE){
    df <- readRDS(paste0(profiles_data_folder, "Test Shiny Data/", filename, "_shiny.rds"))
  }else{
    df <- readRDS(paste0(profiles_data_folder, "Data to be checked/", filename, "_shiny.rds"))
  }
  
#Aggregate info on which rows to filter out
rows_to_filter <- list() #set up empty list

if(iz){ #if iz==TRUE, add S02 to a list of codes to filter out. If iz_years is non-null, specify which years S02 should be filtered out for
  rows_to_filter <- append(rows_to_filter, list(list(pattern = "^S02", years = iz_years))) 
  }

if(locality){ #same for locality
  rows_to_filter <- append(rows_to_filter, list(list(pattern = "^S99", years = locality_years)))
}

if(!is.null(codes)){ #appending any combinations of specific geog codes and years to the list of codes to filter. regex needed because str_detect used below which doesn't expect a vector
  regex_codes <- paste0("^", codes, "$", collapse = "|")
  rows_to_filter <- append(rows_to_filter, list(list(pattern = regex_codes, years = codes_years)))
}

#Apply the filtering rules
for(i in rows_to_filter){
  pattern <- i$pattern
  years <- i$years
  
  if(is.null(years)){
    df <- df |> filter(!str_detect(code, pattern)) #if no specific years mentioned exclude codes for all years
  }else{ #if specific years, then only filter those
    df <- df |> filter(!(stringr::str_detect(code, pattern) & year %in% years))
  }
  } #close for loop

  #Re-export the data
  if(test == FALSE){
    saveRDS(df, paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
    write.csv(df, paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.csv"))
  }else{
    saveRDS(df, paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.rds"))
    write.csv(df, paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.csv"))
  }

df #save to environment
  
}
  
#Example usage

# df <- data.frame(code = c("S12000023", "S99000004", "S299000067", "S02000045", "S02000087"),
#                  year = c(2020, 2021, 2022, 2021, 2022)) #create df
# 
# saveRDS(df, file.path(profiles_data_folder, "Test Shiny Data/exclude_geogs_dummy_data_shiny.rds")) #save to test data folder
# 
# #df file above needs to be re-saved between tests unfortunately, as part of the code outputs back to the data folders.
# df_exclude_iz_all_years <- exclude_geog_codes(filename = "exclude_geogs_dummy_data", iz = TRUE, test = TRUE)
# df_exclude_iz_2021 <- exclude_geog_codes(filename = "exclude_geogs_dummy_data", iz = TRUE, iz_years = 2021, test = TRUE)
# df_exclude_code <- exclude_geog_codes(filename = "exclude_geogs_dummy_data", codes = "S12000023", test = TRUE)
# df_exclude_locality_iz_local <- exclude_geog_codes(filename = "exclude_geogs_dummy_data", iz = TRUE, locality = TRUE, iz_years = 2022, locality_years = 2021, test = TRUE)
# 
# file.remove(file.path(profiles_data_folder, "Test Shiny Data/exclude_geogs_dummy_data_shiny.rds"))
