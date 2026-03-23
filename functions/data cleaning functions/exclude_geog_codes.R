#exclude geog codes function

#This function reads in the file produced by the analysis function,
#removes intermediate zones, localities and/or other codes as specified, and then saves it again
#This function can be used when we receive a datazone-level extract but do not intend to publish 
#at such a granular level
#or when certain areas must be removed due to data quality issues

#filename - the same as used in the analysis functions
#iz - if set to true removes intermediate zones from datafile (no action if argument unspecified)
#locality - if set to true removes localities from datafile (no action if argument unspecified)
#test - if the indicators are in the test folder reads in from there, otherwise reads in from data to be checked
#codes - specific codes can be listed to be removed from the data e.g. due to data quality issues.

exclude_geog_codes <- function(filename, iz = FALSE, locality = FALSE, test = FALSE, codes = NULL){

#Read in the data
  if(test == TRUE){
  df <- readRDS(paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  }else{
    df <- readRDS(paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.rds"))
  }

codes_to_filter <- c() #set up empty vector

if(iz){codes_to_filter <- c(codes_to_filter, "S02")} #if iz == TRUE, add S02 to the list of patterns to filter
if(locality){codes_to_filter <- c(codes_to_filter, "S99")} #if locality == TRUE do the same with S99
#step above is iterative so if both iz and locality true, then both codes get added to the codes_to_filter vector

df <- df |> 
  filter(!str_detect(code, paste(codes_to_filter, collapse = "|"))) #filter out any rows where code is contained in codes_to_filter

if (!is.null(codes)){ #if the code argument contains any specific geographies, these are also filtered out
  df <- df |>
    filter(!code %in% codes)
}
  
  #Re-export the data
  if(test == FALSE){
    saveRDS(df, paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
    write.csv(df, paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.csv"))
  }else{
    saveRDS(df, paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.rds"))
    write.csv(df, paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.csv"))
  }
  
}
  


