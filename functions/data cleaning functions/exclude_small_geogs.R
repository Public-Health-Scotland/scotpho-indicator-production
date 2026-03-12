#exclude small geogs function

#This function reads in the file produced by the analysis function,
#removes intermediate zones and/or localities as specified, and then saves it again
#This function can be used when we receive a datazone-level extract but do not intend to publish 
#at such a granular level

#filename - the same as used in the analysis functions
#iz - set to true by default, which removes intermediate zones
#locality - set to true b y default, which removes localities

exclude_small_geogs <- function(filename, iz = TRUE, locality = TRUE, test = FALSE){

#Read in the data
  if(test == FALSE){
  df <- readRDS(paste0(profiles_data_folder, "/Data to be checked/", filename, "_shiny.rds"))
  }else{
    df <- readRDS(paste0(profiles_data_folder, "/Test Shiny Data/", filename, "_shiny.rds"))
  }

#option 1 - remove IZ and localities
if(iz && locality){
  df <- df |> filter(!str_detect(code, "S02|S99"))
#option 2 - remove IZ only
}else if(iz){
  df <- df |> filter(!str_detect(code, "S02"))
#option 3 - remove locality only
}else if(locality){
  df <- df |> filter(!str_detect(code, "S99"))
#option 4 - remove no geography types
}else{
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
  


