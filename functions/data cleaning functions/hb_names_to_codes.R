#HB Names to Codes Function

#This function takes a dataframe with a column containing the names of health boards
#and converts these into S-codes

#It also fixes common variants of health boards to match what is expected by the lookup

#df - the indicator dataframe that is being prepared
#health_board- the column of the dataframe containing the names of the health boards. 

hb_names_to_codes <- function(df, health_board){
  
  #Converting common versions of council names to match lookup
  #This list could be amended if new variants are identified
  df <- df |> 
    mutate(temp_health_board = str_replace({{health_board}}, "&", "and"), #replace & symbol with "and"
           areaname := case_when(
             !str_starts(temp_health_board, "NHS ") ~ paste0("NHS ", temp_health_board), #if the health board doesn't start with "NHS " prepend it
             TRUE ~ temp_health_board
           )) |> 
    select(-temp_health_board)  
  
  
  #Read in the HB dictionary that matches names to S-codes
  hb_dict <- readRDS(file.path("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/HBdictionary.rds")) 
  
  df2 <- left_join(df, hb_dict) |> #join df passed into function to lookup
    select(-areaname, -{{health_board}}) #drop ca names leaving only S-codes
  
}

#End