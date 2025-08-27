#CA Names to Codes Function

#This function takes a dataframe with a column containing the names of council areas 
#and converts these into S-codes

#It also fixes common variants of council names to match what is expected by the lookup

#df - the indicator dataframe that is being prepared
#council_area - the column of the dataframe containing the names of the council areas. 

ca_names_to_codes <- function(df, council_area){
  
  #Converting common versions of council names to match lookup
  #This list could be amended if new variants are identified
  df <- df |> 
    mutate({{council_area}} := str_replace({{council_area}}, "&", "and"),
           areaname = case_when({{council_area}} == "Eileanan Siar" ~ "Na h-Eileanan Siar", 
                                {{council_area}} == "Eilean Siar" ~ "Na h-Eileanan Siar",
                                {{council_area}} == "Western Isles" ~ "Na h-Eileanan Siar", 
                                {{council_area}} == "Comhairle nan Eilean Siar" ~ "Na h-Eileanan Siar",
                                {{council_area}} == "Edinburgh City" ~ "City of Edinburgh", 
                                {{council_area}} == "Edinburgh, City of" ~ "City of Edinburgh",
                                {{council_area}} == "Orkney" ~ "Orkney Islands", 
                                {{council_area}} == "Shetland" ~ "Shetland Islands",
                                grepl(pattern = "Glasgow", {{council_area}}, ignore.case = TRUE) ~ "Glasgow City",
                                grepl(pattern = "Dundee", {{council_area}}, ignore.case = TRUE) ~ "Dundee City",
                                TRUE ~ {{council_area}})) 
  
  #Read in the CA dictionary that matches names to S-codes
  ca_dict <- readRDS(file.path("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds")) 

  df2 <- left_join(df, ca_dict, by = "areaname") |> #join df passed into function to lookup
    select(-areaname, -{{council_area}}) |> #drop ca names leaving only S-codes
    select(code, everything()) #reordering so code is the first col
  
  return(df2)
}

#End


