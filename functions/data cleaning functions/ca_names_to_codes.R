#Could the function read in the lookup and convert names to S-codes?

library(dplyr)
library(stringr)

ca_dict <- readRDS(file.path("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds"))

test_df <- ca_dict |> 
  select(1) |>  #keep only LA names
  mutate(areaname = str_replace(areaname, " and ", " & "),
         areaname = case_when(areaname == "City of Edinburgh" ~ "Edinburgh City", #another scneario of Edinburgh, City of needed
                              areaname == "Na h-Eileanan Siar" ~ "Eileanan Siar",
                              areaname == "Orkney Islands" ~ "Orkney", 
                              areaname == "Shetland Islands" ~ "Shetland",
                              TRUE ~ areaname)) 


ca_names_to_codes <- function(df, council_area){
  
  df <- df |> 
    mutate(areaname = str_replace(areaname, "&", "and"),
           areaname = case_when(areaname == "Eileanan Siar" ~ "Na h-Eileanan Siar", 
                                areaname == "Western Isles" ~ "Na h-Eileanan Siar", 
                                areaname == "Comhairle nan Eilean Siar" ~"Na h-Eileanan Siar",
                                areaname == "Edinburgh City" ~ "City of Edinburgh", 
                                areaname == "Edinburgh, City of" ~ "City of Edinburgh",
                                areaname == "Orkney" ~ "Orkney Islands", 
                                areaname == "Shetland" ~ "Shetland Islands",
                                TRUE ~ areaname)) 
  
  ca_dict <- readRDS(file.path("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds"))
  
  df2 <- left_join(df)
  
  
  
}

#Check my parsing code from neighbourhood perceptions(?) to try and add a failsafe that 
#rename the la col to areaname if it's not already like that. 




