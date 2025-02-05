create_def_period_column <- function(data, year_type = c("calendar", "financial", "school", "survey", "snapshot"), agg){
  
  # check function arguments
  year_type <- rlang::arg_match(year_type)
  
  
  if (year_type %in% c("calendar", "survey")){
    
    data |>
      mutate(
        def_period = case_when(agg == 1 ~  as.character(year), 
                               TRUE ~ paste0(year - (agg - 1), " - ", year, "; ", agg, " year rolling average"))
        )
    
  } else if(year_type %in% c("financial", "school")){
    
    data |>
      mutate(
        def_period = case_when(
          agg == 1 ~ paste0(year, "/", substr(year + 1, 3, 4)),
          TRUE ~ paste0(year - (agg - 1), "/", substr(year - (agg - 2), 3, 4), 
                        " to ", year,"/",substr((year + 1), 3, 4), "; ", agg, " year rolling average"))
      )
    
  } else if(year_type == "snapshot"){
    data |> 
      mutate(
      def_period = case_when(agg == 1 ~  paste0(year, " - snapshot"), 
                             TRUE ~ paste0(year - (agg - 1), " - ", year, " - snapshot")))
    
  }

}