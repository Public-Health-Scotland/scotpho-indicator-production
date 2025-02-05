check_year_parameters <- function(data, yearend, year_type){
  
  
  # check financial year/school year data has been adjusted accordingly
  if(year_type %in% c("financial", "school")){
    response <- utils::askYesNo(
    "Has the ‘year’ column of the prepared data been converted to the starting year 
    (e.g. data covering financial year 2023/24 should be 2023 in the year column) ?")
    if (isTRUE(response)) {
      cli::cli_alert_success("Continuing with the process.")
    } else {
      cli::cli_abort("Process aborted")
    }
  }
  
  
  # get max year in dataset
  max_year <- max(data$year)
  
  
  # check if user is about to remove years of data
  if(max_year > yearend){
    yearend_fin_year <- paste0(yearend, "/", substr(yearend + 1, 3, 4))
    max_fin_year <- paste0(max_year, "/", substr(max_year + 1, 3, 4))
    
    if(year_type == "financial"){
    cli::cli_alert_warning("Max year in the dataset is {max_year} but 'yearend' has been set to {yearend}. 
                           This will only create data up to {yearend_fin_year} despite you having data to calculate up to {max_fin_year}")
    } else {
      cli::cli_alert_warning("Max year in the dataset is {max_year} but 'yearend' has been set to {yearend}")
    }
  }
  
  # check if user is missing years of data
  if(yearend > max_year){
    if(year_type == "financial"){
      max_fin_year <- paste0(max_year, "/", substr(max_year + 1, 3, 4))
    cli::cli_alert_warning("'yearend' has been set to {yearend} but dataset only includes data up to {max_year}. Data can only be created up to {max_fin_year}")
    } else {
      cli::cli_alert_warning("'yearend' has been set to {yearend} but dataset only includes data up to {max_year}. Data can only be created up to {max_year}")
      
    }
  }
  
  if(yearend != max_year){
    response <- utils::askYesNo(paste0("Do you want to continue filtering data only up to ", yearend, "?"))
    if (isTRUE(response)) {
      cli::cli_alert_success("Continuing with the process.")
    } else {
      cli::cli_abort("Process aborted")
    }
    
  }
  

  
}

