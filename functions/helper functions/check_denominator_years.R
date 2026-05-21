# Check that looks at whether the population denominator file contains all they years that are required given the defined yearend/yearstart period.
# It might be obvious when denominators are missing for single year indicators but when rolling averages are calculated it's possible to miss that one or more years of denominator
# aren't in the population lookup.


check_denominator_years <- function(data, yearend, yearstart){

#filter dataset and check if denominators appear to be present for all the years needed
years_check <- data |>
  select (year, code, denominator) |>
  filter(!is.na(denominator))|>
  summarise(max_year_denominator = max(year),
            min_year_denominator = min(year))

# get max year in dataset
denominator_max_year <- max(years_check$max_year_denominator)
denominator_min_year <- min(years_check$min_year_denominator)

# the population denominator max year should always be greater than or equal to yearend
if(yearend > denominator_max_year){
  response <- utils::askYesNo(paste0("The maximum year in population denominator file appears to be ",denominator_max_year,", but the yearend parameter is ", yearend, ".Are you sure you want to continue
  since multiple year rolling averages will be incorrectly calculated?"))
  if (isTRUE(response)) {
    cli::cli_alert_success("Continuing with the process.")
  } else {
    cli::cli_abort("Process aborted")
  }
  
}
# the population denominator min year should always be equal to or less than yearstart
if(yearstart < denominator_min_year){
  response <- utils::askYesNo(paste0("The minimum year in the population denominator file appears to be",denominator_min_year," but the yearstart parameter is ", yearstart, "Are you sure you want to continue
  since multiple year rolling averages will be incorrectly calculated?"))
  if (isTRUE(response)) {
    cli::cli_alert_success("Continuing with the process.")
  } else {
    cli::cli_abort("Process aborted")
  }
  
}


}