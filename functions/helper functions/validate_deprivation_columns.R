validate_deprivation_columns <- function(data, measure){

  # search for the column containing datazones and return the name of the column
  existing_geo_colname <- names(data)[purrr::map_lgl(data, ~ any(grepl("S01", .)))]
  
  
  # return an error if no datazone code column found
  if(length(existing_geo_colname) == 0){
    cli::cli_abort(
      c(
        "x" = "Datazone column not found.",
        "i" = "Dataset must contain a column containing datazone codes starting with 'S01'."
      )
    )
  }

  
  # rename geography col if required - needs to be named 'datazone' to match with simd datazone lookup
  if(existing_geo_colname != "datazone"){
    data <- data |>
      dplyr::rename("datazone" := !!sym(existing_geo_colname))
    cli::cli_alert_info("Renamed geography column from '{existing_geo_colname}' to 'datazone' for joining with SIMD datazone lookup in next step")
  }
  

  
  # get names of all required columns, depending on measure type
  required_cols <- switch(measure,
                          "percent" = c("datazone", "year", "numerator", "denominator"),
                          "stdrate" = c("datazone", "year", "numerator", "sex_grp", "age_grp"),
                          "crude" = c("datazone", "year", "numerator")
                          )
  
  
  # check for missing columns
  missing_cols <- setdiff(required_cols, names(data)) # name of missing cols
  missing_cols_length <- length(missing_cols) # number of missing cols
  
  
  # return error message if there are missing columns
  if(missing_cols_length > 0) {
    cli::cli_abort(
      c("The following {missing_cols_length} column{?s} {?is/are} missing:",
        "x" = ".var {missing_cols}",
        "i" = "Amend the prepared data file as required and save it again."
      )
    )
  }
  
  
  # check numerator col can be converted to class numeric (i.e. don't contain symbols etc.)
  if(!any(varhandle::check.numeric(data$numerator))){
    cli::cli_abort(
      c(
        "x" = "numerator column cannot be converted to class numeric",
        "i" = "Ensure the column does not contain any special characters."
      )
    )
  }
  
  # check cdenominator can be converted to class numeric (i.e. don't contain symbols etc.)
  if(measure == "percent"){
    if(!any(varhandle::check.numeric(data$denominator))){
      cli::cli_abort(
        c(
          "x" = "denominator column cannot be converted to class numeric",
          "i" = "Ensure the column does not contain any special characters."
        )
      )
    }
  }
  
  # check year col can be converted to class numeric (i.e. don't contain symbols etc.)
  if(!any(varhandle::check.numeric(data$year))){
    cli::cli_abort(
      c(
        "x" = "year column cannot be converted to class numeric",
        "i" = "Ensure the column does not contain any special characters."
      )
    )
    
  }
  
  
  # change column classes if required
  data <- data |>
    mutate(
      across(any_of(c("datazone", "sex_grp", "age_grp")), as.character),
      across(any_of(c("numerator", "denominator", "year")), as.numeric)
    )
  
  
  # select all required columns
  data <- data |>
    select(all_of(required_cols))
  
}