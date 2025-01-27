validate_columns <- function(data, measure, geography){
  
  # ensure data isn't grouped as this will prevent column classes being  successfully changed
  if(is.grouped_df(data)){
    cli::cli_abort(
      c(
        "x" = "Data is still grouped",
        "i" = "Did you forget to ungroup() your dataset before saving data to pass to this function?"
      )
    )
  }
  
  
  # define expected geography code pattern depending on the geography level
  code_pattern <- switch(geography,
                         "scotland" = "S00",
                         "datazone11" = "S01|NA",
                         "intzone11" = "S02|NA",
                         "council" = "S12|NA",
                         "board" = "S08|NA",
                         "multiple" = "S01|S02|S12|S08|S00"
  )
  
  
  # search that geo code pattern across the dataset and return the name of the column
  # where the codes were found 
  existing_geo_colname <- names(data)[purrr::map_lgl(data, ~ any(grepl(code_pattern, .)))]
  
  
  # return an error if no geography code column found
  if(length(existing_geo_colname) == 0){
    cli::cli_abort(
      c(
        "x" = "Geography column not found.",
        "i" = "Dataset should contain a column containing {geography} codes starting with {code_pattern}"
      )
    )
  }
  
  
  # determine what correct name of the column should be
  required_geo_colname <- switch(geography,
                                 "datazone11" = "datazone2011",
                                 "council" = "ca2019",
                                 "board" =  "hb2019",
                                 "intzone11" =  "intzone2011",
                                 "scotland" = "scotland",
                                 "multiple" = "code"
  )
  
  
  # rename geography col if required
  if(existing_geo_colname != required_geo_colname){
    data <- data |>
      dplyr::rename(!!required_geo_colname := !!sym(existing_geo_colname))
    cli::cli_alert_info("Renamed geography column from {existing_geo_colname} to {required_geo_colname} for joining with geography lookup in next step")
  }
  
  
  
  
  # check what geography levels are in the dataset and return an error
  # if multiple levels were found but user has not set geography argument to 'multiple'
  geo_codes <- paste0(unique(stringr::str_sub(data[[required_geo_colname]], 1, 3)))

  if(length(geo_codes) > 1 & geography != "multiple" & (!"NA" %in% geo_codes)){
    cli::cli_abort(
      c(
        "x" = "{required_geo_colname} column contains more than 1 geography level",
        "i" = "Codes starting with: {geo_codes} found. 'geography' argument should be set to 'multiple'"
      )
    )
  }
  
  
  # get names of all required columns, depending on measure type
  required_cols <- switch(measure,
                          "percent" = c(required_geo_colname, "year", "numerator", "denominator"),
                          "stdrate" = c(required_geo_colname, "year", "numerator", "sex_grp", "age_grp"),
                          "crude" = c(required_geo_colname, "year", "numerator")
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
      across(any_of(c({{required_geo_colname}}, "sex_grp", "age_grp")), as.character),
      across(any_of(c("numerator", "denominator", "year")), as.numeric)
    )
  
  
  # select all required columns
  data <- data |>
    select(all_of(required_cols))

  

}
