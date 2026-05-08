validate_popgrps_columns <- function(data, measure, geography, splits){
  
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
  
  
  # remove any whitespace from columns 
  data <- data |>
    mutate(across(everything(), ~ trimws(.)))
  
  
  # check what geography levels are in the dataset and return an error
  # if multiple levels were found but user has not set geography argument to 'multiple'
  geo_codes <- paste0(unique(stringr::str_sub(data[[required_geo_colname]], 1, 3)))
  geo_codes <- setdiff(geo_codes, c("NA", NA, NULL, "", "Unk"))
  
  if(length(geo_codes) > 1 & geography != "multiple"){
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
                          "crude" = c(required_geo_colname, "year", "numerator"),
                          "perc_pcf" = c(required_geo_colname, "year", "numerator","denominator")
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
  
  # Validate split details 
  missing_cols_splits <- setdiff(names(splits), names(data)) #identifies columns specified in split argument that aren't available in the data as col headers
  missing_cols_splits_length <- length(missing_cols_splits)   #counts how many splits are missing
  
  # Return error message if splits are missing or named incorrectly in the data
  if(missing_cols_splits_length > 0 ){
    cli::cli_abort(
      c("The following {missing_cols_splits_length} column{?s} {?is/are} specified in the function arguments
        but are not present in the data as column headings:",
        "x" = ".var {missing_cols_splits}",
        "i" = "Amend the data file or function arguments as required and save again")
    )
  }
  
  # Check that there are no values of the splits in the data that are not covered by the split argument
  invalid_split_values <- purrr::imap(splits, ~ { #iterates across all elements in the splits list (i.e. split names)
    setdiff(unique(data[[.y]]), .x) # and identifies any split values present in the data that have not been specified in the list
  }) |>
    keep(~ length(.x) > 0)
    
  # Keep only split names with invalid values and return an error message if there are any.
  invalid_split_values <- invalid_split_values[sapply(invalid_split_values, length) > 0] #Keep only the splits where there are invalid values
  
  if (length(invalid_split_values) > 0) { 
    cli::cli_abort(
      c("The following split names contain values in the data which have not been specified or have been specified incorrectly in the splits argument of the function:",
      purrr::imap_chr(invalid_split_values, ~ paste0( #loops over all list elements (i.e. split names) then adds to list of split names with invalid values
        "{.var ", .y, "}: ",
        paste(.x, collapse = ", ")
      )) #close mapping loop
    )) #close error message
  } #close condition

  
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
  
  
  # Replace any values in the geography code column that represent an unknown geography with NA - these
  # will still be required for calculating Scotland totals
  data <- data |>
    mutate(!!sym(required_geo_colname) := if_else(!!sym(required_geo_colname) %in% c(NA, "NA", "Unknown", ""), NA_character_, !!sym(required_geo_colname)))
  
  
  # select all required columns
  data <- data |>
    select(all_of(required_cols))
  
  
  
}
