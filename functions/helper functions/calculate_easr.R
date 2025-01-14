calculate_easr <- function(data, epop_total, epop_age = c("normal", "16+", "<16", "0to25", "11to25", "15to25")){
  
  # check function arguments
  if(!is.numeric(epop_total)){
    cli::cli_abort(c(
      "{.var epop_total} must be a number",
      "x" = "You've supplied a {.cls {class(epop_total)}} vector."))
  }
  
  epop_age <- rlang::arg_match(epop_age)
  
  
  
  if (epop_age == "normal") {
    data$epop <- recode(as.character(data$age_grp), 
                        "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                        "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                        "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                        "13" = 6000, "14" = 5500, "15" = 5000,
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } else if (epop_age == "16+") {
    data$epop <- recode(as.character(data$age_grp), 
                        "4" = 4400, "5" = 6000, "6" = 6000, "7" = 6500, 
                        "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                        "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                        "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  } else if (epop_age == "<16") {
    data$epop <- recode(as.character(data$age_grp), 
                        "1" = 5000, "2" = 5500, "3" = 5500, "4" = 1100)
  } else if (epop_age == "0to25") {
    data$epop <- recode(as.character(data$age_grp), 
                        "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                        "5" = 6000, "6" = 1200)
  } else if (epop_age == "11to25") {
    data$epop <- recode(as.character(data$age_grp), 
                        "3" = 4400, "4" = 5500, "5" = 6000, "6" = 1200)
  } else if (epop_age == "15to25") {
    data$epop <- recode(as.character(data$age_grp), 
                        "4" = 5500, "5" = 6000, "6" = 1200)
  }
  
  # Calculating individual easr and variance
  data <- data |>
    mutate(easr_first = numerator * epop/denominator, # easr population
           var_dsr = (numerator * epop^2)/denominator^2) |>  # variance
    # Converting Infinites to NA and NA's to 0s to allow proper functioning
    mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # Caused by a denominator of 0 in an age group with numerator >0
           var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) |>
    mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0))


  # aggregating by year, code and time
  data <- data |>
    select(-c(age_grp, sex_grp))|>
    group_by(year, code) |>
    summarise_all(sum, na.rm =T) |>
    ungroup()

  # Calculating rates and confidence intervals
  data <- data |>
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           o_lower = numerator * (1 - (1/(9 * numerator)) - (1.96/(3 * sqrt(numerator))))^3,  # Lower CI
           o_upper = (numerator + 1)*(1 - (1/(9 * (numerator + 1))) +
                                        (1.96/(3 * sqrt(numerator+1))))^3, # Upper CI
           var = (1/epop_total^2) * var_dsr, # variance
           rate = easr * 100000,  # rate calculation
           lowci = (easr + sqrt(var/numerator) * (o_lower - numerator)) * 100000, # Lower CI final step
           upci = (easr + sqrt(var/numerator) * (o_upper - numerator)) * 100000) # Upper CI final step


  # remove temporary variables
  data <- data |>
    select(-c(var, easr, epop_total, o_lower, o_upper, easr_first, epop, var_dsr, denominator))


  data <- data |>
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) |>
    mutate(lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci))

}










