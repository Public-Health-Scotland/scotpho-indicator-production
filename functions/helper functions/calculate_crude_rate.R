calculate_crude_rate <- function(data, crude_rate){
  
  if(!is.numeric(crude_rate)){
    cli::cli_abort(c(
      "{.var crude_rate} must be a number",
      "x" = "You've supplied a {.cls {class(crude_rate)}} vector."))
  }
  
  # calculate rate
  data <- data |>
    mutate(rate = numerator/denominator * crude_rate)
  
  # calculate CIS
  data <- data |>
    mutate(
      lowci = (numerator * (1-1/9/numerator - 1.96/3/sqrt(numerator))^3) / (denominator) * crude_rate,
      upci = ((numerator + 1) *(1 - 1/9/(numerator + 1) + 1.96/3/sqrt(numerator + 1))^3) / (denominator) * crude_rate
    )
  
}