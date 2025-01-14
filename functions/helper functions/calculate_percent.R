calculate_percent <- function(data){
  
  # calculate rate
  data <- data |>
    mutate(rate = numerator / denominator * 100)
  
  # calculate confidence intervals
  data <- data |>
    mutate(
      # lower CI
      lowci = (2 * numerator + 1.96 * 1.96 - 1.96 * sqrt(1.96 * 1.96 + 4 * numerator * (1 - rate / 100))) 
      / (2 * (denominator + 1.96 * 1.96)) * 100,
      
      # Upper CI
      upci=(2 * numerator +1.96 * 1.96 + 1.96 * sqrt(1.96 * 1.96 + 4 * numerator * (1 - rate / 100)))
      /  (2 * (denominator + 1.96 * 1.96)) * 100
    )
  
  # correct CIs if rates are over 100 or under 0
  # also some cases upci a fraction lower than rate (14 decimal point)
  data <- data |>
    mutate(
      upci = case_when(upci > 100 ~ 100, TRUE ~ round(upci, 2)),
      lowci = case_when(lowci < 0 ~ 0, TRUE ~ round(lowci, 2))
    )
  
}
