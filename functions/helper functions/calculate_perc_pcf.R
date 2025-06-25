calculate_perc_pcf <- function(dataset){
  dataset <- dataset |>
    # if no est_pop then assume is 0
    mutate(est_pop = case_when(is.na(denominator) ~ 0, TRUE ~ est_pop),
           # Calculate the finite population correction factor.
           # Read more about it here: http://www.statisticshowto.com/finite-population-correction-factor/.
           pcf = case_when(est_pop > 0 ~ sqrt((est_pop-denominator)/(est_pop - 1)),
                           # If no population estimate available resorting to calculate it the normal way, so pcf 1.
                           est_pop == 0 ~ 1),
           # compute the percentage and confidence intervals.
           rate = numerator/denominator*100,
           # The denominator may be greater than the population estimate so the population 
           # correction factor would be less than 0.
           # This will mean that there is no CI for those areas, we assume that the whole 
           # population has been assessed and therefore there is 0 variation.
           # So we set them to be the same value as the rate in these cases.
           ci_interval = case_when(est_pop < denominator ~ 0,
                                   est_pop >= denominator ~ 
                                     1.96 * sqrt(((rate * (100-rate))/denominator) * pcf)),
           lowci = rate - ci_interval,
           upci = rate + ci_interval,
           # if over 100 or under 0, set to these values as it is a percentage
           upci = case_when(upci > 100 ~ 100, TRUE ~ upci),
           lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci))
  return(dataset)
}
