get_population_lookup <- function(folder, pop, measure){
  
  # name of the population lookup
  pop_filename <- ifelse(measure == "stdrate", paste0(pop, "_SR.rds"), paste0(pop, ".rds"))
  path <- file.path(folder, pop_filename)
  
  # read in lookup if it exists
  if (!file.exists(path)) {
    cli::cli_abort(
      c(
        "Lookup not found",
        "x" = "There is no lookup called {pop_filename} saved in {folder}",
        "i" = "Check folder for names of available lookups."
      )
    )
  } else {
    pop_lookup <- readRDS(path) |>
      mutate(across(any_of(c("code", "age_grp", "sex_grp")), as.character),
             across(any_of("year"), as.numeric))
    
  }
  
  # rename the column holding population estimates
  # if measureis perc_pcf since this measure type alreaedy contains a column called denominator
  if(measure == "perc_pcf"){
    pop_lookup <- pop_lookup |>
      rename(est_pop = denominator)
  }
  
  return(pop_lookup)
  
}
