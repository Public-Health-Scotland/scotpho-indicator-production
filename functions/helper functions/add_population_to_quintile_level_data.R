#' Function: add_population_to_quintile_level_data
#' ##############################################
#' 
#' Call this function if you already have measure data by SIMD quintile but don't have populations and want to generate other inequality measures (e.g. SII/RII/PAF)
#' This function will add columns to your dataset containing the appropriate SIMD quntile population (can add populations split by sex if required) 
#' 
#' Takes a file with indicator data already aggregated to SIMD level.
#' N.B. Currently only takes Scotland-level data: amend if to be used for HB/CA level.
#' N.B. Currently only takes total population data: needs amending for male/female splits
#' N.B. Currently only handles quintile level data not deciles
#'
#' @param filename 
#' Name of the raw file the function reads without the "_raw.sav" at the end
#'  required fields: "year"       "rate"       "lowci"      "upci"       "numerator"  "def_period"
#'                  "trend_axis" "ind_id"     "code"       "quintile"   "quint_type"
#'  quintile is in format "1" to "5" and "Total" (total must be provided).
#'  sex is an optional column.
#' @param pop Name of the population file.
#' @param ind_id indicator code/number
#' @param ind_name indicator name for the final files
#' 



add_population_to_quintile_level_data <- function(data, # data file to run
                        pop, # what population file to use for denominators
                        ind_id, # the ind_id
                        ind_name) # the indicator name (abbreviated, for output file)
{
  ###############################################.
  ## Read in data----
  ###############################################.
  
  # read in raw data. 
#data_depr <- readRDS(paste0(profiles_data_folder, "/Prepared Data/" ,filename, ".rds")) %>% 
  data <- data |>
  mutate(year = as.numeric(year)) |> 
  filter(ind_id == ind_id) 
  
  yearstart = min(data$year)
  yearend = max(data$year)
  
  # Are the deprivation data grouped by sex? If so this needs to be accounted for in these calculations.
  sex_column <- "sex" %in% names(data) # gives TRUE or FALSE
  
  # What geogs are in the data?
  codes <- unique(data$code)
  
  ###############################################.
  ## Matching with population lookup----
  ###############################################.
  
  if (sex_column == FALSE) {
    
    # Matching with population lookup (denominator required for SIMD analysis)
    pop_depr_lookup <- readRDS(paste0(profiles_lookups, "/Population/", pop,'.rds')) %>% 
      subset(year >= yearstart & year <= yearend) #Reading population file and selecting the right year range
    
    # Matching population with data
    data <- right_join(x=data, y=pop_depr_lookup, 
                            by = c("year", "code", "quintile", "quint_type")) %>%
      filter(code %in% codes) 
    
  } else if (sex_column == TRUE) {
    
    # Matching with population lookup (denominator required for SIMD analysis)
    pop_depr_lookup <- readRDS(paste0(profiles_lookups, "/Population/", pop,'_SR.rds')) %>% 
      subset(year >= yearstart & year <= yearend) %>% #Reading population file and selecting the right year range
      group_by(year, code, sex_grp, quintile, quint_type) %>%
      summarise(denominator = sum(denominator)) %>%
      ungroup() %>%
      rename(sex = sex_grp) %>%
      mutate(sex = case_when(sex==1 ~ "Male",
                             sex==2 ~ "Female"))
    
    # population lookup only contains male/female so need to sum and create populations for both sexes
    pop_depr_lookup_totals <- pop_depr_lookup %>%
      group_by(year, code, quintile, quint_type) %>%
      summarise(denominator = sum(denominator)) %>%
      ungroup() %>%
      mutate(sex = "Total")
    
    # bind male/female and total populations 
    pop_depr_lookup <- rbind(pop_depr_lookup,
                             pop_depr_lookup_totals)  
    
    # Matching population with data
    data <- right_join(x=data, y=pop_depr_lookup, 
                            by = c("year", "code", "quintile", "quint_type", "sex")) %>%
      filter(code %in% codes) |> #remove any geographies not required
      subset(year >= yearstart & year <= yearend) #remove any years where no data
    
  }}

#END
