# Code to produce indicator data both for the old OPT and the Shiny OPT

#TODO
#Make changes in CI calculation (Andy P suggestion)
#Non standard evaluation techniques might help to simplify a lot the syntax
#How to simplify epop, lookup?
#Dealing with incomplete aggregation periods needs improvement, package roll might be
# the solution but it doesn't work in the server


## HOW TO USE THESE FUNCTIONS

# FUNCTION ONE: ANALYZE_FIRST
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# geography - what is the base geography of the raw file: council, datazone2011,
#    or all if your basefile contains all the geographies you require.
# adp - To calculate the data for ADP level as well change it to TRUE, default is false.
# hscp - To calculate the data for HSCP too when data is at CA level change it to TRUE, default is false.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent),
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates when age groups are not the full 5 years eg <16 needs age group 0-4,5-9,10-14 but only pop for 15 & 16 year olds in ageband 15-19

# FUNCTION TWO: ANALYZE_SECOND
# filename -  Name of the formatted file the function reads without the "_formatted.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
#           percentage with finite population correction factor (perc_pcf)
# time_agg - Aggregation period used expressed in year, e.g. 3 
# ind_id - indicator code/number
# year_type - calendar, financial, school, survey or annual snapshot. This last one should
#           be used like "Month snapshot" e.g. "August snapshot"
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# pop - Only for crude rate cases that need finite population correction factor. Reference population.
# qa - to decide if you want to run the quality assurance report. Default is true.

## FUNCTION THREE: RUN_QA (Quality Assurance)
# filename - required - determines which indicator_data file is used for checking

###############################################.
## Packages and filepaths ----
###############################################.
library(foreign) # to read SPSS data.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages
library(readr) # writing csv's
library(odbc) # for reading oracle databases
library(readxl) # for reading excel
library(rmarkdown) # for data quality checking
library(shiny) # for data quality checking
library(flextable) # for output tables
library(plotly) # for data quality checking
library(htmltools) # for data quality checking
library(magrittr) # for other pipe operators
library(stringr) # for manipulating strings
library(janitor) #helps cleaning imported variable names
library(purrr) # for working with functions and vectors

# Detects if session is using Posit Workbench/server or RStudio and sets commonly used filepaths accordingly
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) { #if session on server
    data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
    lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
    
  } else { #else assumes using desktop
    data_folder <- "//stats/ScotPHO/Profiles/Data/"
    lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
  }

# Setting permissions so files can be overwritten, read without permission issues
Sys.umask("006")

###############################################.
## Analysis function one ----
###############################################.
analyze_first <- function(filename, geography = c("council", "datazone11", "all"), 
                          measure = c("percent", "crude", "stdrate"), time_agg, 
                          pop = NULL, yearstart, yearend, epop_age = NULL, 
                          adp = FALSE, hscp = FALSE, source_suppressed=F) {
  
  ##################################################.
  ## Part 1 - Read in raw data and add in lookup info ----
  ##################################################.
  
  # Reading file
  data_indicator <- readRDS(paste0(data_folder, "Prepared Data/", filename, "_raw.rds")) %>% 
    subset(year >= yearstart) # selecting only years of interest
  
 
  ###########################################################
  # If Scotland level totals are provided in the received extract, the following code
  # ensures that the Scotland totals calculated in this function are the same as those provided. 
  # 1) Calculate sum of geography totals
  # 2) Subtract provided Scotland value from geography totals
  # (Scotland column becomes the difference between geography_sum and the given total)
  
  if(geography == "council"){
  # Create an extract of provided Scotland data
  
  scot <- data_indicator %>%
    filter(ca %in% c("S00000001","Scotland"))

  # Create an extract of provided data excluding Scotland
  notscot <- data_indicator %>%
    filter(!ca %in% c("S00000001","Scotland"))

  # Calculate geography_sum and join this with scot data.
  # Reformat data into same format as data_indicator so that it can be bound with data_indicator
  difference <- data_indicator %>%
    filter(!ca %in% c("S00000001","Scotland")) %>%
    group_by(year) %>% summarise(across(any_of(c("numerator","denominator")), list(sum=sum), na.rm=TRUE)) %>% 
    ungroup() %>%
    left_join(scot, by = "year")%>%
    mutate(numerator = numerator - numerator_sum,
           ca = "") %>% select(-c(numerator_sum))
  if ("denominator" %in% colnames(difference)){
    difference= difference %>% 
      mutate(denominator = denominator - denominator_sum) %>% select(-c(denominator_sum)) 
  }

  # Bind the 'notscot' data to the difference data
  data_indicator <- bind_rows(notscot,difference) %>%
    arrange(year)
  }
  
  if(source_suppressed){ # only runs if user indicates that the received data has been suppressed from source i.e source_suppressed= True
      # get the rows where suppresion has been applied
      suppressed_rows = data_indicator%>%
        filter(is.na(numerator))
      # create empty list to store the geo codes and years for the suppressed rows 
      final_suppression_codes = list()
      # iterate over suppressed rows dataframe and add needed values (codes and years) to list
      for (i in unique(suppressed_rows$ca)){
        df = suppressed_rows %>% filter(ca== i)
        codes = list(codes = i,years = unique(df$year))
        list_data = list(codes)
        final_suppression_codes = append(final_suppression_codes,list_data)
      }
      # using the geo_lookup, for each of the identified suppressed geo codes...
      #- ....find all codes built up from the suppressed codes and add them to the suppressed codes list
      # we will now have all the codes suppressed and the years for which to supress for each code
      geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds"))
      
      for (i in (1:length(final_suppression_codes))){
        built_up_codes = geo_lookup %>%
          filter(ca2019==final_suppression_codes[[i]]$codes) %>%
          select(c(4,5,6,7)) %>% # selects hscp,hb,hscp_locality,adp
          as.matrix() %>% 
          as.vector() %>%
          unique()
        
        final_suppression_codes[[i]]$codes = append(final_suppression_codes[[i]]$codes, built_up_codes)
  }
  # we now have a total list of codes and the years for which they should be suppressed
  # final_suppression_codes
  # create a dataframe for it that would exist in the work environment
  # final dataframe is called suppression_df and can be viewed in your environment 
  # it will exist there and be used by the analysis_second function 
  suppression_df <- data.frame()
  
  for (i in  (1:length(final_suppression_codes))){
    sup_codes = final_suppression_codes[[i]]$codes
    sup_years = final_suppression_codes[[i]]$years
    for(geo_code in sup_codes){
      for (geo_year in sup_years){
        entry = list(geo_code,geo_year,"yes")
        suppression_df = rbind(suppression_df, entry)
      }
    }
  }
  
  suppression_df = suppression_df %>% unique()
  names(suppression_df) = c("code","year","flag")
  
  }
  
  
  
  ###########################################################
  
  geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds"))
  
  #Warning if parameter entered for geography is not one of the most used ones
  if (!(geography %in% c("datazone11", "council", "all"))) {
    stop("Your geography parameter is not 'datazone11','council' nor 'all'. 
         Change it to one of the above.")
  }
  # Merging data with lookup depending on geography base
  if (geography == "datazone11") {
    
    # Matching with geography lookup.
    data_indicator <- left_join(x = data_indicator, y = geo_lookup, 
                                by = c("datazone" = "datazone2011")) %>% 
      mutate(scotland = as.factor("S00000001")) # adding Scotland
    
  } else if (geography == "council" ) {
    
    if (adp == FALSE & hscp == FALSE) { # different variables required if ADP/HSCP included
      geo_lookup %<>% select(ca2019, hb2019) 
    } else if (adp==TRUE & hscp == FALSE) {
      geo_lookup %<>% select(ca2019, hb2019, adp)
    } else if (adp==FALSE & hscp == TRUE) {
      geo_lookup %<>% select(ca2019, hb2019, hscp2019)
    } else if (adp==TRUE & hscp == TRUE) {
      geo_lookup %<>% select(ca2019, hb2019, adp, hscp2019)
    }
    
    geo_lookup %<>% distinct %>% rename(ca = ca2019, hb = hb2019)
    
    # Matching with geography lookup.
    data_indicator <- left_join(x=data_indicator, y=geo_lookup, c("ca")) %>% 
      mutate(scotland = as.factor("S00000001")) # adding Scotland
  }
  
  ##################################################.
  ## Part 2 - Aggregate up to get figures for each area. ----
  ##################################################.
  # Need to include different measures and geography 
  # (non standard evaluation could help to make it easy)
  if (measure == "stdrate" & geography == "datazone11" ) {
    data_indicator %<>% gather(geolevel, code, intzone2011:scotland) %>% 
      ungroup() %>% select(-c(geolevel, datazone)) %>% 
      group_by(code, year, sex_grp, age_grp) %>% summarise_all(sum, na.rm =T) %>% ungroup()
    
  } else if (measure == "stdrate" & geography == "council") {
    data_indicator %<>% gather(geolevel, code, ca, hb:scotland) %>% 
      select(-c(geolevel)) %>% 
      group_by(code, year, sex_grp, age_grp) %>% summarise_all(sum, na.rm =T) %>% ungroup()
  } else if (measure %in% c("crude", "percent") & geography == "datazone11" ) {
    data_indicator %<>% gather(geolevel, code, intzone2011:scotland) %>% 
      ungroup() %>% select(-c(geolevel, datazone)) %>% 
      group_by(code, year) %>% summarise_all(sum, na.rm =T) %>% ungroup()
  } else if (measure %in% c("crude", "percent") & geography == "council") {
    data_indicator %<>% gather(geolevel, code, ca, hb:scotland) %>% 
      select(-c(geolevel)) %>% 
      group_by(code, year) %>% summarise_all(sum, na.rm =T) %>% ungroup()
  }
  
  data_indicator = data_indicator %>% filter(!(is.na(code)))
  
  # Matching with population lookup
  if (!is.null(pop)){
    if (measure == "stdrate") {
      pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'_SR.rds')) %>% 
        subset(year >= yearstart) %>% # Reading pop file and selecting only for 2011 onwards
        mutate_at(c("sex_grp", "age_grp"), as.character)
      
      # Excludes ADP level if not wanted
      if (adp == FALSE) {
        pop_lookup %<>% subset(substr(code,1,3) != "S11")
      }
      
      # Excludes HSCP level if not wanted
      if (hscp == FALSE & geography %in% c("council", "all")) {
        pop_lookup %<>% subset(substr(code,1,3) != "S37")
      }
      
      # Merging pop with indicator data
      data_indicator$age_grp <- as.character(data_indicator$age_grp)
      data_indicator <- full_join(x = data_indicator, y = pop_lookup,
                                  by = c("year", "code", "sex_grp", "age_grp"))
      
    } else if (measure %in% c("crude", "percent")){
      
      pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
        subset(year >= yearstart) %>% # Reading population file and selecting only for 2011 onwards
        mutate(code = as.character(code))
      
      # Excludes ADP level if not wanted
      if (adp == FALSE) {
        pop_lookup %<>% subset(substr(code,1,3) != "S11")
      }
      
      # Excludes HSCP level if not wanted
      if (hscp == FALSE & geography == "council") {
        pop_lookup %<>% subset(substr(code,1,3) != "S37")
      }
      
      # Matching population with data
      data_indicator <- full_join(x = data_indicator, y = pop_lookup, 
                                  by = c("year", "code"))
    }
  }
  
  # selecting only years of interest
  data_indicator %<>% subset(year >= yearstart & year <= yearend) 
  
  data_indicator$numerator[is.na(data_indicator$numerator)] <- 0 # Converting NA's to 0s
  
  # Excludes ADP level if not wanted
  if (adp == FALSE) {
    data_indicator %<>% subset(substr(code,1,3) != "S11")
  }
  
  # Excludes HSCP level if not wanted when council data is the base
  if (hscp == FALSE & geography == "council") {
    data_indicator %<>% subset(substr(code,1,3) != "S37")
  }
  
  ##################################################.
  ##  Part 3 - Aggregate by required time periods ----
  ##################################################.
  # The moving average is left aligned this means that the year variable will not 
  # reflect the right time. This is the way to fix it depending on what is the 
  # aggregation period.
  time_fix <- case_when(time_agg < 3 ~ 0, time_agg == 3 ~ 1,
                        time_agg == 5 ~ 2, TRUE ~ NA_real_)
  
  # Calculating moving average for denominator and numerator
  # Data needs to be sorted to calculate the right figures
  if (measure == "stdrate") {
    data_indicator %<>% arrange(code, sex_grp, age_grp, year) %>% 
      group_by(code, sex_grp, age_grp) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  # excluding NA rows 
      mutate(year = year - time_fix) %>%  # year minus to adjust to center year
      ungroup()
    
  } else if (measure %in% c("crude", "percent")) {
    data_indicator %<>% arrange(code, year) %>% 
      group_by(code) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  # excluding NA rows 
      mutate(year = as.numeric(year) - time_fix) %>%  # year minus to adjust to center year
      ungroup()
  }
  
  # Creating variable with European population 2016 depending on age cut
  if (!is.null(pop) & measure == "stdrate"){
    if (epop_age == "normal") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                                    "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                                    "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                                    "13" = 6000, "14" = 5500, "15" = 5000,
                                    "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
    } else if (epop_age == "16+") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "4" = 4400, "5" = 6000, "6" = 6000, "7" = 6500, 
                                    "8" = 7000, "9" = 7000, "10" = 7000, "11" = 7000, 
                                    "12" = 6500, "13" = 6000, "14" = 5500, "15" = 5000, 
                                    "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
    } else if (epop_age == "<16") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "1" = 5000, "2" = 5500, "3" = 5500, "4" = 1100)
    } else if (epop_age == "0to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                                    "5" = 6000, "6" = 1200)
    } else if (epop_age == "11to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "3" = 4400, "4" = 5500, "5" = 6000, "6" = 1200)
    } else if (epop_age == "15to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                                    "4" = 5500, "5" = 6000, "6" = 1200)
    }
  }
  
  #~~~~~~~~~~~~
  # COMMENT OUT FOR NOW UNTIL ABOVE SCOTLAND SUPPRESSION MODIFICATION IS DONE
  # If data provided already had a Scotland level, and the difference was calculated
  # by creating a blank level of ca, the below code will remove this blank level
  # from the analysis_first_result
  
  data_indicator %<>%
    filter(code != "")
  
  #~~~~~~~~~~~~
  
  analysis_first_result <<- data_indicator
  
  if(source_suppressed){ # make suppression_df available in environment 
    suppression_df <<- suppression_df 
  }
  
  saveRDS(data_indicator, file=paste0(data_folder, "Temporary/", filename, "_formatted.rds"))
}

##################################################.
##  Second analysis function ----
##################################################.
analyze_second <- function(filename, measure = c("percent", "crude", "perc_pcf", "stdrate"), 
                           time_agg,  ind_id, year_type,
                           epop_total = NULL, pop = NULL, crude_rate, qa = TRUE) {   
  
  ##################################################.
  ##  Part 4 - Create rates or percentages ----
  ##################################################.
  #Used for trend labels and finite population  correction factor
  time_fix <- case_when(time_agg < 3 ~ 0, time_agg == 3 ~ 1,
                        time_agg == 5 ~ 2, TRUE ~ NA_real_)
  
  data_indicator <- readRDS(file=paste0(data_folder, "Temporary/", filename, "_formatted.rds"))
  
  if (measure == "stdrate"){ # European Age-sex standardized rates
    # Calculating individual easr and variance
    data_indicator %<>% 
      mutate(easr_first = numerator * epop/denominator, # easr population
             var_dsr = (numerator * epop^2)/denominator^2) %>%  # variance
      # Converting Infinites to NA and NA's to 0s to allow proper functioning
      mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # Caused by a denominator of 0 in an age group with numerator >0
             var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) %>%
      mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0))  
    
    # aggregating by year, code and time
    data_indicator %<>% subset(select = -c(age_grp, sex_grp)) %>%
      group_by(year, code) %>% summarise_all(sum, na.rm =T) %>% ungroup()
    
    # Calculating rates and confidence intervals
    data_indicator %<>% 
      mutate(epop_total = epop_total,  # Total EPOP population
             easr = easr_first/epop_total, # easr calculation
             o_lower = numerator * (1 - (1/(9 * numerator)) - (1.96/(3 * sqrt(numerator))))^3,  # Lower CI
             o_upper = (numerator + 1)*(1 - (1/(9 * (numerator + 1))) + 
                                          (1.96/(3 * sqrt(numerator+1))))^3, # Upper CI
             var = (1/epop_total^2) * var_dsr, # variance
             rate = easr * 100000,  # rate calculation
             lowci = (easr + sqrt(var/numerator) * (o_lower - numerator)) * 100000, # Lower CI final step
             upci = (easr + sqrt(var/numerator) * (o_upper - numerator)) * 100000) # Upper CI final step
    
    data_indicator %<>% subset(select = -c(var, easr, epop_total, o_lower, o_upper, 
                                           easr_first, epop, var_dsr, denominator)) # deleting variables
    
  } else if (measure == "percent"){ # Percentage
    data_indicator %<>% 
      mutate(rate = numerator/denominator*100,
             lowci=(2 * numerator + 1.96 * 1.96 - 1.96 * 
                      sqrt(1.96 * 1.96 + 4 * numerator * (1 - rate/100))) 
             / (2 * (denominator + 1.96 * 1.96))*100,
             upci=(2 * numerator +1.96 * 1.96 + 1.96 * 
                     sqrt(1.96 * 1.96 + 4 * numerator*(1 - rate/100)))
             /  (2 * (denominator + 1.96 * 1.96)) * 100,
             # if over 100 or under 0, set to these values as it is a percentage
             # also some cases upci a fraction lower than rate (14 decimal point)
             upci = case_when(upci > 100 ~ 100, TRUE ~ round(upci, 2)),
             lowci = case_when(lowci < 0 ~ 0, TRUE ~ round(lowci, 2)))
    
  } else if (measure == "crude"){ # Crude rates
    data_indicator %<>% 
      mutate(rate = numerator/denominator * crude_rate,
             o_lower = numerator * (1-1/9/numerator - 1.96/3/sqrt(numerator))^3,
             o_upper = (numerator + 1) *(1 - 1/9/(numerator + 1) + 
                                           1.96/3/sqrt(numerator + 1))^3,
             lowci = o_lower/(denominator) * crude_rate,
             upci = o_upper/(denominator) * crude_rate) %>% 
      subset(select = -c(o_upper, o_lower))
    
  } else if (measure == "perc_pcf") { # Percentage with finite population factor
    
    # Bringing reference population and aggregating by the required time period
    pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
      group_by(code) %>%
      mutate(denominator = roll_meanr(denominator, time_agg),
             year = as.numeric(year)-time_fix) %>% # year minus to adjust to center year
      subset(!is.na(denominator)) %>%  # excluding NA rows 
      ungroup() %>% rename(est_pop = denominator)
    
    # Matching population with data
    data_indicator <- left_join(x=data_indicator, y=pop_lookup, by = c("year", "code")) %>% 
      # if no est_pop then assume is 0
      mutate(est_pop = case_when(is.na(est_pop) ~ 0, TRUE ~est_pop),
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
    
  }
  
  ##################################################.
  ##  Part 5 - Adding time labels and indicator info ----
  ##################################################.
  data_indicator %<>% mutate(ind_id = ind_id) %>% # Indicator code 
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) %>% 
    mutate(lowci = case_when(lowci < 0 ~ 0, TRUE ~ lowci)) 
  
  # add in the definition period and trend axis labels
  # Calendar aggregate years
  if (year_type == "calendar" & time_fix > 0){ 
    data_indicator %<>% 
      mutate(trend_axis = paste0(year - time_fix, "-", year + time_fix),  
             def_period = paste0(year - time_fix, " to ", year + time_fix, " ", year_type, 
                                 " years; ", time_agg, "-year aggregates"))
    
    # Calendar two-year aggregates 
  } else if (year_type == "calendar" & time_agg == 2){ 
    data_indicator %<>% 
      mutate(trend_axis=paste0(year - 1, "-", year),  
             def_period=paste0(year - 1, " to ", year, " ", year_type, 
                               " years; 2-year aggregates")) 
    
    # Calendar single years or single survey years
  } else if ((year_type == "calendar" & time_fix == 0 & time_agg != 2) |
             year_type == "survey") { 
    data_indicator <- data_indicator %>% 
      mutate(trend_axis=year,  
             def_period=paste0(year, " ", year_type, " year"))
    
    # Annual snapshot 
  } else if (grepl("snapshot", year_type, fixed=TRUE) ==  TRUE) { 
    data_indicator %<>% 
      mutate(trend_axis=year,  
             def_period=paste0(year, " ", year_type))
    
    # Financial single years
  } else if (year_type %in% c("financial", "school") & time_fix == 0 & time_agg != 2){
    
    data_indicator %<>% 
      mutate(trend_axis = paste0(year, "/", substr(year + 1, 3, 4)),
             def_period = paste0(trend_axis, " ", year_type, " year"))
    
    # Financial two-year aggregates 
  } else if (year_type %in% c("financial", "school") & time_agg == 2){ 
    data_indicator %<>% 
      mutate(trend_axis = paste0(year - 1,  "/", substr(year, 3, 4),
                                 "-", year, "/",substr((year + 1), 3, 4)),  
             def_period = paste0(year - 1, "/", substr(year, 3, 4), " to ",
                                 year, "/", substr((year + 1), 3, 4),
                                 " ", year_type, " years; 2-year aggregates"))
    # Financial aggregate years
  } else if (year_type %in% c("financial", "school") & time_fix > 0){
    
    data_indicator %<>% 
      mutate(trend_axis = paste0(year - time_fix, "/", substr(year, 3, 4),
                                 "-", year + time_fix, "/",
                                 substr((year + time_fix + 1), 3, 4)),
             def_period = paste0(year - time_fix, "/", substr(year, 3, 4), " to ",
                                 year + time_fix, "/", substr((year + time_fix + 1), 3, 4),
                                 " ", year_type, " years; ", time_agg, 
                                 "-year aggregates"))
  }
  
  ################################################################################################
  # after all neccesary formating done in analyse second, check if a dataframe of values to suppress exist in the environment 
  # access the suppression dataframe irt was created in analyse first function and thus exists in environment
  # if it exists it would mean that analyst specified that the data had some geographies suppressed by supplier
  # we have to suppress them and areas built up from them for certain years.
  if(exists("suppression_df")){ 
    data_indicator = left_join(data_indicator,suppression_df ,multiple = "all")  %>%
      mutate(numerator=case_when(flag=="yes" ~ NA_real_, TRUE ~ numerator),
             rate=case_when(flag=="yes" ~ NA_real_, TRUE ~ rate),
             lowci=case_when(flag=="yes" ~ NA_real_, TRUE ~ lowci),
             upci=case_when(flag=="yes" ~ NA_real_, TRUE ~ upci)) %>%
      select(-flag)
  }
  ###################################################################################################
  
  saveRDS(data_indicator, paste0(data_folder, "Temporary/", filename, "_final.rds"))
  
  # Preparing data for Shiny tool
  data_shiny <- data_indicator %>% select(c(code, ind_id, year, numerator, rate, lowci,
                                            upci, def_period, trend_axis))
  
  # Including both rds and csv file for now
  saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/", filename, "_shiny.rds"))
  write_csv(data_shiny, file = paste0(data_folder, "Data to be checked/", filename, "_shiny.csv"))
  
  # Making final dataset available outside the function
  final_result <<- data_indicator
  
  ##################################################.
  ##  Part 6 - Checking results ----
  ##################################################.
  if (qa == FALSE) { #if no quality assurance desired

        # Selecting Health boards and Scotland for latest year in dataset
    ggplot(data = data_indicator %>% filter((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                            & year== max(year)), aes(code, rate) ) +
      geom_point(stat = "identity") +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
  } else  { # Running quality assurance
    run_qa(filename={{filename}},old_file="default")} 
} # End analyze_second

############################################################.
## Function Three: Run Quality Assurance ----
############################################################.
# Function below runs an rmarkdown report (.Rmd) that runs through standard checks 
# of indicator data
# The report requires one manadatory parameter (the indicator filename) to run 
# but there are several optional
#  parameters to adjust
# filename - required - determines which indicator_data file is used for checking
# old_file - (optional - if the indicator has changed name and you want to compare old and 
#                   new files which have different names)
#                  - default set to "default", rmd code default will set "filename" 
#                   parameter as the old_filename
# check_extras - (default empty) parameter can be used to add bespoke geographies 
#       of any geo type to Data Check 3 (comparing old and new figures)

run_qa <- function(filename, old_file="default", check_extras=c()){
   run("../scotpho-indicator-production/3.Data Quality Checks.Rmd")
}  

############################################################.
## Function to create age groups ----
############################################################.
# recode age groups
create_agegroups <- function(dataset) {
    dataset %>% mutate(age_grp = as.character(case_when(between(age, 0, 4) ~ 1,
      between(age, 5, 9) ~ 2, between(age, 10, 14) ~ 3, between(age, 15, 19) ~ 4, 
      between(age, 20, 24) ~ 5, between(age, 25, 29) ~ 6, between(age, 30, 34) ~ 7, 
      between(age, 35, 39) ~ 8, between(age, 40, 44) ~ 9, between(age, 45, 49) ~ 10, 
      between(age, 50, 54) ~ 11, between(age, 55, 59) ~ 12, between(age, 60, 64) ~ 13,
      between(age, 65, 69) ~ 14, between(age, 70, 74) ~ 15,  between(age, 75, 79) ~ 16,
      between(age, 80, 84) ~ 17, between(age, 85, 89) ~ 18, between(age, 90, 200) ~ 19)))
  }

##END
