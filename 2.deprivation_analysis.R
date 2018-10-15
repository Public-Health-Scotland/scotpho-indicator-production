#Function to create data for indicators by deprivation quintile (SIMD).

## HOW TO USE THIS FUNCTION
# File is expected at datazone 2011 level with only these other variables:
# year, code, numerator. denominator, age_grp, sex_grp could be required depending
# on the nature of your data.
# Arguments of the function:
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
# time_agg - Aggregation period used expressed in year, e.g. 3
# ind_id - indicator code/number
# year_type - calendar, financial or school
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)

###############################################.
## Packages and filepaths ----
###############################################.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages 

if (server_desktop == "server") {
  data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
} else if (server_desktop == "desktop") {
  data_folder <- "//stats/ScotPHO/Profiles/Data/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
}

##################################################.
##  Analysis function ----
##################################################.
analyze_deprivation <- function(filename, yearstart, yearend, time_agg, 
                                measure = c("percent", "crude", "stdrate"),
                                epop_age = NULL, epop_total = NULL, pop = NULL, 
                                crude_rate = 0, ind_id, year_type) {
  
  ###############################################.
  ## Part 1 - Read in raw data and add in lookup info ----
  ###############################################.
  # read in raw data. 
  data_depr <- readRDS(paste0(prepared_data, filename, "_raw.rds"))
  # read in deprivation lookup. 
  depr_lookup <- readRDS(paste0(lookups, "Geography/deprivation_geography.rds"))

  #merging them
  data_depr <- left_join(data_depr, depr_lookup, by = c("datazone", "year"))
  
  #Adding Scotland
  data_depr <- data_depr %>% mutate(scotland = "S00000001")
  
  ###############################################.
  ## Part 2 - Aggregate up to get figures for each area. ----
  ###############################################.
  #This function groups the data for the variables selected and then aggregates it
  #It works for the different types of quintiles and for all measures
  create_quintile_data <- function(group_vars, geo, quint) {
    if (measure %in% c("crude", "percent")) {
      data_depr %>% select(c("numerator", group_vars)) %>% 
        group_by_at(group_vars) %>% summarise(numerator= sum(numerator, na.rm =T)) %>% 
        rename_(code = geo, quintile = quint) %>% ungroup() %>% 
        mutate(quint_type = quint)
    } else if (measure == "stdrate") {
      data_depr %>% select(c("numerator", "age_grp", "sex_grp", group_vars)) %>% 
        group_by_at(c("age_grp", "sex_grp", group_vars)) %>% 
        summarise(numerator= sum(numerator, na.rm =T)) %>% 
        rename_(code = geo, quintile = quint) %>% ungroup() %>% 
        mutate(quint_type = quint)
    }
  }
  
  data_depr <- rbind( 
    #Scotland 
    create_quintile_data(geo = "scotland", quint = "sc_quin", 
                         group_vars =  c("scotland", "year", "sc_quin")),
    #Health boards using national quintiles
    create_quintile_data(geo = "hb2014", quint = "sc_quin", 
                         group_vars =  c("hb2014", "year", "sc_quin")),
    #Health boards using health board quintiles
    create_quintile_data(geo = "hb2014", quint = "hb_quin", 
                         group_vars =  c("hb2014", "year", "hb_quin")),
    #Council area using national quintiles
    create_quintile_data(geo = "ca2011", quint = "sc_quin", 
                         group_vars =  c("ca2011", "year", "sc_quin")),
    #Council area using concil quintiles
    create_quintile_data(geo = "ca2011", quint = "ca_quin",
                         group_vars =  c("ca2011", "year", "ca_quin")))
  
  #Creating combined totals
  if (measure %in% c("crude", "percent")) {
    
    data_depr_totals <- data_depr %>% group_by(code, year, quint_type) %>% 
      summarise(numerator= sum(numerator)) %>% 
      mutate(quintile = "Total") %>% ungroup()
    
  } else if (measure == "stdrate") {
    
    data_depr_totals <- data_depr %>% group_by(code, year, age_grp, sex_grp, quint_type) %>% 
      summarise(numerator= sum(numerator)) %>% 
      mutate(quintile = "Total") %>% ungroup()
  }
  
  #And merging them with the rest of the data
  data_depr <- rbind(data_depr, data_depr_totals)
  
  ###############################################.
  ## Part 3 - Matching with population lookup----
  ###############################################.
  # Matching with population lookup
  if (!is.null(pop)){
    if(measure == "stdrate") {
      pop_depr_lookup <- readRDS(paste0(lookups, "Population/", pop,'_SR.rds')) %>% 
        subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
        mutate_at(c("sex_grp", "code"), as.factor)
      
      data_depr <- right_join(x=data_depr, y=pop_depr_lookup, # Matching population with data
                        by = c("year", "code", "sex_grp", "age_grp", "quintile", "quint_type"))
      
    } else if (measure %in% c("crude", "percent")){
      
      pop_depr_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
        subset(year >= yearstart) #Reading population file and selecting only for 2011 onwards
      
      # Matching population with data
      data_depr <- right_join(x=data_depr, y=pop_depr_lookup, 
                             by = c("year", "code", "quintile", "quint_type"))
      
    }
  } 
  #selecting only years of interest
  data_depr <- data_depr %>% subset(year >= yearstart & year <= yearend)
  
  data_depr$numerator[is.na(data_depr$numerator)] <- 0 # Converting NA's to 0s
  
  ###############################################.
  ## Part 4 - Create required time periods ----
  ###############################################.
  #The moving average is left aligned this means that the year variable will not 
  #reflect the right time. This is the way to fix it depending on what is the 
  #aggregation period.
  time_fix <- ifelse(time_agg < 3, 0, ifelse(time_agg == 3, 1,
                                             ifelse(time_agg == 5, 2, "ERROR")))
  
  ## Calculating moving average for denominator and numerator
  ## Data needs to be sorted to calculate the right figures
  if (measure == "stdrate") {
    data_depr <- data_depr %>% 
      arrange(code, quintile, quint_type, sex_grp, age_grp, year) %>% 
      group_by(code, quintile, quint_type, sex_grp, age_grp) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  #excluding NA rows 
      mutate(year = year-time_fix) %>%  # year minus to adjust to center year
      ungroup()
    
  } else if (measure %in% c("crude", "percent")) {
    data_depr <- data_depr %>% 
      arrange(code, quintile, quint_type, year) %>% 
      group_by(code, quintile, quint_type) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  #excluding NA rows 
      mutate(year = as.numeric(year)-time_fix) %>%  # year minus to adjust to center year
      ungroup()
  }
  
  #Creating variable with European population 2016 depending on age cut
  if (!is.null(pop) & measure == "stdrate"){
    if (epop_age == "normal") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                          "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                          "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                          "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "16+") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "4"=4400, "5"=6000, "6"=6000, "7"= 6500, "8"=7000, 
                          "9"=7000, "10"=7000, "11"=7000, "12"=6500, "13"=6000, 
                          "14"=5500, "15"=5000, "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "<16") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=1100)
    } else if (epop_age == "0to25") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "11to25") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "3"=4400, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "15to25") {
      data_depr$epop <- recode(as.character(data_depr$age_grp), 
                          "4"=5500, "5"=6000, "6"= 1200)
    }
  }
  
  saveRDS(data_depr, file=paste0(data_folder, "Temporary/", filename, "_formatted.rds"))
  
  ##################################################.
  ##  Part 5 - Create rates or percentages ----
  ##################################################.

  if (measure == "stdrate"){ #European Age-sex standardized rates
    #Calculating individual easr and variance
    data_depr <- data_depr %>%
      mutate(easr_first = numerator*epop/denominator, #easr population
             var_dsr = (numerator*epop^2)/denominator^2) %>%  # variance
      # Converting NA's to 0s to allow proper functioning
      mutate_at(c("easr_first", "var_dsr"), funs(replace(., is.na(.), 0)))  
    
    # aggregating by year, code and time
    data_depr <- data_depr %>% subset(select= -c(age_grp, sex_grp)) %>%
      group_by(year, code, quintile, quint_type) %>% summarise_all(funs(sum)) %>% ungroup()
    
    #Calculating rates and confidence intervals
    data_depr <- data_depr %>%
      mutate(epop_total = epop_total,  # Total EPOP population
             easr = easr_first/epop_total, # easr calculation
             o_lower = numerator*(1-(1/(9*numerator)) - (1.96/(3*sqrt(numerator))))^3,  # Lower CI
             o_upper = (numerator+1)*(1-(1/(9*(numerator+1))) + (1.96/(3*sqrt(numerator+1))))^3, # Upper CI
             var = (1/epop_total^2)*var_dsr, #variance
             rate = easr*100000,  # rate calculation
             lowci = (easr+sqrt(var/numerator)*(o_lower - numerator))*100000, #Lower CI final step
             upci = (easr+sqrt(var/numerator)*(o_upper - numerator))*100000) #Upper CI final step
    
    data_depr <- data_depr %>% subset(select = -c(var, easr, epop_total, o_lower, o_upper, 
                                        easr_first, epop, var_dsr)) #deleting variables
    
  } else if (measure == "percent"){ #Percentage
    data_depr <- data_depr %>%
      mutate(rate = numerator/denominator*100,
             lowci=(2*numerator+1.96*1.96-1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100))) 
             / (2*(denominator+1.96*1.96))*100,
             upci=(2*numerator+1.96*1.96+1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100)))
             /  (2*(denominator+1.96*1.96))*100.)
    
  } else if (measure == "crude"){ #Crude rates
    data_depr <- data_depr %>%
      mutate(rate = numerator/denominator*crude_rate,
             o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
             o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
             lowci = o_lower/(denominator)*crude_rate,
             upci = o_upper/(denominator)*crude_rate) %>% 
      subset(select = -c(o_upper, o_lower))
    
  }
  
  ##################################################.
  ##  Part 6 - Create SII ----
  ##################################################.
  #Splitting into two files: on with quintiles for SII and one without for RII
  data_depr_sii <- data_depr %>% group_by(code, year, quint_type) %>% 
    mutate(overall_rate = rate[quintile == "Total"]) %>% 
    filter(quintile != "Total") %>% ungroup()
  
  data_depr_totals <- data_depr %>% filter(quintile == "Total")
  
  #calculate the total population for each area (without SIMD).
  # proportion of the population in each SIMD out of the total population. 
  data_depr_sii <- data_depr_sii %>% group_by(code, year, quint_type) %>% 
    mutate(total_pop = sum(denominator),
           proportion_pop = denominator/total_pop,
  # cumulative proportion population for each area
           cumulative_pro = cumsum(proportion_pop),
           relative_rank = case_when(
           quintile == "1" ~ 0.5*proportion_pop,
           quintile != "1" ~ lag(cumulative_pro) + 0.5*proportion_pop)) %>% 
    ungroup()
  
  ###############################################.
  # Calculate the regression coefficient
  # the formula for the regression coefficient can be found on 
  # http://www.statisticshowto.com/how-to-find-a-linear-regression-equation/ . 
  data_depr_sii <- data_depr_sii %>% group_by(code, year, quint_type) %>% 
      mutate(xy = relative_rank * rate,
             x_2 = relative_rank^2,
             sum_x = sum(relative_rank),
             sum_y = sum(rate),
             mean_x = mean(relative_rank),
             slope_coef = abs((5*sum(xy) - sum_x*sum_y) / (5*sum(x_2) - sum_x^2)),
             # calculate the intercept value.
             intercept = (sum_y *sum(x_2) -sum_x*sum(xy))/(5*sum(x_2) - sum_x^2)) %>% 
      ungroup()
    
    ###############################################.
    # Calculate the standard error
    # the formula can be found on http://stattrek.com/regression/slope-confidence-interval.aspx?Tutorial=AP .
    
  data_depr_sii <- data_depr_sii %>% mutate( # first calculate the predicted y values.
        yi = -slope_coef * relative_rank + intercept,
      # calculate the difference between the predicted y and the real y and square the results.
        y_diff = (rate - yi)^2,
      # calculate the difference between relative_rank (x values) and the mean of relative_rank.
        x_diff = (relative_rank - mean_x)^2) %>% 
      group_by(code, year, quint_type) %>% 
      # sum the y difference and the x difference.
      mutate(sum_ydiff = sum(y_diff),
             sum_xdiff = sum(x_diff)) %>%  ungroup() %>% 
      # calculate the standard error. 
      mutate(se = sqrt(sum_ydiff / (5-2) ) / sqrt(sum_xdiff))
    
    ###############################################.
    # Calculate the confidence intervals
  data_depr_sii <- data_depr_sii %>% mutate(
    #use a t-value of 3.15245 as degrees of freedom = n-2 = 3.
      lowci_slope = slope_coef - (3.18245*se),
      upci_slope = slope_coef + (3.18245*se)) 
    
    #if lower confidence interval below zero/goes through zero this means there could be no association between the quintiles
    #as the tool can't cope with negative confidence intervals.  
  data_depr_sii$lowci_slope <- ifelse(data_depr_sii$lowci_slope < 0 , 0, data_depr_sii$lowci_slope)  

  ##################################################.
  ##  Part 7 - Calculate RII, Population attributable risk and range  ----
  ##################################################.
  #Calculating RII
  data_depr <- data_depr_sii %>% 
    mutate(rii = slope_coef / overall_rate,
           lowci_rii = lowci_slope / overall_rate,
           upci_rii = upci_slope / overall_rate)

  #Calculation PAR
  #Formula here: https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf
  # Should we match with the least deprived quintile or with the lowest value?
  #Most and least deprived rates
  most_depr <- data_depr %>% filter(quintile == "1") %>% 
    select(code, year, quint_type, rate) %>% rename(most_rate = rate)
  least_depr <- data_depr %>% filter(quintile == "5") %>% 
    select(code, year, quint_type, rate) %>% rename(least_rate = rate)

  data_depr <- left_join(data_depr, most_depr, by = c("code", "year", "quint_type"))
  data_depr <- left_join(data_depr, least_depr, by = c("code", "year", "quint_type"))
  
  data_depr <- data_depr %>%  group_by(code, year, quint_type) %>%
    mutate(par_rr = (rate/least_rate - 1) * proportion_pop,
           par = sum(par_rr)/(sum(par_rr) + 1) * 100,
           # Calculate ranges 
           abs_range = most_rate - least_rate,
           rel_range = most_rate / least_rate
    ) %>% ungroup()
  
  #Joining with totals.
  data_depr_match <- data_depr %>% filter(quintile == "3") %>% 
    select(code, year, quint_type, slope_coef, upci_slope, lowci_slope, rii, lowci_rii, upci_rii,
           par, abs_range, rel_range)
  
  data_depr_totals <- left_join(data_depr_totals, data_depr_match, 
                                by = c("code", "year", "quint_type"))
  
  data_depr <- bind_rows(data_depr, data_depr_totals) 
  
  ##################################################.
  ##  Part 8 - Adding time labels and indicator info ----
  ##################################################.
  #Indicator code
  data_depr <- data_depr %>% mutate(ind_id = ind_id) %>% 
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), funs(replace(., is.na(.), 0))) 
  data_depr$lowci <- ifelse(data_depr$lowci<0, 0, data_depr$lowci)
  
  #Calendar aggregate years
  if (year_type == "calendar" & time_fix>0){ 
    data_depr <- data_depr %>% 
      mutate(trend_axis=paste0(year-time_fix, "-", year+time_fix),  
             def_period=paste0(year-time_fix, " to ", year+time_fix, " ", year_type, 
                               " years; ", time_agg, "-year aggregates")) 
    #Calendar single years
  } else if (year_type == "calendar" & time_fix==0){ 
    data_depr <- data_depr %>% 
      mutate(trend_axis=year,  
             def_period=paste0(year, " ", year_type, " year"))
    #Financial single years
  } else if (year_type %in% c("financial", "school") & time_fix == 0){
    
    data_depr <- data_depr %>% 
      mutate(trend_axis = paste0(year, "/", substr(year+1, 3, 4)),
             def_period = paste0(trend_axis, " ", year_type, " year"))
    #Financial aggregate years
  } else if (year_type %in% c("financial", "school") & time_fix>0){
    
    data_depr <- data_depr %>% 
      mutate(trend_axis = paste0(year-time_fix, "/", substr(year, 3, 4),
                                 "-", year + time_fix, "/",
                                 substr((year+time_fix+1), 3, 4)),
             def_period = paste0(year-time_fix, "/", substr(year, 3, 4), " to ",
                                 year + time_fix, "/", substr((year+time_fix+1), 3, 4),
                                 " ", year_type, " years; ", time_agg, 
                                 "-year aggregates"))
  }
  
  saveRDS(data_depr, paste0(data_folder, "Temporary/", filename, "_final.rds"))
  
  #Preparing data for Shiny tool
  data_shiny <- data_depr %>% 
    select(c(code, quintile, quint_type, ind_id, year, numerator, rate, lowci, upci, 
             slope_coef, upci_slope, lowci_slope, rii, lowci_rii, upci_rii,
             par, abs_range, rel_range, def_period, trend_axis))
  
  #Saving file
  saveRDS(data_shiny, file = paste0(data_folder, "Shiny Data/", filename, "_ineq.rds"))

  ##################################################.
  ##  Part 9 - Checking results ----
  ##################################################.
  #Selecting Health boards and Scotland for latest year in dataset
  ggplot(data=(data_shiny %>% subset((substr(code, 1, 3)=="S08" | code=="S00000001") 
                               & year==max(year) & quintile == "Total" & quint_type == "sc_quin")), 
         aes(code, rate) ) +
    geom_point(stat = "identity") +
    geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
}

##END
