#Functions to create data for indicators by deprivation quintile (SIMD).
# analyze_deprivation() = for data at datazone level
# analyze_deprivation_aggregated() = for data already aggregated to SIMD level. 

#TODO:
#Instead of showing RII and SII use translation as percentage
#Think about how interpretation (high better or worse) migh affect SII/RII/PAR calculation
#Monte Carlo simulation for CIs - Lumme et al. 2015
#simplify easr ci calculation
#CIs for PAR

#NOTES:
# When not all quintiles present in an area, RII and SII won't be shown in the tool
# I am including negative CI's as this tool can cope with them
# What to do with original RII values keep or just use the interpreations

## HOW TO USE THIS FUNCTION
# File is expected at datazone 2011 level with only these other variables:
# year, code, numerator. denominator, age_grp, sex_grp could be required depending
# on the nature of your data.
# Arguments of the function:
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
#           percentage with finite population correction factor (perc_pcf)
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.
# pop_pcf - Only for crude rate cases that need finite population correction factor. Reference population.
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
# time_agg - Aggregation period used expressed in year, e.g. 3
# ind_id - indicator code/number
# year_type - calendar, financial, school, survey or annual snapshot. This last one should
#           be used like "Month snapshot" e.g. "August snapshot"
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# qa - parameter can be true/false - governs if inequalities indicator QA should be run - by default checks are set to run

###############################################.
## Packages and filepaths ----
###############################################.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages 
library(broom) #for the models
library(purrr) #for the models
library(binom)
library(rmarkdown) #for data quality checking
library(shiny) #for data quality checking
library(flextable) # for output tables
library(plotly) #for data quality checking

# Detects if session is using Posit Workbench/server or RStudio and sets commonly used filepaths accordingly
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) { #if session on server
  data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
  
} else { #else assumes using desktop
  data_folder <- "//stats/ScotPHO/Profiles/Data/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
}

##################################################.
##  Analysis function ----
##################################################.
analyze_deprivation <- function(filename, yearstart, yearend, time_agg, 
                                measure = c("percent", "crude", "stdrate", "perc_pcf"),
                                epop_age = NULL, epop_total = NULL, pop = NULL, 
                                pop_pcf = NULL, crude_rate, ind_id, year_type, qa=TRUE) {
  
  ###############################################.
  ## Part 1 - Read in raw data and add in lookup info ----
  ###############################################.
  # read in raw data. 
  data_depr <- readRDS(paste0(data_folder, "Prepared Data/" ,filename, "_raw.rds")) %>% 
    mutate(year = as.numeric(year))
  
  # read in deprivation lookup. 
  depr_lookup <- readRDS(paste0(lookups, "Geography/deprivation_geography.rds"))

  #merging them and adding Scotland
  data_depr <- left_join(data_depr, depr_lookup, by = c("datazone", "year"))  %>% 
    mutate(scotland = "S00000001") #Adding Scotland
  
  ###############################################.
  ## Part 2 - Aggregate up to get figures for each area. ----
  ###############################################.
  #This function groups the data for the variables selected and then aggregates it
  #It works for the different types of quintiles and for all measures
  create_quintile_data <- function(group_vars, geo, quint) {
    if ("denominator" %in% names(data_depr)) { #if denominator included
      
      if (measure %in% c("crude", "percent", "perc_pcf")) {
        data_depr <- data_depr %>% select(numerator, denominator, {{group_vars}} ) %>% 
          group_by_at(group_vars) 

      } else if (measure == "stdrate") {
        data_depr <- data_depr %>% 
          select(numerator, denominator, age_grp, sex_grp, {{group_vars}}) %>% 
          group_by_at(c("age_grp", "sex_grp", group_vars ) )
      }
      
    } else if (!("denominator" %in% names(data_depr))) { #if denominator not included
      
      if (measure %in% c("crude", "percent", "perc_pcf")) {
        data_depr <- data_depr %>% select(numerator, {{group_vars}}) %>% 
          group_by_at(group_vars)
      } else if (measure == "stdrate") {
        data_depr <- data_depr %>% select(numerator, age_grp, sex_grp, {{group_vars}} ) %>% 
          group_by_at(c("age_grp", "sex_grp", group_vars ) )
      }
    }
    
    # Aggegatingrenaming and creating quint_type common to all cases
    data_depr %>% summarise_all(sum, na.rm = T) %>% 
      rename(code = {{geo}}, quintile = {{quint}}) %>% ungroup() %>% 
      mutate(quint_type = quint)
    
  }

  data_depr <- rbind( 
    #Scotland 
    create_quintile_data(geo = "scotland", quint = "sc_quin", 
                         group_vars =  c("scotland", "year", "sc_quin")),
    #Health boards using national quintiles
    create_quintile_data(geo = "hb", quint = "sc_quin", 
                         group_vars =  c("hb", "year", "sc_quin")),
    #Health boards using health board quintiles
    create_quintile_data(geo = "hb", quint = "hb_quin", 
                         group_vars =  c("hb", "year", "hb_quin")),
    #Council area using national quintiles
    create_quintile_data(geo = "ca", quint = "sc_quin", 
                         group_vars =  c("ca", "year", "sc_quin")),
    #Council area using council quintiles
    create_quintile_data(geo = "ca", quint = "ca_quin",
                         group_vars =  c("ca", "year", "ca_quin")))
  
  #Creating combined totals
    if (measure %in% c("crude", "percent", "perc_pcf")) {
      data_depr_totals <- data_depr %>% group_by(code, year, quint_type)
    
    } else if (measure == "stdrate") {
      data_depr_totals <- data_depr %>% group_by(code, year, age_grp, sex_grp, quint_type) 
    }
  
data_depr_totals <- data_depr_totals %>% summarise_all(sum, na.rm = T) %>% 
  mutate(quintile = "Total") %>% ungroup()

  #And merging them with the rest of the data
  data_depr <- rbind(data_depr, data_depr_totals) %>% 
    # To exclude cases without quintile or code. Caused when raw files have 
    # records withoout datazone needed for get the right Scotland level
    filter(!(is.na(code) | is.na(quintile)))
  
  ###############################################.
  ## Part 3 - Matching with population lookup----
  ###############################################.
  # Matching with population lookup
  if (!is.null(pop)){
    if(measure == "stdrate") {
      pop_depr_lookup <- readRDS(paste0(lookups, "Population/", pop,'_SR.rds')) %>% 
        # subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
        mutate_at(c("sex_grp", "code", "age_grp"), as.factor)
      
      data_depr <- right_join(x=data_depr, y=pop_depr_lookup, # Matching population with data
                        by = c("year", "code", "sex_grp", "age_grp", "quintile", "quint_type"))
      
    } else if (measure %in% c("crude", "percent", "perc_pcf")){
      
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
    
  } else if (measure %in% c("crude", "percent", "perc_pcf")) {
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
      # Converting Infinites to NA and NA's to 0s to allow proper functioning
      mutate(easr_first = ifelse(is.infinite(easr_first), NA, easr_first), # replace inf (caused by a denominator of 0 in an age group with numerator >0) with NA
             var_dsr = ifelse(is.infinite(var_dsr), NA, var_dsr)) %>%
      mutate_at(c("easr_first", "var_dsr"), ~replace(., is.na(.), 0)) #replace na with 0
    
    # aggregating by year, code and time
    data_depr <- data_depr %>% subset(select= -c(age_grp, sex_grp)) %>%
      group_by(year, code, quintile, quint_type) %>% summarise_all(sum) %>% ungroup()
    
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
             /  (2*(denominator+1.96*1.96))*100,
             # if over 100 or under 0, set to these values as it is a percentage
             upci = case_when(upci>100 ~ 100, TRUE ~ upci),
             lowci = case_when(lowci<0 ~ 0, TRUE ~ lowci))
    
  } else if (measure == "crude"){ #Crude rates
    data_depr <- data_depr %>%
      mutate(rate = numerator/denominator*crude_rate,
             o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
             o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
             lowci = o_lower/(denominator)*crude_rate,
             upci = o_upper/(denominator)*crude_rate) %>% 
      subset(select = -c(o_upper, o_lower))
    
  } else if (measure == "perc_pcf") {
    
    #Bringing reference population and aggregating by the required time period
    pop_lookup <- readRDS(paste0(lookups, "Population/", pop_pcf,'.rds')) %>% 
      group_by(code) %>%
      mutate(denominator = roll_meanr(denominator, time_agg),
             year = as.numeric(year)-time_fix) %>% # year minus to adjust to center year
      subset(!is.na(denominator)) %>%  #excluding NA rows 
      ungroup() %>% rename(est_pop = denominator)
    
    # Matching population with data
    data_depr <- left_join(x=data_depr, y=pop_lookup, 
                           by = c("year", "code", "quintile", "quint_type")) %>% 
      # Calculate the finite population correction factor.
      # Read more about it here: http://www.statisticshowto.com/finite-population-correction-factor/.
      mutate(pcf = case_when(est_pop > 0 ~ sqrt((est_pop-denominator)/(est_pop-1)),
                             # If no population estimate available resorting to calculate it the normal way, so pcf 1.
                             est_pop == 0 ~ 1),
             #compute the percentage and confidence intervals.
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
             upci = case_when(upci>100 ~ 100, TRUE ~ upci),
             lowci = case_when(lowci<0 ~ 0, TRUE ~ lowci)) %>% 
      select(-pcf, -est_pop, -ci_interval)
  }
  
  
  # ##################################################.
  # ##  Part 6 - Create SII and RII ----
  # ##################################################.
  
  #call function to generate measures of inequality (function appears at bottom of this script)
  data_depr <- data_depr %>% inequality_measures()

  ##################################################.
  ##  Part 7 - Adding time labels and indicator info ----
  ##################################################.
  #Indicator code
  data_depr <- data_depr %>% mutate(ind_id = ind_id) %>% 
    # fill in missing values and if any have negative lower CI change that to zero.
    mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) 
  data_depr$lowci <- ifelse(data_depr$lowci<0, 0, data_depr$lowci)
  
  #Calendar aggregate years
  if (year_type == "calendar" & time_fix>0){ 
    data_depr <- data_depr %>% 
      mutate(trend_axis=paste0(year-time_fix, "-", year+time_fix),  
             def_period=paste0(year-time_fix, " to ", year+time_fix, " ", year_type, 
                               " years; ", time_agg, "-year aggregates")) 
    #Calendar two-year aggregates 
  } else if (year_type == "calendar" & time_agg==2){ 
    data_indicator <- data_indicator %>% 
      mutate(trend_axis=paste0(year-1, "-", year),  
             def_period=paste0(year-1, " to ", year, " ", year_type, 
                               " years; 2-year aggregates")) 
    #Calendar single years or single survey years
  } else if ((year_type == "calendar" & time_fix==0 & time_agg!=2) |
             year_type == "survey"){ 
    data_depr <- data_depr %>% 
      mutate(trend_axis=year,  
             def_period=paste0(year, " ", year_type, " year"))
    #Annual snapshot 
  } else if (grepl("snapshot", year_type, fixed=TRUE) ==  TRUE) { 
    data_indicator <- data_indicator %>% 
      mutate(trend_axis=year,  
             def_period=paste0(year, " ", year_type))
    #Financial single years
  } else if (year_type %in% c("financial", "school") & time_fix == 0 & time_agg!=2){
    
    data_depr <- data_depr %>% 
      mutate(trend_axis = paste0(year, "/", substr(year+1, 3, 4)),
             def_period = paste0(trend_axis, " ", year_type, " year"))
    #Financial two-year aggregates 
  } else if (year_type %in% c("financial", "school") & time_agg==2){ 
    data_indicator <- data_indicator %>% 
      mutate(trend_axis=paste0(year-1,  "/", substr(year, 3, 4),
                               "-", year, "/",substr((year+1), 3, 4)),  
             def_period = paste0(year-1, "/", substr(year, 3, 4), " to ",
                                 year, "/", substr((year+1), 3, 4),
                                 " ", year_type, " years; 2-year aggregates"))
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
    select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
              least_rate, par_rr, count))

  #Saving file
  saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/", filename, "_ineq.rds"))

  #Making final dataset available outside the function
  final_result <<- data_shiny
  
  ##################################################.
  ##  Part 8 - Checking results ----
  ##################################################.
  
  if (qa == FALSE) { #if you don't want to run full data quality checks set qa=false then only scotland chart will be produced
    #Selecting Health boards and Scotland for latest year in dataset
    ggplot(data=(data_shiny %>% subset((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                       & year==max(year) & quintile == "Total" & quint_type == "sc_quin")), 
           aes(code, rate) ) +
      geom_point(stat = "identity") +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
    
  } else  { # if qa set to true (default behaviour) then inequalities rmd report will run
    
    run_ineq_qa(filename={{filename}}#,old_file="default"
                )} 

}

############################################################.
## Function : Run Inequality Indicator Quality Assurance ----
############################################################.
# Function below runs an rmarkdown report (.Rmd) that runs through standard checks of indicator data
# The report requires one mandatory parameter 
# filename - required - determines which indicator_data file is used for checking

run_ineq_qa <- function(filename){
  run("4.Data Quality Checks_inequalities indicators.Rmd")
}  



##################################################.
## Inequality measures function ---- 
## generating SII, RII, PAF and Ranges.
## this function separate from main body of function as life expectancy 
#   inequality indicators only require this part
##################################################.

inequality_measures <- function(dataset){

#################################################.
##  SII/RII  ----
##################################################.
  
  #Splitting into two files: one with quintiles for SII and one without to keep the total values
  data_depr_sii <- dataset %>% group_by(code, year, quint_type) %>% 
    mutate(overall_rate = rate[quintile == "Total"]) %>% 
    filter(quintile != "Total") %>% 
    #This variables are used for SII, RII and PAR calculation
    mutate(total_pop = sum(denominator), # calculate the total population for each area (without SIMD).
           proportion_pop = denominator/total_pop) %>% # proportion of the population in each SIMD out of the total population. )
    ungroup()
  
  data_depr_totals <- dataset %>% filter(quintile == "Total")
  
  ###############################################.
  # Calculate the regression coefficient
  #Formula from: https://www.scotpho.org.uk/comparative-health/health-inequalities-tools/archive/triple-i-and-hits/
  #https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf
  #The dataframe sii_model will have a column for sii, lower ci and upper ci for each
  # geography, year and quintile type
  sii_model <- data_depr_sii %>% group_by(code, year, quint_type) %>% 
    #Checking that all quintiles are present, if not excluding as we are not showing
    #RII and SII for those. Calculations would need to be adjusted and thought well if we wanted to include them
    mutate(count= n()) %>% filter(count == 5) %>% 
    #This first part is to adjust rate and denominator with the population weights
    mutate(cumulative_pro = cumsum(proportion_pop),  # cumulative proportion population for each area
           relative_rank = case_when(
             quintile == "1" ~ 0.5*proportion_pop,
             quintile != "1" ~ lag(cumulative_pro) + 0.5*proportion_pop),
           sqr_proportion_pop = sqrt(proportion_pop), #square root of the proportion of the population in each SIMD
           relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
           rate_sqr_proppop = sqr_proportion_pop * rate) %>% #rate based on population weights
    nest() %>% #creating one column called data with all the variables not in the grouping
    # Calculating linear regression for all the groups, then formatting the results
    # and calculating the confidence intervals
    mutate(model = map(data, ~ lm(rate_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
           #extracting sii from model, a bit fiddly but it works
           sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
      #     cis = map(model, confint_tidy) # deprecated. next two lines do the same thing
           conf.low = as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[1])),
           conf.high = as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[2]))) %>% #calculating confidence intervals
    ungroup() %>% 
    mutate(lowci_sii = -1 * conf.high, #fixing interpretation
           upci_sii = -1 * conf.low) %>% 
    select(-conf.low, -conf.high, -model, -data) %>% #taking out results as not needed anymore
    mutate_at(c("sii", "lowci_sii", "upci_sii"), ~replace(., is.na(.), NA_real_)) #recoding NAs

 
  #Merging sii with main data set
  data_depr <- left_join(data_depr_sii, sii_model, by = c("code", "year", "quint_type"))
  
  #Calculating RII
  data_depr <- data_depr %>% mutate(rii = sii / overall_rate,
                                    lowci_rii = lowci_sii / overall_rate,
                                    upci_rii = upci_sii / overall_rate,
                                    #Transforming RII into %. This way is interpreted as "most deprived areas are
                                    # xx% above the average" For example: Cancer mortality rate is around 55% higher 
                                    # in deprived areas relative to the mean rate in the population
                                    rii_int = rii * 0.5 *100,
                                    lowci_rii_int = lowci_rii * 0.5 *100,
                                    upci_rii_int = upci_rii * 0.5 *100)

##################################################.
##  PAF Population attributable risk and range  ----
##################################################.

#Calculation PAR
#Formula here: https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf
# https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
#Adding columns for Most and least deprived rates
most_depr <- data_depr %>% filter(quintile == "1") %>% 
  select(code, year, quint_type, rate) %>% rename(most_rate = rate)

least_depr <- data_depr %>% filter(quintile == "5") %>% 
  select(code, year, quint_type, rate) %>% rename(least_rate = rate)

data_depr <- left_join(data_depr, most_depr, by = c("code", "year", "quint_type"))
data_depr <- left_join(data_depr, least_depr, by = c("code", "year", "quint_type"))

data_depr <- data_depr %>%  group_by(code, year, quint_type) %>%
  mutate(#calculating PAR. PAR of incomplete groups to NA
    #CI calculation missing, this can help https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.2779
    #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
    par_rr = (rate/least_rate - 1) * proportion_pop,
    count= n(),
    par = case_when(count != 5 ~ NA_real_,
                    count == 5 ~ sum(par_rr)/(sum(par_rr) + 1) * 100),
    # Calculate ranges 
    abs_range = most_rate - least_rate,
    rel_range = most_rate / least_rate) %>% ungroup()

#Joining with totals.
#dataframe with the unique values for the different inequality measures
data_depr_match <- data_depr %>% 
  select(code, year, quint_type, sii, upci_sii, lowci_sii, rii, lowci_rii, upci_rii,
         rii_int, lowci_rii_int, upci_rii_int, par, abs_range, rel_range) %>% 
  unique()

data_depr_totals <- left_join(data_depr_totals, data_depr_match, 
                              by = c("code", "year", "quint_type"))

data_depr <- bind_rows(data_depr, data_depr_totals) 
data_depr <<- data_depr
} #end sii/rii/paf/range function



## HOW TO USE THIS FUNCTION
# 
# 
# Arguments of the function:
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# required fields: 
  # "year"       "rate"       "lowci"      "upci"       "numerator"  "def_period"
  # "trend_axis" "ind_id"     "code"       "quintile"   "quint_type"
  # quintile is in format "1" to "5" and "Total".
# pop - Name of the population file. 
# ind_id - indicator code/number
# qa - parameter can be true/false - governs if inequalities indicator QA should be run 
# lookup - a lookup list for looking up the indicator name (for file saving purposes). ind_id is string in this list.
# e.g. of lookup creation: 
# ind_lookup <- list("30022"="influence_local_decisions","30043"="managing_well_financially")


#' Function: analyze_deprivation_aggregated()
#' ##############################################
#' A deprivation analysis function for pre-aggregated data.
#' Takes a file with multiple indicators in it that are already aggregated to SIMD level.
#' Currently only takes Scotland-level data: amend if to be used for HB/CA level.
#' NEEDS A TEST TO ENSURE EITHER IND_NAME OR A LOOKUP HAS BEEN PROVIDED
#'
#' @param filename 
#' Name of the raw file the function reads without the "_raw.sav" at the end
#'  required fields: "year"       "rate"       "lowci"      "upci"       "numerator"  "def_period"
#'                  "trend_axis" "ind_id"     "code"       "quintile"   "quint_type"
#'  quintile is in format "1" to "5" and "Total".
#' @param pop Name of the population file.
#' @param ind indicator code/number
#' @param lookup 
#' a lookup list for looking up the indicator name (for file saving purposes). ind_id is string in this list.
#'  e.g. of lookup creation: 
#' ind_lookup <- list("30022"="influence_local_decisions","30043"="managing_well_financially")
#' @param qa parameter can be true/false - governs if inequalities indicator QA should be run 
#'
#' @return prepared data file saved to paste0("Data to be checked/", indicator_name, "_ineq.rds")
#' 
analyze_deprivation_aggregated <- function(filename, # the prepared data, without _raw.rds suffix
                                           pop, # what population file to use for denominators
                                           ind, # the ind_id
                                           ind_name = NULL, # user provides either the indicator name (if input file has one indicator), or a lookup (if contains multiple indicators)
                                           lookup = NULL, # lookup for ind_id to ind_name (indicator name used if not provided)
                                           qa = FALSE) {
  
  ###############################################.
  ## Read in data----
  ###############################################.
  
  # read in raw data. 
  data_depr <- readRDS(paste0(data_folder, "Prepared Data/" ,filename, "_raw.rds")) %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(ind_id == ind) 
  
  indicator_name <- ifelse(is.null(ind_name),
                           recode(as.character(ind_id), !!!ind_lookup, .default = as.character(NA)),
                           ind_name)
  
  #indicator_name <- recode(as.character(ind), !!!ind_lookup, .default = as.character(NA))

  yearstart = min(data_depr$year)
  yearend = max(data_depr$year)
  
  ###############################################.
  ## Matching with population lookup----
  ###############################################.
  
  # Matching with population lookup (denominator required for SIMD analysis)
  pop_depr_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
    subset(year >= yearstart & year <= yearend) #Reading population file and selecting the right year range
  
  # Matching population with data
  data_depr <- right_join(x=data_depr, y=pop_depr_lookup, 
                          by = c("year", "code", "quintile", "quint_type")) %>%
    filter(code=="S00000001") # these data are just Scotland level. Change this if HB/CA data included.
  
  #selecting only years of interest
  data_depr <- data_depr %>% subset(year >= yearstart & year <= yearend) %>%
    filter(!is.na(rate)) # some data biennial, so need this fix
  
  data_depr$numerator[is.na(data_depr$numerator)] <- 0 # Converting NAs to 0s
  
  ##################################################.
  ##  Create SII and RII ----
  ##################################################.
  
  #call function to generate measures of inequality 
  data_depr <- data_depr %>% inequality_measures()
  
  saveRDS(data_depr, paste0(data_folder, "Temporary/", indicator_name, "_final.rds"))
  
  #Preparing data for Shiny tool
  data_shiny <- data_depr %>% 
    select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
              least_rate, par_rr, count))
  
  #Saving file
  saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/", indicator_name, "_ineq.rds"))
  
  #Making final dataset available outside the function
  final_result <<- data_shiny
  
  ##################################################.
  ##  Checking results ----
  ##################################################.
  
  if (qa == FALSE) { #if you don't want to run full data quality checks set qa=false then only scotland chart will be produced
    #Selecting Health boards and Scotland for latest year in dataset
    ggplot(data=(data_shiny %>% subset((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                       & year==max(year) & quintile == "Total" & quint_type == "sc_quin")), 
           aes(code, rate) ) +
      geom_point(stat = "identity") +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
    
  } else  { # if qa set to true (default behaviour) then inequalities rmd report will run
    
    run_ineq_qa(filename={{filename}}
    )} 
  
}

##END
