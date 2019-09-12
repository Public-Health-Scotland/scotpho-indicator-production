#Code to produce indicator data both for the old OPT and the Shiny OPT

#TODO
#Make changes in CI calculation (Andy P suggestion)
#Non standard evaluation techniques might help to simplify a lot the syntax
#How to simplify epop, lookup?
#Dealing with incomplete aggregation periods needs improvement, package roll might be
# the solution but it doesn't work in the server
# Do automatic checks at the end (e.g. CA and HSCP are the same)

## HOW TO USE THESE FUNCTIONS
# FUNCTION ONE: ANALYZE_FIRST
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# geography - what is the base geography of the raw file: council or datazone2011
# adp - To calculate the data for ADP level as well change it to TRUE, default is false.
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent),
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.

# FUNCTION TWO: ANALYZE_SECOND
# filename -  Name of the formatted file the function reads without the "_formatted.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
#           percentage with finite population correction factor (perc_pcf)
# time_agg - Aggregation period used expressed in year, e.g. 3 
# ind_id - indicator code/number
# year_type - calendar, financial, school or annual snapshot. This last one should
#           be used like "Month snapshot" e.g. "August snapshot"
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# pop - Only for crude rate cases that need finite population correction factor. Reference population.

###############################################.
## Packages and filepaths ----
###############################################.
library(foreign) #to read SPSS data.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages
library(readr) # writing csv's
library(odbc) # for reading oracle databases
library(readxl) #for reading excel

# Varies filepaths depending on if using server or not and what organisation uses it.
if (exists("organisation") == TRUE) { #Health Scotland
  if (organisation == "HS") { 
    data_folder <- "X:/ScotPHO Profiles/Data/" 
    lookups <- "X:/ScotPHO Profiles/Data/Lookups/"
  }
} else  { #ISD, first server then desktop
  if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
    data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
    lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
  } else {
    data_folder <- "//stats/ScotPHO/Profiles/Data/"
    lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
  }
}

###############################################.
## Analysis function one ----
###############################################.
analyze_first <- function(filename, geography = c("council", "datazone11"), 
                          measure = c("percent", "crude", "stdrate"), time_agg, 
                          pop = NULL, yearstart, yearend, epop_age = NULL, adp = FALSE) {
  
  ##################################################.
  ## Part 1 - Read in raw data and add in lookup info ----
  ##################################################.
    
    #Reading file
    data_indicator <- readRDS(paste0(data_folder, "Prepared Data/", filename, "_raw.rds")) %>% 
      subset(year >= yearstart) #selecting only years of interest
    
    # Lookup with geographical information.
    if(geography == "datazone11") {
      geo_lookup <- readRDS(paste0(lookups, 'Geography/DataZone11_All_Geographies_Lookup.rds')) 
      
      ## Matching with geography lookup.
      data_indicator <- left_join(x=data_indicator, y=geo_lookup, c("datazone" = "datazone2011")) %>% 
        mutate(scotland = as.factor("S00000001")) # adding Scotland
      
    } else if (geography == "council") {
      geo_lookup <- readRDS(paste0(lookups, 'Geography/DataZone11_All_Geographies_Lookup.rds')) %>% 
        select(ca2019, hb2019) %>% distinct %>%  rename(ca = ca2019, hb = hb2019)
      
      ## Matching with geography lookup.
      data_indicator <- left_join(x=data_indicator, y=geo_lookup, c("ca")) %>% 
        mutate(scotland = as.factor("S00000001")) # adding Scotland
      
    }

    ##################################################.
    ## Part 2 - Aggregate up to get figures for each area. ----
    ##################################################.
   #Need to include different measures and geography (non standard evaluation could help to make it easy)
    if (measure == "stdrate" & geography == "datazone11" ) {
    data_indicator <- data_indicator %>% gather(geolevel, code, intzone2011:scotland) %>% 
      ungroup() %>% select(-c(geolevel, datazone)) %>% 
      group_by(code, year, sex_grp, age_grp) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()

    } else if (measure == "stdrate" & geography == "council") {
      data_indicator <- data_indicator %>% gather(geolevel, code, ca, hb, scotland) %>% 
        select(-c(geolevel)) %>% 
        group_by(code, year, sex_grp, age_grp) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
    } else if (measure %in% c("crude", "percent") & geography == "datazone11" ) {
      data_indicator <- data_indicator %>% gather(geolevel, code, intzone2011:scotland) %>% 
        ungroup() %>% select(-c(geolevel, datazone)) %>% 
        group_by(code, year) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
    } else if (measure %in% c("crude", "percent") & geography == "council") {
      data_indicator <- data_indicator %>% gather(geolevel, code, ca, hb, scotland) %>% 
        select(-c(geolevel)) %>% 
        group_by(code, year) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()
    }

    # Matching with population lookup
    if (!is.null(pop)){
      if (measure == "stdrate") {
        pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'_SR.rds')) %>% 
          subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
          mutate_at(c("sex_grp", "age_grp", "code"), as.factor)
        
        data_indicator$age_grp <-as.factor(data_indicator$age_grp)
        data_indicator <- full_join(x=data_indicator, y=pop_lookup, # Matching population with data
                      by = c("year", "code", "sex_grp", "age_grp"))
        
      } else if (measure %in% c("crude", "percent")){
        
        pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
          subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
          mutate(code = as.factor(code))
        
        # Matching population with data
        data_indicator <- full_join(x=data_indicator, y=pop_lookup, by = c("year", "code"))
        
      }
    }
    #selecting only years of interest
    data_indicator <- data_indicator %>% subset(year >= yearstart & year <= yearend)
    
    data_indicator$numerator[is.na(data_indicator$numerator)] <- 0 # Converting NA's to 0s
    
    # Excludes ADP level if not wanted
    if (adp == FALSE) {
      data_indicator <- data_indicator %>% subset(substr(code,1,3) != "S11")
    }
    
    ##################################################.
    ##  Part 3 - Aggregate by required time periods ----
    ##################################################.
    #The moving average is left aligned this means that the year variable will not 
    #reflect the right time. This is the way to fix it depending on what is the 
    #aggregation period.
    time_fix <- case_when(time_agg < 3 ~ 0, time_agg == 3 ~ 1,
                          time_agg == 5 ~ 2, TRUE ~ NA_real_)
    
    ## Calculating moving average for denominator and numerator
    ## Data needs to be sorted to calculate the right figures
    if (measure == "stdrate") {
    data_indicator <- data_indicator %>% 
      arrange(code, sex_grp, age_grp, year) %>% 
      group_by(code, sex_grp, age_grp) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  #excluding NA rows 
      mutate(year = year-time_fix) %>%  # year minus to adjust to center year
      ungroup()
    
    } else if (measure %in% c("crude", "percent")) {
      data_indicator <- data_indicator %>% 
        arrange(code, year) %>% 
        group_by(code) %>%
        mutate(numerator = roll_meanr(numerator, time_agg), 
               denominator = roll_meanr(denominator, time_agg)) %>% 
        subset(!is.na(denominator)) %>%  #excluding NA rows 
        mutate(year = as.numeric(year)-time_fix) %>%  # year minus to adjust to center year
        ungroup()
    }
    
    #Creating variable with European population 2016 depending on age cut
    if (!is.null(pop) & measure == "stdrate"){
    if (epop_age == "normal") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                        "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                        "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                        "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                        "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "16+") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                          "4"=4400, "5"=6000, "6"=6000, "7"= 6500, "8"=7000, 
                          "9"=7000, "10"=7000, "11"=7000, "12"=6500, "13"=6000, 
                          "14"=5500, "15"=5000, "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "<16") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=1100)
    } else if (epop_age == "0to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "11to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                          "3"=4400, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "15to25") {
      data_indicator$epop <- recode(as.character(data_indicator$age_grp), 
                          "4"=5500, "5"=6000, "6"= 1200)
      }
    }
    
    analysis_first_result <<- data_indicator
    
    saveRDS(data_indicator, file=paste0(data_folder, "Temporary/", filename, "_formatted.rds"))
}

##################################################.
##  Second analysis function ----
##################################################.
analyze_second <- function(filename, measure = c("percent", "crude", "perc_pcf", "stdrate"), 
                           time_agg,  ind_id, year_type = c("calendar", "financial", "school"),
                           crude_rate = 0, epop_total = NULL, pop = NULL) {   
  
  ##################################################.
  ##  Part 4 - Create rates or percentages ----
  ##################################################.
  #Used for trend labels and finite population  correction factor
  time_fix <- case_when(time_agg < 3 ~ 0, time_agg == 3 ~ 1,
                        time_agg == 5 ~ 2, TRUE ~ NA_real_)
  
  data_indicator <- readRDS(file=paste0(data_folder, "Temporary/", filename, "_formatted.rds"))
    
  if (measure == "stdrate"){ #European Age-sex standardized rates
    #Calculating individual easr and variance
    data_indicator <- data_indicator %>%
      mutate(easr_first = numerator*epop/denominator, #easr population
             var_dsr = (numerator*epop^2)/denominator^2) %>%  # variance
      # Converting Infinites to NA and NA's to 0s to allow proper functioning
      na_if(Inf) %>% #Caused by a denominator of 0 in an age group with numerator >0
      mutate_at(c("easr_first", "var_dsr"), funs(replace(., is.na(.), 0)))  
      
    # aggregating by year, code and time
    data_indicator <- data_indicator %>% subset(select= -c(age_grp, sex_grp)) %>%
      group_by(year, code) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()

    #Calculating rates and confidence intervals
    data_indicator <- data_indicator %>%
      mutate(epop_total = epop_total,  # Total EPOP population
             easr = easr_first/epop_total, # easr calculation
             o_lower = numerator*(1-(1/(9*numerator)) - (1.96/(3*sqrt(numerator))))^3,  # Lower CI
             o_upper = (numerator+1)*(1-(1/(9*(numerator+1))) + (1.96/(3*sqrt(numerator+1))))^3, # Upper CI
             var = (1/epop_total^2)*var_dsr, #variance
             rate = easr*100000,  # rate calculation
             lowci = (easr+sqrt(var/numerator)*(o_lower - numerator))*100000, #Lower CI final step
             upci = (easr+sqrt(var/numerator)*(o_upper - numerator))*100000) #Upper CI final step

    data_indicator <- data_indicator %>% subset(select = -c(var, easr, epop_total, o_lower, o_upper, 
                    easr_first, epop, var_dsr, denominator)) #deleting variables
                
    } else if (measure == "percent"){ #Percentage
      data_indicator <- data_indicator %>%
        mutate(rate = numerator/denominator*100,
               lowci=(2*numerator+1.96*1.96-1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100))) 
               / (2*(denominator+1.96*1.96))*100,
               upci=(2*numerator+1.96*1.96+1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100)))
               /  (2*(denominator+1.96*1.96))*100,
        # if over 100 or under 0, set to these values as it is a percentage
               upci = case_when(upci>100 ~ 100, TRUE ~ upci),
               lowci = case_when(lowci<0 ~ 0, TRUE ~ lowci))

    } else if (measure == "crude"){ #Crude rates
      data_indicator <- data_indicator %>%
        mutate(rate = numerator/denominator*crude_rate,
               o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
               o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
               lowci = o_lower/(denominator)*crude_rate,
               upci = o_upper/(denominator)*crude_rate) %>% 
        subset(select = -c(o_upper, o_lower))
      
    } else if (measure == "perc_pcf") {
      
      #Bringing reference population and aggregating by the required time period
      pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
        group_by(code) %>%
        mutate(denominator = roll_meanr(denominator, time_agg),
               year = as.numeric(year)-time_fix) %>% # year minus to adjust to center year
        subset(!is.na(denominator)) %>%  #excluding NA rows 
        ungroup() %>% rename(est_pop = denominator)
      
      # Matching population with data
      data_indicator <- left_join(x=data_indicator, y=pop_lookup, by = c("year", "code")) %>% 
#if no est_pop then assume is 0
         mutate(est_pop = case_when(is.na(est_pop) ~ 0, TRUE ~est_pop),
# Calculate the finite population correction factor.
# Read more about it here: http://www.statisticshowto.com/finite-population-correction-factor/.
          pcf = case_when(est_pop > 0 ~ sqrt((est_pop-denominator)/(est_pop-1)),
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
               lowci = case_when(lowci<0 ~ 0, TRUE ~ lowci))

    }
    
  ##################################################.
  ##  Part 5 - Adding time labels and indicator info ----
  ##################################################.
    #Indicator code 
    data_indicator <- data_indicator %>% mutate(ind_id = ind_id) %>% 
      # fill in missing values and if any have negative lower CI change that to zero.
      mutate_at(c("rate", "lowci", "upci"), funs(replace(., is.na(.), 0))) 
    data_indicator$lowci <- ifelse(data_indicator$lowci<0, 0, data_indicator$lowci)
    
    # add in the definition period and trend axis labels
      #Calendar aggregate years
    if (year_type == "calendar" & time_fix>0){ 
      data_indicator <- data_indicator %>% 
        mutate(trend_axis=paste0(year-time_fix, "-", year+time_fix),  
              def_period=paste0(year-time_fix, " to ", year+time_fix, " ", year_type, 
                                " years; ", time_agg, "-year aggregates")) 
      #Calendar two-year aggregates 
    } else if (year_type == "calendar" & time_agg==2){ 
      data_indicator <- data_indicator %>% 
        mutate(trend_axis=paste0(year-1, "-", year),  
               def_period=paste0(year-1, " to ", year, " ", year_type, 
                                 " years; 2-year aggregates")) 
      #Calendar single years
    } else if (year_type == "calendar" & time_fix==0 & time_agg!=2){ 
      data_indicator <- data_indicator %>% 
        mutate(trend_axis=year,  
               def_period=paste0(year, " ", year_type, " year"))
      #Annual snapshot 
    } else if (grepl("snapshot", year_type, fixed=TRUE) ==  TRUE) { 
      data_indicator <- data_indicator %>% 
        mutate(trend_axis=year,  
               def_period=paste0(year, " ", year_type))
      #Financial single years
    } else if (year_type %in% c("financial", "school") & time_fix == 0 & time_agg!=2){
      
      data_indicator <- data_indicator %>% 
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
  
      data_indicator <- data_indicator %>% 
        mutate(trend_axis = paste0(year-time_fix, "/", substr(year, 3, 4),
                                   "-", year + time_fix, "/",
                                   substr((year+time_fix+1), 3, 4)),
               def_period = paste0(year-time_fix, "/", substr(year, 3, 4), " to ",
                                   year + time_fix, "/", substr((year+time_fix+1), 3, 4),
                                  " ", year_type, " years; ", time_agg, 
                                  "-year aggregates"))
    }
  
    saveRDS(data_indicator, paste0(data_folder, "Temporary/", filename, "_final.rds"))
    
    #Preparing data for Shiny tool
    data_shiny <- data_indicator %>% select(c(code, ind_id, year, numerator, rate, lowci,
                                    upci, def_period, trend_axis))
    #Including both rds and csv file for now
    saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/", filename, "_shiny.rds"))
    write_csv(data_shiny, path = paste0(data_folder, "Data to be checked/", filename, "_shiny.csv"))
  
    #Making final dataset available outside the function
    final_result <<- data_indicator
    
    ##################################################.
    ##  Part 6 - Checking results ----
    ##################################################.
    #Selecting Health boards and Scotland for latest year in dataset
    ggplot(data = data_indicator %>% filter((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                              & year== max(year)), aes(code, rate) ) +
      geom_point(stat = "identity") +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
    
    
    } 

