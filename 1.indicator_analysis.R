#Code to produce indicator data both for the old OPT and the Shiny OPT

#TODO
#Test for 2 year aggregations
#Need to include with finite population correction factor
#Add ADP level? It might require changing the geography lookups. Not easyy.
#Make changes in CI calculation (Andy P suggestion)
#Non standard evaluation techniques might help to simplify a lot the syntax
#How to simplify epop, lookup?
#Dealing with incomplete aggregation periods needs improvement, package roll might be
# the solution but it doesn't work in the server
#What about deprivation? A bit too much
#Need to mount non-confi ScotPHO in server to be able to use it with source from a non-confi area

## HOW TO USE THESE FUNCTIONS
# FUNCTION ONE: ANALYZE_FIRST
# filename -  Name of the raw file the function reads without the "_raw.sav" at the end
# geography - what is the base geography of the raw file: council or datazone2011
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
# time_agg - Aggregation period used expressed in year, e.g. 3
# pop - Name of the population file. Only used for those that need a denominator.  
# yearstart - Start of the period you want to run an analysis for
# yearend -  End of the period you want to run an analysis for
# epop_age - Type of european population to use: 16+, <16, 0to25, 11to25, 15to25. 
#            Only used for standardize rates.

# FUNCTION TWO: ANALYZE_SECOND
# filename -  Name of the formatted file the function reads without the "_formatted.sav" at the end
# measure - crude rate (crude), standardized rate(stdrate), percentage (percent)
# time_agg - Aggregation period used expressed in year, e.g. 3
# ind_id - indicator code/number
# year_type - calendar, financial or school
# crude rate - Only for crude rate cases. Population the rate refers to, e.g. 1000 = crude rate per 1000 people
# epop_total - the total european population for the ages needed. For all ages the Epop_total = 200000 (100000 per sex group)
# min_opt - Only relevant for old OPT. First serial OPT number to include.
# profile - Only relevant for old OPT. Profile initials that will be combined with opt number.

###############################################.
## Packages and filepaths ----
###############################################.
library(foreign) #to read SPSS data.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages
library(readr) # writing csv's

if (server_desktop == "server") {
  data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
} else if (server_desktop == "desktop") {
  data_folder <- "//stats/ScotPHO/Profiles/Data/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
}

###############################################.
## Analysis function one ----
###############################################.

#203 lines in R vs 778 (74% less ) 1 script vs 5
#With 260 lines in R I replaced the 8 major macros we used, plus the two auxilliary ones
analyze_first <- function(filename, geography = c("council", "datazone11"), 
                          measure = c("percent", "crude", "stdrate"), time_agg, 
                          pop = NULL, yearstart, yearend, epop_age = NULL) {
  
  ##################################################.
  ## Part 1 - Read in raw data and add in lookup info ----
  ##################################################.
    
    #Reading file
    data <- readRDS(paste0(data_folder, "Prepared Data/", filename, "_raw.rds")) %>% 
      subset(year >= yearstart) #selecting only years of interes
    
    # Lookup with geographical information.
    if(geography == "datazone11") {
    geo_lookup <- readRDS(paste0(lookups, 'Geography/DataZone11_All_Geographies_Lookup.rds')) 
    
    ## Matching with geography lookup.
    data <- left_join(x=data, y=geo_lookup, c("datazone" = "datazone2011")) 

    } else if (geography == "council") {
      geo_lookup <- read.spss( '/conf/linkage/output/lookups/geography/other_ref_files/CA_HB2014.sav', 
                               to.data.frame=TRUE, use.value.labels=FALSE) %>% 
        mutate(scotland = "S00000001") %>% 
        setNames(tolower(names(.)))   #variables to lower case
      
      ## Matching with geography lookup.
      data <- left_join(x=data, y=geo_lookup, c("ca")) 
      
    }
    
    ##################################################.
    ## Part 2 - Aggregate up to get figures for each area. ----
    ##################################################.
   #Need to include different measures and geography (non standard evaluation could help to make it easy)
    data <- data %>% mutate(scotland = as.factor("S00000001")) 

    if (measure == "stdrate" & geography == "datazone11" ) {
    data <- data %>% gather(geolevel, code, intzone2011:scotland) %>% 
      ungroup() %>% select(-c(geolevel, datazone)) %>% 
      group_by(code, year, sex_grp, age_grp) %>% summarise_all(funs(sum), na.rm =T) 

    } else if (measure == "stdrate" & geography == "council") {
      data <- data %>% gather(geolevel, code, ca, hb2014, scotland) %>% 
        select(-c(geolevel)) %>% 
        group_by(code, year, sex_grp, age_grp) %>% summarise_all(funs(sum), na.rm =T) 
    } else if (measure %in% c("crude", "percent") & geography == "datazone11" ) {
      data <- data %>% gather(geolevel, code, intzone2011:scotland) %>% 
        ungroup() %>% select(-c(geolevel, datazone)) %>% 
        group_by(code, year) %>% summarise_all(funs(sum), na.rm =T) 
    } else if (measure %in% c("crude", "percent") & geography == "council") {
      data <- data %>% gather(geolevel, code, ca, hb2014, scotland) %>% 
        select(-c(geolevel)) %>% 
        group_by(code, year) %>% summarise_all(funs(sum), na.rm =T) 
    }
    
    data <- data %>% ungroup() 

    # Matching with population lookup
    if (!is.null(pop)){
      if(measure == "stdrate") {
        pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'_SR.rds')) %>% 
          subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
          mutate_at(c("sex_grp", "age_grp", "code"), as.factor)
        
        data$age_grp <-as.factor(data$age_grp)
        data <- full_join(x=data, y=pop_lookup, # Matching population with data
                      by = c("year", "code", "sex_grp", "age_grp"))
        
      } else if (measure %in% c("crude", "percent")){
        
        pop_lookup <- readRDS(paste0(lookups, "Population/", pop,'.rds')) %>% 
          subset(year >= yearstart) %>% #Reading population file and selecting only for 2011 onwards
          mutate(code = as.factor(code))
        
        # Matching population with data
        data <- full_join(x=data, y=pop_lookup, by = c("year", "code"))
        
      }
    }
    #selecting only years of interest
    data <- data %>% subset(year >= yearstart & year <= yearend)
    
    data$numerator[is.na(data$numerator)] <- 0 # Converting NA's to 0s
    ##################################################.
    ##  Part 3 - Aggregate by required time periods ----
    ##################################################.
    #The moving average is left aligned this means that the year variable will not 
    #reflect the right time. This is the way to fix it depending on what is the 
    #aggregation period.
    time_fix <- ifelse(time_agg < 3, 0, ifelse(time_agg == 3, 1,
                              ifelse(time_agg == 5, 2, "ERROR")))
    
    ## Calculating moving average for denominator and numerator
    ## Data needs to be sorted to calculate the right figures
    if (measure == "stdrate") {
    data <- data %>% 
      arrange(code, sex_grp, age_grp, year) %>% 
      group_by(code, sex_grp, age_grp) %>%
      mutate(numerator = roll_meanr(numerator, time_agg), 
             denominator = roll_meanr(denominator, time_agg)) %>% 
      subset(!is.na(denominator)) %>%  #excluding NA rows 
      mutate(year = year-time_fix) %>%  # year minus to adjust to center year
      ungroup()
    
    } else if (measure %in% c("crude", "percent")) {
      data <- data %>% 
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
      data$epop <- recode(as.character(data$age_grp), 
                        "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                        "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                        "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                        "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "16+") {
      data$epop <- recode(as.character(data$age_grp), 
                          "4"=4400, "5"=6000, "6"=6000, "7"= 6500, "8"=7000, 
                          "9"=7000, "10"=7000, "11"=7000, "12"=6500, "13"=6000, 
                          "14"=5500, "15"=5000, "16"= 4000, "17"=2500, "18"=1500, "19"=1000)
    } else if (epop_age == "<16") {
      data$epop <- recode(as.character(data$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=1100)
    } else if (epop_age == "0to25") {
      data$epop <- recode(as.character(data$age_grp), 
                          "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "11to25") {
      data$epop <- recode(as.character(data$age_grp), 
                          "3"=4400, "4"=5500, "5"=6000, "6"=1200)
    } else if (epop_age == "15to25") {
      data$epop <- recode(as.character(data$age_grp), 
                          "4"=5500, "5"=6000, "6"= 1200)
      }
    }
    
    saveRDS(data, file=paste0(data_folder, "Temporal/", filename, "_formatted.rds"))
}
##################################################.
##  Second analysis function ----
##################################################.
analyze_second <- function(filename, measure = c("percent", "crude", "stdrate"), 
                           time_agg,  ind_id, year_type = c("calendar", "financial", "school"),
                           min_opt, crude_rate = 0, epop_total = NULL, profile) {   
  
  ##################################################.
  ##  Part 4 - Create rates or percentages ----
  ##################################################.
  data <- readRDS(data, file=paste0(data_folder, "Temporal/", filename, "_formatted.rds"))
    
  if (measure == "stdrate"){ #European Age-sex standardized rates
    #Calculating individual easr and variance
    data <- data %>%
      mutate(easr_first = numerator*epop/denominator, #easr population
             var_dsr = (numerator*epop^2)/denominator^2) %>%  # variance
      # Converting NA's to 0s to allow proper functioning
      mutate_at(c("easr_first", "var_dsr"), funs(replace(., is.na(.), 0)))  
      
    # aggregating by year, code and time
    data <- data %>% subset(select= -c(age_grp, sex_grp)) %>%
      group_by(year, code) %>% summarise_all(funs(sum), na.rm =T) %>% ungroup()

    #Calculating rates and confidence intervals
    data <- data %>%
      mutate(epop_total = epop_total,  # Total EPOP population
             easr = easr_first/epop_total, # easr calculation
             o_lower = numerator*(1-(1/(9*numerator)) - (1.96/(3*sqrt(numerator))))^3,  # Lower CI
             o_upper = (numerator+1)*(1-(1/(9*(numerator+1))) + (1.96/(3*sqrt(numerator+1))))^3, # Upper CI
             var = (1/epop_total^2)*var_dsr, #variance
             rate = easr*100000,  # rate calculation
             lowci = (easr+sqrt(var/numerator)*(o_lower - numerator))*100000, #Lower CI final step
             upci = (easr+sqrt(var/numerator)*(o_upper - numerator))*100000) #Upper CI final step

    data <- data %>% subset(select = -c(var, easr, epop_total, o_lower, o_upper, 
                    easr_first, epop, var_dsr, denominator)) #deleting variables
                
    } else if (measure == "percent"){ #Percentage
      data <- data %>%
        mutate(rate = numerator/denominator*100,
               lowci=(2*numerator+1.96*1.96-1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100))) 
               / (2*(denominator+1.96*1.96))*100,
               upci=(2*numerator+1.96*1.96+1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100)))
               /  (2*(denominator+1.96*1.96))*100.)

    } else if (measure == "crude"){ #Crude rates
      data <- data %>%
        mutate(rate = numerator/denominator*crude_rate,
               o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
               o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
               lowci = o_lower/(denominator)*crude_rate,
               upci = o_upper/(denominator)*crude_rate) %>% 
        subset(select = -c(o_upper, o_lower))
      
    }
    
  ##################################################.
  ##  Part 5 - Adding time labels and indicator info ----
  ##################################################.
    #Indicator code and OPT Number
    data <- data %>% mutate(ind_id = ind_id, 
             uni_id = paste0(profile, (seq_len(nrow(data)) + min_opt - 1))) %>% 
      # fill in missing values and if any have negative lower CI change that to zero.
      mutate_at(c("rate", "lowci", "upci"), funs(replace(., is.na(.), 0))) 
    data$lowci <- ifelse(data$lowci<0, 0, data$lowci)

    # add in the definition period and trend axis labels
    time_fix <- ifelse(time_agg < 3, 0, ifelse(time_agg == 3, 1,
                                               ifelse(time_agg == 5, 2, "ERROR")))
    
      #Calendar aggregate years
    if (year_type == "calendar" & time_fix>0){ 
      data <- data %>% 
        mutate(trend_axis=paste0(year-time_fix, "-", year+time_fix),  
              def_period=paste0(year-time_fix, " to ", year+time_fix, " ", year_type, 
                                " years; ", time_agg, "-year aggregates")) 
      #Calendar single years
    } else if (year_type == "calendar" & time_fix==0){ 
      data <- data %>% 
        mutate(trend_axis=year,  
               def_period=paste0(year, " ", year_type, " year"))
      #Financial single years
    } else if (year_type %in% c("financial", "school") & time_fix == 0){
      
      data <- data %>% 
        mutate(trend_axis = paste0(year, "/", substr(year+1, 3, 4)),
               def_period = paste0(trend_axis, " ", year_type, " year"))
      #Financial aggregate years
    } else if (year_type %in% c("financial", "school") & time_fix>0){
  
      data <- data %>% 
        mutate(trend_axis = paste0(year-time_fix, "/", substr(year, 3, 4),
                                   "-", year + time_fix, "/",
                                   substr((year+time_fix+1), 3, 4)),
               def_period = paste0(year-time_fix, "/", substr(year, 3, 4), " to ",
                                   year + time_fix, "/", substr((year+time_fix+1), 3, 4),
                                  " ", year_type, " years; ", time_agg, 
                                  "-year aggregates"))
    }
  
    saveRDS(data, paste0(data_folder, "Temporal/", filename, "_final.rds"))
    
    #Preparing data for Shiny tool
    data_shiny <- data %>% select(c(code, ind_id, year, numerator, rate, lowci,
                                    upci, def_period, trend_axis))
    #Including both rds and csv file for now
    saveRDS(data_shiny, file = paste0(data_folder, "Shiny Data/", filename, "_shiny.rds"))
    write_csv(data_shiny, path = paste0(data_folder, "Shiny Data/", filename, "_shiny.csv"))
  
    #Preparing data for old OPT tool
    #Excluding HSC locality and partnership.
    data <- data %>% subset(!(substr(code, 1, 3) %in% c('S37', 'S99')))
    
    # Reorder by column index: uni_id code ind_id year numerator rate lowci upci def_period trend_axis.
    data <- data[c(8,2,7,1,3,4,5,6,10,9)] 
    
    write_csv(data, path = paste0(data_folder, "OPT Data/", filename, "_OPT.csv"),
              col_names = FALSE)
    
    ##################################################.
    ##  Part 6 - Checking results ----
    ##################################################.
    #Selecting Health boards and Scotland for latest year in dataset
    ggplot(data=(data %>% subset((substr(code, 1, 3)=="S08" | code=="S00000001") & year==2014 )), aes(code, rate) ) +
      geom_point(stat = "identity") +
      geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5)
    
    } 

