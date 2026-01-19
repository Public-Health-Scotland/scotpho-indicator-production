########################################################################################################################.
## This script contains some packages, filepaths and functions required when calculating small are life expectancy.

###############################################.
## Packages and filepaths ----
###############################################.

library(odbc)     #connections to SMRA
library(readr)    #reading in file
library(tidyr)    #for melt - reshaping of data
library(dplyr)    #data manipulations 
library(RcppRoll) #for rolling sums
library(readr)    #reading in file


#change filepaths depending on whether using PWB or Rstudio desktop
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  temp_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/temporary/"
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/"
  output_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Output Data/"
  shiny_network <- "/PHI_conf/ScotPHO/Profiles/Data/Data to be checked/"



###############################################.
## Create age groups ----
###############################################.

# Function to age bands (<1 years, 1-4 years, then 5 year age bands to 85 years plus)
create_agegroups_85 <- function(df) {
  df <- df %>% mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  
    age >= 10 & age <=14 ~ 4, age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, 
    age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8, age >= 35 & age <=39 ~ 9, 
    age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13, age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,
    age >= 70 & age <=74 ~ 16, age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18, 
    age >= 85 ~ 19))}

# Function to age bands (<1 years, 1-4 years, then 5 year age bands to 90 years plus)
create_agegroups_90 <- function(df) {
  df <- df %>% mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,  
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 & age <=89 ~ 19,age >= 90 ~ 20))}


###############################################.
## LE_function ----
###############################################.

# Parameters:

# max_agegrp - Set to max age group used in calculating LE 85 or 90.
# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of folder to save any output files to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)


LE_function <- function (hle=FALSE, max_agegrp, run_name, fp_deaths, fp_pop, fp_output, yearstart, yearend, time_agg) {
  
  # open population rds file 
  data_deaths <- readRDS(paste0(temp_network,fp_deaths,".rds"))
  
  # open deaths rds file
  data_pop <- readRDS(paste0(temp_network,fp_pop,".rds"))
  
  # join population and deaths files prepared in main program 
  all_data<- left_join(data_pop, data_deaths, by = c("year", "sex_grp", "age_grp","geography")) %>%
    subset(year>=yearstart & year<=yearend)
  
  all_data$deaths[is.na(all_data$deaths)] <- 0 # Converting where deaths are NA's to 0s (mostly required for IZ level)
  
  if(hle==TRUE){
    # open self-assessed health rds file (optional only require for HLE)
    data_sah <- readRDS(paste0(temp_network,"data_sah.rds"))
    
    all_data <- left_join(all_data, data_sah, by=c("year","sex_grp","age_grp"))
  }
  
  #create label for time period calculated - uses time_agg token supplied.
  if(time_agg == 1){
    all_data <- all_data %>%
      mutate(time_period=paste0(as.character(year)," single year estimate"))
  }else if(time_agg != 1){
    all_data <- all_data %>%
      mutate(time_period = paste0(as.character(year-(time_agg-1))," to ",as.character(year)," (",time_agg," year aggregate)"))
  }
  
  #aggreate population & deaths according to time_aggregation token supplied
  all_data <- all_data %>%
    arrange(age_grp,sex_grp,year) %>%
    group_by(age_grp,sex_grp, geography) %>% 
    mutate(totpop = roll_sum(x = pop, time_agg, align = "right", fill = NA),
           totdeaths = roll_sum(x = deaths, time_agg, align = "right", fill = NA)) %>%
    subset(!is.na(totpop)) %>% #excluding NA rows 
    select (-pop, -deaths, -year) %>% #drop unaggregated pop and deaths column
    arrange(geography, time_period,sex_grp,age_grp) %>%
    ungroup ()
  
  #save out RDS for checking purposes
  #saveRDS(all_data, file=paste0(temp_network,run_name,'-le_raw.rds'))
  
  if (max_agegrp == 85){  #Generate life table where max age group 85+ years (19 age bands)
    lifetable_data <- all_data %>%
      group_by(sex_grp, time_period, geography) %>%
      mutate(mx = totdeaths / totpop,              # death rate
             n = c(1,4,rep(5,17)),                 # age intervals (<1=1, 1-4 = 4, 5-85 = 5 years)
             ax = c(0.1, rep(0.5,18)) ,            # ax = Fraction of the age interval lived by those who die in the interval - chiang methodolgy sets ax to 0.1 for first age band and 0.5 for all others
             qx = case_when(age_grp == 19 ~ 1,     # qx = Conditional probability individual entering age band will die
                            TRUE ~ n*mx/(1+n*(1-ax)*mx)),
             px= 1-qx,                             # px = Conditional probability individual entering age band will survive
             radix = 100000 ,                      # set radix (total imaginary cohort population)
             pre_Ix = cumprod(1-qx)*radix ,        # first step in calculating life table pop - second step (to calculate Ix) uses a lag function which only seems to work when lag value already defined.
             Ix= case_when(age_grp == 1 ~ radix,   # Ix =  Life table population (usually 100,000 at birth)
                           TRUE ~ lag(pre_Ix)),
             dx = Ix*qx,                           # dx =  Number of life table deaths
             Lx = case_when(age_grp == 19 ~ Ix/mx,           # Lx = Total number of years lived during the time period. Last age group treated differently as this is wider/open ended age band
                            TRUE ~  n*(lead(Ix,1)+(ax*dx))),
             Tx = case_when(age_grp == 19 ~Lx, TRUE ~ rev(cumsum(rev(Lx)))),
             LEx =Tx/Ix,                          # Life expectancy
             var_qx=(n^2*mx*(1-ax*n*mx))/(totpop*(1+(1-ax)*n*mx)^3),  #variance of qx (prob of death) 1984 method
             se1=case_when(age_grp==19 ~0, TRUE ~ (Ix^2)*(((1-ax)*n+lead(LEx,1))^2)*var_qx), # deriving standard error - requires 2 steps (se1 & se2)
             se2=rev(cumsum(rev(se1))),
             var_ex=se2/(Ix^2),     # variance of life expectancy
             se=sqrt(var_ex),       # standard error of life expectancy
             lci=LEx-(1.96*se),     # 95% lower confidence interval
             uci=LEx+(1.96*se)) %>%     # 95% upper confidence interval
      ungroup ()
    
  }else if(max_agegrp==90){   # Generate life table where max age group 90+ years (20 age bands)
    lifetable_data <- all_data %>%
      group_by(sex_grp, time_period, geography) %>% 
      mutate(mx = totdeaths / totpop,              # death rate
             n = c(1,4,rep(5,17),2/mx[20]),        # age intervals (<1=1, 1-4 = 4, 5-85 = 5 years, 90+ = 2/Mx)
             ax = c(0.1, rep(0.5,19)) ,            # ax = Fraction of the age interval lived by those who die in the interval - chiang methodolgy sets ax to 0.1 for first age band and 0.5 for all others 
             qx = case_when(age_grp == 20 ~ 1,     # qx = Conditional probability individual entering age band will die
                            TRUE ~ n*mx/(1+n*(1-ax)*mx)),
             px= 1-qx,                             # px = Conditional probability individual entering age band will survive
             radix = 100000 ,                      # set radix (total imaginary cohort population)
             pre_Ix = cumprod(1-qx)*radix ,        # first step in calculating life table pop - second step (to calculate Ix) uses a lag function which only seems to work when lag value already defined.
             Ix= case_when(age_grp == 1 ~ radix,   # Ix =  Life table population (usually 100,000 at birth)
                           TRUE ~ lag(pre_Ix)),
             dx = Ix*qx,                           # dx =  Number of life table deaths
             Lx = case_when(age_grp == 20 ~ n*ax*dx,           # Lx = Total number of years lived during the time period. Last age group treated differently as this is wider/open ended age band
                            TRUE ~  n*(lead(Ix,1)+(ax*dx))),
             Tx = rev(cumsum(rev(Lx))),           # Total # years lived beyond entry age
             LEx =Tx/Ix,                          # Life expectancy
             var_qx=(n^2*mx*(1-ax*n*mx))/(totpop*(1+(1-ax)*n*mx)^3),  #variance of qx (prob of death) 1984 method
             se1=case_when(age_grp==20~0, TRUE ~ (Ix^2)*(((1-ax)*n+lead(LEx,1))^2)*var_qx), # deriving standard error - requires 2 steps (se1 & se2)
             se2=rev(cumsum(rev(se1))),
             var_ex=se2/(Ix^2),     # variance of life expectancy
             se=sqrt(var_ex),       # standard error of life expectancy
             lci=LEx-(1.96*se),     # 95% lower confidence interval
             uci=LEx+(1.96*se)) %>%     # 95% upper confidence interval
      ungroup ()
  }
  
  if(hle==TRUE){
    # Healthy life expectancy calculations
    lifetable_data <- lifetable_data %>%
      group_by(sex_grp, time_period, geography) %>% 
      mutate(p_notgood_x=not_healthy/totsah,                  # proportion of ageband with not good health
             goodhealth_Lx=(1-p_notgood_x)*Lx,                # person years lived in good health
             THx = rev(cumsum(rev(goodhealth_Lx))),           # total years lived as healthy from year x
             HLEx = THx/Ix,                                   # healthy life expectancy
             Nx= totsahunw,                                   # number in survey in age interval
             hle_ci1= p_notgood_x*(1-p_notgood_x)/Nx,         # stage1 in calculating HLE variance
             hle_ci2= Lx^2*hle_ci1,                           # stage2
             hle_3=rev(cumsum(rev(hle_ci2))),                 # stage3
             var_hle=hle_3/Ix^2,                              # variance for HLE
             se_hle=sqrt(var_hle),                            # standard error for HLE
             hle_lci=HLEx-(1.96*se_hle),                      # 95% lower confidence interval   
             hle_uci=HLEx+(1.96*se_hle))                      # 95% upper confidence interval
    
    # Summary table for LE and HLE at birth
    le0_data<-  lifetable_data %>% 
      group_by(geography ,time_period, sex_grp) %>%  #generate total populations & deaths to permit testing of conditions such as total pop above a certin size
      mutate(pop=sum(totpop), deaths=sum(totdeaths)) %>%
      ungroup() %>%
      subset(age_grp==1) %>% #generate table for life expectancy at birth with confidence intervals.
      select(geography,time_period,sex_grp, pop, deaths,LEx, lci, uci,HLEx,hle_lci, hle_uci ) %>%
      arrange(geography, sex_grp,time_period)
    
  }else if(hle==FALSE){
    le0_data<-  lifetable_data %>% 
      group_by(geography,time_period, sex_grp) %>%
      mutate(pop=sum(totpop), deaths=sum(totdeaths)) %>%
      ungroup() %>%
      subset(age_grp==1) %>% #generate table for life expectancy at birth with confidence intervals.
      select(geography,time_period,sex_grp,pop,deaths, LEx, lci, uci ) %>%
      arrange(geography, sex_grp,time_period)
  }
  
  #saveRDS(lifetable_data, file=paste0(output_network,fp_output,"/",run_name,'_full life table.rds'))  
  #Save out life expectancy at birth data
  saveRDS(le0_data, file=paste0(output_network,fp_output,"/",run_name, '_life expectancy at birth.rds'))
  
  # Making R datasets available outside the function
  all_data <<- all_data  # all_data should be population and deaths data added
  final_lifetable <<- lifetable_data #lifetable contains invidual age groups plus all stages of calculation
  final_le0_data <<- le0_data #le0 just a summary of life expectancy at birth data.
  
  
}