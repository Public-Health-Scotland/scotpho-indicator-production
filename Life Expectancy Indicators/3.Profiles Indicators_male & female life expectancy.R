# ScotPHO Profiles Indicators- Male & Female Life expectancy

# Before this script can be run the R script 'LE_generate small area estimates.R' must be run (script should be found within same folder)
# This script will generate IZ 2011 and HSCP locality level life expectancy figures which are joined to larger geography LE estimates.
# Depending on if new SAPE files are available you may wish tp update  the profiles indicator for only larger geographies - this will result
# in longer time trend for indicators for the larger geographies than the IZ/HSCP locality data.

# ScotPHO health and wellbeing profile includes 2 Life Expectancy indicators (Male and Female)
# ScotPHO calculate life expectancy for IZ level geographies which are 5 year aggregates, using 85+ as max age band.
# We use NRS life expectancy estimates for Scotland, NHS Board and LA level which are 3 year aggregates and use 90+ as max age band.

# Using NRS LE estimates avoids confusion caused by having different LE to those that many users expect (ie the national statistic)
# New annual NRS life expectancy figures are sourced from NRS website or by request to NRS and manually formatted then added to previous years data file.
# Smaller geographies cannot use the exact same methodology because 3 years of data insufficient to produce robust LE estimates for small geogrpahies.


## Part 1 - Read in LE at IZ & HSCP locality level data file (data prepared using LE for small areas.R)
## Part 2 - Read Life Expectancy figures sourced from NRS (& create HSCP partnership dummy data)
## Part 3 - Format & prepare file for ScotPHO Profiles Shiny tool

###############################################.
## Packages/Filepaths ----
###############################################.

source("Life Expectancy Indicators/1.Functions_life_expectancy.R")
source("functions/main_analysis.R") #doesn't use the functions, but quick way of getting packages and folders

##########################################################################################.
## Part 1 - read in file with life expectancy figures at IZ  ----
##########################################################################################.

# Set run name - this will dictate which iteration of IZ level life expectancy source data to use
# if no IZ/Hscp locality update is available then reuse last run.
run_name="2001to2023 IZ&Locality LE(85+)_20241127"

le0_data<- readRDS(paste0(output_network,"4_Intermediate Zone LE (annual)/",run_name,"_life expectancy at birth.rds"))

#LE estimates only provided where population & death counts considered robust
#remove any estimates where geography has population less than 5000 or less than 40 deaths over 5 year period
#add time period labels
le0_iz_profiles <- le0_data %>%
  subset(pop>=5000 & deaths>=40 & is.finite(LEx)) %>% #Including only cases where pop >=5000 and total deaths >= 40 or where LEx can't be calulated (likely do to deaths>pop in some age groups eg 85+)
  mutate(def_period=time_period,
         year=as.numeric(substr(time_period,1,4))+2, # year should be mid-point - this forumla assumes 5 year time period
         trend_axis=paste0(as.character(year)," Midpoint")) %>%
  select (geography,sex_grp, year, LEx,lci,uci,def_period,trend_axis) %>%
  rename(code=geography)

#close all life expectancy data - the difference in row numbers will be those areas excluded because of small numbers
#it can be useful to know which areas were excluded if customers ask why the figure is missing
rm(le0_data)

##########################################################################################.
## Part 2 - Read in Life Expectancy estimates from NRS at Scotland, NHS Board and LA level ----
## Note these estimates are the official national statistics and are 3 year rolling averages.
## Figures orignally supplied by population & migration team at NRS but issues reported in July 2025 meant indicator rerun using
## data sourced from Statistics.gov.scot which may contain rebased estimates calculated following 2022census
##########################################################################################. 

# NRS_data <- read_csv(paste0(source_network,"NRS LE data with CI 2001 to 2023_corrected LA codes.csv")) %>%
#   arrange(code, time_period, sex_grp)

# open data from statistics.gov
statsgov_data <- read_csv(paste0(source_network,"statistics_gov 2001 to 2023.csv")) |>
  arrange(code, time, sex_grp) |>
  mutate(time_period=paste0(substr(time,1,4)," to ",substr(time,6,9))) |>
  select(-time)

#format fields
NRS_data <- statsgov_data %>%
  mutate(def_period=paste0(time_period," (3 year aggregate)"),
         year=as.numeric(substr(time_period,1,4))+1,# year should be mid-point of time series - this formula assumes 3 year time period
         trend_axis=paste0(as.character(year)," Midpoint"),
         sex=as.character(sex_grp)) %>%
  select(-sex_grp, -time_period) |> #-geography) %>%
  rename(sex_grp=sex)

## Create HSCP geography data file from council figures 
#  One HSCP (Stirling & Clacks) is formed of two council areas combined
#  This HSCP will be excluded as it is not possible to average LE, NRS do not produce HSCP geography estimates and ScotPHO cannot exactly replicate NRS methodology which uses modelling

profiles_tool_geo_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds") |>
  select (ca2019,hscp2019) |>
  unique() |>
  filter(hscp2019 != "S37000005") # exclude Clackmannanshire & Stirling HSCP since we can't combine LE estimates from two distinct councils (2 councils but only 1 HSCP)

# duplicate the LA adata and call it HSCP data 
hscp_data <- NRS_data %>%
  subset(substr(NRS_data$code, 1, 3) =="S12") 

#match to HSCP lookup so we can convert LA life expectancy as if it was for HSCP (note clacks & stirling will be missing)
hscp_data <- left_join(hscp_data, profiles_tool_geo_lookup ,by=c('code' = 'ca2019'))
hscp_data <- hscp_data |>
  filter(!is.na(hscp2019))

#bind data so it will include Scotland, NHS board, Local Authority and HSCP level
le_data <- bind_rows(NRS_data, hscp_data)


##########################################################################################.
## Part3 - Generate data files for Shiny profile tool ----
##########################################################################################.

all_le_data<- bind_rows(le0_iz_profiles, NRS_data) %>%
  mutate(ind_id= case_when(sex_grp=="1" ~ "20101", #male indicator number
                           sex_grp=="2" ~ "20102", #female indicator number
                           TRUE ~"x"),
         code = case_when(code=="S92000003"~"S00000001", TRUE~as.character(code))) %>%
  arrange(ind_id, year, code) %>% 
  mutate(numerator="") %>% #insert column where numerator would ordinarily be - there is no numerator for LE
  select(code, ind_id, year, numerator, rate=lex,lowci,upci, def_period, trend_axis)

## Male life expectancy file
profile_data_male_LE <- all_le_data %>% subset(ind_id=="20101") 

#save files to profiles indicator data to be checked folder on network
write_csv(profile_data_male_LE, file = paste0(shiny_network, "life_expectancy_male_shiny.csv"))
write_rds(profile_data_male_LE, file = paste0(shiny_network, "life_expectancy_male_shiny.rds"))

# This indicator script doesn't use analysis functions but indicator checking report can still be called:
run_qa(filename="life_expectancy_male", type="main",check_extras = "S12000005")


## Female life expectancy file
profile_data_female_LE <- all_le_data %>% subset(ind_id=="20102") 

write_csv(profile_data_female_LE, file = paste0(shiny_network, "life_expectancy_female_shiny.csv"))
write_rds(profile_data_female_LE, file = paste0(shiny_network, "life_expectancy_female_shiny.rds"))

# This indicator script doesn't use analysis functions but indicator checking report can still be called:
run_qa(filename="life_expectancy_female", type="main",check_extras = "S12000005")