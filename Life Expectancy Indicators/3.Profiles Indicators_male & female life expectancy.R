# ScotPHO Profiles Indicators- Male & Female Life expectancy

# Before this script can be run the R script 'LE_generate small area estimates.R' must be run (script should be found within same folder)
# This script will generate IZ 2011 and HSCP locality level life expectancy figures which are joined to 

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
source("1.indicator_analysis.R") #doesn't use the functions, but quick way of getting packages and folders

##########################################################################################.
## Part 1 - read in file with life expectancy figures at IZ  ----
##########################################################################################.

# Set run name - this will dictate which iteration of IZ level life expectancy source data to use
run_name="2001to2021 IZ&Locality LE(85+)_20210927"

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
## Figures orginally supplied by population & migration team at NRS but in future may be available online.
##########################################################################################. 

NRS_data <- read_csv(paste0(source_network,"NRS LE data with CI 2001 to 2021.csv")) %>%
  arrange(code, time_period, sex_grp)

NRS_data <- NRS_data %>%
  mutate(def_period=paste0(time_period," (3 year aggregate)"),
         year=as.numeric(substr(time_period,1,4))+1,# year should be mid-point of time series - this forumla assumes 3 year time period
         trend_axis=paste0(as.character(year)," Midpoint"),
         sex=as.character(sex_grp)) %>%
  select(-sex_grp, -geography, -time_period) %>%
  rename(sex_grp=sex)

## Create HSCP geography data file from council figures 
#  One HSCP (Stirling & Clacks) is formed of two council areas combined
#  This HSCP will be excluded as it is not possible to average LE, NRS do not produce HSCP geography estimates and ScotPHO cannot exactly replicate NRS methodology which uses modelling

lookup_hscp <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/HSCPdictionary.rds") %>%
  mutate(areaname=case_when(
    areaname == "Edinburgh" ~ 'City of Edinburgh', #names of partnership and ca don't quite match
    areaname == "Western Isles" ~ "Na h-Eileanan Siar",#names of partnership and ca don't quite match
    TRUE ~ areaname)) 

lookup_ca <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds")

#create lookup that matches HSCP code to CA code (note stirling and clacks have no match and will be dropped)
hscp_ca <- merge(lookup_ca,lookup_hscp, by="areaname", all.x = TRUE) %>%
  rename(code=code.x) %>%
  select(-areaname) %>% 
  tibble::add_row(code = c("S12000015", "S12000024", "S12000046", "S12000044"), 
                  code.y = c("S37000032", "S37000033", "S37000034", "S37000035"))   # Adds old CA code for Fife, Perth, Glasgow and North Lanarkshire

NRS_ca_data <- NRS_data %>%  #select only council data from NRS file
  subset(substr(NRS_data$code, 1, 3) =="S12") 

NRS_ca_data <- left_join(NRS_ca_data, hscp_ca, by="code") # match on 

NRS_hscp_data <- NRS_ca_data %>%
  select(-code) %>%
  rename(code=code.y.y) %>%
  filter(!is.na(code))

NRS_data <- bind_rows(NRS_data,NRS_hscp_data)

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
  select(code, ind_id, year, numerator, rate=LEx,lowci=lci,upci=uci, def_period, trend_axis)

## Male life expectancy file
profile_data_male_LE <- all_le_data %>% subset(ind_id=="20101") 

# This indicator script doesn't use analysis functions but indicator checking report can still be called:
run_qa(filename="life_expectancy_male",old_file="default")

write_csv(profile_data_male_LE, file = paste0(shiny_network, "life_expectancy_male_shiny.csv"))
write_rds(profile_data_male_LE, file = paste0(shiny_network, "life_expectancy_male_shiny.rds"))

## Female life expectancy file
profile_data_female_LE <- all_le_data %>% subset(ind_id=="20102") 

# This indicator script doesn't use analysis functions but indicator checking report can still be called:
run_qa(filename="life_expectancy_female",old_file="default")

write_csv(profile_data_female_LE, file = paste0(shiny_network, "life_expectancy_female_shiny.csv"))
write_rds(profile_data_female_LE, file = paste0(shiny_network, "life_expectancy_female_shiny.rds"))