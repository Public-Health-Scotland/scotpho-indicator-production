################################################################################
################################################################################
#########                                                              #########
#####      Children referred to the SCRA for care and protection           #####
#########                                                              #########
################################################################################
################################################################################

## This script analyses the number of children referred to the Scottish Children's
## Reporter Administration (SCRA) for reasons of care and protection.  The data is
## presented at local authority level as a rate per 1000 children aged 0-15.

## The data is requested from Donald Lamb at the SCRA.

################################################################################
#####                          install packages etc                        #####
################################################################################
## remove any existing objects from global environment
rm(list=ls()) 

## install packages
#install.packages("tidyverse")

library(tidyverse) # all kinds of stuff 
library(stringr) # for strings
library(openxlsx) # to read in wide format data from excel

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

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

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

################################################################################
#####                          read in prepared data                       #####
################################################################################

# NOTE: If data received in wide format, uncomment this section to reformat:

# 1) Read in excel:
# ~~~~~~~~~~~~~~~~~
SCRA_care <- openxlsx::read.xlsx(paste0(data_folder,"Received Data/children_referred_to_scra_offence_and_non-offence.xlsx"),
                                 sheet = "2. Referral Type", startRow = 8, colNames = TRUE,
                                 cols = c(2:22), rows = c(8:42)) 

# 2) Reformat data from wide to long, and add in ca codes.
SCRA_care %<>%
  rename("ca" = "Children/YP") %>%
  select(-X2) %>%
  pivot_longer(!ca, names_to = "year", values_to = "numerator") %>%
  mutate(ca = case_when(
    str_detect(ca,"Clackmannanshire") ~ "S12000005",
    str_detect(ca,"Dumfries") ~ "S12000006",
    str_detect(ca,"East Ayrshire") ~ "S12000008",
    str_detect(ca,"East Lothian") ~ "S12000010",
    str_detect(ca,"East Renfrewshire") ~ "S12000011",
    str_detect(ca,"Siar") ~ "S12000013",
    str_detect(ca,"Falkirk") ~ "S12000014",
    str_detect(ca,"Highland") ~ "S12000017",
    str_detect(ca,"Inverclyde") ~ "S12000018",
    str_detect(ca,"Midlothian") ~ "S12000019",
    str_detect(ca,"Moray")	~ "S12000020",
    str_detect(ca,"North Ayrshire") ~ "S12000021",
    str_detect(ca,"Orkney") ~ "S12000023",
    str_detect(ca,"Scottish Borders") ~ "S12000026",
    str_detect(ca,"Shetland") ~ "S12000027",
    str_detect(ca,"South Ayrshire") ~ "S12000028",
    str_detect(ca,"South Lanarkshire") ~ "S12000029",
    str_detect(ca,"Stirling") ~ "S12000030",
    str_detect(ca,"Aberdeen City") ~ "S12000033",
    str_detect(ca,"Aberdeenshire") ~ "S12000034",
    str_detect(ca,"Argyll") ~ "S12000035",
    str_detect(ca,"Edinburgh")	~ "S12000036",
    str_detect(ca,"Renfrewshire") ~"S12000038",
    str_detect(ca,"West Dunbartonshire")	~"S12000039",
    str_detect(ca,"West Lothian") ~"S12000040",
    str_detect(ca,"Angus") ~"S12000041",
    str_detect(ca,"Dundee") ~"S12000042",
    str_detect(ca,"East Dunbartonshire") ~"S12000045",
    str_detect(ca,"Fife") ~"S12000047",
    str_detect(ca,"Perth") ~"S12000048",
    str_detect(ca,"Glasgow") ~"S12000049",
    str_detect(ca,"North Lanarkshire") ~"S12000050",
    TRUE ~ ca),
  year = as.numeric(paste0("20",str_sub(year, 1,2)))) %>% 
  relocate(year,ca,numerator)

# 3) Read in population look up and and format select only relevant dates 
#    and ages (<16years)))

pop_lookup <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/",
                      "Estimates/CA2019_pop_est_1981_2021.rds")) %>% 
  filter(year %in% c(2004:2021),
         age < 16) %>% 
  group_by(year,ca2019) %>% 
  summarise(denominator = sum(pop)) %>% 
  ungroup()

# 4) Match on population lookup to SCRA_care as denominator

SCRA_care <- left_join(SCRA_care, pop_lookup, by = c("year", "ca" = "ca2019"))

#Scotland totals have been provided, which differ from the sum of the council areas.
# To ensure that the analyze_first() function calculates the given Scotland totals:
# 1) Calculate council_area_sum 
# 2) Subtract provided Scotland value from council_area_sum
# (Scotland column becomes the difference between council_area_sum and the given total)

# Create an extract of provided Scotland data
scot <- SCRA_care %>%
  filter(ca == "Scotland")

# Create an extract without Scotland data
notscot <- SCRA_care %>%
  filter(ca != "Scotland")

# Calculate council_area_sum and join this data set with scot data.
# Reformat data into same format as SCRA_care so that it can be bound with SCRA_care
difference <- SCRA_care %>%
  filter(ca != "Scotland") %>%
  group_by(year) %>%
  summarise(council_area_sum = sum(numerator)) %>%
  ungroup() %>%
  left_join(scot, by = "year") %>%
  mutate(numerator = numerator - council_area_sum,
           ca = "") %>%
  select(-council_area_sum)

# Bind the 'notscot' data to the difference data
SCRA_care <- bind_rows(notscot,difference) %>% 
  arrange(year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Otherwise, if data sent in usual indicator long format, run this code:
# Read in csv:
#SCRA_care<- read.csv(paste0(data_folder, "Received Data/scra_care_protection_raw.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Then save to Prepared Data folder:
saveRDS(SCRA_care, file=paste0(data_folder, "Prepared Data/scra_care_protection_raw.rds"))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "scra_care_protection", geography = "council", 
              measure = "crude", yearstart = 2004, yearend = 2021, time_agg = 1)

analyze_second(filename = "scra_care_protection", measure = "crude", time_agg = 1,
              ind_id = 13001, year_type = "financial", crude_rate = 1000)

