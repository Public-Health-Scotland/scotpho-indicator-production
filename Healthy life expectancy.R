#  ScotPHO indicators: 2 indicator outputs from this script 
#  Healthy life expectancy, males
#  Healthy life expectancy, females

# HLE can only be generated at Scotland, NHS board and CA level.  It is NOT possibly to generate at smaller geographies as robust data on SAH (self assessed health is not available)

# HLE data published annually by NRS - usually in December - check website to see if new data has been published
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland
# it may take some time before this data is then available in statistics.gov 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") #Normal indicator functions

# Data queried directly from statistics.gov 
# install the dev tools/opendata scotland r package which communicates with the statistics.gov website api - if you don't already have them.
# install.packages("devtools") #commented out as only needs to be run once and you may already have the packages installed.
# devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov

# Extracts for Life Expectancy data saved in left expectancy network folder.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
} else {
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
}

###############################################.
# Extract Healthy life expectancy data ----
## by CA & NHS board
###############################################.

ods_structure("healthy-life-expectancy") # see structure and variables of this dataset

# Set filter parameters to use in open data extraction
# reminder: there is a limit on number of rows that can be extracted using this particular method - in case the extracted data doesn't look complete.

date_range_hle <- c("2015-2017", "2016-2018", "2017-2019", "2018-2020", "2019-2021") # add most recent year each update cycle
measures <-c("count","95-upper-confidence-limit","95-lower-confidence-limit")
simd <- c("all")
urban_rural <- c("all")
age_select <- c("0-years") #include filter in a extraction as it is needed to ensure number of rows to extract is within limits however (annoyingly initial extratction returns '0 years' and '90 years' so need to filter again)

# extract data
hle = ods_dataset("healthy-life-expectancy", refPeriod = date_range_hle,
                  urbanRuralClassification = urban_rural,age = age_select,
                  simdQuintiles = simd, measureType = measures) %>%
  setNames(tolower(names(.))) %>%
  filter(age == age_select) %>%  # filter again as this will select correctly filter only '0 years' (ie healthy life expectancy at birth) otherwise you have '90 years' 
  rename("code" = refarea, "trend_axis" = refperiod) %>% 
  select(c("code", "trend_axis", "measuretype", "value", "sex")) %>% 
  arrange(code, trend_axis, sex) %>% 
  mutate(sex = case_when(sex == "male" ~ "1",
                         sex == "female" ~ "2"),
         measuretype = case_when(measuretype == "count" ~ "rate", measuretype == "95-lower-confidence-limit" ~ "lowci", measuretype == "95-upper-confidence-limit" ~ "upci", TRUE ~ "other"),
         code = ifelse(code == "S92000003", "S00000001", code)) #recode standard geo code for scotland to the ScotPHO dictionary where S00000001 is Scotland

hle <- hle %>%
  pivot_wider(names_from = measuretype, values_from = value) %>%
  mutate(numerator=" ", #insert column where numerator would ordinarily be - there is no numerator for LE
         ind_id= case_when(sex=="1" ~ "99102", #male indicator number
                           sex=="2" ~ "99101", #female indicator number
                           TRUE ~"x"),
         def_period=paste0(trend_axis," (3 year aggregate)"),
         year= as.numeric(substr(trend_axis,1,4))+1) %>% # its 3 year average so take first year of time period then add 1 to find mid-year
  arrange(ind_id, year, code) %>% 
  select(sex, code, ind_id, year, numerator, rate, lowci, upci, def_period, trend_axis)


###############################################.
## Generate Male healthy life expectancy shiny file ----
###############################################.
hle_male <- hle %>% subset(sex=="1") %>%
  select(-sex)

write_csv(hle_male, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_male_shiny.csv"))
write_rds(hle_male, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_male_shiny.rds"))


###############################################.
## Generate Female healthy life expectancy file ----
###############################################.
hle_female <- hle %>% subset(sex=="2") %>%
  select(-sex)

write_csv(hle_female, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_female_shiny.csv"))
write_rds(hle_female, file = paste0(data_folder, "Data to be checked/healthy_life_expectancy_female_shiny.rds"))


