###  notes
# ScotPHO indicators
# 20804: domestic_abuse
# data queried directly from statistics.gov


# install the opendata scotland r package which communicates with the statistics.gov wesbite api
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")

source("1.indicator_analysis.R")
library(opendatascot)

# see structure and variables contained in the domestic abuse dataset
# Note: years in ods structure result below represent financial years e.g 2022  is for 2021/2022 , 2021 is for 2020/21
ods_structure("domestic-abuse-recorded-by-the-police-number-of-incidents-and-rates")
# see dates available so that the most recent year can be included by adjusting the date range below 

date_range = (as.character(2004:2022)) # adjust to include most recent year in ods sctructure above

domestic_abuse = ods_dataset("domestic-abuse-recorded-by-the-police-number-of-incidents-and-rates",geography = "la", refPeriod=date_range
                             ,measureType="count") %>%  
  setNames(tolower(names(.))) %>%
  select("ca"= "refarea","year"="refperiod","numerator"="value") %>%
  mutate(year=substr(year,1,4)) %>%
  mutate(across(c("numerator","year"), ~ as.numeric(.)))

saveRDS(domestic_abuse, file=paste0(data_folder, 'Prepared Data/20804_domestic_abuse_raw.rds')) 

# adjust the yearend to include most recent year
analyze_first(filename = "20804_domestic_abuse", geography = "council", measure = "crude", yearstart = 2003,
              yearend = 2021, time_agg = 1, pop = "CA_pop_allages")

analyze_second(filename = "20804_domestic_abuse", measure = "crude", time_agg = 1,
               ind_id = 20804, year_type = "financial",crude_rate = 10000) 

# END
