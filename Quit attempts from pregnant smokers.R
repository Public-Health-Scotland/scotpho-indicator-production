# Analyst notes ----------------------------------------------------------------

# This script updates the following indicator:
# 1526 - Smoking quit attempts from pregnant smokers

# Data should be requested from the drugs and alcohol team alongside a range of other quit attempts data extracts
# The numerator is provided from the drugs and alcohol team
# The denominator comes from data published on the open data platform by the maternity team (Smoking behaviour during pregnancy) 


# Part 1 - read in data
# Part 2 - prepare data
# Part 3 - run analysis functions 

# dependencies -----------------------------------------------------------------
source("1.indicator_analysis.R")
library(tidyr) # for pivoting 



# 1. read in data --------------------------------------------------------------

# numerator (number of quit attempts from pregnant smokers)
numerator <- read_csv(paste0(data_folder, "Received Data/Smoking quit attempts/2023 request/pregnant_quit_attempts_2022.csv")) %>%
  setNames(tolower(names(.))) 

# denominator (number of pregnant smokers)
denominator <- read_csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/e87a7673-0397-43ca-91a5-166184319728/download/11.4_smoking.csv") %>%
  setNames(tolower(names(.))) 



# 2. Prepare data --------------------------------------------------------------

# prepare numerator data
numerator <- numerator %>%
  select(-council) %>%
  pivot_longer(!ca2019, names_to = "year", values_to = "numerator") %>%
  rename(ca = ca2019) %>%
  mutate(year = substr(year, start = 1, stop = 4))


# prepare denominator data
denominator <- denominator %>%
  filter(smokingatbooking == "Current smoker") %>%
  mutate(year = substr(financialyear, start = 1, stop = 4)) %>%
  filter(year >= 2011) %>%
  rename(denominator = maternities) %>%
  select(ca, year, denominator) %>%
  group_by(ca, year) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  filter(ca != "RA2704") # exclude no fixed abode


# combine numerator and denominator
combined <- left_join(numerator, denominator, by = c("ca", "year"))


# save file to pass to functions 
saveRDS(combined, file=paste0(data_folder, 'Prepared Data/quitattempts_pregnant_raw.rds'))




# 3. Run analysis functions ----------------------------------------------------
analyze_first(filename = "quitattempts_pregnant", geography = "council", 
              measure = "percent", yearstart = 2011, yearend = 2021, time_agg = 3)


analyze_second(filename = "quitattempts_pregnant", measure = "percent", time_agg = 3, 
               ind_id = 1526, year_type = "financial")


# END