# ScotPHO indicators: Child dental health at P1 and P7

#   Part 1 - P1 Child dental raw data 
#   Part 2 - P7 Child dental raw data 
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(readxl) #for reading excel

server_desktop <- "server" # change depending if you are using R server or R desktop
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

#Function to read raw data for each year
read_excel_sheet <- function(sheet, range) {
  data <- read_excel(paste0(data_folder, "Received Data/IR2018-01708 DZ2011 Child dental health.xlsx"), 
                            sheet = sheet, range = range) %>% 
    setNames(tolower(names(.))) %>%   #variables to lower case
    mutate(year = substr(school_year,1,4)) %>% #creating year variable
    rename(datazone = datazone2011) %>% 
    subset(datazone != "unknown") #excluding unknown datazones
}
###############################################.
## Part 1 - P1 Child dental raw data ----
###############################################.
data_p1 <- as.data.frame(rbind(
  read_excel_sheet(sheet = "2013_P1_C_DZ2011", range = "A5:E13075"),
  read_excel_sheet(sheet = "2014_P1_C_DZ2011", range = "A5:E13189"),
  read_excel_sheet(sheet = "2015_P1_C_DZ2011", range = "A5:E13186"),
  read_excel_sheet(sheet = "2016_P1_C_DZ2011", range = "A5:E13004"),
  read_excel_sheet(sheet = "2017_P1_letter_C", range = "A4:E12971"),
  read_excel_sheet(sheet = "2018_P1_letter_C", range = "A4:E13075"))) %>% 
  group_by(datazone, year) %>% #aggregating 
  mutate(numerator = as.numeric(numerator)) %>% 
  summarise(numerator = sum(numerator, na.rm =T), 
            denominator = sum(denominator, na.rm =T)) %>%  ungroup()

saveRDS(data_p1, file=paste0(data_folder, 'Prepared Data/child_dental_p1_raw.rds'))
#Saving file for deprivation, only from 2014 for simd2016
data_p1_depr <- data_p1 %>% filter(year>=2014)
saveRDS(data_p1_depr, file=paste0(data_folder, 'Prepared Data/child_dental_p1_depr_raw.rds'))

###############################################.
## Part 2 - P7 Child dental raw data ----
###############################################.
data_p7 <- as.data.frame(rbind(
  read_excel_sheet(sheet = "2013_P7_C_DZ2011", range = "A5:E12816"),
  read_excel_sheet(sheet = "2014_P7_C_DZ2011", range = "A5:E12775"),
  read_excel_sheet(sheet = "2015_P7_C_DZ2011", range = "A5:E12721"),
  read_excel_sheet(sheet = "2016_P7_C_DZ2011", range = "A5:E12836"),
  read_excel_sheet(sheet = "2017_P7_letter_C", range = "A4:E12954"),
  read_excel_sheet(sheet = "2018_P7_letter_C", range = "A4:E12968"))) %>% 
  group_by(datazone, year) %>% #aggregating 
  mutate(numerator = as.numeric(numerator)) %>% 
  summarise(numerator = sum(numerator, na.rm =T), 
            denominator = sum(denominator, na.rm =T)) %>% ungroup()

saveRDS(data_p7, file=paste0(data_folder, 'Prepared Data/child_dental_p7_raw.rds'))

#Saving file for deprivation, only from 2014 for simd2016
data_p7_depr <- data_p7 %>% filter(year>=2014)
saveRDS(data_p7_depr, file=paste0(data_folder, 'Prepared Data/child_dental_p7_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#I need to amend the function so it works with population finite correction factor
#Children at P1
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2017, time_agg = 1)

analyze_second(filename = "child_dental_p1", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21005, year_type = "school", profile = "HN", min_opt = 2999,
               pop="DZ11_pop_5")

#Deprivation analysis function
analyze_deprivation(filename="child_dental_p1_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend=2017, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_5", ind_id = 21005)

###############################################.
#Children at P7
analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2017, time_agg = 1)

analyze_second(filename = "child_dental_p7", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21006, year_type = "school",  profile = "HN", min_opt = 2999,
               pop="DZ11_pop_11")

analyze_deprivation(filename="child_dental_p7_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend=2017, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_11", ind_id = 21006)

##END