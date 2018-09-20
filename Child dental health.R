# ScotPHO indicators: Child dental health at P1 and P7

#   Part 1 - P1 Child dental raw data 
#   Part 2 - P7 Child dental raw data 
#   Part 3 - Run macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
lapply(c("dplyr", "readxl"), library, character.only = TRUE)
server_desktop <- "server" # change depending if you are using R server or R desktop

if (server_desktop == "server") {
  prepared_data <- "/PHI_conf/ScotPHO/Profiles/Data/Prepared Data/"
  received_data <- "/PHI_conf/ScotPHO/Profiles/Data/Received Data/"
  # functions <- ""
} else if (server_desktop == "desktop") {
  prepared_data <- "//stats/ScotPHO/Profiles/Data/Prepared Data/"
  received_data <- "//stats/ScotPHO/Profiles/Data/Received Data/"
  # functions <- ""
}

data <- read_excel(paste0(received_data, "IR2017-01731_DZ2011 Child dental Health.xlsx"), 
                   sheet = "2013_P1_C_DZ2011", range = "A5:E13075")

read_excel_sheet <- function(sheet, range) {
  data <- read_excel(paste0(received_data, "IR2017-01731_DZ2011 Child dental Health.xlsx"), 
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
  read_excel_sheet(sheet = "2017_P1_C_DZ2011", range = "A5:E12972"))) %>% 
  group_by(datazone, year) %>% #aggregating 
  summarise(numerator = sum(numerator), denominator = sum(denominator)) 
  
saveRDS(data_p1, file=paste0(prepared_data, 'child_dental_p1_raw.rds'))

###############################################.
## Part 2 - P7 Child dental raw data ----
###############################################.
data_p7 <- as.data.frame(rbind(
  read_excel_sheet(sheet = "2013_P7_C_DZ2011", range = "A5:E12816"),
  read_excel_sheet(sheet = "2014_P7_C_DZ2011", range = "A5:E12775"),
  read_excel_sheet(sheet = "2015_P7_C_DZ2011", range = "A5:E12721"),
  read_excel_sheet(sheet = "2016_P7_C_DZ2011", range = "A5:E12836"),
  read_excel_sheet(sheet = "2017_P7_C_DZ2011", range = "A5:E12955"))) %>% 
  group_by(datazone, year) %>% #aggregating 
  summarise(numerator = sum(numerator), denominator = sum(denominator)) 

saveRDS(data_p7, file=paste0(prepared_data, 'child_dental_p7_raw.rds'))

###############################################.
## Part 3 - Calling the  macros ----
###############################################.
source("./function_analysis.R") #It will need to sit on non-confi

#I need to amend the function so it works with population finite correction factor
#Children at P1
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2016, time_agg = 1)

analyze_second(filename = "child_dental_p1", measure = "percent", time_agg = 1, 
               ind_id = 21005, year_type = "school", profile = "HN", min_opt = 2999)

#Children at P7
analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2016, time_agg = 1)

analyze_second(filename = "child_dental_p7", measure = "percent", time_agg = 1, 
               ind_id = 21006, year_type = "school",  profile = "HN", min_opt = 2999)


##END