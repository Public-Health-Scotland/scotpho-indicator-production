# ScotPHO indicators: Maternities with drug use

#   Part 1 - Extract data from open data platform
#   Part 2 - P7 Child dental raw data 
#   Part 3 - Run analysis functions

# TODO:
#   Need to bring older data in
#I probably don't need the functions, maybe the second one
#check the spss code

###############################################.
## Packages/Filepaths/Functions ----
library(dplyr)
library(readr)

server_desktop <- "server" # change depending if you are using R server or R desktop
if (server_desktop == "server") {
  prepared_data <- "/PHI_conf/ScotPHO/Profiles/Data/Prepared Data/"
  formatted_data <- "/PHI_conf/ScotPHO/Profiles/Data/Temporary/"
} else if (server_desktop == "desktop") {
  prepared_data <- "//stats/ScotPHO/Profiles/Data/Prepared Data/"
  formatted_data <- "//stats/ScotPHO/Profiles/Data/Temporary/"
}

###############################################.
## Part 1 - Extract data from open data platform ----
###############################################.
data_drugmat_ca <- read_csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/3e96277a-9029-4390-ab90-ec600f9926a5/download/11.6_ca_drugmisuse.csv") %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(code = ca2011) #to allow merging

data_drugmat_hb <- read_csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/8c8377e1-b1c7-48e7-b313-79eb5ac3c110/download/11.6_hb_drugmisuse.csv") %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  rename(code = hbr2014) %>%  select(-hbr2014qf) #to allow merging

data_drugmat <- rbind(data_drugmat_ca, data_drugmat_hb) %>% 
  #selecting only totals and hb, ca and scotland
  filter(simdquintileqf == "d" &
           substr(code, 1, 3) %in% c("S92", "S08", "S12")) %>% 
  rename(year = financialyears, numerator = drugmisuse, denominator = maternities) %>% 
  select(year, code, numerator, denominator) %>% 
  #reformatting year to style needed
  mutate(year = as.numeric(paste0(substr(year, 1, 2), substr(year, 6, 7))))

saveRDS(data_drugmat, paste0(formatted_data, "maternities_drug_use_formatted.rds"))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_second(filename = "maternities_drug_use", measure = "crude", crude_rate = 1000, 
               ind_id = aaaa, year_type = "financial", profile = "HN", min_opt = 1245385)

##END