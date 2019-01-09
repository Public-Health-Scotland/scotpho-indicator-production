# ScotPHO indicators: Availability of smoking cessation products

#   Part 1 - Create basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
lapply(c("dplyr", "readxl"), library, character.only = TRUE)

server_desktop <- "server" # change depending if you are using R server or R desktop
source("./1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
data_products <- read_excel(paste0(data_folder, "Received Data/IR2018-01479-smoking cessation products.xlsx"), 
                            sheet = "Output", range = cell_limits(c(8, 2), c(NA, 10))) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename_all(funs(gsub("\\s","_",.))) # changing spaces for underscores

# Bringing code information for LA.
ca_lookup <- read_csv("/conf/linkage/output/lookups/geography/Codes_and_Names/Council Area 2011 Lookup.csv") %>% 
  setNames(tolower(names(.))) %>% rename(ca = councilarea2011code)

data_products <- left_join(data_products, ca_lookup, #Merging both
                           by = c("dispensing_council_area" = "councilarea2011name")) %>% 
  filter(!(is.na(ca))) #excluding values without a valid ca

# aggregate to get total DDDs for each datazone/year.
data_products <- data_products %>% rename(year = financial_year) %>% 
  group_by(ca, year) %>% summarize(numerator = sum(defined_daily_doses, na.rm = T)) %>%  
  mutate(numerator = numerator/365) %>%  # value is defined daily doses, so needs to be divided by 365.
  ungroup() %>% select(ca, year, numerator) 

saveRDS(data_products, file=paste0(data_folder, 'Prepared Data/cessation_products_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "cessation_products", geography = "council", measure = "crude", 
              yearstart = 2002, yearend = 2017, time_agg = 1, pop="CA_pop_12+")

analyze_second(filename = "cessation_products", measure = "crude", time_agg = 1,
               crude_rate = 1000, ind_id = 1544, year_type = "financial", 
               profile = "TP", min_opt = 1006439)

##END
