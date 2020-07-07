# ScotPHO indicators: Child Immunisations Uptake at 24 Months - MMR & 5-in-1. 

#   Part 1 - Prepare basefiles (DZ01 & DZ11): 5 in 1 vaccine
#   Part 2 - Prepare basefiles (DZ01 & DZ11): MMR vaccine
#   Part 3 - Call analysis macros: 5 in 1 vaccine
#   Part 4 - Call analysis macros: MMR vaccine

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
#install.packages('RcppRoll')
#install.packages('flextable')
#install.packages('plotly')
#install.packages('binom')
#library(haven)

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

# data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
# lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 

###############################################.
## Part 1 - Prepare basefile: 5 in 1 ----
###############################################.

# Datazone2001 - Read data (Immunisation uptake at 24 month) provided by child health team and aggregate 
five01_data <- read.spss(paste0(data_folder, "Received Data/2003_2019_Scotpho_ChildhoodImms_DZ_DZ2001.zsav"), 
                        to.data.frame=TRUE, use.value.labels=FALSE) %>%  
  setNames(tolower(names(.))) %>% rename(datazone = datazone2001) %>% 
  mutate(datazone = substr(datazone, 1, 9)) %>%   #trimming datazone to 9 characters
  # aggregate to get the count, removing sex
  group_by(year, datazone) %>% 
  summarise_at(c("total24", "five24"), list(sum), na.rm =T) %>% 
  ungroup()

# Rename variables into numerator and denominator
five01_data <- five01_data %>% rename(denominator = total24, numerator = five24)

# Datazone2011 - Read data (Immunisation uptake at 24 month) provided by child health team and aggregate
five11_data <- read.spss( paste0(data_folder, "Received Data/2004_2019_Scotpho_ChildhoodImms_DZ_DZ2011.zsav"), 
                          to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% rename(datazone = datazone2011) %>%
  mutate(datazone = substr(datazone, 1, 9)) %>%   #trimming datazone to 9 characters
  # aggregate to get the count, removing sex
  group_by(year, datazone) %>%
  summarise_at(c("total24", "five24"), list(sum), na.rm =T) %>% ungroup()

# Rename variables into numerator and denominator
five11_data <- five11_data %>% rename(denominator = total24, numerator = five24)

saveRDS(five11_data, file=paste0(data_folder, 'Prepared Data/Immunisation_5in1_dz11_raw.rds'))

#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
five_dep_file <- rbind(five01_data %>% subset(year<=2013), five11_data %>% subset(year>=2014)) 

saveRDS(five_dep_file, file=paste0(data_folder, 'Prepared Data/Immunisation_5in1_depr_raw.rds'))

###############################################.
## Part 2 - Prepare basefile: MMR ----
###############################################.

# Datazone2001 - Read data (Immunisation uptake at 24 month) provided by child health team and aggregate 
mmr01_data <- read.spss( paste0(data_folder, "Received Data/2003_2019_Scotpho_ChildhoodImms_DZ_DZ2001.zsav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% rename(datazone = datazone2001) %>% 
  mutate(datazone = substr(datazone, 1, 9)) %>%   #trimming datazone to 9 characters
  # aggregate to get the count, removing sex
  group_by(year, datazone) %>% 
  summarise_at(c("total24", "mmr24"), list(sum), na.rm =T) %>% ungroup() %>%
  # Rename variables into numerator and denominator
  rename(denominator = total24, numerator = mmr24)

# Datazone2011 - Read data (Immunisation uptake at 24 month) provided by child health team and aggregate
mmr11_data <- read.spss( paste0(data_folder, "Received Data/2004_2019_Scotpho_ChildhoodImms_DZ_DZ2011.zsav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% rename(datazone = datazone2011) %>% 
  mutate(datazone = substr(datazone, 1, 9)) %>%   #trimming datazone to 9 characters
  # aggregate to get the count, removing sex
  group_by(year, datazone) %>% 
  summarise_at(c("total24", "mmr24"), list(sum), na.rm =T) %>% ungroup()  %>% 
  # Rename variables into numerator and denominator
  rename(denominator = total24, numerator = mmr24)

saveRDS(mmr11_data, file=paste0(data_folder, 'Prepared Data/Immunisation_MMR_dz11_raw.rds'))

#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
mmr_dep_file <- rbind(mmr01_data %>% subset(year<=2013), mmr11_data %>% subset(year>=2014))

saveRDS(mmr_dep_file, file=paste0(data_folder, 'Prepared Data/Immunisation_MMR_depr_raw.rds'))

###############################################.
## Part 3 - Call analysis macros: 5 in 1 ----
###############################################.

analyze_first(filename = "Immunisation_5in1_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2004, yearend = 2019, time_agg = 3)

analyze_second(filename = "Immunisation_5in1_dz11", measure = "percent", time_agg = 3, 
               ind_id = 21103, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="Immunisation_5in1_depr", measure="percent", time_agg=3, 
                    yearstart= 2003, yearend=2019,   year_type = "calendar", 
                    ind_id = 21103)

###############################################.
## Part 4 - Call analysis macros: MMR ----
###############################################.

analyze_first(filename = "Immunisation_MMR_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2004, yearend = 2019, time_agg = 3)

analyze_second(filename = "Immunisation_MMR_dz11", measure = "percent", time_agg = 3, 
               ind_id = 21104, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="Immunisation_MMR_depr", measure="percent", time_agg=3, 
                    yearstart= 2003, yearend=2019,   year_type = "calendar", 
                    ind_id = 21104)

##END
