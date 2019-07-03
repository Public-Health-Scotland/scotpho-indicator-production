# ScotPHO indicators: Single Adult Dwellings

#   Part 1 - Create basefile
#   Part 2 - Preparation of geograpy files
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
sad_data_extract <- read_excel(paste0(data_folder, "Received Data/Single dwellings estimates 2018.xlsx"), 
                            sheet = "Formatted") %>% 
  setNames(tolower(names(.)))   #variables to lower case
  
#varstocases: split out 'multiple gather' function
sad_data_denom <- sad_data_extract %>% select(-c(n2007:n2018)) %>% 
    gather(year, denominator, d2007:d2018, na.rm = TRUE, convert = FALSE) %>% 
    mutate(year = substr(year, 2, 5)) #Remove leading "d" in year

sad_data_numer <- sad_data_extract %>% select(-c(d2007:d2018)) %>% 
    gather(year, numerator, n2007:n2018, na.rm = TRUE, convert = FALSE) %>% 
    mutate(year = substr(year, 2, 5)) #Remove leading "n" in year

sad_data <- inner_join(sad_data_numer, sad_data_denom) #joining together 

saveRDS(sad_data, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_depr_raw.rds'))

#### Match lookup - datazone with local authority

# dz01 Lookup file for CA 
dz01_lookup <- read.spss('/conf/linkage/output/lookups/geography/other_ref_files/DataZone2001.sav',
                       to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(ca2011, datazone2001)

#Preparing file for CA for period 2007 to 2014 (2014 only including dz <= S01006505)
sad01_data <- sad_data %>% filter(year<=2014)
#Merging with lookup
sad01_data <- left_join(sad01_data, dz01_lookup, by = c("datazone" = "datazone2001")) %>% 
  rename(ca = ca2011) %>% filter(datazone<='S01006505') %>% mutate(dz = "dz01")

# dz11 Lookup file for CA 
dz11_lookup <- read.spss('/conf/linkage/output/lookups/geography/DataZone2011/DataZone2011.sav',
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(ca2011, datazone2011)

#Preparing file for CA for period 2014 to 2017 (2014 only including dz > S01006505)
sad11_data <- sad_data %>% filter(year>=2014)
#Merging with lookup
sad11_data <- left_join(sad11_data, dz11_lookup, by = c("datazone" = "datazone2011")) %>% 
  rename(ca = ca2011) %>% filter(datazone>'S01006505') %>% mutate(dz = "dz11")

# Merge dz01 & dz11 data into single file (Basefile)
sad_data_raw <- full_join(sad01_data, sad11_data)

###############################################.
## Part 2 - Preparation of geograpy files ----
###############################################.

#### Prepare / Aggregate for specified geographies - LA, DZ11 & base IRs
# Prepare / Aggregate by la
sadla_data_raw <- sad_data_raw %>%
  group_by(ca, year, dz) %>% 
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% 
  filter(dz != "dz01" | year != "2014") %>% ungroup()

sadla_data_raw <- select(sadla_data_raw,-c(dz))

saveRDS(sadla_data_raw, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_LA_raw.rds'))

# Prepare / Aggregate by dz11
sad11_data_raw <- sad_data_raw %>%
  group_by(datazone, year, dz) %>% 
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% 
  filter(dz == "dz11") %>% ungroup()

sad11_data_raw <- select(sad11_data_raw,-c(dz))

saveRDS(sad11_data_raw, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_dz11_raw.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.

#Calling first analysis function
analyze_first(filename = "Single_Dwellings_LA", geography = "council", measure = "percent", 
              yearstart = 2007, yearend = 2013, time_agg = 1)

analyze_first(filename = "Single_Dwellings_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2014, yearend = 2018, time_agg = 1)

# Merging CA, DZ11 together and save both periods together
all_data <- rbind(readRDS(paste0(data_folder, "Temporary/Single_Dwellings_LA_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporary/Single_Dwellings_dz11_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporary/Single_Dwellings_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "Single_Dwellings_all", measure = "percent", time_agg = 1, 
               ind_id = 20504, year_type = "calendar", profile = "HN", min_opt = 1133276)

#Deprivation analysis function
analyze_deprivation(filename="Single_Dwellings_depr", measure="percent", time_agg=1, 
                    yearstart= 2007, yearend=2018,   year_type = "calendar", 
                    ind_id = 20504)

##END
