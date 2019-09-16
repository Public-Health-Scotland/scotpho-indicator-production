# ScotPHO indicators: people living in 15% most access deprived areas

# Part 1 - Create population files
# Part 2 - Create access rank data and save basefiles
# Part 3 - Calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
# Varies filepaths depending on if using server or not.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
} else {
  cl_out_depr <- "//stats/linkage/output/lookups/Unicode/Deprivation/"
}

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

#Small function to standarize each years info. Function parameters:
#Data is for what basefile to use, list_pos is for the position of the data frame
#simd for which simd variables-year to look at, year for what year is the data created.
read_simd <- function(data, simd, year, list_pos) {
  
  datazone <- tolower(substr(data,1,12))
  
  data_simd <- readRDS(paste0(cl_out_depr, data, '.rds')) %>% 
    setNames(tolower(names(.))) %>%   #variables to lower case
    select({{simd}}, datazone) %>% 
    rename(rank = {{simd}}, datazone = datazone) %>% 
    mutate(year = year)
  
  data_access[[list_pos]] <<- data_simd #assigning to list
}

###############################################.
## Part 1 - Create population files ----
###############################################.
# Creating base populations using dz2001 before 2014 and dz2011 onwards
#This is better to be run in R server.
dz01_base <- readRDS(paste0(data_folder, "Lookups/Population/DZ01_pop_basefile.rds")) %>% 
  filter(year<2014) %>% # 2014 uses simd2016 based on dz2011
  rename(datazone = datazone2001)

dz11_base <- readRDS(paste0(data_folder, "Lookups/Population/DZ11_pop_basefile.rds")) %>% 
  subset(year>2013) %>% # 2014 onwards uses simd based on dz2011
  rename(datazone = datazone2011)

dz_pop_base <- rbind(dz01_base, dz11_base)
rm(dz01_base, dz11_base)  

# This creates a file with the number of population that represents Scotland's 15%
scot_pop_base <- dz_pop_base %>% group_by(year) %>% 
  mutate(pop_15 = denominator/20*3) %>%  # creating 15% population 
  summarise(pop_15 = sum(pop_15)) #obtaining total pop

# Data set with the population for each datazone
dz_pop_base <- dz_pop_base %>% group_by(year, datazone) %>% 
  summarise(pop = sum(denominator)) #obtaining total pop for each datazone

###############################################.
## Part 2 - Create access rank data ----
###############################################.
data_access <- list() #creating empty list for placing data created by function

# The function creates the dataset with the rank for each datazone and assigns it to the list
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2004_access_rank", 
       year = 2002:2003, list_pos = 1:2) #simd version 2004
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2006_access_rank", 
       year = 2004:2006, list_pos = 3:5) #simd version 2006
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2009v2_access_rank", 
       year = 2007:2009, list_pos = 6:8) #simd version 2009
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2012_access_rank", 
       year = 2010:2013, list_pos = 9:12) #simd version 2012
mapply(read_simd, data = "DataZone2011_simd2016", simd = "simd2016_access_rank", 
       year = 2014:2018, list_pos = 13:17) #simd version 2016

data_access <- do.call("rbind", data_access) # converting from list into dataframe

# Joining with both of the populations: dz and scotland 15%
data_access <- left_join(data_access, dz_pop_base, by = c("datazone", "year"))
data_access <- left_join(data_access, scot_pop_base, by = "year")

# Creating cumulative populations for each year based on access rank
data_access <- data_access %>% group_by(year) %>% arrange(rank) %>% 
  mutate(cum_pop = cumsum(pop)) %>% ungroup() %>% 
  # If the datazone is included in the 15% more access deprived then use its
  # population as numerator, if not consider 0
  mutate(numerator = case_when((pop_15 - cum_pop)>=0 ~ pop,
                                TRUE ~ 0)) %>% 
  select(datazone, year, numerator)

#File for deprivation analysis
saveRDS(data_access, file = paste0(data_folder, "Prepared Data/access_deprived_depr_raw.rds"))

#File for DZ11 for 2014 onwards
data_access_dz11 <- data_access %>% filter(year>2013)

saveRDS(data_access_dz11, file = paste0(data_folder, "Prepared Data/access_deprived_dz11_raw.rds"))

#Preparing file for CA for period 2004 to 2013
data_access_dz01 <- data_access %>% filter(year<2014)
#Lookup file for CA
ca_lookup <- read.spss('/conf/linkage/output/lookups/Archive/geography/other_ref_files/DataZone2001.sav',
                       to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% select(ca2011, datazone2001)

#Merging with lookup and aggregating by ca
data_access_dz01 <- left_join(data_access_dz01, ca_lookup, by = c("datazone" = "datazone2001")) %>% 
  rename(ca = ca2011) %>% group_by(ca, year) %>% 
  summarise(numerator=sum(numerator, na.rm = T)) %>% ungroup() %>% 
  #Dealing with changes in ca codes. Transforms old code versions into 2019 ones
  mutate(ca = recode(ca, "S12000015"='S12000047', "S12000024"='S12000048', 
                     "S12000046"='S12000049', "S12000044"='S12000050'))

saveRDS(data_access_dz01, file = paste0(data_folder, "Prepared Data/access_deprived_ca_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
#Normal indicator analysis, first for CA and then DZ11
analyze_first(filename = "access_deprived_ca", geography = "council", measure = "percent", 
              yearstart = 2002, yearend = 2013, time_agg = 1, pop = "CA_pop_allages")
analyze_first(filename = "access_deprived_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2014, yearend = 2018, time_agg = 1, pop = "DZ11_pop_allages")

#Merging CA and DZ11 together
all_data <- rbind(readRDS(paste0(data_folder, "Temporary/access_deprived_dz11_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporary/access_deprived_ca_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporary/access_deprived_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "access_deprived_all", measure = "percent", 
               time_agg = 1, ind_id = 20902, year_type = "calendar")

###############################################.
#Deprivation analysis function
analyze_deprivation(filename="access_deprived_depr", measure="percent", time_agg=1, 
                    yearstart= 2002, yearend=2018,  
                    year_type = "calendar", pop = "depr_pop_allages", ind_id = 20902)

## END
