# ScotPHO indicators: population income deprived including deprivation.

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

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
    rename(numerator = {{simd}}, datazone = datazone) %>% 
    mutate(year = year)
  
  data_inc_dep[[list_pos]] <<- data_simd #assigning to list
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.
data_inc_dep <- list() #creating empty list for placing data created by function

# The function creates the dataset and assigns it to the list
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2006_inc_dep_n", 
       year = 2004:2006, list_pos = 1:3) #simd version 2006
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2009v2_inc_dep_n", 
       year = 2007:2009, list_pos = 4:6) #simd version 2009
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2012_inc_dep_n", 
       year = 2010:2013, list_pos = 7:10) #simd version 2012
mapply(read_simd, data = "DataZone2011_simd2016", simd = "simd2016_inc_dep_n", 
       year = 2014:2018, list_pos = 11:15) #simd version 2016

data_inc_dep <- do.call("rbind", data_inc_dep) # converting from list into dataframe

#File for deprivation analysis
saveRDS(data_inc_dep, file = paste0(data_folder, "Prepared Data/income_deprived_depr_raw.rds"))

#File for DZ11 for 2014 onwards
data_inc_depdz11 <- data_inc_dep %>% filter(year>2013)

saveRDS(data_inc_depdz11, file = paste0(data_folder, "Prepared Data/income_deprived_dz11_raw.rds"))

#Preparing file for CA for period 2004 to 2013
data_inc_depdz01 <- data_inc_dep %>% filter(year<2014)
#Lookup file for CA
ca_lookup <- read.spss('/conf/linkage/output/lookups/Archive/geography/other_ref_files/DataZone2001.sav',
                       to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% select(ca2011, datazone2001)

#Merging with lookup and aggregating by ca
data_inc_depdz01 <- left_join(data_inc_depdz01, ca_lookup, by = c("datazone" = "datazone2001")) %>% 
  rename(ca = ca2011) %>% group_by(ca, year) %>% 
  summarise(numerator=sum(numerator, na.rm = T)) %>% ungroup() %>% 
  #Dealing with changes in ca codes. Transforms old code versions into 2019 ones
  mutate(ca = recode(ca, "S12000015"='S12000047', "S12000024"='S12000048', 
                     "S12000046"='S12000049', "S12000044"='S12000050'))

saveRDS(data_inc_depdz01, file = paste0(data_folder, "Prepared Data/income_deprived_ca_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
#Normal indicator analysis, first for CA and then DZ11
analyze_first(filename = "income_deprived_ca", geography = "council", measure = "percent", 
              yearstart = 2004, yearend = 2013, time_agg = 1, pop = "CA_pop_allages")
analyze_first(filename = "income_deprived_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2014, yearend = 2018, time_agg = 1, pop = "DZ11_pop_allages")

#Merging CA and DZ11 together
all_data <- rbind(readRDS(paste0(data_folder, "Temporary/income_deprived_dz11_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporary/income_deprived_ca_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporary/income_deprived_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "income_deprived_all", measure = "percent", 
               time_agg = 1, ind_id = 20701, year_type = "calendar")

###############################################.
#Deprivation analysis function
analyze_deprivation(filename="income_deprived_depr", measure="percent", time_agg=1, 
                    yearstart= 2004, yearend=2018,  
                    year_type = "calendar", pop = "depr_pop_allages", ind_id = 20701)

#END