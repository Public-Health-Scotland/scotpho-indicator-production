# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# This indicator has been made inactive as of Feb 2025.
# It has been replaced with a new 'recorded crime' indicator using Police Scotland data
# See 'Recorded Crime.R script for details on new indicator



# ScotPHO indicators: crime rate, including deprivation.

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
  
  data_crime[[list_pos]] <<- data_simd #assigning to list
  
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.
data_crime <- list() #creating empty list for placing data created by function

# The function creates the dataset and assigns it to the list
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2006_crime_n", 
       year = 2004:2006, list_pos = 1:3) #simd version 2006
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2009v2_crime_n", 
       year = 2007:2009, list_pos = 4:6) #simd version 2009
mapply(read_simd, data = "DataZone2001_all_simd", simd = "simd2012_crime_n", 
       year = 2010:2013, list_pos = 7:10) #simd version 2012
mapply(read_simd, data = "DataZone2011_simd2016", simd = "simd2016_crime_n", 
       year = 2014:2016, list_pos = 11:13) #simd version 2016
mapply(read_simd, data = "DataZone2011_simd2020v2", simd = "simd2020v2_crime_count", 
       year = 2017:2020, list_pos = 14:17) #simd version 2020

data_crime <- do.call("rbind", data_crime) # converting from list into dataframe

#File for deprivation analysis
saveRDS(data_crime, file = paste0(data_folder, "Prepared Data/crime_rate_depr_raw.rds"))

#File for DZ11 for 2014 onwards
data_crimedz11 <- data_crime %>% filter(year>2013)

saveRDS(data_crimedz11, file = paste0(data_folder, "Prepared Data/crime_rate_dz11_raw.rds"))

#Preparing file for CA for period 2004 to 2013
data_crimedz01 <- data_crime %>% filter(year<2014)
#Lookup file for CA
ca_lookup <- read_xlsx(paste0(data_folder, "Lookups/Geography/DataZone2001.xlsx")) %>% 
  setNames(tolower(names(.))) %>% select(ca, datazone)

#Merging with lookup and aggregating by ca
data_crimedz01 <- left_join(data_crimedz01, ca_lookup) %>% 
  group_by(ca, year) %>% 
  summarise(numerator=sum(numerator, na.rm = T)) %>% ungroup() %>% 
  #Dealing with changes in ca codes. Transforms old code versions into 2019 ones
  mutate(ca = recode(ca, "S12000015"='S12000047', "S12000024"='S12000048', 
                       "S12000046"='S12000049', "S12000044"='S12000050'))

saveRDS(data_crimedz01, file = paste0(data_folder, "Prepared Data/crime_rate_ca_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
#Normal indicator analysis, first for CA and then DZ11
analyze_first(filename = "crime_rate_ca", geography = "council", measure = "crude", hscp = T, 
              yearstart = 2004, yearend = 2013, time_agg = 1, pop = "CA_pop_allages")
analyze_first(filename = "crime_rate_dz11", geography = "datazone11", measure = "crude", 
              yearstart = 2014, yearend = 2020, time_agg = 1, pop = "DZ11_pop_allages")

#Merging CA and DZ11 together
all_data <- rbind(readRDS(paste0(data_folder, "Temporary/crime_rate_dz11_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporary/crime_rate_ca_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporary/crime_rate_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "crime_rate_all", measure = "crude", time_agg = 1, 
               crude_rate = 1000, ind_id = 20801, year_type = "calendar")

###### Save final result before it is overwritten by analyze_deprivation() and filter correct years to include in Plot
data_shiny_filtered <- final_result %>% 
  select(c(code, ind_id, year, numerator, rate, lowci, upci, def_period, trend_axis)) %>% 
  filter(year %in% c(2004, 2007, 2010, 2014, 2017))

###############################################.
#Deprivation analysis function
analyze_deprivation(filename="crime_rate_depr", measure="crude", time_agg=1, 
                    crude_rate = 1000, yearstart= 2004, yearend=2020,  
                    year_type = "calendar", pop = "depr_pop_allages", ind_id = 20801)


####### Filter depirvation data to include correct years
data_shiny_deprivation_filtered <- final_result %>%  
  filter(year %in% c(2004, 2007, 2010, 2014, 2017))



# Save to Data to be checked folder

saveRDS(data_shiny_filtered, file = paste0("/PHI_conf/ScotPHO/Profiles/Data/", "Data to be checked/", "crime_rate_all", "_shiny.rds"))
write_csv(data_shiny_filtered, file = paste0("/PHI_conf/ScotPHO/Profiles/Data/", "Data to be checked/", "crime_rate_all", "_shiny.csv"))
saveRDS(data_shiny_deprivation_filtered, file = paste0("/PHI_conf/ScotPHO/Profiles/Data/", "Data to be checked/", "crime_rate_depr_ineq.rds"))

#END IS THE BEST