# ScotPHO indicators: employment deprived, including deprivation.

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
#Filepaths, change from server to desktop depending on R version you use
server_desktop <- "server"
if (server_desktop == "server") {
  cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
} else if (server_desktop == "desktop") {
  cl_out_depr <- "//stats/linkage/output/lookups/Unicode/Deprivation/"
}

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

#Small function to standarize each years info. Function parameters:
#Data is for what basefile to use, Datazone is for what dz type using, 
#simd for which simd variables-year to look at, year for what year is the data created.
read_simd_quint <- function(data, simd, datazone, year) {
  data_simd <- read.spss(paste0(cl_out_depr, data, '.sav'), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
    setNames(tolower(names(.))) %>%   #variables to lower case
    select_(simd, datazone) %>% 
    rename_(numerator = simd, datazone = datazone) %>% 
    mutate(year = year)
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.
data_employ <- as.data.frame(rbind(
  read_simd_quint(year = 2004, data = "DataZone2001_all_simd", 
                  simd = "simd2006_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2005, data = "DataZone2001_all_simd", 
                  simd = "simd2006_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2006, data = "DataZone2001_all_simd", 
                  simd = "simd2006_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2007, data = "DataZone2001_all_simd", 
                  simd = "simd2009v2_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2008, data = "DataZone2001_all_simd", 
                  simd = "simd2009v2_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2009, data = "DataZone2001_all_simd", 
                  simd = "simd2009v2_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2010, data = "DataZone2001_all_simd", 
                  simd = "simd2012_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2011, data = "DataZone2001_all_simd", 
                  simd = "simd2012_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2012, data = "DataZone2001_all_simd", 
                  simd = "simd2012_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2013, data = "DataZone2001_all_simd", 
                  simd = "simd2012_emp_dep_n", datazone = "datazone2001"),
  read_simd_quint(year = 2014, data = "DataZone2011_simd2016", 
                  simd = "simd2016_emp_dep_n", datazone = "datazone2011"),
  read_simd_quint(year = 2015, data = "DataZone2011_simd2016", 
                  simd = "simd2016_emp_dep_n", datazone = "datazone2011"),
  read_simd_quint(year = 2016, data = "DataZone2011_simd2016", 
                  simd = "simd2016_emp_dep_n", datazone = "datazone2011"),
  read_simd_quint(year = 2017, data = "DataZone2011_simd2016", 
                  simd = "simd2016_emp_dep_n", datazone = "datazone2011")
))

#File for deprivation analysis
saveRDS(data_employ, file = paste0(data_folder, "Prepared Data/employment_deprived_depr_raw.rds"))

#File for DZ11 for 2014 onwards
data_employdz11 <- data_employ %>% filter(year>2013)

saveRDS(data_employdz11, file = paste0(data_folder, "Prepared Data/employment_deprived_dz11_raw.rds"))

#Preparing file for CA for period 2004 to 2013
data_employdz01 <- data_employ %>% filter(year<2014)
#Lookup file for CA
ca_lookup <- read.spss('/conf/linkage/output/lookups/geography/other_ref_files/DataZone2001.sav',
                       to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(ca2011, datazone2001)

#Merging with lookup and aggregating by ca
data_employdz01 <- left_join(data_employdz01, ca_lookup, by = c("datazone" = "datazone2001")) %>% 
  rename(ca = ca2011) %>% group_by(ca, year) %>% summarise(numerator=sum(numerator, na.rm = T))

saveRDS(data_employdz01, file = paste0(data_folder, "Prepared Data/employment_deprived_ca_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
#Normal indicator analysis, first for CA and then DZ11
analyze_first(filename = "employment_deprived_ca", geography = "council", measure = "percent", 
              yearstart = 2004, yearend = 2013, time_agg = 1, pop = "CA_working_pop")
analyze_first(filename = "employment_deprived_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2014, yearend = 2017, time_agg = 1, pop = "DZ11_working_pop")

#Merging CA and DZ11 together
all_data <- rbind(readRDS(paste0(data_folder, "Temporal/employment_deprived_dz11_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporal/employment_deprived_ca_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporal/employment_deprived_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "employment_deprived_all", measure = "percent", time_agg = 1, 
               ind_id = 20702, year_type = "school", profile = "HN", min_opt = 2999)

###############################################.
#Deprivation analysis function
analyze_deprivation(filename="employment_deprived_depr", measure="percent", time_agg=1, 
                    yearstart= 2004, yearend=2017, year_type = "calendar", 
                    pop = "depr_working_pop", ind_id = 20702)

#END