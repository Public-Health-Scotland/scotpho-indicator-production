# ScotPHO indicators: 
# Young people living in the most access deprived quintile, aged 0-25 years
# Young people living in the most crime deprived quintile, aged 0-25 years
# Young people living in the most income deprived quintile, aged 0-25 years

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

#Small function to standarize each years info. Function parameters:
#Data is for what basefile to use, list_pos is for the position of the data frame
#simd for which simd variables-year to look at, year for what year is the data created.
prepare_file <- function(dz_list) {
  raw_data <<- readRDS(paste0(lookups, "Population/DZ11_pop_basefile.rds")) %>% 
    filter(age<26 & datazone2011 %in% dz_list & year>2010) %>% group_by(year, datazone2011) %>% 
    summarise(numerator = sum(denominator, na.rm= T)) %>% ungroup %>% 
    rename(datazone = datazone2011)
  
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.

simd_data <- readRDS(paste0(cl_out_depr, 'DataZone2011_simd2016.rds')) %>% 
  setNames(tolower(names(.))) %>% 
  select(datazone2011, simd2016_crime_rank, simd2016_access_rank, simd2016_inc_rank)

# Population 25 or under
pop <- readRDS(paste0(lookups, "Population/DZ11_pop_basefile.rds")) %>% 
  filter(year == "2014") %>% group_by(year, datazone2011) %>% 
  summarise(pop = sum(denominator, na.rm= T)) %>% ungroup

pop_total <- pop %>% group_by(year) %>% summarise(pop = sum(pop, na.rm= T)) %>% 
  ungroup %>% pull(pop)
  
cut_breaks <- c(0, pop_total/5, pop_total/5*2, pop_total/5*3, pop_total/5*4, pop_total)

simd_data <- left_join(simd_data, pop, by = c("datazone2011")) %>% 
  arrange(simd2016_crime_rank) %>% # crime pop weighted quintile
  mutate(cum_pop_crime=cumsum(pop),
         crime_quintile = as.numeric(paste(cut(cum_pop_crime, cut_breaks, include.lowest=TRUE,
                                        labels=c("5", "4", "3", "2", "1"))))) %>% 
  arrange(simd2016_access_rank) %>% # access pop weighted quintile
  mutate(cum_pop_access=cumsum(pop),
         access_quintile = as.numeric(paste(cut(cum_pop_access, cut_breaks, include.lowest=TRUE,
                                               labels=c("5", "4", "3", "2", "1"))))) %>% 
  arrange(simd2016_inc_rank) %>% # income pop weighted quintile
  mutate(cum_pop_inc=cumsum(pop),
         inc_quintile = as.numeric(paste(cut(cum_pop_inc, cut_breaks, include.lowest=TRUE,
                                               labels=c("5", "4", "3", "2", "1"))))) %>% 
  select(-starts_with("cum_pop"), -starts_with("simd"))

crime_dz <- simd_data %>% filter(crime_quintile == "5") %>% pull(datazone2011)
inc_dz <- simd_data %>% filter(inc_quintile == "5") %>% pull(datazone2011)
access_dz <- simd_data %>% filter(access_quintile == "5") %>% pull(datazone2011)

saveRDS(prepare_file(inc_dz), paste0(data_folder, "Prepared Data/young_people_income_raw.rds"))
saveRDS(prepare_file(crime_dz), paste0(data_folder, "Prepared Data/young_people_crime_raw.rds"))
saveRDS(prepare_file(access_dz), paste0(data_folder, "Prepared Data/young_people_access_raw.rds"))

###############################################.
## Part 2 - Calling the analysis functions ----
###############################################.
###############################################.
# Crime 
analyze_first(filename = "young_people_crime", geography = "datazone11", measure = "percent", 
              yearstart = 2011, yearend = 2018, time_agg = 1, pop = "DZ11_pop_under26")

analyze_second(filename = "young_people_crime", measure = "percent", time_agg = 1, 
               ind_id = 13005, year_type = "calendar", qa = F)

###############################################.
# Access 
analyze_first(filename = "young_people_access", geography = "datazone11", measure = "percent", 
              yearstart = 2011, yearend = 2018, time_agg = 1, pop = "DZ11_pop_under26")

analyze_second(filename = "young_people_access", measure = "percent", time_agg = 1, 
               ind_id = 13003, year_type = "calendar", qa = F)

###############################################.
# Income 
analyze_first(filename = "young_people_income", geography = "datazone11", measure = "percent", 
              yearstart = 2011, yearend = 2018, time_agg = 1, pop = "DZ11_pop_under26")

analyze_second(filename = "young_people_income", measure = "percent", time_agg = 1, 
               ind_id = 13004, year_type = "calendar", qa = F)
## END

