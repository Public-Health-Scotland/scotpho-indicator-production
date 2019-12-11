# ScotPHO indicators: smoking during pregnancy quintile 1

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") #Deprivation functions

#Function to create files for each quintile
create_quintile_data <- function(quint_number) {
  smoking_preg_quint <-smok_preg_depr %>% 
    subset(quintile == quint_number) %>%
    select(year, datazone, numerator, denominator)
  
  saveRDS(smoking_preg_quint, file=paste0(data_folder, 'Prepared Data/smoking_preg_q', quint_number, '_raw.rds'))
}

###############################################.
## Part 1 - Format raw data ready for analysis functions ----
###############################################.
# Bringing both dz01 and dz11 data as we use dz01 for period before 2014 and dz11 onwards
smoking_pregnant_dz11 <- read_csv(paste0(data_folder, "Received Data/IR2019-01566(smoking).csv")) %>%
  setNames(tolower(names(.))) %>%
  rename(year = finyear, datazone = datazone2011, numerator = smoker, 
         denominator = known_status) %>%
# As mentioned by the maternity team when receiving the data, the year variable refers to the year end e.g. 2004 = 2003/04.
# Change this to fit the method used for profiles i.e year=start of financial year.
  mutate(year = year-1) 

# Saving file used for all women
saveRDS(smoking_pregnant_dz11, file=paste0(data_folder, 'Prepared Data/smoking_preg_raw.rds'))

# Datazone 2001 data
smoking_pregnant_dz01 <- read_excel(paste0(data_folder, "Received Data/smoking pregnancy (IR2015-02431) dz01-dont delete.xlsx"), 
                                    sheet = "smoking pregnancy (IR2015-02431") %>%
  setNames(tolower(names(.))) %>%
  rename(year = finyear, datazone = datazone2001, numerator = smk, 
         denominator = total) %>%
  # As mentioned by the maternity team when receiving the data, the year variable refers to the year end e.g. 2004 = 2003/04.
  # Change this to fit the method used for profiles i.e year=start of financial year.
  mutate(year = year-1) 

# Joining them together
smok_preg_depr <- rbind(smoking_pregnant_dz11 %>% filter(year>2013),
                        smoking_pregnant_dz01 %>% filter(year<2014)) 

# Reading deprivation lookup
dep_lookup <- read_rds(paste0(data_folder, "Lookups/Geography/deprivation_geography.rds")) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  select(hb, ca, datazone, sc_quin, year) %>%
  rename(quintile = sc_quin)

# Joining with deprivation lookup
smok_preg_depr <- left_join(smok_preg_depr, dep_lookup, by=c("datazone", "year")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(year, hb, ca, datazone, quintile, numerator, denominator) %>%
  mutate_if(is.character, factor) # converting variables into factors

# Saving file used for the health inequalities section
saveRDS(smok_preg_depr %>% select(datazone, year, numerator, denominator), 
        file=paste0(data_folder, 'Prepared Data/smoking_preg_depr_raw.rds'))

# Creating a file for each quintile
mapply(create_quintile_data, quint_number = c(1,2,3,4,5))

###############################################.
## Part 2 - calling the analysis functions
###############################################.
# For all quintiles analysis
analyze_first(filename = "smoking_preg", geography = "datazone11", measure = "percent", 
              yearstart = 2003, yearend = 2018, time_agg = 3)

analyze_second(filename = "smoking_preg", measure = "percent", time_agg = 3, 
               ind_id = 21002, year_type = "financial")

#Deprivation analysis function
analyze_deprivation(filename="smoking_preg_depr", measure="percent", time_agg=3, 
                    yearstart= 2003, yearend=2018, year_type = "financial", ind_id = 21002)

###############################################.
# For deprivation quintile indicators 
quint_files <- c("smoking_preg_q1", "smoking_preg_q2", "smoking_preg_q3", 
                 "smoking_preg_q4", "smoking_preg_q5")

mapply(analyze_first, filename = quint_files, geography = "datazone11", 
       measure = "percent", yearstart = 2003, yearend = 2018, time_agg = 3)

mapply(analyze_second, filename = quint_files, measure = "percent", time_agg = 3, 
       year_type = "financial", ind_id = c(1521, 1522, 1523, 1524, 1525))

#END
