## ScotPHO Profile Indicator:  Dying in hospital 

#   The number/percantage of deaths from all causes that occurred in hospital in a given year.
#   Numerator = Number of deaths from any cause that occurred in hospital in a given year.
#   Denominator = Number of deaths from any cause in a given year, irrespective of the place of death.

#   Part 1 - Extract deaths occuring in hospital from SMR01 dataset
#   Part 2 - Extract total deaths from SMRA deaths dataset
#   Part 3 - Combined hospital deaths and total deaths files 
#   Part 4 - Run analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


###########################################################################.
## Part 1 - Extract deaths occuring in hospital from SMR01 dataset ----
###########################################################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extract smr01 records where cases discharged type is 40,41,42,43 - all codes where patient discharged dead.
data_hosp_deaths_raw <- as_tibble(dbGetQuery(channel, statement=
                                            "SELECT  
                                          LINK_NO, DR_POSTCODE pc7, CIS_MARKER,
                                          CASE WHEN extract(month from discharge_date) > 3 
                                          THEN extract(year from discharge_date) 
                                          ELSE extract(year from discharge_date) -1 END as year
                                          FROM ANALYSIS.SMR01_PI z 
                                          WHERE DISCHARGE_DATE between '1 April 2002' AND '31 March 2023' 
                                          AND DISCHARGE_TYPE in (40,41,42,43) ")) %>%
  
  setNames(tolower(names(.)))

# Bringing LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2023_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

# match deaths data and datazone lookup
data_hosp_deaths <- left_join(data_hosp_deaths_raw, postcode_lookup, "pc7") %>% 
  select(link_no, cis_marker, year, datazone2001, datazone2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors     

# Datazone2011 basefile
hosp_deaths_dz11 <- data_hosp_deaths %>% group_by(year, datazone2011) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

#saveRDS(hosp_deaths_dz11, file=paste0(data_folder, 'Prepared Data/hosp_deaths_dz11_raw.rds'))


# Deprivation basefile
# Datazone2001. DZ 2001 data needed up to 2013 to enable matching to advised SIMD
hosp_deaths_dz01 <- data_hosp_deaths %>% count(year, datazone2001, name = "numerator") %>%  
  subset(year<=2013) %>% rename(datazone = datazone2001)

hosp_deaths_dz01 <- data_hosp_deaths %>% group_by(year, datazone2001) %>%  
  summarize(numerator = n()) %>% ungroup() %>% subset(year<=2013) %>% rename(datazone = datazone2001)

hosp_deaths_depr <- rbind(hosp_deaths_dz01, hosp_deaths_dz11 %>% subset(year>=2014)) #join dz01 and dz11

###########################################################################.
## Part 2 - Extract total deaths from SMRA deaths dataset ----
###########################################################################.

all_deaths_raw <- as_tibble(dbGetQuery(channel, statement=
                                      "SELECT POSTCODE pc7, 
                                    CASE WHEN extract(month from DATE_OF_DEATH) > 3 
                                    THEN extract(year from DATE_OF_DEATH) 
                                    ELSE extract(year from DATE_OF_DEATH) -1 END as year
                                    FROM ANALYSIS.GRO_DEATHS_C
                                    WHERE sex <> 9
                                    AND DATE_OF_DEATH between '1 April 2002' and '31 March 2023'")) %>% 
  
  setNames(tolower(names(.)))  #variables to lower case      

data_all_deaths <- left_join(all_deaths_raw, postcode_lookup, "pc7") %>% 
  select(year, datazone2001, datazone2011) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors     

# Datazone2011 basefile
data_all_deaths_dz11 <- data_all_deaths %>% count(year, datazone2011, name = "denominator") %>%  
  rename(datazone = datazone2011)

# Datazone2001 basefile
data_all_deaths_dz01 <- data_all_deaths %>% count(year, datazone2001, name = "denominator") %>%  
  rename(datazone = datazone2001)

all_deaths_depr <- rbind(data_all_deaths_dz01 %>% subset(year<2014), data_all_deaths_dz11 %>% subset(year>=2014)) #join dz01 and dz11 

###########################################################################.
## Part 3 - Combined hospital deaths and total deaths files ----
###########################################################################.

# generate main profile tool dying in hospital basefile
dying_in_hosp <- left_join(data_all_deaths_dz11, hosp_deaths_dz11, by = c("year", "datazone")) %>% 
  replace_na(list(numerator=0))

saveRDS(dying_in_hosp, file=paste0(data_folder, 'Prepared Data/dying_in_hosp_raw.rds'))

# generate deprivation module dying in hospital basefile
depr_deaths <- left_join(all_deaths_depr, hosp_deaths_depr, by = c("year", "datazone")) %>% 
  replace_na(list(numerator=0))

saveRDS(depr_deaths, file=paste0(data_folder, 'Prepared Data/dying_in_hosp_depr_raw.rds'))


###############################################.
## Part 4 - Run analysis functions ----
###############################################.

#first analysis function  
analyze_first(filename = "dying_in_hosp", geography = "datazone11",
              measure = "percent", yearstart = 2002, yearend = 2023,
              time_agg = 3)

#second analysis function 
analyze_second(filename = "dying_in_hosp", measure = "percent", 
               time_agg = 3, ind_id = "6", year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="dying_in_hosp_depr", measure="percent", time_agg = 3, 
                    yearstart= 2002, yearend=2023,   year_type = "calendar", 
                    ind_id = 6)  
