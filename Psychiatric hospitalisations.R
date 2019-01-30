# ScotPHO indicators: Patients with a Psychiatric Hospitalisation. 

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(odbc) #for reading oracle databases

server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

#Looking to admissions with a main diagnosis of asthma, excluding unknown sex, by financial year. 
#Creates one record per CIS and selects only one case per patient/year.
data_psychiatric <- tbl_df(dbGetQuery(channel, statement=
                                        "SELECT distinct link_no linkno, max(AGE_IN_YEARS) age, max(SEX) sex_grp, max(DATAZONE_2001) datazone_2001, max(DATAZONE_2011) datazone_2011,
                                      CASE WHEN extract(month from admission_date) > 3 THEN extract(year from admission_date) 
                                      ELSE extract(year from discharge_date) -1 END as year 
                                      FROM ANALYSIS.SMR04_PI z
                                      WHERE discharge_date between '1 April 2002' and '31 March 2018'
                                      AND speciality <> 'G5' 
                                      AND sex <> 0 
                                      AND datazone_2011 is not null 
                                      GROUP BY link_no, CASE WHEN extract(month from discharge_date) > 3 THEN
                                      extract(year from discharge_date) ELSE extract(year from discharge_date) -1 END")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Creating age groups for standardization.
# mutate add new column for age_grp
data_psychiatric <- data_psychiatric %>% mutate(age_grp = case_when( 
  age < 5 ~ 1, age > 4 & age <10 ~ 2, age > 9 & age <15 ~ 3, age > 14 & age <20 ~ 4,
  age > 19 & age <25 ~ 5, age > 24 & age <30 ~ 6, age > 29 & age <35 ~ 7, 
  age > 34 & age <40 ~ 8, age > 39 & age <45 ~ 9, age > 44 & age <50 ~ 10,
  age > 49 & age <55 ~ 11, age > 54 & age <60 ~ 12, age > 59 & age <65 ~ 13, 
  age > 64 & age <70 ~ 14, age > 69 & age <75 ~ 15, age > 74 & age <80 ~ 16,
  age > 79 & age <85 ~ 17, age > 84 & age <90 ~ 18, age > 89 ~ 19, 
  TRUE ~ as.numeric(age)
))

# Datazone2001 - Equivalent of aggregate
dz01 <- data_psychiatric %>% group_by(year, datazone_2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone_2001)

#equivalent of select if
dz01 <- dz01 %>% subset(year<2011) 

saveRDS(dz01, file=paste0(data_folder, 'Prepared Data/psychiatric_discharges_dz01_raw.rds'))
datadz01 <- readRDS(paste0(data_folder, 'Prepared Data/psychiatric_discharges_dz01_raw.rds'))


# Datazone2011 - Equivalent of aggregate
dz11 <- data_psychiatric %>% group_by(year, datazone_2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone_2011)

saveRDS(dz11, file=paste0(data_folder, 'Prepared Data/psychiatric_discharges_dz11_raw.rds'))
datadz11 <- readRDS(paste0(data_folder, 'Prepared Data/psychiatric_discharges_dz11_raw.rds'))

###############################################.
# IR basefile
#equivalent of select if
ir_file <- dz11 %>% subset(year>2010) %>% subset(datazone>'S01006505')
ir_file <- rbind(dz01, ir_file) #joining together

saveRDS(ir_file, file=paste0(data_folder, 'Prepared Data/DZ_psychistric_discharges_IR_raw.rds'))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.
#All patients psychiatric discharge
analyze_first(filename = "psychiatric_discharges_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2017,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "psychiatric_discharges_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20402, year_type = "financial", 
               profile = "HN", min_opt = 1347346)



odbcClose(channel) # closing connection to SMRA

##END