############################################################################### 
############################################################################### 
########                                                              ######### 
####Children protection parental alcohol, drugs, substance misuse indicator prep ##### 
########                                                              ######### 
############################################################################### 
############################################################################### 

## JP April 2022  

# This script analyses Scottish Government data on the number of children ####
# protection parental alcohol, drugs, substance misuse by a local authority, ADP. 

## Data are downloaded from SG website:
# https://www.gov.scot/collections/childrens-social-work/

################################################################################ 
#####                          install packages etc                        ##### 
################################################################################ 


## Previous years are saved here:
# \\Isdsf00d03\ScotPHO\1.Analysts_space\Jane\data/CSWS 2012-2020 revised 18'19.csv
# A:\ScotPHO Profiles\Data\Received Data/CSWS 2012-2020 revised 18'19.csv

## Manually added 2020/21 data into:
#\\Isdsf00d03\ScotPHO\1.Analysts_space\Jane\data/CP CSWS 2012_21 revised 19'20.xlsx
# \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data/CP CSWS 2012_21 revised 19_20.xlsx

# note that revised data for 2020 - Glasgow drug adn SM, N Ayreshire all 3, 
# S Lanarkshire all 3, W Dunbartonshire alcohol and SM 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./1.indicator_analysis.R") #Normal indicator functions
#source("./2.deprivation_analysis.R") # deprivation function - not required

################################################################################ 
#####                          read in prepared data                       ##### 
################################################################################ 

# read in excel - note now have revised 2020 data as well as new 2021
# values for total of suppressed data also present each year but with no ca identified,  
# these will be added to Scotland total by R but will not appear as a row in final dataset

df_received<- read.excel(filename = "/PHI_conf/ScotPHO/Profiles/Data/Received Data/CP CSWS 2012_21 revised 19_20.xlsx") 

## add la geog codes 
# create dataframe for lookup 
ca_lookup <- data.frame( 
  ca2019 = c("S12000005", "S12000006", "S12000006",  
                       "S12000008", "S12000010", "S12000011",  
                             "S12000013", "S12000013", "S12000013", "S12000014",  
                             "S12000047", "S12000017", "S12000018",  
                               "S12000019", "S12000020", "S12000021",  
                                "S12000023", "S12000048", "S12000048",  
                              "S12000026", "S12000027", "S12000028",  
                                "S12000029", "S12000030", "S12000033",  
                                "S12000034", "S12000035", "S12000035","S12000036",  
                               "S12000036", "S12000038", "S12000039",  
                                "S12000040", "S12000041", "S12000042",  
                               "S12000050", "S12000045", "S12000049"), 
   la = c("Clackmannanshire","Dumfries & Galloway", "Dumfries and Galloway", 
                   "East Ayrshire", "East Lothian", "East Renfrewshire",  
                      "Na h-Eileanan Siar", "Na h-Eilanan Siar", "Eilean Siar","Falkirk",  
                     "Fife", "Highland", "Inverclyde", 
                      "Midlothian", "Moray", "North Ayrshire",  
                      "Orkney Islands", "Perth & Kinross", "Perth and Kinross",  
                      "Scottish Borders", "Shetland Islands", "South Ayrshire",  
                      "South Lanarkshire", "Stirling", "Aberdeen City",  
                      "Aberdeenshire", "Argyll and Bute","Argyll & Bute","Edinburgh, City of",  
                      "City of Edinburgh", "Renfrewshire", "West Dunbartonshire",  
                     "West Lothian", "Angus", "Dundee City",  
                      "North Lanarkshire", "East Dunbartonshire", "Glasgow")) 

# match to raw data (change df name as required) 
df_prepared <- df_received %>% left_join(ca_lookup, by = "la")  %>% 
  rename("ca" = "ca2019")
# check for unmatched cases 
sum(is.na(df_prepared$ca)) 

df_drug <- df_prepared %>% select(c(year, ca, drug)) %>% 
  rename("numerator"="drug") 

df_drug$numerator <- as.numeric(df_drug$numerator)

df_alcohol <- df_prepared %>% select(c(year, ca, alcohol)) %>% 
  rename("numerator"="alcohol")

df_alcohol$numerator <- as.numeric(df_alcohol$numerator)

df_substance_misuse <- df_prepared %>% select(c(year, ca, substance_misuse)) %>% 
  rename("numerator"="substance_misuse")

df_substance_misuse$numerator <- as.numeric(df_substance_misuse$numerator)

saveRDS(df_drug, file=paste0(data_folder, "Prepared Data/df_drug_raw.rds")) 
saveRDS(df_alcohol, file=paste0(data_folder, "Prepared Data/df_alcohol_raw.rds")) 
saveRDS(df_substance_misuse, file=paste0(data_folder, "Prepared Data/df_substance_misuse_raw.rds")) 

###############################################. 
## Part 2 - Run analysis functions ---- 
###############################################. 
# require note in technical document that geographies with suppressed data removed from final shiny output 
# (not removed from raw as will affect Scotland total)

# Parental drug use
analyze_first(filename = "df_drug", geography = "council",               
              measure = "crude", yearstart = 2012, yearend = 2021, 
              time_agg = 1, pop = "CA_pop_under18", adp = TRUE) 

# then complete analysis with the updated '_formatted.rds' file 
analyze_second(filename = "df_drug", measure = "crude", time_agg = 1,               
               ind_id = 4130, year_type = "July snapshot", crude_rate = 10000)
               
##############################
#Parental alcohol use
analyze_first(filename = "df_alcohol", geography = "council",               
              measure = "crude", yearstart = 2012, yearend = 2021, 
              time_agg = 1, pop = "CA_pop_under18", adp = TRUE) 

# then complete analysis with the updated '_formatted.rds' file 
analyze_second(filename = "df_alcohol", measure = "crude", time_agg = 1,               
               ind_id = 4110, year_type = "July snapshot", crude_rate = 10000)
               
########################################
# Parental substance misuse
analyze_first(filename = "df_substance_misuse", geography = "council",               
              measure = "crude", yearstart = 2012, yearend = 2021, 
              time_agg = 1, pop = "CA_pop_under18", adp = TRUE) 

# then complete analysis with the updated '_formatted.rds' file 
analyze_second(filename = "df_substance_misuse", measure = "crude", time_agg = 1,               
               ind_id = 4153, year_type = "July snapshot", crude_rate = 10000)

#### ----------------