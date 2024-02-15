#Abbie's comment
# ScotPHO indicator: Drug and alcohol co-dependency treatment waiting times - % where LDP Standard (90%) not met
# New indicator added in October 2022

#   Part 1 - Create basefile
#   Part 2 - Format  Basefile for macro
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by DWT team
cwt <- read_csv(paste0(data_folder, "Received Data//Drug and alcohol treatment waiting times/2023 request/co-dependency_waiting_times.csv")) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  mutate(code = case_when( #create a code variable for each ADP and HB
    geography == "Clackmannanshire_ADP" ~ "S11000005", geography == "Falkirk_ADP" ~ "S11000013", 
    geography == "Stirling_ADP" ~ "S11000029", geography == "East Ayrshire_ADP" ~ "S11000008", 
    geography == "North Ayrshire_ADP" ~ "S11000020", geography == "South Ayrshire_ADP" ~ "S11000027", 
    geography == "Mid and East Lothian_ADP" ~ "S11000051", geography == "West Lothian_ADP" ~ "S11000031", 
    geography == "City of Edinburgh_ADP" ~ "S11000012", geography == "East Renfrewshire_ADP" ~ "S11000011", 
    geography == "East Dunbartonshire_ADP" ~ "S11000009", geography == "Inverclyde_ADP" ~ "S11000017", 
    geography == "Renfrewshire_ADP" ~ "S11000024", geography == "West Dunbartonshire_ADP" ~ "S11000030", 
    geography == "City of Glasgow_ADP" ~ "S11000015", geography == "Highland_ADP" ~ "S11000016", 
    geography == "Argyll & Bute_ADP" ~ "S11000004", geography == "Moray_ADP" ~ "S11000019", 
    geography == "Aberdeen City_ADP" ~ "S11000001", geography == "Aberdeenshire_ADP" ~ "S11000002", 
    geography == "Perth & Kinross_ADP" ~ "S11000023", geography == "Angus_ADP" ~ "S11000003", 
    geography == "Dundee City_ADP" ~ "S11000007", geography == "Dumfries & Galloway_ADP" ~ "S11000006", 
    geography == "Fife_ADP" ~ "S11000014", geography == "Orkney Islands_ADP" ~ "S11000022", 
    geography == "Scottish Borders_ADP" ~ "S11000025", geography == "Shetland Islands_ADP" ~ "S11000026", 
    geography == "Western Isles_ADP" ~ "S11000032", geography == "Ayrshire & Arran_HB" ~ "S08000015", 
    geography == "Borders_HB" ~ "S08000016", geography == "Dumfries & Galloway_HB" ~ "S08000017", 
    geography == "Fife_HB" ~ "S08000029", geography == "Forth Valley_HB" ~ "S08000019",
    geography == "Grampian_HB" ~ "S08000020", geography == "Greater Glasgow & Clyde_HB" ~ "S08000031", 
    geography == "Highland_HB" ~ "S08000022", geography == "Lanarkshire_HB" ~ "S08000032", 
    geography == "Lothian_HB" ~ "S08000024", geography == "Orkney_HB" ~ "S08000025", 
    geography == "Shetland_HB" ~ "S08000026", geography == "Tayside_HB" ~ "S08000030", 
    geography == "Western Isles_HB" ~ "S08000028", 
    geography == "Scotland" ~ "S00000001", TRUE ~ "Error")) %>% 
  mutate(year = as.numeric(substr(financial_year,1,4)))   #converting to numeric as needed for functions 


# ADPs for North and South Lanarkshire need to be combined as they are still combined as Lanarkshire in Profiles lookups
cwt <- cwt %>%
  mutate(code = case_when(geography == "South Lanarkshire_ADP" ~ "S11000052",
                          geography == "North Lanarkshire_ADP" ~ "S11000052",
                          TRUE ~ as.character(code))) %>% 
  mutate(geography = case_when(code == "S11000052" ~ "Lanarkshire_ADP",
                               TRUE ~ as.character(geography))) %>% 
  group_by(year, geography, code) %>% 
  summarise(numerator = sum(numerator),
            denominator = sum(denominator)) %>% 
  ungroup()


saveRDS(cwt, file=paste0(data_folder, 'Temporary/Co-dependency_waiting_times_formatted.rds'))

###############################################.
## Part 2 - Call analysis macros ----
###############################################.
analyze_second(filename = "Co-dependency_waiting_times", measure = "percent", 
               time_agg = 1, ind_id = 4150, year_type = "financial")


##END