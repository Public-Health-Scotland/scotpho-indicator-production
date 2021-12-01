# ScotPHO indicators: Alcohol Waiting Times

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
#Reading data provided by AWT team
awt <- read_excel(paste0(data_folder, "Received Data/alcohol_waiting_times_2020.xlsx"), 
                               sheet = "alcohol_waiting_times") %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  mutate(code = case_when( #dumfries ADP missing, fife, western isles/outer hebrides, orkney, shetland, borders, lanark needs to be merged
    geography == "Clackmannanshire ADP" ~ "S11000005", geography == "Falkirk ADP" ~ "S11000013", 
    geography == "Stirling ADP" ~ "S11000029",   geography == "East Ayrshire ADP" ~ "S11000008", geography == "North Ayrshire ADP" ~ "S11000020", 
    geography == "South Ayrshire ADP" ~ "S11000027", geography == "Midlothian and East Lothian" ~ "S11000051", 
    geography == "West Lothian ADP" ~ "S11000031", geography == "Edinburgh City ADP" ~ "S11000012", 
    geography == "East Renfrewshire ADP" ~ "S11000011", geography == "East Dunbartonshire ADP" ~ "S11000009", 
    geography == "Inverclyde ADP" ~ "S11000017", geography == "Renfrewshire ADP" ~ "S11000024", 
    geography == "West Dunbartonshire ADP" ~ "S11000030", geography == "Mid and East Lothian ADP" ~ "S11000051", 
    geography == "Glasgow City ADP" ~ "S11000015", geography == "Highland ADP" ~ "S11000016", 
    geography == "Argyll & Bute ADP" ~ "S11000004", geography == "Moray ADP" ~ "S11000019", 
    geography == "Aberdeen City ADP" ~ "S11000001", geography == "Aberdeenshire ADP" ~ "S11000002", 
    geography == "Perth & Kinross ADP" ~ "S11000023", geography == "Angus ADP" ~ "S11000003", geography == "Dundee City ADP" ~ "S11000007", 
    geography == "Ayrshire & Arran" ~ "S08000015", 
    geography == "Borders" ~ "S08000016", geography == "Dumfries & Galloway" ~ "S08000017", 
    geography == "Fife" ~ "S08000029", geography == "Forth Valley" ~ "S08000019",
    geography == "Grampian" ~ "S08000020", geography == "Greater Glasgow & Clyde" ~ "S08000031", 
    geography == "Highland" ~ "S08000022", geography == "Lanarkshire" ~ "S08000032", 
    geography == "Lothian" ~ "S08000024", geography == "Orkney" ~ "S08000025", 
    geography == "Shetland" ~ "S08000026", geography == "Tayside" ~ "S08000030", geography == "Western Isles" ~ "S08000028", 
    geography == "Scotland" ~ "S00000001", TRUE ~ "Error")) %>% 
  mutate(year = as.numeric(substr(year,1,4))) %>%   #converting to numeric as needed for functions 
  filter(!(geography %in% c("East Lothian", "Mid Lothian", 
                            "South Lanarkshire", "North Lanarkshire")  )) # excluding councils

# Duplicating rows for ADPs not included in file using HB data.
awt_agg <- awt %>%   
  filter(geography %in% c("Fife", "Dumfries & Galloway", "Orkney", "Shetland",
                          "Western Isles", "Borders", "Lanarkshire")  ) %>%  
  mutate(code = case_when( #dumfries ADP missing, fife, western isles/outer hebrides, orkney, shetland, borders, lanark needs to be merged
    geography == "Borders" ~ "S11000025", geography == "Dumfries & Galloway" ~ "S11000006", 
    geography == "Fife" ~ "S11000014", 
    geography == "Lanarkshire" ~ "S11000052", geography == "Orkney" ~ "S11000022", 
    geography == "Shetland" ~ "S11000026", geography == "Western Isles" ~ "S11000032", TRUE ~ "Error")) 

awt <- rbind(awt, awt_agg)  

saveRDS(awt, file=paste0(data_folder, 'Temporary/Alcohol_waiting_times_formatted.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.
analyze_second(filename = "Alcohol_waiting_times", measure = "percent", 
               time_agg = 1, ind_id = 4119, year_type = "financial")

##END
