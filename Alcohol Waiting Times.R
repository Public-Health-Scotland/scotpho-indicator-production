# ScotPHO indicators: Alcohol Waiting Times

#   Part 1 - Create basefile
#   Part 2 - Format  Basefile for macro
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
library(readxl) #for reading excel

server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data provided by Prescribing team
AWT_data_extract <- read_excel(paste0(data_folder, "Received Data/Alcohol_waiting_times_2018.xlsx"), 
                               sheet = "Alcohol") %>% 
  setNames(tolower(names(.)))   #variables to lower case

#### Prepare lookup Files

# Create codes for NHS & ADP.
AWT_data_raw <- AWT_data_extract %>%
  mutate(code = case_when( 
    area == "Clackmannanshire ADP" ~ "S11000005", area == "Falkirk ADP" ~ "S11000013", area == "Stirling ADP" ~ "S11000029", 
    area == "Dumfries & Galloway ADP" ~ "S11000006", area == "East Ayrshire ADP" ~ "S11000008", area == "North Ayrshire ADP" ~ "S11000020", 
    area == "South Ayrshire ADP" ~ "S11000027", area == "Midlothian and East Lothian" ~ "S11000051", area == "West Lothian ADP" ~ "S11000031", 
    area == "Edinburgh City ADP" ~ "S11000012", area == "East Renfrewshire ADP" ~ "S11000011", area == "East Dunbartonshire ADP" ~ "S11000009", 
    area == "Inverclyde ADP" ~ "S11000017", area == "Renfrewshire ADP" ~ "S11000024", area == "West Dunbartonshire ADP" ~ "S11000030", 
    area == "Midlothian and East Lothian ADP (MELDAP)" ~ "S11000051", area == "Glasgow City ADP" ~ "S11000015", area == "Outer Hebrides ADP" ~ "S11000032", 
    area == "Fife ADP" ~ "S11000014", area == "Highland ADP" ~ "S11000016", area == "Argyll & Bute ADP" ~ "S11000004", 
    area == "Moray ADP" ~ "S11000019", area == "Aberdeen City ADP" ~ "S11000001", area == "Aberdeenshire ADP" ~ "S11000002", 
    area == "Orkney ADP" ~ "S11000022", area == "Perth & Kinross ADP" ~ "S11000023", area == "Angus ADP" ~ "S11000003", 
    area == "Dundee City ADP" ~ "S11000007", area == "Borders ADP" ~ "S11000025", area == "Shetland ADP" ~ "S11000026", 
    area == "Lanarkshire ADP" ~ "S11000052", area == "Ayrshire & Arran NHS" ~ "S08000015", area == "Borders NHS" ~ "S08000016", 
    area == "Dumfries & Galloway NHS" ~ "S08000017", area == "Fife NHS" ~ "S08000018", area == "Forth Valley NHS" ~ "S08000019",
    area == "Grampian NHS" ~ "S08000020", area == "Greater Glasgow & Clyde NHS" ~ "S08000021", area == "Highland NHS" ~ "S08000022", 
    area == "Lanarkshire NHS" ~ "S08000023", area == "Lothian NHS" ~ "S08000024", area == "Orkney NHS" ~ "S08000025", 
    area == "Shetland NHS" ~ "S08000026", area == "Tayside NHS" ~ "S08000027", area == "Outer Hebrides NHS" ~ "S08000028", TRUE ~ "Error")) 

saveRDS(AWT_data_raw, file=paste0(data_folder, 'Prepared Data/Alcohol_waiting_times_raw.rds'))

###############################################.
## Part 2 - Format  Basefile for macro ----
###############################################.

# Compute Scotland numerator denominator
AWT_data_scotland <- AWT_data_raw %>% filter(str_detect(code, "S08")) %>% mutate(code_s = "S00000001") %>% 
  select(area, denominator, numerator, year, code_s) %>% mutate(area = "Scotland") %>% rename("code" = "code_s") %>%  group_by(area, year, code) %>%
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% ungroup() 

AWT_data_formatted <- full_join(AWT_data_raw, AWT_data_scotland) %>% mutate(type = "percent", time = "single years")

saveRDS(AWT_data_formatted, file=paste0(data_folder, 'Prepared Data/Alcohol_waiting_times_formatted.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.

######################*******************************#############################
analyze_second(filename = "Alcohol_waiting_times", measure = "percent", time_agg = 1, 
               ind_id = 4119, year_type = "financial", profile = "AL", min_opt = 1005872)

#Deprivation analysis function
analyze_deprivation(filename="Alcohol_waiting_times", measure="percent", time_agg=1, 
                    yearstart= 2011, yearend=2017,   year_type = "financial", 
                    pop = "depr_pop_allages", ind_id = 4119)
######################*******************************#############################

##END
