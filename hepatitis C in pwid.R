# ScotPHO indicators: Hepatitis C in people who injects drugs

#   Part 1 - read in data
#   Part 2 - Prepare geographies
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Read in data ----
###############################################.

hepc_pwid <- read_csv(paste0(data_folder, "Received Data/Hep C Among PWID_2008-09 to 2017-18_compiled.csv")) %>%
  setNames(tolower(names(.))) %>% 
  rename("areaname" = "code") %>%
  mutate(areaname = recode(areaname, "AA" = "NHS Ayrshire and Arran", "BR" = "NHS Borders", "DG" = "NHS Dumfries and Galloway",
                           "FF" = "NHS Fife", "FV" = "NHS Forth Valley", "GGC" = "NHS Greater Glasgow and Clyde",
                           "GR" = "NHS Grampian", "HG" = "NHS Highland", "LN" = "NHS Lanarkshire", "LO" = "NHS Lothian",
                           "TY" = "NHS Tayside")) %>%
  mutate_at(c("denominator", "numerator"), as.numeric)

geography_codes <- readRDS('/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/codedictionary.rds') %>% 
  filter(substr(code,1,3) %in% c("S08", "S12", "S00")) #names only needed for HB, CA, Scot

hepc_pwid <- left_join(hepc_pwid, geography_codes, "areaname") %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% #format as financial year
  select(year, code, numerator, denominator) 

# Aggregating to obtain ADP level
hepc_adp <- hepc_pwid %>% filter(substr(code,1,3) == "S12") %>% 
  mutate(code = case_when(
    code == "S12000005" ~ "S11000005", code == "S12000006" ~ "S11000006", code == "S12000008" ~ "S11000008", 
    code == "S12000010" ~ "S11000051", code == "S12000011" ~ "S11000011", code == "S12000014" ~ "S11000013", 
    code == "S12000017" ~ "S11000016", code == "S12000018" ~ "S11000017", code == "S12000019" ~ "S11000051", 
    code == "S12000020" ~ "S11000019", code == "S12000021" ~ "S11000020", code == "S12000026" ~ "S11000025", 
    code == "S12000028" ~ "S11000027", code == "S12000029" ~ "S11000052", code == "S12000030" ~ "S11000029", 
    code == "S12000033" ~ "S11000001", code == "S12000034" ~ "S11000002", code == "S12000035" ~ "S11000004", 
    code == "S12000036" ~ "S11000012", code == "S12000038" ~ "S11000024", code == "S12000039" ~ "S11000030", 
    code == "S12000040" ~ "S11000031", code == "S12000041" ~ "S11000003", code == "S12000042" ~ "S11000007", 
    code == "S12000050" ~ "S11000052", code == "S12000045" ~ "S11000009", code == "S12000049" ~ "S11000015", 
    code == "S12000047" ~ "S11000014", code == "S12000048" ~ "S11000023", code == "S12000013" ~ "S11000032", 
    code == "S12000027" ~ "S11000026", code == "S12000023" ~ "S11000022", TRUE ~ "Error")) %>% 
  group_by(year, code) %>% summarise_all(sum, na.rm=T) %>% ungroup()
  
hepc_pwid <- rbind(hepc_adp, hepc_pwid) #joining both datasets together

saveRDS(hepc_pwid, file=paste0(data_folder, 'Temporary/hepc_pwid_formatted.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_second(filename = "hepc_pwid", measure = "percent", time_agg = 1, 
               ind_id = 4122, year_type = "financial")

##END