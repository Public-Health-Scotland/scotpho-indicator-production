# ScotPHO indicators: ABIs delivered ### WORK IN PROGRESS ##

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
ABIs_delivered <- read_csv(paste0(data_folder, "Received Data/2019-06-25_AlcoholBriefInterventions.csv")) %>%
  setNames(tolower(names(.))) %>%
  rename(year = finyear, code = nhsboard, numerator = number, rate = percentage) %>%
  group_by(code, ind_id, year, rate) %>% 
  summarise(numerator = sum(numerator, na.rm =T)) %>% ungroup() %>%
  mutate(code = recode(code, "Scotland" = "S00000001", "NHS Ayrshire & Arran" = "S08000015", "NHS Borders" = "S08000016",
                             "NHS Dumfries & Galloway" = "S08000017", "NHS Grampian" = "S08000020", "NHS Tayside" = "S08000030",
                             "NHS Lothian" = "S08000024", "NHS Greater Glasgow & Clyde" = "S08000031", "NHS Lanarkshire" = "S08000032",
                             "NHS Fife" = "S08000029", "NHS Highland" = "S08000022", "NHS Shetland" = "S08000026",
                             "NHS Forth Valley" = "S08000019", "NHS Orkney" = "S08000025", "NHS Western Isles" = "S08000028"),
         trend_axis = paste0(year, "/", substr(year+1, 3,4)),
         lowci = NA, upci = NA, def_period = NA)

saveRDS(ABIs_delivered, file=paste0(data_folder, 'Data to be checked/ABIs_delivered_final.rds'))




