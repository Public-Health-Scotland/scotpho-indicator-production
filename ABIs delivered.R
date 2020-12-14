# ScotPHO indicators: Alcohol brief interventions delivered #

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") # Normal indicator functions

###############################################.
## - Prepare file ----
###############################################.

# Data from official publication, formatting for tool
abis_delivered <- read_csv(paste0(data_folder, "Received Data/2020_AlcoholBriefInterventions.csv")) %>%
  setNames(tolower(names(.))) %>% #set names to lower case
  rename(year = finyear, code = nhsboard, numerator = number, rate = percentage) %>%
  mutate(code = recode(code, "Scotland" = "S00000001", "NHS Ayrshire & Arran" = "S08000015", "NHS Borders" = "S08000016",
                             "NHS Dumfries & Galloway" = "S08000017", "NHS Grampian" = "S08000020", "NHS Tayside" = "S08000030",
                             "NHS Lothian" = "S08000024", "NHS Greater Glasgow & Clyde" = "S08000031", "NHS Lanarkshire" = "S08000032",
                             "NHS Fife" = "S08000029", "NHS Highland" = "S08000022", "NHS Shetland" = "S08000026",
                             "NHS Forth Valley" = "S08000019", "NHS Orkney" = "S08000025", "NHS Western Isles" = "S08000028"), #change names to codes for tool
         trend_axis = paste0(year, "/", substr(year+1, 3,4)), #show axis labels in financial year layout
         def_period = paste0(trend_axis, " financial year"),
         lowci = NA, upci = NA)

saveRDS(abis_delivered, file=paste0(data_folder, 'Data to be checked/ABIs_delivered_shiny.rds'))
write_csv(abis_delivered, paste0(data_folder, 'Data to be checked/ABIs_delivered_shiny.csv'))

## END
