# ScotPHO indicators: SDMD follow-up completeness and SDMD initial completeness
# Raw files comes from ISD drugs team. 
# Initial: Individuals on SDMD compared with those at DATWT. 
# Follow up: Individuals on SDMD with 12 week follow up record. 

#   Part 1 - Prepare basefiles
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
server_desktop <- "server" # change depending if you are using R server or R desktop

source("./1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
#Initial completeness file from drugs team
sdmd_initial <- read.spss(paste0(data_folder, "Received Data/IR2018-01102_SDMD_compliance_with_DATWT.sav"),
                                to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(denominator = sdmdpeople, numerator = datwtpeople) %>% select(-hbt)

saveRDS(sdmd_initial, file=paste0(data_folder, 'Temporary/sdmd_initialcompl_formatted.rds'))

#Follow-up completeness file from drugs team
sdmd_follow <- read.spss(paste0(data_folder, "Received Data/IR2018-01102_SDMD_follow_up_completeness.sav"),
                              to.data.frame=TRUE, use.value.labels=FALSE) %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(denominator = sdmdpeople, numerator = followuppeople) %>% select(-name)

saveRDS(sdmd_follow, file=paste0(data_folder, 'Temporary/sdmd_followcompl_formatted.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
#Initial completeness
analyze_second(filename = "sdmd_initialcompl", measure = "percent", time_agg = 1, 
               ind_id = 4137, year_type = "financial", profile = "DU", min_opt = 160720)

#Follow-up completeness
analyze_second(filename = "sdmd_followcompl", measure = "percent", time_agg = 1, 
               ind_id = 4138, year_type = "financial", profile = "DU", min_opt = 160450)


##END