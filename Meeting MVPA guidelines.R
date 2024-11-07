# ScotPHO indicators: Percentage of Scottish population meeting the MPVA physical activity guidelines (#88007)

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#read in HB/CA lookups
ca_codes <- readRDS(paste0(lookups, "Geography/CAdictionary.rds")) |> 
  mutate(Geographylevel = "Local Authority") #adding area type col
hb_codes <- readRDS(paste0(lookups, "Geography/HBdictionary.rds")) |> 
  mutate(Geographylevel = "Health Board")

lookup<-rbind(ca_codes, hb_codes) |> 
  rename(Location = areaname) #aligning for lookup


#read in main data (no splits)
mvpa <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/shs_activity_levels_rank_data_2012_15_2018_22_v2.csv")) |>
  select(-c(4,8,11)) |> #drop unnecessary cols
  filter(Categories == "Meets recommendations",
         Sex == "All") |>  #filter for both sexes and for only meets recommendations
  mutate(Location = case_when(Geographylevel == "Health Board"~ paste0("NHS ", Location), #Paste NHS onto the front of HB names to match lookup
                              Location == "Edinburgh City" ~ "City of Edinburgh", #Change Edinburgh City to City of Edinburgh
                              TRUE ~ Location) 
         )


mvpa <- left_join(mvpa, lookup) |> 
  mutate(code = case_when(Location == "Scotland" ~ "S00000001", #Add in Scotland code
                          TRUE ~ code)) |> 
  mutate(ind_id = c("88007"), #add indicator id
         numerator = "NA", #add numerator
         year = as.numeric(str_sub(Year, 1, 4))+ 1, #create year variable from second year of range
         def_period = paste0((str_replace(Year, pattern = c("-"),  replacement = " to ")), " calendar years; 4-year aggregates"))  |> 
  rename(rate = Percent, #renaming columns to necessary format
         trend_axis = Year,
         lowci = LowerCI,
         upci = UpperCI) |> 
  select(c(9:10, 12, 11, 6:8, 13, 1)) #dropping and reordering columns
  

###############################################.
## Part 2 - Save Basefile ----
###############################################.

saveRDS(mvpa, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny.rds"))
write.csv(mvpa, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny.csv"),row.names = F)
