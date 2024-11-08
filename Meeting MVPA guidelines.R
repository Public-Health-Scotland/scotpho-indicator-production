# ScotPHO indicators: Percentage of Scottish population meeting the MPVA physical activity guidelines (#88007)
#Potentially renumber indicators in this profile

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #normal indicator functions
source("2.deprivation_analysis.R") #deprivation function


###############################################.
## Part 1 - Prepare file for main dataset ----
###############################################.

#read in HB/CA lookups
ca_codes <- readRDS(paste0(lookups, "Geography/CAdictionary.rds")) |> 
  mutate(Geographylevel = "Local Authority") #adding area type col
hb_codes <- readRDS(paste0(lookups, "Geography/HBdictionary.rds")) |> 
  mutate(Geographylevel = "Health Board")

lookup<-rbind(ca_codes, hb_codes) |> 
  rename(Location = areaname) #aligning col names to match lookup


#read in main data (no splits)
mvpa <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/shs_activity_levels_rank_data_2012_15_2018_22_v2.csv")) |>
  select(-c(4,8,11)) |> #drop unnecessary cols
  filter(Categories == "Meets recommendations",
         Sex == "All") |>  #filter for both sexes combined and for only meets recommendations
  mutate(Location = case_when(Geographylevel == "Health Board"~ paste0("NHS ", Location), #Paste NHS onto the front of HB names to match lookup
                              Location == "Edinburgh City" ~ "City of Edinburgh", #Change Edinburgh City to City of Edinburgh to match lookup
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
  
#save files
saveRDS(mvpa, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny.rds"))
write.csv(mvpa, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny.csv"),row.names = F)

run_qa(filename = "mvpa")

###############################################.
## Part 2 - Prepare deprivation file ----
###############################################.
depr <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/trend_data_simd_2012_2022.csv"))


###############################################.
## Part 3 - Prepare population groups file ----
###############################################.

#Aggregates - HB and CA reporting 4-year aggregate vs. single year figures here. 
#Disability - does there need to be a total? Also do we want 2 disability groups or 3? How would we aggregate anyway?

#read in data files
Sex_split <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/trend_data_sex_2012_2022.csv"))
Age_split <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/trend_data_age_2012_2022.csv"))
Disability_split <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/trend_data_ltc_2012_2022.csv"))

#create data cleaning function
data_cleaning <- function(df){
  
  df_name <- deparse(substitute(df)) #convert df name into a string to extract split name
  
  df <- df |> 
    select(-c(2:3, 10)) |> #drop unnecessary cols
    filter(Categories == "Meets recommendations") |>  #keep only rate of those meeting guidelines
    rename(split_value = 3, #rename third column split_value
           year = Year, #rename other cols to necessary format
           rate = Percent,
           numerator = Mean,
           lowci = LowerCI,
           upci = UpperCI) |> 
    mutate(code = "S00000001", #create col with Scotland s-code
           ind_id = c("88007"), #add indicator id
           split_name = c(str_extract(df_name, "[^_]+")), #extract df name before underscore to populate split_name column
           def_period = paste0(year, " survey year"),
           trend_axis = year) |> 
    select(c(8:10, 3, 1, 5, 4, 6:7, 11:12))
  
}

#run data cleaning function
Sex_split_cleaned <- data_cleaning(Sex_split)
Age_split_cleaned <- data_cleaning(Age_split)
Disability_split_cleaned <- data_cleaning(Disability_split)


#join dfs for sex, age and disability
meeting_mvpa_shiny_popgrp <- rbind(Sex_split_cleaned, Age_split_cleaned, Disability_split_cleaned)

#save files
saveRDS(meeting_mvpa_shiny_popgrp, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny_popgrp.rds"))
write.csv(meeting_mvpa_shiny_popgrp, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny_popgrp.csv"),row.names = F)

