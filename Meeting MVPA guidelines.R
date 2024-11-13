# ScotPHO indicators: Percentage of Scottish population meeting the MPVA physical activity guidelines (#88007)
#Potentially renumber indicators in this profile
#Rurality data presented in existing PA tool but unavailable for download thru Scottish Health Survey Shiny App


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #normal indicator functions
source("2.deprivation_analysis.R") #deprivation function

###############################################.
## Part 1 - Prepare file for main dataset ----
###############################################.

#read in HB/CA lookups and combine
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
                              TRUE ~ Location))


#data cleaning
mvpa <- left_join(mvpa, lookup) |> 
  mutate(code = case_when(Location == "Scotland" ~ "S00000001", #add in Scotland code
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
## Part 2 - Prepare population groups file ----
###############################################.

#Aggregates - HB and CA reporting 4-year aggregate vs. single year figures here. 

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

#Extract annual totals from sex_split and append to disability as no totals in this dataset
Sex_split_totals <- Sex_split_cleaned |> 
  filter(split_value == "All") |> 
  mutate(split_name = c("Disability")) 

Disability_split_cleaned <- rbind(Disability_split_cleaned, Sex_split_totals)

#join dfs for sex, age and disability
meeting_mvpa_shiny_popgrp <- rbind(Sex_split_cleaned, Age_split_cleaned, Disability_split_cleaned)

#save files
saveRDS(meeting_mvpa_shiny_popgrp, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny_popgrp.rds"))
write.csv(meeting_mvpa_shiny_popgrp, file = paste0(data_folder, "Data to be checked/meeting_mvpa_shiny_popgrp.csv"),row.names = F)

#run qa
run_qa(filename = "meeting_mvpa_shiny_popgrp")

###############################################.
## Part 3 - Prepare deprivation file ----
###############################################.

#read in data file
SIMD_split <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/trend_data_simd_2012_2022.csv"))

#start with using pop groups data cleaning function from Part 2
SIMD_split_cleaned <- data_cleaning(SIMD_split)

#final tweaks to match deprivation file formatting
SIMD_split_cleaned <- SIMD_split_cleaned |> 
  select(-c(3)) |>  #Drop split name col as not relevant
  rename(quintile = split_value) |> 
  mutate(quintile = str_sub(quintile, 1, 1),  #Drop additional text from quintile col
         quint_type = c("sc_quin")) #create quintile type col - Scotland or local, in this case Scotland as only Scotland data available

#using sex splits data to add SIMD all quintiles figures
SIMD_splits_totals <- Sex_split_totals |> 
  rename(quintile = split_name,
         quint_type = split_value) |> 
  mutate(quintile = c("Total"), #must be named total rather than all for aggregate deprivation function to work
         quint_type = c("sc_quin"))

#append onto basefile
SIMD_split_cleaned <- rbind(SIMD_split_cleaned, SIMD_splits_totals)

#save basefile
saveRDS(SIMD_split_cleaned, file=paste0(data_folder, 'Prepared Data/meeting_mvpa_depr_raw.rds'))

#run aggregate analysis function
analyze_deprivation_aggregated(filename = "meeting_mvpa_depr", pop = "depr_pop_16+", 
                               ind_id = 88007, ind_name = "meeting_mvpa", qa = TRUE)




