#########################################################
# Scottish Household Survey Physical Activity Data
#########################################################

#Update ScotPHO indicators sourced from SHoS microdata
#Pre-processeing occurs in the ScotPHO Survey Data Repo
#Then final prep occurs in this script

#5 indicators
#14004 - Adults participating in recreational walking (sprt3aa)
#14005 - Adults participating in sport (anysportnowalk)
#14008 - Adults who visit the outdoors at least once per week (outdoor)
#14009 - Satisfaction with local sport and leisure facilities (serv3a)
#14010 - Satisfaction with local parks and open spaces (serv3e)

#Availability:
#all indicators available with deprivation, sex, age and disability splits for Scotland, HB and CA

################################################################################
#1. functions, filepaths and packages

source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

library(tidyr) #for pivoting

################################################################################
#2. Read in pre-processed data and tidy up

shos_pa <- read.csv(file.path(profiles_data_folder, "/Received Data/Physical Activity/Scottish Household Survey/SHoS_PA.csv")) 
  
#Read in lookup to get S-codes
codedictionary <- readRDS(file.path(profiles_data_folder, "/Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S12|S08|S00")) #keep only HB, CA and Scotland 

#Join to main df
shos_pa <- left_join(shos_pa, codedictionary, by = "areaname")

################################################################################
#3. Create functions to split data into individual data files (main, depr, pop groups)

split_main_data <- function(df, indicator, ind_id, filename) {
  df <- df |> 
    filter(indicator == {{indicator}}, split_name == "Long-term Illness (LTI)", split_value == "Total") |> #filter to get total counts on one split
    select(code, year, numerator, denominator, rate, lowci, upci, trend_axis, def_period) |> #select necessary variables
    mutate(ind_id = {{ind_id}}) #add ind_id col
  
  saveRDS(df, file.path(profiles_data_folder, "Data to be checked", paste0({{filename}}, "_shiny.rds")))
  write.csv(df, file.path(profiles_data_folder, "Data to be checked", paste0({{filename}}, "_shiny.csv")), row.names = FALSE)
  
  return(df)
}

#Deprivation split function

split_depr_data <- function(df, indicator, ind_id, filename){
  df <- df |> 
    filter(indicator == {{indicator}}, split_name == "Deprivation") |> #filter on correct indicator and deprivation data only
    rename(quintile = split_value) |> #rename quintiles 
    mutate(quint_type = "sc_quin") |> #add quintile type col
    select(-split_name, -areaname, -spatial.scale) |> #drop unneeded cols
    mutate(ind_id = {{ind_id}}) #add a new col with specified indicator ID
  
  #Tentatively removing adding of population figures - not sure why they're needed?
  # df <- df |> #Add population figures
  #   add_population_to_quintile_level_data(pop="depr_pop_16+", ind_id, indicator) |> 
  #   filter(quint_type == "sc_quin") |> 
  #   filter(!(year %in% c("2020", "2021")))
  
 df <- calculate_inequality_measures(df)
 
 saveRDS(df, file.path(profiles_data_folder, "Data to be checked", paste0({{filename}}, "_depr_ineq.rds")))
 
 return(df)
  
}

#Population groups split function

split_popgrps_data <- function(df, indicator, ind_id, filename){
  df <- df |> 
    filter(indicator == {{indicator}}, split_name != "Deprivation") |> #filter on correct indicator and all splits
    select(code, year, numerator, denominator, split_name, split_value, rate, lowci, upci, trend_axis, def_period) |> #select necessary variables
    mutate(ind_id = {{ind_id}}) #add ind_id col
  
  saveRDS(df, file.path(profiles_data_folder, "Data to be checked", paste0({{filename}}, "_shiny_popgrp.rds")))
  write.csv(df, file.path(profiles_data_folder, "Data to be checked", paste0({{filename}}, "_shiny_popgrp.csv")), row.names = FALSE)
  
  return(df)
    
}

################################################################################
#4. Adults participating in recreational walking (14004)

rec_walk <- split_main_data(shos_pa, indicator = "sprt3aa", ind_id = "14004", filename = "recreational_walking")
run_qa("recreational_walking", type = "main")

rec_walk_depr <- split_depr_data(shos_pa, indicator = "sprt3aa", ind_id = "14004", filename = "recreational_walking")
run_qa("recreational_walking_depr", type = "deprivation")

rec_walk_popgrps <- split_popgrps_data(shos_pa, indicator = "sprt3aa", ind_id = "14004", filename = "recreational_walking")
run_qa("recreational_walking", type = "popgrp")

################################################################################
#5. Adults participating in sport (14005)

anysportnowalk <- split_main_data(shos_pa, indicator = "anysportnowalk", ind_id = "14005", filename = "sport_participation")
run_qa("sport_participation", type = "main")

anysportnowalk_depr <- split_depr_data(shos_pa, indicator = "anysportnowalk", ind_id = "14005", filename = "sport_participation")
run_qa("sport_participation_depr", type = "deprivation")

anysportnowalk_popgrps <- split_popgrps_data(shos_pa, indicator = "anysportnowalk", ind_id = "14005", filename = "sport_participation")
run_qa("sport_participation", type = "popgrp")
################################################################################
#6. Adults who visit the outdoors at least once per week (14008)]

outdoors <- split_main_data(shos_pa, indicator = "outdoor", ind_id = "14008", filename = "weekly_outdoors_visits")
run_qa("weekly_outdoors_visits", type = "main")

outdoors_depr <- split_depr_data(shos_pa, indicator = "outdoor", ind_id = "14008", filename = "weekly_outdoors_visits")
run_qa("weekly_outdoors_visits_depr", type = "deprivation")

outdoors_popgrps <- split_popgrps_data(shos_pa, indicator = "outdoor", ind_id = "14008", filename = "weekly_outdoors_visits")
run_qa("weekly_outdoors_visits", type = "popgrp")

################################################################################
#7. Satisfaction with local sport and leisure facilities (14009)

leisure_satisfaction <- split_main_data(shos_pa, indicator = "serv3a", ind_id = "14009", filename = "leisure_satisfaction")
run_qa("leisure_satisfaction", type = "main")

leisure_satisfaction_depr <- split_depr_data(shos_pa, indicator = "serv3a", ind_id = "14009", filename = "leisure_satisfaction")
run_qa("leisure_satisfaction_depr", type = "deprivation")

leisure_satisfaction_popgrps <- split_popgrps_data(shos_pa, indicator = "serv3a", ind_id = "14009", filename = "leisure_satisfaction")
run_qa("leisure_satisfaction", type = "popgrp")

################################################################################
#8. Satisfaction with local parks and open spaces (14010)

parks_satisfaction <- split_main_data(shos_pa, indicator = "serv3e", ind_id = "14010", filename = "parks_satisfaction")
run_qa("parks_satisfaction", type = "main")

parks_satisfaction_depr <- split_depr_data(shos_pa, indicator = "serv3e", ind_id = "14010", filename = "parks_satisfaction")
run_qa("parks_satisfaction_depr", type = "deprivation")

parks_satisfaction_popgrps <- split_popgrps_data(shos_pa, indicator = "serv3e", ind_id = "14010", filename = "parks_satisfaction")
run_qa("parks_satisfaction", type = "popgrp")

################################################################################
#End