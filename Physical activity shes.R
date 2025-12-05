#########################################################
# Scottish Health Survey Physical Activity Data
#########################################################

#Update ScotPHO indicators sourced from SHeS microdata
#Pre-processeing occurs in the ScotPHO Survey Data Repo
#Then final prep occurs in this script

#5 indicators
#14001 - Adults meeting muscle strengthening recommendations
#14002 - Adults with very low activity
#14003 - Children with very low activity
#14006 - Children participating in sport
#14007 - Children engaged in active play

#Things to consider - how does the meets PA indicator derive its CA data??

#Availability: 
#all indicators available with sex splits for Scotland and HB
#all indicators available with age and deprivation splits (Scotland only)

################################################################################
#1. functions, filepaths and packages

source("functions/main_analysis.R") # for packages and QA
source("functions/deprivation_analysis.R") # for packages and QA

library(tidyr) #for pivoting

################################################################################
#2. Read in pre-processed data

shes_ukds_pa <- read_rds(file.path(profiles_data_folder, "/Prepared Data/shes_pa_raw.rds")) |> 
  mutate(code = as.character(code)) 

################################################################################
#3. Tidy up adult indicators

shes_adults <- shes_ukds_pa |> 
  filter(ind_id %in% c(14001, 14002)) #filter on adult indicators

#Splitting off age data to add age groups
shes_adults_main <- shes_adults |> 
  filter(split_name != "Age") 

shes_adults_age <- shes_adults |> 
  filter(split_name == "Age") |> 
  mutate(split_value = str_sub(split_value, end = -6), #remove " years" from end
         split_value = as.numeric(split_value), #convert age to numeric
         agegp7 = case_when(between(split_value, 16, 24) ~ "16-24", 
                            between(split_value, 25, 34) ~ "25-34",
                            between(split_value, 35, 44) ~ "35-44",
                            between(split_value, 45, 54) ~ "45-54",
                            between(split_value, 55, 64) ~ "55-64",
                            between(split_value, 65, 74) ~ "65-74",
                            split_value >=75 ~ "75+",
                            TRUE ~ as.character(NA))) |> 
  group_by(indicator, ind_id, code, split_name, year, trend_axis, def_period, sex, agegp7) |> 
  summarise(numerator = sum(numerator), denominator = sum(denominator), .groups = "drop") |> #sum nums and denoms across age groups
  rename(split_value = agegp7)

shes_adults <- bind_rows(shes_adults_main, shes_adults_age) #join age data back onto other splits

################################################################################
#4. Tidy up child indicators

shes_child <- shes_ukds_pa |> 
filter(ind_id %in% c("14003", "14006", "14007")) #filter on child indicators by id

################################################################################
#5. Create functions to split data into individual data files (main, depr, pop groups)

split_main_data <- function(df, indicator, aggregated = NULL) {
  df <- df |> 
    filter(indicator == {{indicator}}, split_name == "Sex", split_value == "Total",
           code != "S00000001")
  
  if(isTRUE(aggregated)){
    df <- filter(df, str_detect(def_period, "Aggregated"))
  }else{
    df <- filter(df, !str_detect(def_period, "Aggregated"))
  }
  
  df <- df |>  select(code, year, numerator, denominator)
  
  df
}

split_depr_data <- function(df, indicator){
  df <- df |> 
    filter(indicator == {{indicator}}, split_name == "Deprivation (SIMD)",
           str_detect(def_period, "Survey year ")) |> 
    rename(quintile = split_value) |> 
    mutate(quint_type = "sc_quin") |> 
    select(-split_name)
  
  #Add population data
  df <- df |> 
    add_population_to_quintile_level_data(pop="depr_pop_16+",ind = ind_id,ind_name = ind_name) |> #!!!!!!!!!!!!!!!!!! Next to fix
  
  #Calculate inequalities measures
    calculate_inequality_measures() |> # call helper function that will calculate sii/rii/paf
    select(-c(indicator,overall_rate, total_pop, proportion_pop, most_rate,least_rate, par_rr, count)) #delete unwanted fields
  
}

mus_rec_depr <- split_depr_data(shes_adults, indicator = "meeting_muscle_strengthening_recommendations")


mus_rec_depr <- shes_adults |> 
  filter(indicator == "meeting_muscle_strengthening_recommendations", split_name == "Deprivation (SIMD)",
         str_detect(def_period, "Survey year ")) |> 
  rename(quintile = split_value) |> 
  mutate(quint_type = "sc_quin") |> 
  select(-split_name)


################################################################################
#6. Adults meeting muscle strengthening recommendations (14001)

#Main data file
mus_rec <- split_main_data(shes_adults, indicator = "meeting_muscle_strengthening_recommendations", aggregated = FALSE)

saveRDS(mus_rec, file.path(profiles_data_folder, "/Prepared Data/meets_mus_rec_raw.rds"))

main_analysis("meets_mus_rec", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14001, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

#Deprivation file




################################################################################
#7. Adults with very low activity levels (14002)

very_low_act <- split_main_data(shes_adults, indicator = "adults_very_low_activity", aggregated = FALSE)

saveRDS(very_low_act, file.path(profiles_data_folder, "/Prepared Data/very_low_activity_raw.rds"))

main_analysis("very_low_activity", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14002, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

################################################################################
#8. Children with very low activity levels (14003)

children_very_low_act <- split_main_data(shes_child, indicator = "children_very_low_activity", aggregated = TRUE)

saveRDS(children_very_low_act, file.path(profiles_data_folder, "/Prepared Data/children_very_low_act_raw.rds"))

main_analysis("children_very_low_act", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14006, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 


################################################################################
#9. Children participating in sport (14006)

children_in_sport <- split_main_data(shes_child, indicator = "children_participating_sport", aggregated = TRUE)

saveRDS(children_in_sport, file.path(profiles_data_folder, "/Prepared Data/children_in_sport_raw.rds"))

main_analysis("children_in_sport", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14006, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 


################################################################################
#10. Children engaged in active play (14007)

children_active_play <- split_main_data(shes_child, indicator = "children_active_play", aggregated = TRUE)

saveRDS(children_active_play, file.path(profiles_data_folder, "/Prepared Data/children_active_play_raw.rds"))

main_analysis("children_active_play", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14007, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

################################################################################
#End


