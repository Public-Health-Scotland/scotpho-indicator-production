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
  filter(split_name != "Age") |> 
  select(-rate, -upci, -lowci) #removing rates and cis so they're all added in the analysis function

shes_adults_age <- shes_adults |> 
  filter(split_name == "Age") |> 
  mutate(split_value = str_sub(split_value, end = -6),
         split_value = as.numeric(split_value), 
         agegp7 = case_when(between(split_value, 16, 24) ~ "16-24", 
                            between(split_value, 25, 34) ~ "25-34",
                            between(split_value, 35, 44) ~ "35-44",
                            between(split_value, 45, 54) ~ "45-54",
                            between(split_value, 55, 64) ~ "55-64",
                            between(split_value, 65, 74) ~ "65-74",
                            split_value >=75 ~ "75+",
                            TRUE ~ as.character(NA))) |> 
  group_by(indicator, ind_id, code, split_name, year, trend_axis, def_period, sex, agegp7) |> 
  summarise(numerator = sum(numerator), denominator = sum(denominator), .groups = "drop") |> 
  rename(split_value = agegp7)

shes_adults <- bind_rows(shes_adults_main, shes_adults_age)

################################################################################
#4. Tidy up child indicators

shes_child <- shes_ukds_pa |> 
filter(ind_id %in% c("14003", "14006", "14007")) #filter on child indicators by id

################################################################################
#5. Adults meeting muscle strengthening recommendations (14001)

#Main data file
mus_rec <- filter(shes_adults, ind_id == 14001, split_name == "Sex", split_value == "Total", !str_detect(def_period, "Aggregated"),
                       code != "S00000001") |> 
  select(code, year, numerator, denominator)

saveRDS(mus_rec, file.path(profiles_data_folder, "/Prepared Data/meets_mus_rec_raw.rds"))

main_analysis("meets_mus_rec", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14001, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) #looks fine - but can I get CA another way?

################################################################################
#6. Adults with very low activity levels (14002)

very_low_act <- filter(shes_adults, ind_id == 14002, split_name == "Sex", split_value == "Total", !str_detect(def_period, "Aggregated"),
                                            code != "S00000001") |> 
  select(code, year, numerator, denominator)

saveRDS(very_low_act, file.path(profiles_data_folder, "/Prepared Data/very_low_activity_raw.rds"))

main_analysis("very_low_activity", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14002, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) #looks fine - but can I get CA another way?

################################################################################
#7. Children with very low activity levels (14003)

children_very_low_act <- filter(shes_child, ind_id == 14006, split_name == "Sex", split_value == "Total", str_detect(def_period, "Aggregated"),
                                code != "S00000001") |> 
  select(code, year, numerator, denominator)

saveRDS(children_very_low_act, file.path(profiles_data_folder, "/Prepared Data/children_very_low_act_raw.rds"))

main_analysis("children_very_low_act", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14006, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

#How do I handle pre-aggregated figures in the analysis function? Figures are correct but are mislabelled
#Need to use multi-year aggregates as island board denoms too small

################################################################################
#8. Children participating in sport (14006)

children_in_sport <- filter(shes_child, ind_id == 14006, split_name == "Sex", split_value == "Total", str_detect(def_period, "Aggregated"),
                            code != "S00000001") |> 
  select(code, year, numerator, denominator)

saveRDS(children_in_sport, file.path(profiles_data_folder, "/Prepared Data/children_in_sport_raw.rds"))

main_analysis("children_in_sport", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14006, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

#same issue as above

################################################################################
#9. Children engaged in active play (14007)

children_active_play <- filter(shes_child, ind_id == 14007, split_name == "Sex", split_value == "Total", str_detect(def_period, "Aggregated"),
                            code != "S00000001") |> 
  select(code, year, numerator, denominator) #Still one data point beneath threshold

saveRDS(children_active_play, file.path(profiles_data_folder, "/Prepared Data/children_active_play_raw.rds"))

main_analysis("children_active_play", measure = "percent", geography = "board", year_type = "survey", 
              ind_id = 14007, time_agg = 1, yearstart = 2012, yearend = 2022, test_file = TRUE) 

#same issue as above



