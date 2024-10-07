######################################################################
# THIS SCRIPT HAS BEEN RETIRED:
# Indicators 12551 and 12550 now replaced by 30001
# 30003 is produced by Scottish Health Survey.R script
######################################################################


# ### notes --------------------------------------------------------------
# 
# # 12551: Mental wellbeing score, females
# # 12550: Mental wellbeing score, males
# # Data downloaded from https://scotland.shinyapps.io/sg-scottish-health-survey/
# 
# ### 1. load packages/dependencies ----------------------------------------------
# source("1.indicator_analysis.R") # sourcing custom indicator functions
# 
# library(reactable)
# 
# ### 2. read in downloaded data -------------------------------------------------
# 
# #Set filepath
# raw_data_filepath <- paste0(data_folder, "Received Data/Mental Wellbeing/rank_data.csv")
# raw_data <- read.csv(raw_data_filepath) #read csv file in
# 
# 
# # clean the column names 
# data <- raw_data |> 
#   clean_names() 
# 
# 
# ### 3. select columns  ---------------------------------------------------------
# 
# data <- data |>
#   select(year, geographylevel, location, sex, mean, lower_ci, upper_ci)
# 
# 
# ### 4. filter the sex column to remove combined --------------------------------
# data <- data |>
#   filter(sex!="All")
# 
# 
# ### 5. rename some of the existing columns to match what is required in the final output 
# data <- data |>
#   rename(lowci=lower_ci, upci=upper_ci, rate=mean)
# 
# #Add a trend_axis column
# data$trend_axis <- data$year
# 
# 
# ### 6. read in geography lookup named 'codedictionary' from the scotpho lookups folder ------------
# geography_lookup <- readRDS(paste0(lookups, "Geography/codedictionary.rds"))
# 
# #Filter for HBs, CAs and Scotland
# geography_lookup <- geography_lookup |>
#   filter(str_detect(code,"S08|S12|S00"))
# 
# # Paste NHS on the beginning health boards...
# data_correct <- data |>
#   mutate(location_new = case_when(
#     geographylevel=="Health Board" ~ paste0('NHS ', location),
#     TRUE ~ location
#   ))
# 
# #Tidy up some LA names
# data_correct <- data_correct |>
#   mutate(location_new = case_when(
#     location_new == "Edinburgh City" ~ "City of Edinburgh",
#     TRUE ~ location_new
#   ))
# 
# 
# # now try join the codes again 
# data_correct <- data_correct |>
#   left_join(geography_lookup, by = c("location_new" = "areaname"))
# 
# 
# # we can now get rid of the columns geographylevel and location as the final file should only have the geography code.
# data_correct <- data_correct |>
#   select(-c(geographylevel, location))
# 
# 
# # create a def_period column 
# data_correct <- data_correct |>
#   mutate(def_period= paste0("4-year aggregate"," (", year, ")"))
# 
# 
# # Create a year column
# #Get a single year from the given range
# data_correct <- data_correct |>
#   mutate(year = str_sub(year, start = -4, end = -1)) %>% 
#   mutate(numerator = NA)
# 
# 
# #Make the year variable numeric rather than character
# data_correct$year <- as.numeric(data_correct$year)
# 
# #Change to required year by subtracting 1 if before 2020 and 2 if after 2020 since the 4 year aggregate changes
# data_correct$year <- ifelse(data_correct$year < 2020, data_correct$year - 1, data_correct$year - 2)
# 
# # split the data into 2 separate indicators by sex and create ind_id column 
# males <- data_correct |>
#   filter(sex=="Male") |>
#   mutate(ind_id="12550") |>
#   select(-sex, -location_new)
# 
# females <- data_correct |>
#   filter(sex=="Female") |>
#   mutate(ind_id="12551") |>
#   select(-sex, -location_new)
# 
# # saving the male indicator data
# write.csv(males, paste0(data_folder, "Data to be checked/mental-wellbeing-male_shiny.csv"), row.names = FALSE)
# saveRDS(males, paste0(data_folder, "Data to be checked/mental-wellbeing-male_shiny.rds"))
# 
# # saving the female indicator data
# write.csv(females, paste0(data_folder, "Data to be checked/mental-wellbeing-female_shiny.csv"), row.names = FALSE)
# saveRDS(females, paste0(data_folder, "Data to be checked/mental-wellbeing-female_shiny.rds"))
# 
# 
# # running quality assurance 
# run_qa(filename = "mental-wellbeing-female", old_file="default", check_extras=c())
# 
# 
# run_qa(filename = "mental-wellbeing-male", old_file="default", check_extras=c())
# 
# ### END
