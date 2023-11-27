# Analyst notes ----------------------------------------------------------------

# This script updates the following indicators:
# 21005 - Child dental health P1
# 21006 - Child dental health P7


# A data request is sent to the dental team following the release of National Dental Inspection Programme (NDIP) publication
# publication link: https://publichealthscotland.scot/publications/national-dental-inspection-programme/ 
# (usually published in October)


# The team usually sent on 1 year worth of data each year
# file should then saved in the 'Child dental health' folder
# This script will then combine the latest years data with the historic data and save a new historic data file, ready to be used the following year
# Once you have finished step 3 (saving the new data files) move the latest years data into the 'Archive' sub-folder.


# Missing data:
# no 2020/21 data for Child Dental health P1, due to the pandemic
# no 2020/21 and 2021/22 data for Child Dental health P1, due to the pandemic


# script changes:
# need to change the year being populated when creating the 'year' column in p1_new and p7_new
# i.e. if the latest data is for school year 2022/23, the year should be 2022.


# script outline:
# 1 - Prepare data
# 2 - Run analysis functions 
# 3 - Save new historic data files



# dependencies -----------------------------------------------------------------
source("1.indicator_analysis.R") # Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

dental_health_subfolder <- "Received Data/Child dental health/" # dental health folder


# 1 - Prepare data  ------------------------------------------------------------

# read in historic data 
p1_historic <- readRDS(paste0(data_folder, dental_health_subfolder, "P1_data_historic_DO_NOT_DELETE.rds"))
#p7_historic <- readRDS(paste0(data_folder, dental_health_subfolder, "P7_data_historic_DO_NOT_DELETE.rds"))

# read in new data
p1_new <- read_csv(paste0(data_folder, dental_health_subfolder, "Final_P1_Letter_2021.csv")) 
# p7_new <- read_csv(paste0(data_folder, dental_health_subfolder, ".csv")) 


# tidy up 
p1_new <- p1_new %>%
  mutate(year = 2021) %>%
  rename(datazone = datazone2011) %>%
  select(datazone,year, numerator, denominator) 


# p7_new <- p7_new %>%
#     mutate(year = )
#   rename(datazone = datazone2011) %>%
#   select(datazone,year, numerator, denominator)


# combine data
p1_combined <- rbind(p1_historic, p1_new)
#p7_combined <- rbind(p7_historic, p7_new)

# save file to be used in analysis function 
saveRDS(p1_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p1_raw.rds'))
#saveRDS(p7_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p7_raw.rds'))



# Part 2: Run analysis functions  ----------------------------------------------

# Child dental health P1
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2021, time_agg = 1) 


analyze_second(filename = "child_dental_p1", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21005, year_type = "school", pop="DZ11_pop_5")


analyze_deprivation(filename="child_dental_p1_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend=2021, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_5", ind_id = 21005)



# Child dental health P7
# analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
#               yearstart = 2012, yearend = 2020, time_agg = 1)
# 
# 
# analyze_second(filename = "child_dental_p7", measure = "perc_pcf", time_agg = 1, 
#                ind_id = 21006, year_type = "school", pop="DZ11_pop_11")
# 
# analyze_deprivation(filename="child_dental_p7_depr", measure="perc_pcf",  
#                     yearstart= 2014, yearend=2020, time_agg=1,
#                     year_type = "school", pop_pcf = "depr_pop_11", ind_id = 21006)


# 3. Save new historic data files ----------------------------------------------

# if everything looks fine fro DQ checks - overwrite the old historic data files
saveRDS(p1_combined, paste0(data_folder, dental_health_subfolder, "P1_data_historic_DO_NOT_DELETE.rds"))
#saveRDS(p7_combined, paste0(data_folder, dental_health_subfolder, "P7_data_historic_DO_NOT_DELETE.rds"))


# END