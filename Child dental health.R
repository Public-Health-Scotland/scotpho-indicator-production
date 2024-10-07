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
# no 2020/21 and 2021/22 data for Child Dental health P7, due to the pandemic
# no 2021/22 P1 data for NHS Western Isles and partial data for NHS Highland, 
# which has been excluded here.


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
p7_historic <- readRDS(paste0(data_folder, dental_health_subfolder, "P7_data_historic_DO_NOT_DELETE.rds"))

# read in new data
p1_new <- read.csv(paste0(data_folder, dental_health_subfolder, "Final P1 Letter IIa.csv")) 
p7_new <- read.csv(paste0(data_folder, dental_health_subfolder, "Final P7 Letter IIa.csv")) 


#tidy up p1
p1_new <- p1_new |> 
  select(c(1:4)) |> #keeps only first 4 cols w/ data and drops others
  tail(-7) |>  #drops first 7 rows of metadata
  row_to_names(row_number = 1) |>  #from janitor package, sets row as header by index
  clean_names() |> 
  mutate(year = 2022) |> #converting school year to single year 
  rename('datazone' = datazone_2011, #standardising headings to be read into profiles tool
         'denominator' = total_inspected,
         'numerator' = number_of_c_letters_issued) |> 
  mutate(numerator = as.numeric(numerator), #converting numerator and denominator to numeric from character
         denominator = as.numeric(denominator)) |> 
  select(1, 3, 4, 5)

#tidy up p7
p7_new <- p7_new |> 
  select(c(1:4)) |> 
  tail(-7) |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  mutate(year = 2022) |> 
  rename('datazone' = datazone_2011,
         'denominator' = total_inspected,
         'numerator' = number_of_c_letters_issued) |> 
  mutate(numerator = as.numeric(numerator), 
         denominator = as.numeric(denominator)) |> 
  select(1, 3, 4, 5)


#tidy up p1 historic data to remove NHS Highland's data
#remove this section after 
# 
# #read in lookup with all geography levels
# lookup_all <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds"))
# 
# #join lookup to historic p1 data
# p1_historic_noH <- p1_historic |> 
#   left_join(lookup_all, by = c("datazone" = "datazone2011"))
# 
# p1_historic_noH_2<-p1_historic_noH |> 
#   filter(!(hb2019 == c("S08000022") & year == c("2021"))) |> #excluding datazones with Highland HB code from 21/22
#   select(c(1:4)) #Removing the additional lookup columns 
# 
# write.csv(p1_historic_noH_2, paste0(data_folder, "Data to be checked/p1-historic-fix.csv"))
# write.csv(p1_historic, paste0(data_folder, "Data to be checked/p1_historic_original.csv"))
# 
# p1_historic <- p1_historic_noH_2


# combine data
p1_combined <- rbind(p1_historic, p1_new)
p7_combined <- rbind(p7_historic, p7_new)

# save files to be used in analysis and deprivation functions
saveRDS(p1_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p1_raw.rds'))
saveRDS(p7_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p7_raw.rds'))

saveRDS(p1_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p1_depr.rds'))
saveRDS(p7_combined, file=paste0(data_folder, 'Prepared Data/child_dental_p7_depr.rds'))


# Part 2: Run analysis functions  ----------------------------------------------

# Child dental health P1
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2022, time_agg = 1) 


analyze_second(filename = "child_dental_p1", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21005, year_type = "school", pop="DZ11_pop_5")


analyze_deprivation(filename="child_dental_p1_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend = 2022, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_5", ind_id = 21005)



# Child dental health P7
analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2022, time_agg = 1)


analyze_second(filename = "child_dental_p7", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21006, year_type = "school", pop="DZ11_pop_11")

analyze_deprivation(filename="child_dental_p7_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend = 2022, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_11", ind_id = 21006)


# 3. Save new historic data files ----------------------------------------------

# if everything looks fine fro DQ checks - overwrite the old historic data files
saveRDS(p1_combined, paste0(data_folder, dental_health_subfolder, "P1_data_historic_DO_NOT_DELETE.rds"))
saveRDS(p7_combined, paste0(data_folder, dental_health_subfolder, "P7_data_historic_DO_NOT_DELETE.rds"))

