# ScotPHO indicators: Child dental health at P1 and P7..

#   Part 1 - Prepare P1 data
#   Part 2 - Prepare P7 data


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Prepare P1 data  ----
###############################################.

# read in data received
data_p1 <- read_csv(paste0(data_folder, "Received Data/Final_P1_Letter_2021.csv"))

# remove gender column and aggregate
data_p1 <- data %>% 
  group_by(datazone2011) %>%
  summarise(numerator = sum(numerator, na.rm =T), 
            denominator = sum(denominator, na.rm =T)) %>%  ungroup() %>%
  mutate(year = 2021) %>% # add year column
  rename("datazone" = "datazone2011")


#saving file for analyze_first and analyze_second functions
saveRDS(data_p1, file=paste0(data_folder, 'Prepared Data/child_dental_p1_raw.rds'))


#Saving file for deprivation function
saveRDS( data_p1, file=paste0(data_folder, 'Prepared Data/child_dental_p1_depr_raw.rds'))


# run analyse functions 
analyze_first(filename = "child_dental_p1", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2021, time_agg = 1) 


analyze_second(filename = "child_dental_p1", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21005, year_type = "school", pop="DZ11_pop_5")


analyze_deprivation(filename="child_dental_p1_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend=2021, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_5", ind_id = 21005)


# once run analyze_first() and analyze_second(), combine old data with latest year 
old <- readRDS(paste0(data_folder, "Shiny Data/child_dental_p1_shiny.rds"))
new <- readRDS(paste0(data_folder, "Data to be checked/child_dental_p1_shiny.rds"))

final <- rbind(old, new)
  
#save final files to be checked 
saveRDS(final, paste0(data_folder, "Data to be checked/child_dental_p1_shiny.rds"))
write_csv(final, paste0(data_folder, "Data to be checked/child_dental_p1_shiny.csv"))


# QA the final file
run_qa("child_dental_p1", old_file = "default", check_extras = c())

# once run analyze_deprivation, combine old data with latest year 
old <- readRDS(paste0(data_folder, "Shiny Data/child_dental_p1_depr_ineq.rds"))
new <- readRDS(paste0(data_folder, "Data to be checked/child_dental_p1_depr_ineq.rds"))


final <- rbind(old, new)


#save final deprivation file 
saveRDS(final, paste0(data_folder, "Data to be checked/child_dental_p1_depr_ineq.rds"))



###############################################.
## Part 2 - Prepare P7 data ----
###############################################.
data_p7 <- as.data.frame(rbind(
  read_excel_sheet(extract = "IR2019-01329", sheet = "2013_P7_C_DZ2011", range = "A5:E12816"),
  read_excel_sheet(extract = "IR2019-01329", sheet = "2014_P7_C_DZ2011", range = "A5:E12775"),
  read_excel_sheet(extract = "IR2019-01329", sheet = "2015_P7_C_DZ2011", range = "A5:E12721"),
  read_excel_sheet(extract = "IR2019-01329", sheet = "2016_P7_C_DZ2011", range = "A5:E12836"),
  read_excel_sheet(extract = "IR2021-00006", sheet = "2016.17_P7_letter_C", range = "A4:E12954"),
  read_excel_sheet(extract = "IR2021-00006", sheet = "2017.18_P7_letter_C", range = "A4:E12968"),
  read_excel_sheet(extract = "IR2021-00006", sheet = "2018.19_P7_letter_C", range = "A4:E13000"),
  read_excel_sheet(extract = "IR2021-00006", sheet = "2019.20_P7_letter_C", range = "A4:E13000"))) %>% 
  group_by(datazone, year) %>% #aggregating 
  mutate(numerator = as.numeric(numerator)) %>% 
  summarise(numerator = sum(numerator, na.rm =T), 
            denominator = sum(denominator, na.rm =T)) %>% ungroup()

saveRDS(data_p7, file=paste0(data_folder, 'Prepared Data/child_dental_p7_raw.rds'))

#Saving file for deprivation, only from 2014 for simd2016
saveRDS(data_p7 %>% filter(year>=2014), file=paste0(data_folder, 'Prepared Data/child_dental_p7_depr_raw.rds'))




###############################################.
#Children at P7
analyze_first(filename = "child_dental_p7", geography = "datazone11", measure = "percent", 
              yearstart = 2012, yearend = 2020, time_agg = 1)



analyze_second(filename = "child_dental_p7", measure = "perc_pcf", time_agg = 1, 
               ind_id = 21006, year_type = "school", pop="DZ11_pop_11")

analyze_deprivation(filename="child_dental_p7_depr", measure="perc_pcf",  
                    yearstart= 2014, yearend=2020, time_agg=1,
                    year_type = "school", pop_pcf = "depr_pop_11", ind_id = 21006)

##END