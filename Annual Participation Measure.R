## N.B. THIS MEASURE ALSO IN THE NPF DATA READ IN IN THE NPF SCRIPT. COULD DO ALL PROCESSING THERE IF COVERAGE IS THE SAME.
#SEE SCRIPT 'NATIONAL PERFORMANCE FRAMEWORK.R' COULD WE ADD THIS INDICATOR TO THAT SCRIPT SINCE NPF DATA (WHERE INDICATOR CALLED
#'Young peoples participation' AS THIS WOULD MEAN some population group splits could be presented at scotland level
# after a bit of digging it looks like NPF dataset only scotland but SDS dataset include board and CA breakdown.
# maybe we need to use both sources - speak with PIA about how to proceed with 2024 update
###############################################################################################################################

### analyst notes ----

# this script produces data for the following indicator:- 13053- Annual participation (in education, training or employment) measure for 16 – 19 year olds
# save latest supplementary tables in the 'data recieved folder' before running this script 
# and make sure to update the year in the  filepath when reading in the data

# data source: https://www.skillsdevelopmentscotland.co.uk/publications-statistics/statistics/annual-participation-measure/

###############################################.
## Part 1 - Prepare data ----
###############################################.

###1.a load dependencies/functions ----

source("./functions/main_analysis.R") 

###1.b read in data ----

dat <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Annual participation measure/annual-participation-measure-2024-supplementary-tables.xlsx"), sheet = "Table 1.7") #aps data

###1.c clean data ----

dat_cleaned <- dat |> 
  slice(8:305) |>  #remove metadata from top and bottom of spreadsheet
  row_to_names(row_number = 1) |>  #set 1st row as column names
  clean_names() |> #tidy up column names
  select(year, council_area_2019_code, participating_16_19, total_cohort_16_19) |> #drop unnecessary variables
  rename(numerator = participating_16_19,
         denominator = total_cohort_16_19,
         ca = council_area_2019_code) |> #rename columns to format expected by analysis functions
  mutate(across(c(year, numerator, denominator), as.numeric)) |>  #convert numerator and denominator from string to numeric
  filter(!is.na(ca)) #drop Scotland totals which are showing as NA codes - will be re-added by analysis functions

#1.d. Save file to be used in part 2 ----
  saveRDS(dat_cleaned, file=paste0(profiles_data_folder, '/Prepared Data/participation_raw.rds'))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.
  
 main_analysis(filename = "participation", measure = "percent", geography = "council",
               year_type = "calendar", ind_id = 13053, time_agg = 1, yearstart = 2016,
               yearend = 2024)


#End.