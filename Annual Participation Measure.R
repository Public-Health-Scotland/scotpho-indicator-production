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

source("functions/main_analysis.R") 

###1.b read in data ----

dat <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Annual participation measure/2025-annual-participation-measure-supplementary-tables.xlsx"), sheet = "Table 1.7") #aps data

###1.c clean data ----

dat_cleaned <- dat |> 
  slice(9:339) |> # This will need adjusted each year
  select(
    year = 1, # Year
    ca = 3, # Council Area 2019 Code
    denominator = 4, # Total Cohort (16-19)
    numerator = 5) |> # Participating (16-19)
  slice(-1) |>
  mutate(across(c(year, numerator, denominator), as.numeric)) |>  # convert cols to numeric
  filter(!is.na(ca)) # drop Scotland totals which are showing as NA codes - will be calculated in analysis function

#1.d. Save file to be used in part 2 ----
saveRDS(dat_cleaned, file=paste0(profiles_data_folder, '/Prepared Data/participation_raw.rds'))


###############################################.
## Part 2 - Run analysis function ----
###############################################.
  
 main_analysis(filename = "participation", measure = "percent", geography = "council",
               year_type = "calendar", ind_id = 13053, time_agg = 1, yearstart = 2016,
               yearend = 2025)


#End.