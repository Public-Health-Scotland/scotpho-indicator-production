# ScotPHO indicator ID 13018: 
# Employment rate for 16to24 years.R (percent of population in employment)

# Note that employment rate 16-24 year olds is sourced from a different NOMIS table to that used for the same indicator that covers 16-64 year olds. For some reason this age breakdown seems to become available
# more quickly through this route. NOMIS query extractions seem to have redundancy so more than one way to access the same data but appears that not all tables get updated at the same point in time.

#  Data source is the Annual Population Survey (APS)/Labour force Survey published by NOMIS 
#  https://www.nomisweb.co.uk/


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R")

###############################################.
## Read in data  ----
###############################################.


# To find data table behind the data update follow these selections in the NOMIS data download tool found at the url above

#  - Data downloads (from https://www.nomisweb.co.uk/)
# 	-Query		(https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)
# 	-Annual population survey (APS)
# 	-STEP 1: Geography  (select countries>some> : Scotland ,  Local authorities - District (within Scotland)>some> then tick all within Scotland)
#   -STEP 2: Date (12 months to December) – pick time periods required – full series if refreshing all or single year to add one year to existing dataset)
# 	-STEP 3: 
#             using the category drop down : select 'employment rate by age'
#                                     tick "Employment rate - aged 16-24" (only select for both sexes we don't report this indicator by population groups)
#   -STEP 4: choose option for 'Nomis API'
#   -STEP 5: Copy link to download data as csv into R script below (the link is the reference for API_link object)


## api for 16-24 rate - contains numerator, denominator and pre calculated percentage - however if you want to put through functions to generate NHS board data you don't need 
#api_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS79,latestMINUS75,latestMINUS71,latestMINUS67,latestMINUS63,latestMINUS59,latestMINUS55,latestMINUS51,latestMINUS47,latestMINUS43,latestMINUS39,latestMINUS35,latestMINUS31,latestMINUS27,latestMINUS23,latestMINUS19,latestMINUS15,latestMINUS11,latestMINUS7,latestMINUS3&variable=1207&measures=20599,21001,21002,21003")

api_link <- c("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190786,1774190787,1774190793,1774190788,1774190789,1774190768,1774190769,1774190794,1774190770,1774190795,1774190771,1774190772,1774190774,1774190796,1774190798,1774190775...1774190778,1774190773,1774190779,1774190799,1774190780,1774190797,1774190790,1774190781...1774190785,1774190791,1774190792,2092957701&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=1207&measures=20599,21001,21002,21003")

# Reads the API as a csv using readr and assigns it the name raw_data
data1624_raw <- read_csv(api_link) %>%
  clean_names()  # Clean column data

# Format extracted data
data1624 <- data1624_raw %>%
  #create numeric year column
  mutate(year = as.numeric(str_sub(date, start= 1,4)),
         #replace scotland geo code with scotpho version
         geography_code = if_else(geography_code == "S92000003", "S00000001", geography_code)) |>
  select(year, geography_name, geography_code,variable_name, measures_name, obs_value) |>
  
  # Pivot the data to a wider format, spreading the 'obs_value' across new columns based on 'measures_name'
  pivot_wider(names_from = measures_name, values_from = obs_value)


# NOMIS extract contains Scotland and local authority data however we can choose to pass the data through ScotPHO
# functions that enable generation of NHS board/HSCP level data.  If we use the functions then we need to exclude Scotland
# level data as this is generated as part of the function. Including in original extract does serve as a useful cross check though

function_data <- data1624 |>
  filter(geography_code != "S00000001") |> #exclude scotland data since we are genertating through function
  rename(numerator=Numerator, denominator=Denominator, measure = Variable, ca=geography_code) |>
  select(ca, year, numerator, denominator)

#save files to be used in analysis functions
saveRDS(function_data, file=paste0(profiles_data_folder, "/Prepared Data/employment_rate_16to24_raw.rds"))


main_analysis(
  filename = "employment_rate_16to24",  measure = "percent",  geography = "council",
  year_type = "calendar",  ind_id = "13018",  time_agg = 1,  yearstart = 2004, yearend = 2024)


# note some island area - namely shetland data not available for latest years due to suppression/disclosive numbers
#no deprivation split possible given source data


# qa report shows some area like Shetland have no data for most recent years - this is due to suppression/disclosure control applied to NOMIS data and we are unable to source figures for some areas.
#


