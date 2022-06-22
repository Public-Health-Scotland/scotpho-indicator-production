### 1. notes

# 4165: Alcohol consumption exceeding weekly limits, females
# 4163: Alcohol consumption exceeding weekly limits, males
# 4164: Alcohol consumption exceeding weekly limits, all


# source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data 

# data is automatically extacted from statistics.gov.scot when this script is run 
# just update the date period below to ensure the latest date period is extracted: - 


date_period <- "2016 - 2019" 



### 2. extract data -----

source("1.indicator_analysis.R")

start_year <- substring(date_period, 1, 4)


data_extract <- bind_rows(
  
  # 95% upper confidence interval data
  read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-upper-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fhttp://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-upper-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2015-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Falcohol-consumption-hazardous%2Fharmful-drinker"), skip = 8) %>%
    mutate(measure = "upci"),
  
  # 95% lower confidence interval data
  read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-lower-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fhttp://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-lower-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2015-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Falcohol-consumption-hazardous%2Fharmful-drinker"), skip = 8) %>%
    mutate(measure = "lowci"),
  
  # % exceeding 14 units alcohol a week (hazardous/harmful drinking)
  read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fpercent&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fhttp://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fpercent&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F2015-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Falcohol-consumption-hazardous%2Fharmful-drinker"), skip = 8) %>%
    mutate(measure = "rate")
) %>%
  setNames(tolower(names(.)))



# add in additonal columns 
data_extract <- data_extract %>%
  rename_at(1,~"geography") %>%
  mutate(code = gsub("http://statistics.gov.scot/id/statistical-geography/","", geography),
         code = ifelse(code == "S92000003", "S00000001", code),
         year = as.numeric(start_year) + 2,
         numerator = "",
         def_period = paste0("4-year aggregate (",date_period,")"),
         trend_axis = date_period) %>%
  filter_all(any_vars(grepl("S08|S00", .))) # remove council area figures - only available from 2015 onwards



### 3. prepare final files -----

# create function to prepare final shiny outputs
prepare_shiny_file <- function(indicator, sex_grp) {
  
  # read in data from previous update
  old_dat <- readRDS(paste0(data_folder, "Shiny Data/alcohol-exceeding-weekly-limits-",sex_grp,"_shiny.rds"))
  
  # combine new data with old update
  dat <- data_extract %>%
    select(sex_grp, code, year, measure, numerator, def_period, trend_axis) %>%
    pivot_wider(names_from = "measure", values_from = sex_grp) %>%
    mutate(ind_id = indicator) %>%
    rbind(old_dat) %>%
    arrange(year) %>%
    mutate(numerator = "")
  
  # save files in folder to be checked
  write.csv(dat, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-",sex_grp,"_shiny.csv"), row.names = FALSE)
  write_rds(dat, paste0(data_folder, "Data to be checked/alcohol-exceeding-weekly-limits-",sex_grp,"_shiny.rds"))
  
  
}


prepare_shiny_file(indicator = "4163", sex_grp = "male")
prepare_shiny_file(indicator = "4165", sex_grp = "female")
prepare_shiny_file(indicator = "4164", sex_grp = "all")



### 4. QA the indicators ------

run_qa <- function(filename, old_file="default", check_extras=c()){
  run("Data Quality Checks.Rmd")
}  


# QA alcohol consumption exceeding weekly limits, females
run_qa(filename = "alcohol-exceeding-weekly-limits-female", old_file="default", check_extras=c())

# QA alcohol consumption exceeding weekly limits, males
run_qa(filename = "alcohol-exceeding-weekly-limits-male", old_file="default", check_extras=c())

# QA alcohol consumption exceeding weekly limits, all
run_qa(filename = "alcohol-exceeding-weekly-limits-all", old_file="default", check_extras=c())
