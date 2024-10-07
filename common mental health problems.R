######################################################################
# THIS SCRIPT HAS BEEN RETIRED:
# Indicators 12557 and 12556 now replaced by 30003
# 30003 is produced by Scottish Health Survey.R script
######################################################################


# ### 1. notes --------
# 
# # 12557 - common mental health problems (females)
# # 12556 - common mental health problems (males)
# 
# # source: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data 
# 
# # data is automatically extacted from statistics.gov.scot when this script is run 
# # just update the date period below to ensure the latest date period is extracted: - 
# 
# 
# date_period <- "2016 - 2019"
# 
# 
# 
# ### 2. extract data -----
# 
# source("1.indicator_analysis.R")
# 
# start_year <- substring(date_period, 1, 4)
# 
# data_extract <- bind_rows(
#   
#   # 95% upper confidence interval data
#   read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-upper-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fgeneral-health-questionnaire-ghq-12-score-4"), skip = 8) %>%
#     mutate(measure = "upci"),
#   
#   # 95% lower confidence interval data
#   read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2F95-lower-confidence-limit&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fgeneral-health-questionnaire-ghq-12-score-4"), skip = 8) %>%
#     mutate(measure = "lowci"),
#   
#   # percentage scoring 4+ on GHQ-12
#   read_csv(paste0("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-health-survey-local-area-level-data&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fpercent&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fgregorian-interval%2F",start_year,"-01-01T00%3A00%3A00%2FP4Y&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FscottishHealthSurveyIndicator=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fscottish-health-survey-indicator%2Fgeneral-health-questionnaire-ghq-12-score-4"), skip = 8) %>%
#     mutate(measure = "rate")
# ) %>%
#   setNames(tolower(names(.)))
# 
# 
# # add in additonal columns -----
# data_extract <- data_extract %>%
#   rename_at(1,~"geography") %>%
#   mutate(code = gsub("http://statistics.gov.scot/id/statistical-geography/","", geography),
#          code = ifelse(code == "S92000003", "S00000001", code),
#          year = as.numeric(start_year) + 2,
#          numerator = "",
#          def_period = paste0("4-year aggregate (",date_period,")"),
#          trend_axis = date_period) 
# 
# 
# 
# ### 3. prepare final files -----
# 
# 
# # function to prepare final shiny outputs
# prepare_shiny_file <- function(indicator, sex_grp) {
#   
#   # read in data from previous update
#   old_dat <- readRDS(paste0(data_folder, "Shiny Data/common-mh-problems-",sex_grp,"_shiny.rds"))
#   
#   # combine new data with old update
#   dat <- data_extract %>%
#       select(sex_grp, code, year, measure, numerator, def_period, trend_axis) %>%
#       pivot_wider(names_from = "measure", values_from = sex_grp) %>%
#       mutate(ind_id = indicator) %>%
#       rbind(old_dat) %>%
#     arrange(year)
#   
#   # save files in folder to be checked
#   write.csv(dat, paste0(data_folder, "Data to be checked/common-mh-problems-",sex_grp,"_shiny.csv"), row.names = FALSE)
#   write_rds(dat, paste0(data_folder, "Data to be checked/common-mh-problems-",sex_grp,"_shiny.rds"))
#     
#   
# }
# 
# 
# prepare_shiny_file(indicator = "12556", sex_grp = "male")
# prepare_shiny_file(indicator = "12557", sex_grp = "female")
# 
# 
# 
# 
# ### 4. QA the indicators ------
# 
# run_qa <- function(filename, old_file="default", check_extras=c()){
#   run("Data Quality Checks.Rmd")
# }  
# 
# 
# # QA common mental health problems, females
# run_qa(filename = "common-mh-problems-female", old_file="default", check_extras=c())
# 
# # QA common mental health problems, males
# run_qa(filename = "common-mh-problems-male", old_file="default", check_extras=c())
# 
# 
# 
