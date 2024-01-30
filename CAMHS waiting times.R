### SCRIPT UNDER DEVELOPMENT!!!


### Update ScotPHO Care and Wellbeing indicators:
# 99129: CAMHS waiting times


# Data source is Public Health Scotland Child and Adolescent Mental Health Waiting Times data:
# https://www.opendata.nhs.scot/dataset/child-and-adolescent-mental-health-waiting-times/resource/7a2fe10d-1339-41c1-a2f2-a469644fd619


### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

# Access adjusted patients seen data via API
ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "7a2fe10d-1339-41c1-a2f2-a469644fd619"

data <- dplyr::tbl(src = ckan$con, from = res_id) %>%
  as_tibble() %>%
  clean_names()



### 2. Prepare data  -----

data <- data %>%
  
  # Select relevant variables
  select(hb, month, total_patients_seen, number_of_patients_seen0to18weeks) %>%
  
  # Rename variables
  rename(code = hb,
         date = month,
         denominator = total_patients_seen,
         numerator = number_of_patients_seen0to18weeks) %>%
  
         # Calculate percentage seen within 18 weeks
  mutate(rate = numerator / denominator,
         
         # Amend Scotland area code
         code = ifelse(code == "S92000003", "S00000001", code),
         
         # Create new columns
         year = as.numeric(str_sub(date, start= 1, end = 4)),
         #def_period = as.Date(as.character(date), format = "%Y%m"),
         ind_id = 99129)



### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/CAMHS_waiting_times_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/CAMHS_waiting_times_shiny.rds"))


# Run QA reports and check the output files
run_qa(filename="CAMHS_waiting_times")



#END