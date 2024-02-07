###   Update ScotPHO Care and Wellbeing indicator: 
#   99130: Work-related ill health


# Data source is Labour Force Survey from HSE
# Country and region of residence (LFSILLREG) tables:
# https://www.hse.gov.uk/statistics/lfs/index.htm



### functions/packages -----
source("1.indicator_analysis.R") 


### 1. Read in data ----

# Read in data from Table 1: Estimated prevalence and rates of self-reported illness 
# caused or made worse by work, by usual country and region of residence, 
# for people working in the last 12 months, Great Britain

data <- read_xlsx(paste0(data_folder, "Received Data/Labour Force Survey - Self-reported work-related ill health.xlsx"),
                  sheet = "Table-1", skip = 9, col_names = TRUE) %>%
  clean_names()

# Rename columns
colnames(data) <- c("italic", "sort", "average", "level", "code",
                    "country", "year", "prev_est", "prev_lowci", "prev_upci",
                    "rate_est", "rate_lowci", "rate_upci", "sig_GB", "sig_prev")


### 2. Prepare data  -----

data <- data %>% 
  
  # Filter for Scotland data
  filter(country == "Scotland") %>% 
  
  # Data is currently in thousands so multiple for true data
  mutate(prev_est = prev_est * 1000,
         prev_lowci = prev_lowci * 1000,
         prev_upci = prev_upci * 1000,
         
         # Change Scotland area code
         code = ifelse(code == "S92000003", "S00000001", code),
         
         # Remove reference to notes in year variable
         year = gsub("\\(Note A\\)", "", year),
         
         # Create new columns
         trend_axis = year,
         year = as.numeric(str_sub(year, start = 1, end = 4)) +1,
         def_period = paste0(trend_axis, " financial years; 3-year average"),
         ind_id = 99130) %>% 
  
  # Rename columns
  rename(numerator = prev_est,
         lowci = prev_lowci,
         upci = prev_upci) %>% 

  # Select relevant columns
  select(code, year, trend_axis, def_period, numerator, lowci, upci)



### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, paste0(data_folder, "Data to be checked/work_related_ill_health_shiny.csv"), row.names = FALSE)
write_rds(data, paste0(data_folder, "Data to be checked/work_related_ill_health_shiny.rds"))


# Run QA reports and check the output files
run_qa(filename = "work_related_ill_health")



#END