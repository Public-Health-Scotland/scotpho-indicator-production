###   Update ScotPHO Care and Wellbeing indicator: 
#   99132: Air quality

# Data source is DEFRA Population-weighted annual mean PM2.5 data:
# https://uk-air.defra.gov.uk/data/pcm-data#population_weighted_annual_mean_pm25_data

# Each year, download the latest 2 csv files from the link above (UK local authority and UK country files)
# and save in this folder: /PHI_conf/ScotPHO/Profiles/Data/Recieved Data/Air quality

### functions/packages -----
source("functions/main_analysis.R") 


### 1. Read in data ----

# Identify data files
data_files <- list.files(path = file.path(profiles_data_folder,"Received Data/Air quality"), pattern = ".csv", full.names = TRUE)

# Define column names
column_names <- c("areacode", "total", "non_anthropogenic", "anthropogenic", "areaname")

# Read in data
data_list <- lapply(data_files, function(x) read_csv(data_files, col_names = column_names, skip = 3, id = "file_name")) 


# Save as single data frame
dat <- reduce(data_list, full_join)

# Read in geography lookup
dictionary <- readRDS(file.path(profiles_data_folder, "Lookups/Geography/opt_geo_lookup.rds")) %>%
  filter(areatype %in% c("Scotland", "Council area")) %>% 
  select(c(code, areaname))



### 2. Prepare data  -----

data <- dat %>% 
  
  # Create year variable by substracting characters from file name
  mutate(year = as.numeric(substr(file_name, start = 68, stop = 71)),
         
         # Tidy LA names
         areaname = str_replace_all(areaname, c(" and " = " & ",
                                                "Edinburgh, City of" = "City of Edinburgh",
                                                "Eilean Siar" = "Na h-Eileanan Siar")),
         
         # Create new columns
         trend_axis = year,
         def_period = paste0(year, " calendar year"),
         lowci = NA, upci = NA, rate = total,
         ind_id = 99132,
         numerator = total) %>%

  # Join area codes by area name
  left_join(dictionary, by = "areaname") %>%
  #air quality data only available for scotland and council area
  droplevels() %>% # remove all traces of non-matched geographies

  # Filter for Scottish data
  filter(str_detect(code, "^S")) %>% 
  
  # Select relevant columns
  select(ind_id, code, year, trend_axis, def_period, numerator, rate, lowci, upci)


### 3. Prepare final files -----

# Save files in folder to be checked
write.csv(data, file.path(profiles_data_folder, "Data to be checked/air_quality_shiny.csv"), row.names = FALSE)
write_rds(data, file.path(profiles_data_folder, "Data to be checked/air_quality_shiny.rds"))

# QA data file
run_qa(filename = "air_quality", type = "main")

#END
