###   Update avoidable mortality indicators:
#   99124: Avoidable mortality, females
#   99125: Avoidable mortality, males


# Data source is National Records of Scotland avoidable mortality publications (older data from the archive):
# 2022 (2019-2021 data): https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/avoidable-mortality
# 2021 (2018-2020 data): https://webarchive.nrscotland.gov.uk/20220317012600/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/avoidable-mortality
# 2019 (2017-2019 data): https://webarchive.nrscotland.gov.uk/20210317012600/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/avoidable-mortality



### functions/packages -----
source("1.indicator_analysis.R")



### 1. Read in data ----

# Identify files
source_file_17_19 <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), 
                                                                        pattern="avoid-mortality-19"))
source_file_18_20 <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), 
                                                                        pattern="avoid-mortality-20"))
source_file_19_21 <- paste0(data_folder, "Received Data/", list.files(path=paste0(data_folder,"Received Data/"), 
                                                                        pattern="avoid-mortality-21"))


## Read in 2017-2019 data

# Save sheets to read in and the corresponding column names
colnames <- c("sex", "areaname", "rate", "lowci", "upci", "number")
sheets_17_19 <- c("Table 4", "Table 5")

# Read in Tables 4 (HB) and 5 (CA) data
tibble_17_19 <- lapply(sheets_17_19, function(x) readxl::read_excel(source_file_17_19, sheet = x, 
                                                        range = cell_cols("A:F"), col_names = colnames)) 
# Save as two data frames 
HB_data_17_19 <- as_tibble(tibble_17_19[[1]])
CA_data_17_19 <- as_tibble(tibble_17_19[[2]])



## Read in 2018-2020 data

# Save sheets to read in
sheets <- c("Table 8", "Table 10")

# Read in Table 8 (HB) and Table 10 (CA) data
tibble_18_20 <- lapply(sheets, function(x) readxl::read_excel(source_file_18_20, sheet = x, 
                                                        range = cell_cols("A:F"), col_names = colnames)) 
# Save as two data frames 
HB_data_18_20 <- as_tibble(tibble_18_20[[1]])
CA_data_18_20 <- as_tibble(tibble_18_20[[2]])



## Read in 2019-2021 data

# Read in Table 8 (HB) and Table 10 (CA) data
tibble_19_21 <- lapply(sheets, function(x) readxl::read_excel(source_file_19_21, sheet = x, 
                                                              range = cell_cols("A:F"), col_names = colnames)) 
# Save as two data frames 
HB_data_19_21 <- as_tibble(tibble_19_21[[1]])
CA_data_19_21 <- as_tibble(tibble_19_21[[2]])


# Read in geography lookup
dictionary <- readRDS(paste0(lookups, "Geography/opt_geo_lookup.rds")) %>%
  select(!c(parent_area, areaname_full))



### 2. Prepare data  -----

data <- HB_data_17_19 %>%
  
  # Bind all data frames together
  bind_rows(list(HB_data_18_20, HB_data_19_21, CA_data_17_19, CA_data_18_20, CA_data_19_21), .id = "id") %>%
  
  # Copy values in the sex column down through blank cells
  fill(sex) %>%
  
  # Keep only male and female rows
  filter(sex %in% c("Male", "Female")) %>%

         # Create new area type column from data frame id
  mutate(areatype = case_when(id %in% c("1", "2", "3") ~ "Health board",
                              id %in% c("4", "5", "6") ~ "Council area"),
         
         # Create new time period column from data frame id
         trend_axis = case_when(id %in% c("1", "4") ~ "2017-2019",
                                id %in% c("2", "5") ~ "2018-2020",
                                id %in% c("3", "6") ~ "2019-2021"),
         
         # Create new indicator id column from sex
         ind_id = case_when(sex == "Female" ~ 99124,
                            sex == "Male" ~ 99125),
         
         # Tidy geography names
         areaname = if_else(areatype == "Health board", paste0("NHS ", areaname), areaname),
         areaname = str_replace_all(areaname, "\\band", "&"),
         sex = tolower(sex),
         
         # Create new columns
         numerator = "",
         year = as.numeric(str_sub(trend_axis, start= 1, end = 4))+1,
         def_period = paste0("3-year aggregate (",trend_axis,")")) %>%
  
  # Remove blank rows
  na.omit(rate) %>%
  
  # Transform numeric variables to the right format
  mutate_at(c("rate", "lowci", "upci", "number"), as.numeric) %>%
  
  # Join area codes
  left_join(dictionary, by = c("areaname", "areatype")) %>%
  
  # Select relevant columns
  select(c(sex, ind_id, code, year, def_period, trend_axis, rate, lowci, upci, numerator)) 






### 3. Prepare final files -----

# Create function to prepare final shiny outputs
prepare_shiny_file <- function(sex_grp) {
  
  #  Select relevant data
  dat <- data %>%
    filter(sex == sex_grp) %>%
    select(!sex)
  
  # Save files in folder to be checked
  write.csv(dat, paste0(data_folder, "Data to be checked/avoidable_mortality_", sex_grp, "_shiny.csv"), row.names = FALSE)
  write_rds(dat, paste0(data_folder, "Data to be checked/avoidable_mortality_", sex_grp, "_shiny.rds"))
  
  # Make data file created available outside of function so it can be visually inspected if required
  indicator_result <<- dat 

}


# Create files for each indicator and sex
for (i in unique(data$sex)){ 
  prepare_shiny_file(sex_grp = i)
}


# Run QA reports for each indicator check the output files
run_qa(filename="avoidable_mortality_female")
run_qa(filename="avoidable_mortality_male")



#END
