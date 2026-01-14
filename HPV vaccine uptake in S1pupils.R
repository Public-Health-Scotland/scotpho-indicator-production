# ~~~~~~~~~~~~~~~~~~~~~~~~
# Analyst notes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~

# This script updates the following 2 indicators:
# 99145 - HPV vaccine uptake in S1 females
# 99146 - HPV vaccine uptake in S1 pupils (males and females)


# As of Aug 2025, these replace the 'HPV vaccine uptake in S3 girls' indicator (see R script in Archived Indicators folder)
# In early days of HPV vaccine program, it was 3 doses for females only starting in S2
# Currently, it's administered as a single dose to both males and females in S1
# Therefore old indicator was archived and new indicators created to better
# reflect current JCVI guidance and vaccination schedule in schools 

# Trend for females starts from 2014/15 as this was the first year offer shifted to S1 females. 
# The universal offer (both males and females in S1) wasn't introduced until 2019/20 so 
# trend for all pupils indicator starts later.


# files produced:
# main: Y (2 files)
# poproups: Y (sex splits to go alongside ind_id 99146)
# deprivation: Y (2 files)

# Data request to child health team each year following release of publication:
# https://publichealthscotland.scot/publications/hpv-immunisation-statistics-scotland/

# Data to be saved in following folder:
# \\Isdsf00d03\ScotPHO\Profiles\Data\Received Data\HPV Vaccine uptake in S1


# ~~~~~~~~~~~~~~~~~~~~~~
# Dependencies -----
# ~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
source("functions/deprivation_analysis.R")
library(rio) # for importing/exporting multiple files



# ~~~~~~~~~~~~~~~~~~~~~~
# Read data ----
# ~~~~~~~~~~~~~~~~~~~~~~

# folder where rds data files saved
folder <- file.path(profiles_data_folder, "Received Data", "HPV Vaccine uptake in S1")

# data provided over the years
# note 'hpv_s1_2015_2024.rds' file contains multiple years worth of data - this was the first data request for this indicator
# all subsequent files contain 1 year of data
files <- list.files(folder, full.names = TRUE)
print(files)

# read in all files and combine 
# note setting trust = TRUE when reading RDS files avoids warning in rio package as of V2.0.0
data <- rio::import_list(files, rbind = TRUE, trust = TRUE) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare temp files for analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create year column using starting year of school year
data$year <- data$schoolyear_ending - 1


# select required columns 
data <- data |>
  select(datazone2011, year, sex, numerator, denominator)


# Data are derived based on school/class year, with other characteristics (such as data zone) 
# appended subsequently. There are some earlier years where e.g. a school may not have yet offered the vaccine
# and so some DZs may appear missing from the dataset i.e. entry/row entirely missing from recieved data file
# (unless the child was offered the vaccine elsewhere - which may then result in small numbers for some DZs)

# Need to add rows for those missing DZs/year combinations and fill with 0
# to ensure rolling average calculation is done correctly.
# This is required as for some years the missing datazones make up entire localities
data <- data |>
  group_by(sex) |> # group by sex as we only expect some 
  complete(year, datazone2011, fill = list(numerator = 0, denominator = 0)) |>
  ungroup()
  

# split data by sex column into a list of 3 dataframes (male/female/both)
df_list <- split(data, data$sex)





# save the 3 dfs
export_list(df_list, file.path(profiles_data_folder, "Prepared Data", paste0("%s", "_hpv_uptake_s1_raw.rds")))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run main analysis ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Uptake in females only 
main_analysis(filename = "Female_hpv_uptake_s1", ind_id = 99145,  measure = "percent", 
              geography = "datazone11", year_type = "school", time_agg = 3, yearstart = 2014, yearend = 2024)


# Uptake in all pupils 
main_analysis(filename = "Both_hpv_uptake_s1", ind_id = 99146,  measure = "percent", 
              geography = "datazone11", year_type = "school", time_agg = 3, yearstart = 2019, yearend = 2024)


# Uptake in males only (created only to be used in popgroups file to go alongside ind_id 99146)
main_analysis(filename = "Male_hpv_uptake_s1", ind_id = 99146,  measure = "percent", 
              geography = "datazone11", year_type = "school", time_agg = 3, yearstart = 2019, yearend = 2024)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare deprivation files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Uptake in females only 
deprivation_analysis(filename = "Female_hpv_uptake_s1", ind_id = 99145,  measure = "percent", 
                    year_type = "school", time_agg = 3, yearstart = 2014, yearend = 2024)


# Uptake in all pupils 
deprivation_analysis(filename = "Both_hpv_uptake_s1", ind_id = 99146,  measure = "percent", 
                     year_type = "school", time_agg = 3, yearstart = 2019, yearend = 2024)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare popgroup file ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# folder where analysis results saved
output_folder <- file.path(profiles_data_folder, "Data to be checked")


# get filepaths to 3 x files produced from main analysis
files <- list.files(
  path = output_folder,
  pattern = "hpv_uptake_s1_shiny.csv", 
  full.names = TRUE
  )


# read and combine the 3 files 
popgroups <- import_list(files, rbind = TRUE, rbind_label = "filename")

# prepare final file
popgroups <- popgroups |>
  # create split name and split value cols
  mutate(split_name = "sex", 
         split_value = case_when(grepl("Male", filename) ~ "Males",
                            grepl("Female", filename) ~ "Females",
                            TRUE ~ "All")) |>
  select(-filename) |>
  # filter on Scotland, council and health boards
  filter(grepl("S00|S08|S12", code)) |> 
  # assign correct indicator id
  mutate(ind_id = 99146)  


# save file 
write.csv(popgroups, file.path(output_folder, "Both_hpv_uptake_s1_shiny_popgrp.csv"), row.names = FALSE)
saveRDS(popgroups, file.path(output_folder, "Both_hpv_uptake_s1_shiny_popgrp.rds"))

# delete male only files from folder
file.remove(file.path(output_folder, "Male_hpv_uptake_s1_shiny.csv"))
file.remove(file.path(output_folder, "Male_hpv_uptake_s1_shiny.rds"))


# QA popgroups file 
run_qa(filename = "Both_hpv_uptake_s1", type = "popgrp")



## END



