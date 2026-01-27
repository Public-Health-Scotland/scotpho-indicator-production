# ScotPHO indicators: New cancer registrations (20301)

#   Part 1 - Prepare the data
#   Part 2 - Run analysis functions

# Files produced:
# Main: Y
# Deprivation: Y
# Popgroups: N

#Update process:
#SMR06 access no longer authorised for ScotPHO team as of 2025 so now obtaining data through IR instead
#Request data from phs.cancerstats@phs.scot
#Add any steps needed
#Change year end parameters in analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") # deprivation function
library(readxl) #for reading in data

###############################################.
## Part 1 - Prepare the data ----
###############################################.
cancer_reg <- read_xlsx(file.path(profiles_data_folder, "Received Data/Cancer/IR2025-01013.xlsx"),
                        sheet = 2) |> 
  rename(year = `Year of Diagnosis`,
         datazone = `Datazone 2011`,
         numerator = `Number of Tumours Diagnosed`) |> #rename cols
  mutate(year = as.numeric(year)) #convert year to numeric
 
saveRDS(cancer_reg, file.path(profiles_data_folder, "Prepared Data/cancer_registrations_raw.rds"))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
main_analysis(filename = "cancer_registrations", geography = "datazone11", measure = "stdrate", 
              yearstart = 2002, yearend = 2023, time_agg = 3, epop_age = "normal", 
              epop_total = 200000, year_type = "calendar", pop = "DZ11_pop_allages", 
              ind_id = 20301)


deprivation_analysis(filename="cancer_registrations", measure="stdrate", time_agg= 3, 
                    yearstart= 2002, yearend=2023, year_type = "calendar", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total= 200000, ind_id = 20301)

##END