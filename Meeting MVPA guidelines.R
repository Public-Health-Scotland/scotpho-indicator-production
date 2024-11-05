# ScotPHO indicators: Percentage of Scottish population meeting the MPVA physical activity guidelines (#88007)
## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#read in main data (no splits)
meet_mvpa <- read.csv(paste0(data_folder, "Received Data/Physical Activity/MVPA/shs_activity_levels_rank_data_2012_15_2018_22_v2.csv"))

