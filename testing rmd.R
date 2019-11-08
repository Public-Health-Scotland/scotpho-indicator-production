# Try these to test rmd report 

#need to run indicator analysis function before any checking reports will work.
source("1.indicator_analysis.R") #Normal indicator functions


#once values from function are set the lines below should run checking report.
#below is a list of various different combinations of report.


####ISSUES TO BE FIXED
#can't text how report works when CI are absent - although it ought to work
#something weird with one of the flextables in certain circumstances.


# option to have different old file name
qa_function(filename = "alcohol_deaths_male", old_file = "alcohol_deaths_male")

# simple check no optional parameters
qa_function(filename = "copd_hospital_dz11")
qa_function(filename = "alcohol_deaths_female")

# Function runs fine for alcohol indicator when no additional geos therefore something goes wrong when extra s12?
qa_function(filename = "alcohol_deaths_dz11")

# include additional optional geographies (stops working)
qa_function(filename = "alcohol_deaths_dz11",check_extras = c("S02001255","S12000014"))

# include additional optional geographies (seems ok when S02 geo added)
qa_function(filename = "alcohol_deaths_dz11",check_extras = c("S02001255"))

# include additional optional geographies (S12 geo introduces issue - can't figure out why)
qa_function(filename = "alcohol_deaths_dz11",check_extras = c("S12000014"))

#no numerator (works fine)
qa_function(filename = "life_expectancy_male")


#no confidence intervals 
# abi or active travel - files in shiny data folder don't seem to have usual naming conventions
# abi don't have council data which causes report checking to fail at the moment.
qa_function(filename = "ABIs_delivered_final")


#Read in indicator file being checked
data_indicator <- readRDS(paste0(data_folder, "Data to be checked/", filename, "_shiny.rds")) 

