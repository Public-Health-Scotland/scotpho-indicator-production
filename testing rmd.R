# Try these to test rmd report 

#need to run indicator analysis function before any checking reports will work.
source("1.indicator_analysis.R") #Normal indicator functions


#once values from function are set the lines below should run checking report.
#below is a list of various different combinations of report.


# option to have different old file name
run_qa(filename = "alcohol_deaths_male", old_file = "alcohol_deaths_male")

# simple check no optional parameters
run_qa(filename = "copd_hospital_dz11")
run_qa(filename = "alcohol_deaths_female")
run_qa(filename = "alcohol_deaths_dz11")
run_qa(filename = "copd_hospital_dz11")
run_qa(filename = "copd_deaths")
run_qa(filename = "copd_incidence")

#no numerator (works fine)
run_qa(filename = "life_expectancy_male")


# include additional optional geographies 
run_qa(filename = "alcohol_deaths_dz11",check_extras = c("S12000014")) #works
run_qa(filename = "alcohol_deaths_dz11",check_extras = c("S12000020","S12000014")) #stops working with 2 geos
run_qa(filename = "alcohol_deaths_dz11",check_extras = c("S02001255","S12000014")) #not working



##no confidence intervals 
# abi or active travel - files in shiny data folder don't seem to have usual naming conventions
# abi don't have council data which causes report checking to fail at the moment.
run_qa(filename = "ABIs_delivered_final")





