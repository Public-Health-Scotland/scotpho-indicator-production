############################################.
## Analyst notes ----
############################################.

# This script covers three indicators:
# 
# - People perceiving rowdy behaviour very/fairly common in their neighbourhood (4115)
# - Adults rating neighbourhood as very good place to live (20903)
# - Perception of drug misuse in neighbourhood (4203)
#  
#  Data is sourced from the Scottish Household Survey - contact
#   Hannah.Wolfram@gov.scot
#   
# CAVEAT: Typically, SHS respondents are interviewed face-to-face, in their homes.
# However, in March 2020 the fieldwork approach was altered in response to the 
# Covid-19 pandemic. 
# - most 2020 survey fieldwork and all 2021 used telephone interviews
# - these are 'experimental statistics and not directly comparable to 2019 and before
# - As with the 2020 results, the results of the 2021 SHS telephone
# - The results from 2020 and 2021 telephone surveys are broadly comparable. 
# - 2020 data was collected in October 2020 and January-March of 2021, 
# - 2021 data was collected over the course of a whole year, April 2021 - March 2022.
#  So users should consider potential seasonal effects when making comparison
#  between the two survey years.
#  
#  NOTE: for these indicators, we are provided data with confidence intervals 
#  and pre-calculated percentages etc for all geographies we need. As a result
#  this indicator does not need to be run through analyze_first and analyse_second.
#  The data just needs to be formatted to match the last updates format 
#  (e.g /Shiny Data/4203 Perception drug misuse_shiny)
#  
#  Section 4 - Checks includes only a very rudimentary check of this years update
#  against last years, grouping by year


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

filepath <- paste0(data_folder, "Received Data/Neighbourhood perceptions/Final tables 2024.xlsx")

###############################################.
## Read in all data ----
###############################################.

## Create function to read in all sheets of received data file
multiplesheets <- function(filepath) { 
  
  # getting info about all excel sheets 
  sheets <- excel_sheets(filepath) 
  tibble <- lapply(sheets, function(x) read_excel(filepath, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  # cleaning column names
  clean_names(data_frame)
  
} 

#running function
all_data <- multiplesheets(filepath)

###############################################.
## Create functions for cleaning data
###############################################.

#Takes Scotland sheets for all 3 indicators and performs basic cleaning ready for combination w/ HB and CA
scotland_cleaning <- function(data_frame){ 
  
  data_frame <- data_frame |> 
    row_to_names(row_number = 1) |>  #set first row as headings
    mutate(areatype = c("Scotland")) |> #create areatype variable and set to Scotland
    mutate(areaname = c("Scotland")) |>  #create areaname variable and set to Scotland
    clean_names() #cleans column names 
    
}

#Takes HB and CA sheets for all 3 indicators and performs basic cleaning
hb_ca_cleaning <- function(data_frame){
  
  data_frame <- data_frame |> 
    row_to_names(row_number = 1)
    
    
}



###############################################.
## Create drug misuse file ----
###############################################.

drug_misuse_scot <- scotland_cleaning(all_data$table_1)


###############################################.
## Create rowdy behaviour file ----
###############################################.

rowdy_behaviour_scot <- scotland_cleaning(all_data$table_4)


###############################################.
## Create very good place to live file ----
###############################################.

good_place_scot <- scotland_cleaning(all_data$table_7)



