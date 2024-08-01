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
## Filepaths/Functions/Lookups ----
###############################################.
source("1.indicator_analysis.R") 

filepath <- paste0(data_folder, "Received Data/Neighbourhood perceptions/Final tables 2024.xlsx")


# The ADP lookup needs to be read in and matched to the data separately as there 
# are some local authorities that have the same name.
area_codes <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S00|S12|S00|S08"))

area_codes_adp <- readRDS(paste0(data_folder,"Lookups/Geography/codedictionary.rds")) |> 
  filter(str_detect(code, "S11"))

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
  
  #removing cover sheet and contents (first two sheets)
  data_frame[-c(1,2)]
  
} 

#running function
all_data <- multiplesheets(filepath)


###############################################.
## Create function for cleaning data
###############################################.

indicator_cleaning <- function(scot_df, hb_df, adp_df = NULL, ca_df = NULL){
  
  #scotland dfs
  scot_df <- scot_df |> 
    row_to_names(row_number = 1) |>  #set first row as headings
    mutate(areatype = c("Scotland")) |> #create areatype variable and set to Scotland
    mutate(areaname = c("Scotland")) |>  #create areaname variable and set to Scotland
    clean_names() #cleans column names
  
  #hb dfs
  hb_df <- hb_df |> 
    row_to_names(row_number = 1) |> 
    mutate(areatype = c("Health board")) |> 
    rename(areaname = `NHS Board`) |> 
    clean_names()
  
  #function can only take adp OR ca, not both, 
  # and produces 1 df containing whichever of ca/adp is passed into function
  if(is.null(adp_df)){ 
    ca_adp_df <- ca_df |> 
      row_to_names(row_number = 1) |> 
      mutate(areatype = c("Council area")) |> 
      rename(areaname = `Local authority`) |> 
      clean_names()
  } else {
    ca_adp_df <- adp_df |> 
      row_to_names(row_number = 1) |> 
      mutate(areatype = c("Alcohol & drug partnership")) |> 
      rename(areaname = `Alcohol & Drug Partnership`) |> 
      clean_names()
  }
  
  #combine scot, hb and adp/ca dfs
  cleaned_df <- rbind(scot_df, hb_df, ca_adp_df) 
  
  cleaned_df <- cleaned_df |> 
    mutate_at(c(2:5), as.numeric) |> #convert columns with data to numeric
    mutate(across(where(is.numeric), round, 1)) #round to 1dp

  
}


###############################################.
## Create drug misuse file ----
###############################################.

drug_misuse <- indicator_cleaning(all_data$Table_1, all_data$Table_2, all_data$Table_3)


###############################################.
## Create rowdy behaviour file ----
###############################################.

rowdy_behaviour <- indicator_cleaning(all_data$Table_4, all_data$Table_5, all_data$Table_6)


###############################################.
## Create very good place to live file ----
###############################################.

good_place <- indicator_cleaning(all_data$Table_7, all_data$Table_8, ca_df = all_data$Table_9)




