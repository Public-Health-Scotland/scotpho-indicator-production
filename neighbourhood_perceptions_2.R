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
  filter(str_detect(code, "S00|S12|S08"))

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
## Create function for cleaning data ----
###############################################.

indicator_cleaning <- function(id, scot_df, hb_df, adp_df = NULL, ca_df = NULL, area_codes, area_codes_adp){
  
  #scotland dfs
  scot_df <- scot_df |> 
    row_to_names(row_number = 1) |>  #set first row as headings
    mutate(areatype = c("Scotland")) |> #create areatype variable and set to Scotland
    mutate(areaname = c("Scotland")) |>  #create areaname variable and set to Scotland
    mutate(code = c("S00000001")) |> #create code variable with Scotland code
    select(areaname, everything()) |> #makes area name column first in line with other areatypes
    clean_names() #cleans column names
  
  #hb dfs
  hb_df <- hb_df |> 
    row_to_names(row_number = 1) |> 
    mutate(areatype = c("Health board")) |> 
    rename(areaname = `NHS Board`) |> 
    mutate(areaname = case_when(areaname == "Orkney Islands" ~ "Orkney", 
                                areaname == "Shetland Islands" ~ "Shetland",
                                .default = areaname)) |> #removing "Islands" from Orkney and Shetland 
    clean_names()

  hb_df$areaname = paste0("NHS ", hb_df$areaname) #pasting NHS onto hb names
  
  hb_df <- hb_df |> 
    left_join(filter(area_codes, str_detect(code, "S08"))) #joining with lookup
  
  
  
  #function can only take adp OR ca, not both, 
  # and produces 1 df containing whichever of ca/adp is passed into function
  if(is.null(adp_df)){ 
    ca_adp_df <- ca_df |> 
      row_to_names(row_number = 1) |> 
      mutate(areatype = c("Council area")) |> 
      rename(areaname = `Local authority`) |>
      mutate(areaname = case_when(str_detect(areaname, "&") ~ str_replace(areaname, "&", "and"),
                                  areaname == "Edinburgh, City of" ~ "City of Edinburgh", 
                                  .default = areaname)) |> 
      left_join(filter(area_codes, str_detect(code, "S12"))) |>
      clean_names()
  } else {
    ca_adp_df <- adp_df |> 
      row_to_names(row_number = 1) |> 
      mutate(areatype = c("Alcohol & drug partnership")) |> 
      rename(areaname = `Alcohol & Drug Partnership`) |>
      left_join(area_codes_adp) |>
      mutate(code = case_when(areaname == "MALDEP" ~ "S11000051", 
                                  areaname == "Lanarkshire ADP" ~ "S11000052",
                                  .default = code)) |> 
      clean_names()
  }
  
  #combine scot, hb and adp/ca dfs
  cleaned_df <- rbind(scot_df, hb_df, ca_adp_df) 
  
  cleaned_df <- cleaned_df |> 
    mutate_at(c(3:6), as.numeric) |> #convert columns with data to numeric
    mutate(across(where(is.numeric), round, 1)) |>  #round to 1dp 
    mutate(ind_id = id, #create indicator id col based on argument to function
           numerator = "NA", #create numerator 
           def_period = year, #duplicate year column
           trend_axis = case_when(def_period == "2007-2008" ~ "2007/2008", #create trend axis col
                                  def_period == "2009-2010" ~ "2009/2009",
                                  .default = def_period),
           def_period = case_when(def_period == "2007-2008" ~ "2007 to 2008 survey years; 2-year aggregates",
                                  def_period == "2009-2010" ~ "2009 to 2010 survey years; 2-year aggregates",
                                  .default = paste(def_period,"survey year")), #create def_period col 
           year = substr(year, 1, 4), #keep only first year for multi-year rows
           year = as.integer(year)) |>  
    select(code, ind_id, year, numerator, percent, lower_95_percent_ci, upper_95_percent_ci, def_period, trend_axis) |> #drop unnecessary cols 
    rename(rate = percent, #rename cols to align with shiny data
           lowci = lower_95_percent_ci,
           upci = upper_95_percent_ci)

  
}


###############################################.
## Run function for drug misuse (4203) ----
###############################################.

drug_misuse <- indicator_cleaning(id = "4203" ,
                                  all_data$Table_1, all_data$Table_2, all_data$Table_3, 
                                  area_codes = area_codes, area_codes_adp = area_codes_adp)

saveRDS(drug_misuse, file = paste0(data_folder, "Data to be checked/perception_drug_misuse_shiny.rds"))
write.csv(drug_misuse, file = paste0(data_folder, "Data to be checked/perception_drug_misuse_shiny.csv"),row.names = F)

###############################################.
## Run function for rowdy behaviour (4115) ------
###############################################.

rowdy_behaviour <- indicator_cleaning(id = "4115",
                                      all_data$Table_4, all_data$Table_5, all_data$Table_6, 
                                      area_codes = area_codes, area_codes_adp = area_codes_adp)

saveRDS(rowdy_behaviour, file = paste0(data_folder, "Data to be checked/perceiving_rowdy_behaviour_shiny.rds"))
write.csv(rowdy_behaviour, file = paste0(data_folder, "Data to be checked/perceiving_rowdy_behaviour_shiny.csv"),row.names = F)

###############################################.
## Run function for neighbourhood good place (20903) ----
###############################################.

good_place <- indicator_cleaning(id = "20903",
                                 all_data$Table_7, all_data$Table_8, ca_df = all_data$Table_9, 
                                 area_codes = area_codes, area_codes_adp = area_codes_adp)

saveRDS(good_place, file = paste0(data_folder, "Data to be checked/adults_rating_neighbourhood_very_good_shiny.rds"))
write.csv(good_place, file = paste0(data_folder, "Data to be checked/adults_rating_neighbourhood_very_good_shiny.csv"),row.names = F)




# 4. Checks ---------------------------------------------------------------

## As there is no analyse_second function used for these indicators, run the following to 
## check data against last years (may need to change the file names)

# a) Read in last years data
last_year_rowdy_behaviour <- read.csv(paste0(data_folder, "Shiny Data/4115 Rowdy behaviour_shiny.csv"))
last_year_good_place <- read.csv(paste0(data_folder, "Shiny Data/20903_Neighbourhood_rating_shiny.csv"))
last_year_drug_misuse <- read.csv(paste0(data_folder, "Shiny Data/4203 Perception drug misuse_shiny.csv"))

# b) function to check totals of shared years

check_year_totals <- function(last_year_data, this_year_data){
  
  last_year_max <- as.numeric(max(last_year_data$year))
  
  last_year <- last_year_data |> 
    group_by(year) |> 
    summarize(last_year_sum = sum(rate)) |> 
    ungroup()
  
  this_year <- this_year_data |> 
    filter(last_year_max >= year) |> 
    group_by(year) |> 
    summarize(this_year_sum = sum(rate)) |> 
    ungroup()
  
  both_years <- last_year |> 
    left_join(this_year, by = "year") |> 
    mutate(check = case_when(last_year_sum == this_year_sum ~ 0,
                             is.na(this_year_sum) ~1,
                             is.na(last_year_sum) ~ 1,
                             .default = 1 ))
  
  test <- both_years |> 
    mutate(test_column = sum(check))
  
  # if last year matches, then 0 is returned. For the check if they all match,
  # the sum of check should be 0, if not return all non-matching rows
  
  if(test$test_column[1] == 0) {
    
    print("All totals match")
    
    
  } else {
    non_match <- filter(both_years,
                        check != 0) 
    
    return(non_match)
    print("Totals don't match. See non_match dataframe.")
  }
}

# Check totals

check_year_totals(last_year_data = last_year_rowdy_behaviour, this_year_data = rowdy_behaviour)
check_year_totals(last_year_data = last_year_good_place, this_year_data = good_place)
check_year_totals(last_year_data = last_year_drug_misuse, this_year_data = drug_misuse)



