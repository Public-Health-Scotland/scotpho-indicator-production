#fix_fin_year function

#This function takes a dataframe with a column containing financial year in various formats
#then keeps relevant characters and converts to numeric. I.e. 2022/23 becomes 2022 as a numeric object.  
#It then renames the column to "year" as expected by the analysis functions.
#This function is compatible with the output from extract_fin_year() within the package phsmethods
#If a column called year is present within the dataframe, it will be dropped and replaced with the new fin_year data.

#df - the indicator dataframe that is being prepared
#fy_col_name - the column of the dataframe containing strings of financial years. must be passed in in quotes e.g. fy_col_name = "fin_year"
#first_year_digits - takes either "2" or "4" i.e. does fin_year start with YY or YYYY. 

library(dplyr)
library(stringr)
library(rlang)

fix_fin_year <- function(df, fy_col_name, first_year_digits = c("2", "4")){
  
  #If first_year_digits are neither 2 nor 4 stop the function and print an error
  if (!(first_year_digits %in% c("2", "4"))) {
    stop("first_year_digits must be either '2' or '4'.")
  }
  
  fy_col_name_sym <- rlang::sym(fy_col_name) #used for tidyverse data cleaning of column with variable name
  fy_col_name_chr <- rlang::as_string(fy_col_name_sym) #used for detecting column names and conditional renaming
  
  #If fin_year format is e.g. YYYY/YY or YYYY-YY or YYYY-Y etc. First part has 4 digits
  if(first_year_digits == "4"){
    df <- df |> 
      mutate(!!fy_col_name_sym := as.character(!!fy_col_name_sym), #convert to character in case fy provided as a number e.g. 202223
             !!fy_col_name_sym := str_sub(!!fy_col_name_sym, 1, 4)) #keep only first four digits
    
    #If fin_year format is e.g. YY/YY, YY-YY. First part has 2 digits            
  }else if(first_year_digits == "2"){
    df <- df |> 
      mutate(
        !!fy_col_name_sym := as.character(!!fy_col_name_sym), #convert to character in case fy provided as a number e.g. 2223
        temp_year := as.numeric(str_sub(!!fy_col_name_sym, 1, 2)), #keep only first two digits and convert to numeric, store in temp variable
        !!fy_col_name_sym := case_when(
          temp_year > 50 ~ paste0("19", temp_year),  #This is guesswork for correct century!
          TRUE ~ paste0("20", temp_year))) |> #If remaining 2 digits >50 guess 1900s, if <=50 guess 2000s
      select(-temp_year) #drop the temporary year variable
  }
  
  #Convert output from both scenarios to numeric
  df <- df |> 
    mutate(!!fy_col_name_sym := as.numeric(!!fy_col_name_sym))
  
  #Final formatting step - rename fin_year col to fin as expected by analysis functions if needed. If there's already a col called year, drop it
  if (fy_col_name_chr != "year" && "year" %in% colnames(df)) { #if both year and fin_year present
    df <- df |> 
      select(-c("year")) |>  #drop the extra year col 
      rename(year := !!fy_col_name_sym) #rename fin_year to year
  } else if(fy_col_name_chr != "year"){ #if fin_year needs renaming but there is no other col called year
    df <- df |> 
      rename(year := !!fy_col_name_sym) #rename fin_year to year
  }else{
    df #no action needed if column was passed in as "year" to begin with 
  }
  
}

#End
