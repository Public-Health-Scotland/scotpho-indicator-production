#fix_fin_year function

#This function takes a dataframe with a column containing financial year in a string format YYYY/YYYY
#keeps only first four characters in the string then converts these to numeric. 
#It then renames the column to "year" as expected by the analysis functions.
#This function is compatible with the output from extract_fin_year() within the package phsmethods
#If a column called year is present within the dataframe, it will be dropped and replaced with the new fin_year data.

#df - the indicator dataframe that is being prepared
#fin_year - the column of the dataframe containing strings of financial years
#This argument does not need to take any particular value as the function will change it to "year". 

fix_fin_year <- function(df, fin_year){
  
  df <- df |> 
    select(-any_of("year")) |> #drops column called year if present - prevents duplication when financial year "year" col is created
    mutate({{fin_year}} := str_sub({{fin_year}}, start = 1, end = 4), #keep only first four digits
           {{fin_year}} := as.numeric({{fin_year}})) |> #convert from string
    rename(year = {{fin_year}}) #renames to "year"
}

#End 

#Testing - delete during PR
# library(lubridate)
# library(phsmethods)
# 
# test <- data.frame(year = as_date(ymd(c("2010-08-03", "2010-02-01", "2021-02-23", "2022-06-30"))))
# 
# test <- test |> 
#   mutate(fin_year = phsmethods::extract_fin_year(year)) |> 
#   fix_fin_year(fin_year)
