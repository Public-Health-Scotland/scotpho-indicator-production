#############################.
# Analyst notes ----
#############################.

#This script updates the following indicators
#Personal licences in force (4140)
#Premise licences in force - off trade (4139)
#Premise licences in force - on trade (4114)
#Premise licences in force - Total (4144)

# Files produced:
# Main: Y
# Deprivation: N
# Popgroups: N

#Update process:
#- Check if this publication has been added to statistics.gov or opendatascot. 
#If not, download from https://www.gov.scot/publications/scottish-liquor-licensing-statistics/
#Save as CSV file
#Double check layout of file is same as last year
#Change list of years vector in Part 1 to include latest year
#Change year end parameters in analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/data cleaning functions/ca_names_to_codes.R") #Converts council names to codes
library(tidyr) #For pivoting

################################################################################
#####  Part 1) Read in data --------------------------------
################################################################################

liquor_folder <- file.path(profiles_data_folder, "Received Data/Liquor Licences")

files <- list.files(liquor_folder, pattern = "\\.csv$", full.names = TRUE)

dfs <- map(files, read_csv)  # Reads in each year as a list. Takes a few seconds to run.

years <- 2010:2022 #update this each year

################################################################################
#####  Part 2) Convert list into a dataframe  --------------------------------
################################################################################
#Up until 2019/20, there were no blank rows between the title and the CA names. Since 2020/21, there have been 2 blank rows
#This code (note it is base R Map() not purrr's map() leaves the first 9 years untouched (i<=9) and removes the 2 blank rows (c(1:2)) for all following years)
#Assuming these 2 blank rows remain this should continue to work going forward

df2 <- Map(\(e, i) if (i <= 9) e else e[-c(1:2), , drop = FALSE], 
           dfs, seq_along(dfs)) 

df2 <- imap(df2, ~ .x |>
              select(1:34) |> #remove extra blank cols
              row_to_names(1) |> #set ca names to headings
              rename(measure = 1) |>   #renaming the blank first row heading to measure - needed to prevent issues slicing
              mutate(year = year[.y]) |>  #adding a year col to each element in list based on years vector created above
              select(year, everything()) |> #moving year to the beginning 
              slice(c(2:5, 19:22))) #keep only rows with relevant measures. Can't be precise with indexes because it varies from year to year.

ca_col_names <- names(df2[[12]]) #creating a list of all the column names to apply to all dataframes. Choosing the 12th year of data because it had the least footnotes so less string manipulation needed

df3 <- map(df2, ~ .x |> 
             set_names(ca_col_names))|> #set the ca names to be the column names based on vector
  bind_rows() #join each year of data into a df


################################################################################
#####  Part 3) Tidy up the data --------------------------------
################################################################################

df4 <- df3 |> 
  filter(str_detect(measure, str_c(c("Licences in", "on sale", "off sale", "both"), collapse = "|")))  |>  #filter to exclude any rows accidentally picked up due to imprecise slicing
  mutate(measure = str_replace_all(measure, 
                                   c("force.*" ="force",
                                     "Force.*" = "force",
                                     "on sale.*" = "on sale",
                                     "off sale.*" = "off sale"))) |> #cutting off any footnotes etc for each of the main 4 categories
  group_by(year, measure) |> #in earlier years, licences measure names for personal and premise licences were identical.
  mutate(.pos = row_number()) |> 
  ungroup() |> 
  mutate(measure = if_else(.pos == 2, paste0("Personal ", measure), measure)) |> 
  select(-.pos) #If 2 identical measures in a year, prepend "Personal" to the second one

df5 <- pivot_longer(df4, cols = -c(year, measure), names_to = "areaname", values_to = "numerator") |>  #pivoting council names longer. cols=-2 pivots everything except the first 2 cols year and measure
  mutate(numerator = dplyr::na_if(numerator, "-"), #need to reference dplyr as it's being masked by another package
         numerator = str_replace(numerator, ",", ""), #remove commas from numbers
         numerator = as.numeric(numerator)) #convert numerator to numeric type

df6 <- pivot_wider(df5, id_cols = c(areaname, year), names_from = measure, values_from = numerator) |> #pivot wider so each indicator numerator gets a column
  filter(areaname != "SCOTLAND") |> #drop scotland figs - to be re-added in analysis functions
  ca_names_to_codes(areaname) |> #convert ca names to codes
  rename(personal = `Personal Licences in force`, #simplify column headings
         premise = `Licences in force`,
         on_premise = `(a) on sale`,
         off_premise = `(b) off sale`,
         both = `(c) both`) |> 
  mutate(on_premise = if_else(!is.na(both), on_premise + both, on_premise),
         off_premise = if_else(!is.na(both), off_premise + both, off_premise)) |> 
  select(-both) #add both - The totals in the original files are on + off + both for total. 
#So I think both needs to be added to on and off, with the understanding that the sum of off and on will exceed the total

################################################################################
#####  Part 4) Personal licences in force (4140) --------------
################################################################################  
personal_licences <- df6 |> select(code, year, personal) |> 
  rename(numerator = personal)

saveRDS(personal_licences, file.path(profiles_data_folder, "Prepared Data/personal_licences_raw.rds"))

main_analysis("personal_licences", measure = "crude", geography = "council",
              year_type = "financial", ind_id = "4140", time_agg = 1, yearstart = 2010,
              yearend = 2022, pop = "CA_pop_18+", crude_rate = 10000)

################################################################################
#####  Part 4) Premise licences in force - total (4144) --------------
################################################################################ 
premises_total <- df6 |> select(code, year, premise) |> 
  rename(numerator = premise)

saveRDS(premises_total, file.path(profiles_data_folder, "Prepared Data/premise_licences_raw.rds"))

main_analysis("premise_licences", measure = "crude", geography = "council",
              year_type = "financial", ind_id = "4144", time_agg = 1, yearstart = 2010,
              yearend = 2022, pop = "CA_pop_18+", crude_rate = 10000)

################################################################################
#####  Part 5) Premise licences in force - on trade (4114) --------------
################################################################################ 
premises_on <- df6 |> select(code, year, on_premise) |> 
  rename(numerator = on_premise)

saveRDS(premises_on, file.path(profiles_data_folder, "Prepared Data/premise_licences_on_trade_raw.rds"))

main_analysis("premise_licences_on_trade", measure = "crude", geography = "council",
              year_type = "financial", ind_id = "4114", time_agg = 1, yearstart = 2010,
              yearend = 2022, pop = "CA_pop_18+", crude_rate = 10000)

################################################################################
#####  Part 6) Premise licences in force - on trade (4139) --------------
################################################################################
premises_off <- df6 |> select(code, year, off_premise) |> 
  rename(numerator = off_premise) |> 
  filter(year != 2010) #no data for 2010

saveRDS(premises_off, file.path(profiles_data_folder, "Prepared Data/premise_licences_off_trade_raw.rds"))

main_analysis("premise_licences_off_trade", measure = "crude", geography = "council",
              year_type = "financial", ind_id = "4139", time_agg = 1, yearstart = 2011,
              yearend = 2022, pop = "CA_pop_18+", crude_rate = 10000)



####End.