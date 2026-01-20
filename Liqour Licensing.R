#############################.
# Analyst notes ----
#############################.

#This script updates the following indicators
#Personal licences in force (4140)
#Premise licences in force - off trade (4139)
#Premise licences in force - on trade (4114)
#Premise licences in force - Total (4144)

#Data downloaded from the SG website at CA level
#2027 update - see if this data file has been added to statistics.gov or opendatascot package
#https://www.gov.scot/publications/scottish-liquor-licensing-statistics/

# Files produced:
# Main: Y
# Deprivation: N
# Popgroups: N

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/data cleaning functions/ca_names_to_codes.R")
library(tidyr)

################################################################################
#####  Part 1) Read in data --------------------------------
################################################################################

liquor_folder <- file.path(profiles_data_folder, "Received Data/Liquor Licences")

files <- list.files(liquor_folder, pattern = "\\.csv$", full.names = TRUE)

dfs <- map(files, ~ read_csv(.x))  # Reads in each year as a list. Takes a few seconds to run.

################################################################################
#####  Part 2) Convert list into a dataframe  --------------------------------
################################################################################
#Up until 2019/20, there were no blank rows between the title and the CA names. Since 2020/21, there have been 2 blank rows
#This code (note it is base R Map() not purrr's map() leaves the first 9 years untouched (i<=9) and removes the 2 blank rows (c(1:2)) for all following years)
#Assuming these 2 blank rows remain this should continue to work going forward

df2 <- Map(\(e, i) if (i <= 9) e else e[-c(1:2), , drop = FALSE],
  dfs, seq_along(dfs))

year <- c(2010:2022) #adding year column

df2 <- imap(df2, ~ .x |>
              select(1:34) |> #remove extra blank cols
              row_to_names(1) |> #set ca names to headings
              rename(measure = 1) |>   #renaming the blank first row heading to measure
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
strings_list <- c("Licences in", "on sale", "off sale", "both") #list of measures to filter on

df4 <- df3 |> 
  filter(str_detect(measure, str_c(c("Licences in", "on sale", "off sale", "both"), collapse = "|"))) |>  #filter to exclude any rows accidentally picked up due to imprecise slicing
  mutate(measure = str_replace(measure, "force.*", "force"),
         measure = str_replace(measure, "Force.*", "force"),
         measure = str_replace(measure, "on sale.*", "on sale"),
         measure = str_replace(measure, "off sale.*", "off sale")) |> #cutting off any footnotes etc for each of the main categories
  group_by(year, measure) |>
  mutate(.pos = row_number()) |> 
  ungroup() |> 
  mutate(measure = if_else(.pos == 2, paste0("Personal ", measure), measure)) |> #in earlier licences measure names for personal and premise licences were identical.
  select(-.pos) #If "licence" features twice in a year, prepend "Personal" to the second one

df5 <- pivot_longer(df4, cols = -c(1:2), names_to = "areaname", values_to = "numerator") |>  #pivoting council names longer. cols=-2 pivots everything except the first 2 cols year and measure
  mutate(numerator = dplyr::na_if(numerator, "-"), #need to reference dplyr as it's being masked by another package
         numerator = str_replace(numerator, ",", ""),
         numerator = as.numeric(numerator)) #NAs may be produced, not a problem
  
  df6 <- pivot_wider(df5, id_cols = c(areaname, year), names_from = measure, values_from = numerator) |> 
  filter(areaname != "SCOTLAND") |> #drop scotland figs - to be readded in analysis functions
  ca_names_to_codes(areaname) |> #convert ca names to codes
  rename(personal = `Personal Licences in force`,
         premise = `Licences in force`,
         on_premise = `(a) on sale`,
         off_premise = `(b) off sale`,
         both = `(c) both`) |> 
  mutate(on_premise = if_else(!is.na(both), on_premise + both, on_premise),
         off_premise = if_else(!is.na(both), off_premise + both, off_premise)) |> 
    select(-both) #add both - The totals in the original files are on + off + both for total. 
  #So I think both needs to be added to on and off, with the understanding that the total will be less than the sum of on and both?
  
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
  #####  Part 4) Personal licences in force (4140) --------------
  ################################################################################ 
  premises_total <- df6 |> select(code, year, premise) |> 
    rename(numerator = premise)
  
  saveRDS(premises_total, file.path(profiles_data_folder, "Prepared Data/premise_licences_raw.rds"))
  
  
  
  
  
  
  analyze_first(filename = "premises_total", geography = "council", adp = TRUE,
                measure = "crude", yearstart = 2011, yearend = 2021, 
                pop = "CA_pop_18+", time_agg = 1)
  
  # then complete analysis with the updated '_formatted.rds' file
  analyze_second(filename = "premises_total", measure = "crude", crude_rate = 10000,
                 time_agg = 1, ind_id = "4144", year_type = "financial", pop = "CA_pop_18+")
  
  
  
  saveRDS(premises_total, paste0(data_folder,"Prepared Data/premises_total_raw.rds"))
  
  saveRDS(premises_on, paste0(data_folder,"Prepared Data/premises_on_raw.rds"))
  
  saveRDS(premises_off, paste0(data_folder,"Prepared Data/premises_off_raw.rds"))
  
  saveRDS(personal, paste0(data_folder,"Prepared Data/personal_raw.rds"))
  
  

  #Personal licences in force (4140)
  #Premise licences in force - off trade (4139)
  #Premise licences in force - on trade (4114)
  #Premise licences in force - Total (4144)
#different amounts of whitespace before ca names. Standardise to 2 in excel just for simplicity?

### open received data
## this function will read all excel sheets as a list
read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

## call function with received data
mysheets <- read_excel_allsheets(filename = "/PHI_conf/ScotPHO/Profiles/Data/Received Data/llis_2011-12_to_ 2021-22_combined.xlsx")

## bind list items together into dataframe
df_received <- bind_rows(mysheets) %>% as_tibble() %>% 
  filter(la != "SCOTLAND") # filter out Scotland total

## add la geog codes
# create dataframe for lookup
ca_lookup <- data.frame(
  ca2019 = c("S12000005", "S12000006", "S12000006", 
             "S12000008", "S12000010", "S12000011", 
             "S12000013", "S12000013", "S12000013", "S12000014", 
             "S12000047", "S12000017", "S12000018", 
             "S12000019", "S12000020", "S12000021", 
             "S12000023", "S12000048", "S12000048", 
             "S12000026", "S12000027", "S12000028", 
             "S12000029", "S12000030", "S12000033", 
             "S12000034", "S12000035", "S12000036", 
             "S12000036", "S12000038", "S12000039", 
             "S12000040", "S12000041", "S12000042", 
             "S12000050", "S12000045", "S12000049"),
  la = c("Clackmannanshire","Dumfries & Galloway", "Dumfries and Galloway",
         "East Ayrshire", "East Lothian", "East Renfrewshire", 
         "Na h-Eileanan Siar", "Na h-Eilanan Siar", "Eilean Siar","Falkirk", 
         "Fife", "Highland", "Inverclyde",
         "Midlothian", "Moray", "North Ayrshire", 
         "Orkney Islands", "Perth & Kinross", "Perth and Kinross", 
         "Scottish Borders", "Shetland Islands", "South Ayrshire", 
         "South Lanarkshire", "Stirling", "Aberdeen City", 
         "Aberdeenshire", "Argyll & Bute", "Edinburgh, City of", 
         "City of Edinburgh", "Renfrewshire", "West Dunbartonshire", 
         "West Lothian", "Angus", "Dundee City", 
         "North Lanarkshire", "East Dunbartonshire", "Glasgow City"))

# match to raw data (change df name as required)
df_prepared <- df_received %>% left_join(ca_lookup, by = "la") 
# check for unmatched cases
sum(is.na(df_prepared$ca2019))

df_prepared <- df_prepared %>% select(year, ca2019, premises_total, premises_on,
                                      premises_off, personal) %>% 
  rename(ca = ca2019)

## create separate df for each indicator
premises_total <- df_prepared %>% select(year, ca, premises_total) %>% 
  rename(numerator = premises_total)
premises_on <- df_prepared %>% select(year, ca, premises_on) %>% 
  rename(numerator = premises_on)
premises_off <- df_prepared %>% select(year, ca, premises_off) %>% 
  rename(numerator = premises_off)
personal <- df_prepared %>% select(year, ca, personal) %>% 
  rename(numerator = personal)


# save rds raw files for use in analysis funtions

saveRDS(premises_total, paste0(data_folder,"Prepared Data/premises_total_raw.rds"))

saveRDS(premises_on, paste0(data_folder,"Prepared Data/premises_on_raw.rds"))

saveRDS(premises_off, paste0(data_folder,"Prepared Data/premises_off_raw.rds"))

saveRDS(personal, paste0(data_folder,"Prepared Data/personal_raw.rds"))


###############################################.
## Part 2 - Run analysis functions ----
###############################################.

###### premises licenses total --------
# didnt seem to run all these at once so ran indicators separately

analyze_first(filename = "premises_total", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2021, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_total", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4144", year_type = "financial", pop = "CA_pop_18+")

###### premises licenses on trade --------
analyze_first(filename = "premises_on", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2021, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_on", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4114", year_type = "financial", pop = "CA_pop_18+")


###### premises licenses off trade --------
analyze_first(filename = "premises_off", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2021, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "premises_off", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4139", year_type = "financial", pop = "CA_pop_18+")

###### personal licenses --------
analyze_first(filename = "personal", geography = "council", adp = TRUE,
              measure = "crude", yearstart = 2011, yearend = 2021, 
              pop = "CA_pop_18+", time_agg = 1)

# then complete analysis with the updated '_formatted.rds' file
analyze_second(filename = "personal", measure = "crude", crude_rate = 10000,
               time_agg = 1, ind_id = "4140", year_type = "financial", pop = "CA_pop_18+")


#### ----------------

