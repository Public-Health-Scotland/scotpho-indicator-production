### 1. notes -----

# this script updates the following indicator: 13016 - School exclusion rate

# numerator source (no. of exclusions): https://www.gov.scot/publications/school-exclusion-statistics/ (Table 2.1: Cases of exclusion by local authority)
# denominator source (no. of pupils):   https://www.gov.scot/publications/pupil-census-supplementary-statistics/  (Table 5.1: Schools by local authority)
# update file names below when new data from links above are saved in "data received" folder.

# required packages/functions -----
source("1.indicator_analysis.R")


### 2. read in data -----

# exclusions by council area (numerator)
ca_exclusions <- read_excel(paste0(data_folder, "Received Data/School Exclusion Rates/Exclusions_202223.xlsx"), sheet = "Table 2.1", skip = 3) |> 
  head(32) #select first 32 rows

# exclusions scotland level (numerator - sits on different tab than council area figures)
scotland_exclusions <- read_excel(paste0(data_folder, "Received Data/School Exclusion Rates/Exclusions_202223.xlsx"), sheet = "Table 1.1", skip = 3) |> 
  filter(`Exclusion type` == "All cases of exclusion") |> #excludes breakdown of exclusion types
  select(-`Exclusion type`) |> 
  select(-c(1:5)) |> #excluding 02/03 to 06/07 as these not available at LA level
  mutate("Local Authority" = "All local authorities", .before = "2007/08") 
  

# total pupils by council area (denominator)
total_pupils <- read_excel(paste0(data_folder, "Received Data/School Exclusion Rates/Pupils_Census_2022.xlsx"), sheet = "Table 5.2", skip = 2) |>  
  head(34) |> 
  row_to_names(row_number = 1) #from janitor package, sets row as header by index

### 3. read in lookups -----

# council area lookup
ca <- readRDS(paste0(lookups,"Geography/CAdictionary.rds")) # council area lookup

# health board lookup
hb <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  select(ca2019, hb2019) %>%
  distinct(.)



### 4. prepare data ------

prepare_dat <- function(df, val_name) {
  
  df |> 
    # renaming/replacing values
    rename_at(1, ~"Local Authority") |> 
    mutate(across(-"Local Authority", ~replace(., . %in% c("c", "low", "x", "z"), 0))) %>% # replace suppression symbols with 0
    mutate(`Local Authority` = str_replace(`Local Authority`, "&","and")) |>  #replace & with "and"
    
    # including council area/scotland codes
    left_join(ca, by = c("Local Authority" = "areaname")) |>  # add in council area codes
    mutate(code = ifelse(`Local Authority` == "All local authorities", "S00000001", code)) |>  #add in scotland code
    
    # reshape from wide to long to include a calendar year column
    pivot_longer(-c("Local Authority", "code"), names_to = "year", values_to = val_name) |>  # pivot longer to create year column
    mutate(year = as.numeric(substr(year, 1, 4))) |> # change date from FY to calendar year
    mutate(across(-c("Local Authority", "code"), as.numeric)) |> # convert values to numeric
    select(-`Local Authority`) #Exclude local authority name
}


# prepare denominator data 
denominator_data <- prepare_dat(total_pupils, "denominator")


# prepare numerator data
numerator_data <- bind_rows(prepare_dat(ca_exclusions, "numerator"), 
                            prepare_dat(scotland_exclusions, "numerator"))          

# combine numerator and denominator
combined_data <- left_join(numerator_data, denominator_data, by = c("code", "year"))


#create figures at health board level
hb_data <- combined_data |>
  filter(code != "S00000001") |>
  left_join(hb, by = c("code" = "ca2019")) |>
  select("code" = "hb2019", "numerator", "denominator", "year") |>
  group_by(code, year) |>
  summarise_all(sum)



#combine ca, scotland and board figures
all <- bind_rows(combined_data, hb_data) |> 
  mutate(rate = (numerator/denominator) * 1000) #calculate crude rate



#save file to be used in analyze_second() function
saveRDS(all, paste0(data_folder, "Temporary/school_exclusions_formatted.rds"))



analyze_second(filename = "school_exclusions", measure = "crude", time_agg = 1,
               ind_id = 13016, year_type = "school", crude_rate = 1000)




