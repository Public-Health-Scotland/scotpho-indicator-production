##Housing indicators

# load 
library(readr)
library(janitor)
library(tidyverse)
library(readxl)
library(scales)
library(openxlsx)
library(ggplot2)
library(phsstyles)
library(tidylog)
library(stringr)
library(purrr)

################################################################################-----------------------------------------------


# Set file paths for folders
fp_cpp <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/"

# All areas lookup
lookup <- read_rds("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/DataZone11_All_Geographies_Lookup.rds")
  

# Long lookup with all codes of interest
long_lookup <- lookup %>% 
  select(datazone2011,intzone2011, ca2019,hscp2019,hb2019,hscp_locality) 


# Set rounding function
rounding <- function(x){
  case_when(x < 1 ~ round_half_up(x, 2),
            between(x, 1, 1000) ~ round_half_up(x, 1),
            x > 1000 ~ round_half_up(x, 0))
}

# Turn off scientific notation
options(scipen = 999)

# System unmask function so files have read-write permissions
Sys.umask("006")



#################### Section 2: Data imports and cleaning ######################

## Dwelling estimates and occupants---------------------------------------------

# Path to your Excel file
file_path <- "//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/household_estimates.xlsx"

## identify sheet names 
sheets <- excel_sheets(file_path) %>%  str_subset(pattern = "^\\d+$")

# read in data for years available
dwelling_estimates <-
  map_df(sheets, ~read_excel(paste0("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/household_estimates.xlsx"),
                                            sheet =.x,
                                            skip = 3) %>%
           mutate(year = as.integer(.x))) %>%
           clean_names()


# clean data
dwelling_estimates <- dwelling_estimates %>% 
  clean_names() %>% 
  select(c(year, "area_code" = data_zone_code, council_area_code, "total_dwellings" = total_number_of_dwellings,
           occupied_dwellings, 
           occupied_dwellings_exempt_from_paying_council_tax))


#Get Scotland numbers
dwelling_scot <- dwelling_estimates %>% 
  select(-area_code, -council_area_code) %>% 
  group_by(year) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(area_code = "S92000003") %>% select(year,area_code,everything())

  #dz level
  aggregated_dz <- dwelling_estimates %>%
    group_by(year, area_code) %>%
    summarise(total_dwellings= sum(`total_dwellings`, na.rm = TRUE),
      occupied_dwellings     = sum(`occupied_dwellings`, na.rm = TRUE),
      occupied_dwellings_exempt_from_paying_council_tax   = sum(`occupied_dwellings_exempt_from_paying_council_tax`, na.rm = TRUE)) %>%
    arrange(year, area_code)
  
  #CA level
  aggregated_council <- dwelling_estimates %>%
    group_by(year, council_area_code) %>%
    summarise(total_dwellings= sum(`total_dwellings`, na.rm = TRUE),
              occupied_dwellings     = sum(`occupied_dwellings`, na.rm = TRUE),
              occupied_dwellings_exempt_from_paying_council_tax   = sum(`occupied_dwellings_exempt_from_paying_council_tax`, na.rm = TRUE)) %>%
    arrange(year, council_area_code) %>% rename(area_code=council_area_code)
  
  #IZ level
  dwelling_esti_iz <- dwelling_estimates %>% 
    inner_join(select(long_lookup, datazone2011, intzone2011),
               by = c("area_code" = "datazone2011")) %>% 
    select(-area_code, -council_area_code) %>% 
    rename("area_code" = intzone2011) %>% 
    group_by(year, area_code) %>% 
    summarise_all(sum) %>% 
    ungroup()

  #hscp level
  dwelling_esti_hscp <- dwelling_estimates %>% 
    inner_join(select(long_lookup, datazone2011, hscp2019),
               by = c("area_code" = "datazone2011")) %>% 
    select(-area_code, -council_area_code) %>% 
    rename("area_code" = hscp2019) %>% 
    group_by(year, area_code) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
  #health board level
  dwelling_esti_hb<- dwelling_estimates %>% 
    inner_join(select(long_lookup, datazone2011, hb2019),
               by = c("area_code" = "datazone2011")) %>% 
    select(-area_code, -council_area_code) %>% 
    rename("area_code" = hb2019) %>% 
    group_by(year, area_code) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
  #hscp locality level
  dwelling_esti_hscp_locality <- dwelling_estimates %>% 
    inner_join(select(long_lookup, datazone2011, hscp_locality),
               by = c("area_code" = "datazone2011")) %>% 
    select(-area_code, -council_area_code) %>% 
    rename("area_code" = hscp_locality) %>% 
    group_by(year, area_code) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
 ##final aggregation
  
  agg_final <- bind_rows(aggregated_dz, dwelling_esti_iz,aggregated_council,dwelling_esti_hscp,dwelling_esti_hb,dwelling_esti_hscp_locality)%>%
     arrange(year, area_code)
  
  
  ##add other columns
  dwelling_est_final <- agg_final |>  
    mutate(ind_id = 30001, #adding indicator code and chart labels
           trend_axis = year,
           def_period = paste0(year , " mid-year estimate"),
           lowci = NA, upci = NA, 
           rate = NA)   # blank variables are needed
  
  ## Dwelling estimates and occupants
  
  # Total number of households
  st_total_households <- dwelling_est_final  %>% 
    select(trend_axis, "numerator"= total_dwellings, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  
    
  
  # Occupied households
  st_occupied_dwellings <- dwelling_est_final %>% 
    mutate(rate = occupied_dwellings/total_dwellings*100) %>% 
    select(trend_axis, "numerator"= occupied_dwellings, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  
 
 
  # Occupied households exempt from council tax
  st_tax_exempt <- dwelling_est_final %>% 
    mutate(rate = occupied_dwellings_exempt_from_paying_council_tax/total_dwellings*100) %>% 
    select(trend_axis, "numerator"= occupied_dwellings_exempt_from_paying_council_tax, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  
  
  
## Household council tax bands--------------------------------------------------------

# identify sheet names 
sheet_names_council_tax <- excel_sheets(paste0("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/dwelling_est.xlsx")) %>%
  str_subset(pattern = "^\\d+$")

# read in data for years available 
council_tax_bands <- 
  map_df(sheet_names_council_tax, ~read_excel(paste0("//conf/LIST_analytics/West Hub/02 - Scaled Up Work/CPP Community Profiles/Data Extracts/dz_pop_scotpho_scripts/dwelling_est.xlsx"),
                                              sheet =.x,
                                              skip = 4) %>% 
  mutate(year = as.integer(.x))) %>% 
  clean_names() %>%
  select(year, "area_code" = data_zone_code, council_area_code, "total_dwellings" = total_number_of_dwellings,
         council_tax_band_a:council_tax_band_h) %>% 
  na.omit()

#Get Scotland numbers
council_tax_bands_scot <- council_tax_bands %>% 
  select(-area_code, -council_area_code) %>% 
  group_by(year) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(area_code = "S92000003") %>% select(year,area_code,everything())


#dz level tax band values
aggregated_dz_tax_bands <- council_tax_bands %>%
  group_by(year, area_code) %>%
  summarise(total_dwellings= sum(`total_dwellings`, na.rm = TRUE),
            council_tax_band_a = sum(`council_tax_band_a`, na.rm = TRUE),
            council_tax_band_b = sum(`council_tax_band_b`, na.rm = TRUE),
            council_tax_band_c = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_d = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_e = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_f = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_g = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_h = sum(`council_tax_band_c`, na.rm = TRUE))%>%
  arrange(year, area_code)


#CA level tax bands values
aggregated_council_tax_bands <- council_tax_bands %>%
  group_by(year, council_area_code) %>%
  summarise(total_dwellings= sum(`total_dwellings`, na.rm = TRUE),
            council_tax_band_a = sum(`council_tax_band_a`, na.rm = TRUE),
            council_tax_band_b = sum(`council_tax_band_b`, na.rm = TRUE),
            council_tax_band_c = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_d = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_e = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_f = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_g = sum(`council_tax_band_c`, na.rm = TRUE),
            council_tax_band_h = sum(`council_tax_band_c`, na.rm = TRUE)) %>%
  arrange(year, council_area_code) %>% rename(area_code=council_area_code)


#IZ level
tax_bands_iz <- council_tax_bands %>% 
  inner_join(select(long_lookup, datazone2011, intzone2011),
             by = c("area_code" = "datazone2011")) %>% 
  select(-area_code, -council_area_code) %>% 
  rename("area_code" = intzone2011) %>% 
  group_by(year, area_code) %>% 
  summarise_all(sum) %>% 
  ungroup()


#hscp level
tax_bands_hscp <- council_tax_bands %>% 
  inner_join(select(long_lookup, datazone2011, hscp2019),
             by = c("area_code" = "datazone2011")) %>% 
  select(-area_code, -council_area_code) %>% 
  rename("area_code" = hscp2019) %>% 
  group_by(year, area_code) %>% 
  summarise_all(sum) %>% 
  ungroup()


#health board level
tax_bands_hb<- council_tax_bands %>% 
  inner_join(select(long_lookup, datazone2011, hb2019),
             by = c("area_code" = "datazone2011")) %>% 
  select(-area_code, -council_area_code) %>% 
  rename("area_code" = hb2019) %>% 
  group_by(year, area_code) %>% 
  summarise_all(sum) %>% 
  ungroup()


#hscp locality level
tax_bands_hscp_locality <- council_tax_bands %>% 
  inner_join(select(long_lookup, datazone2011, hscp_locality),
             by = c("area_code" = "datazone2011")) %>% 
  select(-area_code, -council_area_code) %>% 
  rename("area_code" = hscp_locality) %>% 
  group_by(year, area_code) %>% 
  summarise_all(sum) %>% 
  ungroup()

##final aggregation

agg_final_tax_bands  <- bind_rows(aggregated_dz_tax_bands, tax_bands_iz,aggregated_council_tax_bands,tax_bands_hscp,tax_bands_hb,tax_bands_hscp_locality)%>%
  arrange(year, area_code)


##add other columns
tax_bands_final <- agg_final_tax_bands |>  
  mutate(ind_id = 30001, #adding indicator code and chart labels
         trend_axis = year,
         def_period = paste0(year , " mid-year estimate"),
         lowci = NA, upci = NA, 
         rate = NA)   # blank variables are needed



## Household council tax bands

# Households in council tax bands A-C
st_tax_band_ac <- tax_bands_final %>% 
  mutate(band_ac = council_tax_band_a + council_tax_band_b + council_tax_band_c,
         rate = band_ac/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= band_ac, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  


# Households in council tax bands F-H
st_tax_band_fh <- tax_bands_final %>% 
  mutate(band_fh = council_tax_band_f + council_tax_band_g + council_tax_band_h,
         rate = band_fh/total_dwellings*100) %>% 
  select(trend_axis, "numerator"= band_fh, rate, lowci,upci, ind_id, "code" = area_code, year,def_period,rate )  

##save indicator outputs to workbooks

#total number of households
# create excel workbook 

wb1 <- createWorkbook()

# add full table to first sheet
addWorksheet(wb1, sheetName = "total number of households")

# Write the filtered table to the current sheet
writeData(wb1, sheet = "total number of households", x =st_total_households)

# add full table to second sheet
addWorksheet(wb1, sheetName = "occupied households")

# Write the filtered table to the current sheet
writeData(wb1, sheet = "occupied households", x =st_occupied_dwellings)

# add full table to third sheet
addWorksheet(wb1, sheetName = "occupied exempt tax bands")

# Write the filtered table to the current sheet
writeData(wb1, sheet = "occupied exempt tax bands", x =st_tax_exempt)

# add full table to fourth sheet
addWorksheet(wb1, sheetName = "households tax bands A-C")

# Write the filtered table to the current sheet
writeData(wb1, sheet = "households tax bands A-C", x =st_tax_band_ac)

# add full table to fifth sheet
addWorksheet(wb1, sheetName = "households tax bands F-H")

# Write the filtered table to the current sheet
writeData(wb1, sheet = "households tax bands F-H", x =st_tax_band_fh)

saveWorkbook(wb1, paste0(fp_cpp,"Outputs/South Lanarkshire", "/household_indicators.xlsx"), overwrite = TRUE)


