# ScotPHO indicators: Single Adult Dwellings

#   Part 1 - Create basefile
#   Part 2 - Preparation of geograpy files
#   Part 3 - Call analysis macros

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Reading data in directly from website that was manually downloaded for previous program


col_names_n <- c("datazone", "name", 2006:2023)
col_names_d <- c("datazone", "name", 2001:2023)

#read data in direct from source
sad_data_extract <- bind_rows(read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Fwith-single-adult-discounts",
                                       skip=8, col_names = col_names_n) |>  mutate(type = "numerator"),
                              read_csv("https://statistics.gov.scot/slice/observations.csv?&dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhousehold-estimates&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fcount&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Findicator%28dwellings%29=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Findicator-dwellings%2Ftotal-dwellings", 
                                       skip = 8, col_names = col_names_d) |> mutate(type = "denominator")) #|> 
#janitor::clean_names()
sad_data_extract <-subset(sad_data_extract, select = -c (name, `2006`, `2001`:`2005`))
sad_data_extract_format <- sad_data_extract |> 
  mutate (datazone=gsub("http://statistics.gov.scot/id/statistical-geography/","", datazone))
#filter to only datazone level
sad_data_extract_format <- filter(sad_data_extract_format,substr(datazone,1,3)=="S01")
#pivot_longer years to one column
sad_data_extract_pivot <- sad_data_extract_format |> 
  pivot_longer(cols = c(`2007`:`2023`), names_to = "year", values_to = "count") |> 
  #pivot_wider type to two different columns for numerator and denominator
  pivot_wider(names_from = type, values_from = count) |> 
  filter(!is.na(denominator))


saveRDS(sad_data_extract_pivot, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_depr_raw.rds'))
#old_data <- readRDS(paste0(data_folder, 'Prepared Data/Single_Dwellings_depr_raw.rds'))
#### Match lookup - datazone with local authority

# dz01 Lookup file for CA 
dz01_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2001_all_simd.rds')|> 
  clean_names() |> #variables to lower case
  select(ca2019, data_zone2001) |> 
  rename(datazone2001 = data_zone2001)
# #Dealing with changes in ca, hb and hscp codes. Transforms old code versions into 2019 ones
#mutate(ca2011 = recode(ca2011, "S12000015"='S12000047', "S12000024"='S12000048',
#                  "S12000046"='S12000049', "S12000044"='S12000050'))

# \\Isdsf00d03\cl-out\lookups\Unicode\Geography\DataZone2011
#Preparing file for CA for period 2007 to 2014 (2014 only including dz <= S01006505)
sad01_data <- sad_data_extract_pivot |> filter(year<=2014)
#Merging with lookup
sad01_data <- left_join(sad01_data, dz01_lookup, by = c("datazone" = "datazone2001")) |> 
  rename(ca = ca2019) |> filter(datazone<='S01006505') |> mutate(dz = "dz01")


dz11_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2020v2.rds')|> 
  clean_names() |> #variables to lower case
  select(ca2019, datazone2011) #|> 

#Preparing file for CA for period 2014 to 2017 (2014 only including dz > S01006505)
sad11_data <- sad_data_extract_pivot |> filter(year>=2014)
#Merging with lookup
sad11_data <- left_join(sad11_data, dz11_lookup, by = c("datazone" = "datazone2011")) |> 
  rename(ca = ca2019) |> filter(datazone>'S01006505') |> mutate(dz = "dz11")

# Merge dz01 & dz11 data into single file (Basefile)
sad_data_raw <- full_join(sad01_data, sad11_data)

###############################################.
## Part 2 - Preparation of geograpy files ----
###############################################.

#### Prepare / Aggregate for specified geographies - LA, DZ11 & base IRs
# Prepare / Aggregate by la
sadla_data_raw <- sad_data_raw |>
  group_by(ca, year, dz) |> 
  summarise_at(c("numerator", "denominator"), sum, na.rm =T) |> 
  filter(dz != "dz01" | year != "2014") |> ungroup()

sadla_data_raw <- select(sadla_data_raw,-c(dz))

saveRDS(sadla_data_raw, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_LA_raw.rds'))

# Prepare / Aggregate by dz11
sad11_data_raw <- sad_data_raw |>
  group_by(datazone, year, dz) |> 
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) |> 
  filter(dz == "dz11") |> ungroup()

sad11_data_raw <- select(sad11_data_raw,-c(dz))

saveRDS(sad11_data_raw, file=paste0(data_folder, 'Prepared Data/Single_Dwellings_dz11_raw.rds'))

###############################################.
## Part 3 - Call analysis macros ----
###############################################.

#Calling first analysis function
analyze_first(filename = "Single_Dwellings_LA", geography = "council", measure = "percent",  
              yearstart = 2007, yearend = 2013, time_agg = 1)

analyze_first(filename = "Single_Dwellings_dz11", geography = "datazone11", measure = "percent", 
              yearstart = 2014, yearend = 2023, time_agg = 1)

# Merging CA, DZ11 together and save both periods together
all_data <- rbind(readRDS(paste0(data_folder, "Temporary/Single_Dwellings_LA_formatted.rds")),
                  readRDS(paste0(data_folder, "Temporary/Single_Dwellings_dz11_formatted.rds")))
saveRDS(all_data, file = paste0(data_folder, "Temporary/Single_Dwellings_all_formatted.rds"))

#Calling second analysis function
analyze_second(filename = "Single_Dwellings_all", measure = "percent", time_agg = 1, 
               ind_id = 20504, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="Single_Dwellings_depr", measure="percent", time_agg=1, 
                    yearstart= 2007, yearend=2022,   year_type = "calendar", 
                    ind_id = 20504)

##END
