# ScotPHO indicator:Exposure to secondhand smoke

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
#data from child health team
exposure_smoking <- read_csv(paste0(data_folder, "Received Data/secondhandsmoke_valid.csv")) %>%
  setNames(tolower(names(.))) %>%
  rename(datazone2011 = datazone) %>%
  mutate(year=case_when(nchar(year)==3 ~ paste0("200",substr(year,1,1)), 
                        TRUE ~ paste0("20",substr(year,1,2))))

ca_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(datazone2011, ca2019) %>% distinct()

exposure_geography <- left_join(exposure_smoking, ca_lookup, by = "datazone2011") %>% 
  rename(ca = ca2019 ) %>% group_by(ca, year) %>% 
  summarise(numerator = sum(numerator), denominator = sum(denominator)) %>% 
  ungroup() %>% 
  # Selecting out a few cases from early years in Highland CA before the system was 
  # properly in place that would cause confusion.
  filter(!(ca == "S12000017" & year<2007))

saveRDS(exposure_geography, file=paste0(data_folder, 'Prepared Data/exposure_smoking_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "exposure_smoking", geography = "council", 
              measure = "percent", yearstart = 2002, yearend = 2018, time_agg = 3)

analyze_second(filename = "exposure_smoking", measure = "percent", time_agg = 3, 
               ind_id = 13037, year_type = "financial")

##END
