# ScotPHO indicators: DEVELOPMENTAL CONVERNS AT 27-30 MONTHS

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
dev_concerns <- read_csv(paste0(data_folder, "Received Data/IR2019-00435.csv")) %>%
  setNames(tolower(names(.))) %>%
  rename(hb2019 = hb_code)

dev_concerns <- dev_concerns %>%
  mutate( #creates year field based on lenght of fin_year
    year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                   TRUE ~ paste0("20",substr(fin_year,1,2))))

ca_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(hb2019, ca2019) %>% distinct()

dev_concerns_geography <- left_join(dev_concerns, ca_lookup, by = "hb2019") %>% 
  rename(ca = ca2019 ) %>% group_by(ca, year) %>% 
  summarise(numerator = sum(no_with_concern), denominator = sum(no_reviews)) %>% 
  ungroup()

saveRDS(dev_concerns_geography, file=paste0(data_folder, 'Prepared Data/dev_concerns_raw.rds')) 

###############################################.
## Part 2 - calling analysis functions ----
###############################################.

analyze_first(filename = "dev_concerns", geography = "council", hscp = T, measure = "percent", 
              yearstart = 2013, yearend = 2019, time_agg = 3)

analyze_second(filename = "dev_concerns", measure = "percent", time_agg = 3, 
               ind_id = 13048, year_type = "financial")
