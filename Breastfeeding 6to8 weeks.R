#Incomplete script - still work in progress 

# ScotPHO indicators: Breastfeeding at 6to8 weeks

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 
## Part 3 - Editing shiny data file to exclude data for regions where data is known to be incomplete

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
breastfed <- read_csv(paste0(data_folder, "Received Data/2021_breastfeeding_dz.csv")) %>%
  setNames(tolower(names(.))) %>%
  mutate(datazone = as.factor(datazone2011))

#fin year is financial year - needs reformatting 203 = 2002/03 recode to 2002 
breastfed <- breastfed %>%
  mutate( #creates year field based on lenght of fin_year
    year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                   TRUE ~ paste0("20",substr(fin_year,1,2)))) %>%
  group_by(year, datazone) %>%
  summarise(numerator = sum(excbf_6to8wk), denominator = sum(tot_6to8wk))

saveRDS(breastfed, file=paste0(data_folder, 'Prepared Data/breastfed_raw.rds')) 

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "breastfed", geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2020, time_agg = 3 )

## Exclusions at this point for geographies where denominator <=5 for an area  
data_indicator <- readRDS(file=paste0(data_folder, "Temporary/breastfed_formatted.rds")) %>%
  subset(denominator>5)

saveRDS(data_indicator, file=paste0(data_folder, "Temporary/breastfed_formatted.rds"))

analyze_second(filename = "breastfed", measure = "percent", time_agg = 3, 
               ind_id = 21004, year_type = "financial")

#These exclusions need to be applied to exlcude areas with incomplete data submissions.
#If NHS boards & council have incomplete data then all sub geographies should also be excluded.

#   *Excluding years/areas where the data is incomplete during 3 year average (year before through to year after).
#   *Western Isles (S08000028, S12000013) data available from 2006/07
#   *Highland (S08000022, S12000017) data available from 2008/09. Pre May 2008 data is only for Argyll & Bute LA from 2001/02
#   *Shetland (S08000026, S12000027) data available from 2008/09
#   *Grampian (S08000020, S12000020, S12000033, S12000034) & Orkney (S08000025, S12000023) data available  from 2010/11.

# Excluding data for boards, hscps, las, localities and izs with incomplete data
# Merging final data with parent geographies lookup and then filtering
geo_parents <- readRDS(paste0(lookups, "Geography/IZtoPartnership_parent_lookup.rds")) %>% 
  #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(hscp_partnership = recode(hscp_partnership, "S37000014"='S37000032', 
                                   "S37000023"='S37000033')) %>% 
  gather(geotype, code, c(intzone2011, hscp_locality)) %>% distinct() %>% 
  select(-geotype) %>% rename(parent_area = hscp_partnership)

data_shiny <- left_join(readRDS(file = paste0(data_folder, "Data to be checked/breastfed_shiny.rds")),                         
                        geo_parents, by = "code") %>%
  mutate(parent_area=coalesce(as.character(parent_area),"x")) #assign value to an 'NA' values to allow filtering to work correctly.

data_shiny <- data_shiny %>%
  filter(!((code %in% c('S08000028','S12000013', 'S37000031') | parent_area == 'S37000031') &
             year %in% c(2005, 2006))) %>% #filter for Western Isle and associated sub geographies
  filter(!((code %in% c('S08000022','S12000017', 'S37000016') | parent_area == 'S37000016') &
             year <= 2008)) %>%  # filter for Highland and associated sub geographies
  filter(!((code %in% c('S08000026','S12000027', 'S37000026') | parent_area == 'S37000026') &
             year %in% c(2007, 2008))) %>% #filter for Shetland
  filter(!((code %in% c('S08000020', 'S08000025', 'S12000020', 'S12000023', 'S12000033', 
                        'S12000034', 'S37000019', 'S37000022', 'S37000001', 'S37000002') |
              parent_area %in% c('S37000019', 'S37000022', 'S37000001', 'S37000002')) & 
             year %in% c(2009, 2010))) %>% #filter for Grampian and Orkney + associated geographies.
  select(-parent_area)

# Resave both rds and csv files with exclusions
saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/breastfed_shiny.rds"))
write_csv(data_shiny, path = paste0(data_folder, "Data to be checked/breastfed_shiny.csv"))

#qa_function(filename = "breastfed", iz=TRUE)

##END



