#Incomplete script - still work in progress 

# ScotPHO indicators: Breastfeeding at 6to8 weeks

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 
## Part 3 - Editing shiny data file to exclude data for regions where data is known to be incomplete

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
# Varies filepaths depending on if using server or not.
if (sessionInfo()$platform == "x86_64-redhat-linux-gnu (64-bit)") {
  cl_out_depr <- "/conf/linkage/output/lookups/Unicode/Deprivation/"
} else {
  cl_out_depr <- "//stats/linkage/output/lookups/Unicode/Deprivation/"
}

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


library(stringi)


###############################################.
## Part 1 - Prepare basefile ----
###############################################.
breastfed <- read_csv(paste0(data_folder, "Received Data/IR2018-01997_6to8_breastfeeding.csv")) %>%
  setNames(tolower(names(.))) %>%
  filter(!is.na(datazone2011)) %>% # exclude rows with no datazone.
  rename(datazone = datazone2011)


#fin year is financial year - needs reformatting 203 = 2002/03 recode to 2002 
  breastfed <- breastfed %>%
    mutate(length=stri_length(fin_year),
           year1=as.character(fin_year),
           year=case_when(length==3 ~ paste0("200",substr(year1,1,1)), TRUE ~ paste0("20",substr(year1,1,2)))) %>%
    group_by(year, datazone) %>%
    summarise(numerator = sum(excbf_6to8wk),
              denominator = sum(tot_6to8wk))
  
  
saveRDS(breastfed, file=paste0(data_folder, 'Prepared Data/breastfed_raw.rds')) 
  
  
  
  ###############################################.
  ## Part 2 - Run analysis functions ----
  ###############################################.
  analyze_first(filename = "breastfed", geography = "datazone11", measure = "percent", 
                yearstart = 2002, yearend = 2017, time_agg = 3 )
  
  
  
  analyze_second(filename = "breastfed", measure = "percent", time_agg = 3, 
                 ind_id = 21004, year_type = "financial", Profile = "HN", min_opt = 1423811) 
  
  
#These exclusions need to be applied somewhere in the program
#assuming that if NHS boards are excluded then all sub geographies should also be exclusded
  
#   *Excluding years/areas where the data is incomplete during 3 year average (year before through to year after).
#   *Western Isles (S08000028, S12000013) data available from 2006/07
#   *Highland (S08000022, S12000017) data available from 2008/09. Pre May 2008 data is only for Argyll & Bute LA from 2001/02
#   *Shetland (S08000026, S12000027) data available from 2008/09
#   *Grampian (S08000020, S12000020, S12000033, S12000034) & Orkney (S08000025, S12000023) data available  from 2010/11.
#   
  
  