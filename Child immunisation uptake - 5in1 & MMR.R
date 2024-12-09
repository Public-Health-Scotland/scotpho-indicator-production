# Childhood Immunisation Uptake at 24 Months 
# Script will update 2 ScotPHO indicators: 
#       Immunisation Uptake at 24 Months 5-in-1
#       Immunisation Uptake at 24 Months MMR

# Immunisation uptake data by 2011 datazone requested from PHS immunisation team on annual basis
# Complete refresh of historic data is requested for 2011 data however 2001 datazone data still required for deprivation time series data
# The 2001 datazone data is no longer refreshed when indicator is updated, the same data file is used to source dz01 uptake for period 2003 to 2013 
# Prior to 2022 immunisation team provided data in spss format however 


#   Part 1 - Prepare basefiles (DZ01 & DZ11): 5 in 1 vaccine
#   Part 2 - Prepare basefiles (DZ01 & DZ11): MMR vaccine
#   Part 3 - Call analysis macros: 5 in 1 vaccine
#   Part 4 - Call analysis macros: MMR vaccine

###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

#############################################################################.
## Part 0 - Prepare datazone 2001 immunisations data ----
############################################################################.

## DZ01 data now fixed (only requires period 2003-2013 as this is used in inequalities trends)
# Shouldn't need to rerun this preparation part 0 again 
# Script commented out as not necesary to run but many be useful in future.

# Read spss data provided by child health team, aggregate and convert to rds format.
# library(haven) #imms team previously provided data in spss format - if opening historic .zsav files required then haven package can do this.

# 5 in 1 at 24 months
# recieved_5in1_dz01 =  read_spss(paste0(data_folder,"Received Data/childhood immunisation/2003_2020_scotpho_childhoodimms_dz_dz2001.zsav")) %>%
#   setNames(tolower(names(.))) %>%
#   rename(datazone = datazone2001) %>%
#   # Remove assorted SPSS stuff
#   zap_label() %>%
#   zap_labels() %>%
#   zap_formats() %>%
#   zap_widths() %>%
#   # aggregate to get the count, removing sex
#   group_by(year, datazone) %>%
#   summarise_at(c("total24", "five24"), list(sum), na.rm =T) %>%
#   ungroup() %>%
#   rename(denominator = total24, numerator = five24) %>%
#   subset(year<=2013)
# saveRDS(recieved_5in1_dz01, paste0(data_folder, "Received Data/childhood immunisation/immunisations_2003to2013_dz01_5in1_DO_NOT_DELETE.rds"))
# 
# # MMR 24 months
# recieved_mmr_dz01 =  read_spss(paste0(data_folder,"Received Data/childhood immunisation/2003_2020_scotpho_childhoodimms_dz_dz2001.zsav")) %>%
#   setNames(tolower(names(.))) %>%
#   rename(datazone = datazone2001) %>%
#   # Remove assorted SPSS stuff
#   zap_label() %>%
#   zap_labels() %>%
#   zap_formats() %>%
#   zap_widths() %>%
#   # aggregate to get the count, removing sex
#   group_by(year, datazone) %>%
#   summarise_at(c("total24", "mmr24"), list(sum), na.rm =T) %>%
#   ungroup() %>%
#   # Rename variables into numerator and denominator
#   rename(denominator = total24, numerator = mmr24) %>%
#   subset(year<=2013)
# saveRDS(recieved_mmr_dz01, paste0(data_folder, "Received Data/childhood immunisation/immunisations_2003to2013_dz01_mmr_DO_NOT_DELETE.rds"))
############################################################################.


#############################################################################.
## Part 1 - Read in datazone 2001 and datazone 2011 immunisations data----
############################################################################.

## DATAZONE 2001 ----
## Data now fixed so just read in data that was prepared in part 0
immunisation_5in1_dz01 <-readRDS(paste0(data_folder,"Received Data/childhood immunisation/immunisations_2003to2013_dz01_5in1_DO_NOT_DELETE.rds"))
immunisation_mmr_dz01 <-readRDS(paste0(data_folder,"Received Data/childhood immunisation/immunisations_2003to2013_dz01_mmr_DO_NOT_DELETE.rds"))


## DATAZONE 2011 ----
## This data file should be refreshed with each annual indicator update with new data requested from PHS immunisations team
## phs.immunisation@phs.scot

# Read in datazone2011 immunisation uptake at 24 month provided by child health team and aggregate
immunisations_data_dz11 <- readRDS(paste0(data_folder,"Received Data/childhood immunisation/2004_2023_scotpho_childhoodimms_dz_dz2011.rds"))


###############################################.
## Part 2 - Prepare basefiles: 5 in 1 ----
###############################################.

# filter 2011dz received data for columns related to 5in1.
immunisation_5in1_dz11 <- immunisations_data_dz11 %>%
  select (datazone2011, year, total24, five24) %>%
  rename(denominator = total24, numerator = five24, datazone = datazone2011) 

# save dz11 basefile
saveRDS(immunisation_5in1_dz11, file=paste0(data_folder, 'Prepared Data/Immunisation_5in1_dz11_raw.rds'))

#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
five_dep_file <- rbind(immunisation_5in1_dz01 %>% subset(year<=2013), immunisation_5in1_dz11 %>% subset(year>=2014))

saveRDS(five_dep_file, file=paste0(data_folder, 'Prepared Data/Immunisation_5in1_depr_raw.rds'))

#tidy
rm(immunisation_5in1_dz01,immunisation_5in1_dz11)

###############################################.
## Part 3 - Prepare basefile: MMR ----
###############################################.

# filter 2011dz received data for columns related to 5in1.
immunisation_mmr_dz11 <- immunisations_data_dz11 %>%
  select (datazone2011, year, total24, mmr24) %>%
  rename(denominator = total24, numerator = mmr24, datazone = datazone2011)

# save dz11 basefile
saveRDS(immunisation_mmr_dz11, file=paste0(data_folder, 'Prepared Data/Immunisation_MMR_dz11_raw.rds'))


#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
mmr_dep_file <- rbind(immunisation_mmr_dz01 %>% subset(year<=2013), immunisation_mmr_dz11  %>% subset(year>=2014))

saveRDS(mmr_dep_file, file=paste0(data_folder, 'Prepared Data/Immunisation_MMR_depr_raw.rds'))

#tidy
rm(immunisation_mmr_dz01,immunisation_mmr_dz11)
rm(immunisations_data_dz11)

###############################################.
## Part 4 - Call analysis macros: 5 in 1 ----
###############################################.

analyze_first(filename = "Immunisation_5in1_dz11", geography = "datazone11", measure = "percent",
              yearstart = 2004, yearend = 2023, time_agg = 3)

analyze_second(filename = "Immunisation_5in1_dz11", measure = "percent", time_agg = 3,
               ind_id = 21103, year_type = "calendar")

#Deprivation analysis function
# 23/10/2023 delays to NRS publication of 2022 MYE mean we can't update inequalities part of this indicator yet
analyze_deprivation(filename="Immunisation_5in1_depr", measure="percent", time_agg=3,
                    yearstart= 2003, yearend=2023,   year_type = "calendar",
                    ind_id = 21103)

###############################################.
## Part 5 - Call analysis macros: MMR ----
###############################################.

analyze_first(filename = "Immunisation_MMR_dz11", geography = "datazone11", measure = "percent",
              yearstart = 2004, yearend = 2023, time_agg = 3)

analyze_second(filename = "Immunisation_MMR_dz11", measure = "percent", time_agg = 3,
               ind_id = 21104, year_type = "calendar")

#Deprivation analysis function
# 23/10/2023 delays to NRS publication of 2022 MYE mean we can't update inequalities part of this indicator yet
analyze_deprivation(filename="Immunisation_MMR_depr", measure="percent", time_agg=3,
                    yearstart= 2003, yearend=2023,   year_type = "calendar",
                    ind_id = 21104)

##END
