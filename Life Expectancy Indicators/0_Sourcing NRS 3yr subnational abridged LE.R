#########################################################.
## Extracting Life Expectancy Data from Statistics.Gov ----
## Sources NRS Scotland, NHS board and Local Authority (sub-national abridged)LE data which can then be combined with small area estimates created by ScotPHO

## NRS publish annually (usually in December) life expectancy figures for Scotland, NHS boards and Local Authorities.
## These are 3 year rolling averages
## Multiple series are available for Scotland life expectancy estimates, 
#  unabridge Single Year of age (SYOA) estimates are headline figures produce at scotland level, these are published to 2 decimal places
#  abridged (meaning using 5 year age bands) estimates are produced for scotland and sub-national areas but are now published to 1 decimal place only.

# When comparing LE estimates between geography types it is best pracitce to use the same methodology i.e. using abridged or unabridged
# The scotpho profiles indicators uses abridged estimates for all geography levels which means the scotpho indicator on life expectancy is
# rounded to 1 decimal place and may differ very slightly from the headline Scotland figures published by NRS.

# (Healthy life expectancy figures are a separate release published at a different point in the year)
# SIMD splits for LE are sometimes published at the same time period but these are usually based on 5 year periods to ensure enough data for LA/NHS boards 


###############################################.
## Packages/Filepaths ----
###############################################.

source("Life Expectancy Indicators/1.Functions_life_expectancy.R")
source("functions/main_analysis.R") #doesn't use the functions, but quick way of getting packages and folders

# Data queried directly from statistics.gov
# If you need to install the opendata scotland r package which communicates with the statistics.gov wesbite api
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov


###############################################.
# Explore what Life expectancy data present in statistics.gov
# https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2FLife-Expectancy
###############################################.

# Use this script to explore what time periods/fields are available within the life expectancy tables (output appears in console)
ods_structure("Life-Expectancy") 

##########################################################################.
# Extract Scotland/NHS Board and Local Authority Life expectancy data
##########################################################################.

# There is limitation on number of rows that can be extract from opendata platform in any one extraction 
# need to apply some filters to the data during extraction process

simd_select= "all" # not interested in SIMD extractions in this script
age_select = "0-years"  # note that extraction filter seems to return both '90-years' and '0-years' so need to reapply this filter
urban_rural_select <- c("all") #not including urban rural (breakdown only available at scotland level)

# date range for extraction (note if no filter applied to extraction then for some reason time period provided only starts at 2016-2018)
date_range_le <- c("2001-2003", "2002-2004", "2003-2005", "2004-2006", "2005-2007",
                   "2006-2008", "2007-2009", "2008-2010", "2009-2011", "2010-2012",
                   "2011-2013", "2012-2014", "2013-2015", "2014-2016", "2015-2017",
                   "2016-2018", "2017-2019", "2018-2020", "2019-2021", "2020-2022",
                   "2021-2023", "2022-2024") # add most recent year


# extract data for Scotland, NHS board and Local Authority separately to avoid issues with row limits
le_scot=ods_dataset("Life-Expectancy", geography= "sc", age=age_select, simdQuintiles=simd_select,refPeriod = date_range_le) #scotland data includes rural urban split which can go in pop group
le_hb=ods_dataset("Life-Expectancy", geography= "hb", age=age_select, simdQuintiles=simd_select,urbanRuralClassification=urban_rural_select,refPeriod = date_range_le)
le_la=ods_dataset("Life-Expectancy", geography= "la", age=age_select, simdQuintiles=simd_select,urbanRuralClassification=urban_rural_select,refPeriod = date_range_le)


#bind the geographies and manipulate data format
le_statsgov <-bind_rows(le_scot,le_hb,le_la)|>
clean_names() |> #converts column names to lowercase
  filter(age == age_select) |> #reapply age filter since ods_extraction not specific enough and returns 90-year and 0-year
  mutate(sex_grp = case_when(sex == "male" ~ '1',
                         sex == "female" ~ '2'),
         code=case_when(ref_area=="S92000003" ~ "S00000001", TRUE ~ref_area)) |>
  select(c("ref_period", "code", "sex_grp", "urban_rural_classification","measure_type", "value")) |>
  pivot_wider(names_from="measure_type" ,values_from="value") |>
  rename(rate = count,
         lowci = "95-lower-confidence-limit",
         upci = "95-upper-confidence-limit",
         urban = urban_rural_classification) |>
  arrange(code,sex_grp, ref_period)

rm(le_scot,le_hb,le_la)

#Save out life expectancy at birth data into network area ata folder
#note le figures are rounded to 1 decimal place but ideally we would want 2 decimal places
saveRDS(le_statsgov, file=paste0("/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS_statistics_gov 2001 to 2024.rds"))      

#write_csv(le_statsgov, file=paste0(paste0(source_network,"statistics_gov 2001 to 2024.csv")))


