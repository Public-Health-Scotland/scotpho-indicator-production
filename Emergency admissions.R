# ScotPHO indicators: emergency admissions and multiple emergency admissions(+65). 
# Part 1 takes about ~20 minutes to run
# Ensure your session has enough memory as some files are large (estimate ~15MB)

#   Part 1 - Extract data from SMRA
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

# TODO
#how to deal with deprivation

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
library(data.table) # for faster grouping/aggregating large datasets
library(nanoparquet) # for faster reading/writing admissions basefile as contains +12 mill rows


###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# recommended SMR01 sort order
sort_var <- "link_no, admission_date, discharge_date, admission, discharge, uri"

#read in SMR01 data. Following Secondary Care Team definitions.
#Excluding people with no valid sex or age.
#Only emergency or urgent admissions. Selecting one record per admission.
# Information from first episode in stay that meets criteria
data_adm <- as_tibble(dbGetQuery(channel, statement=paste0(
  "SELECT distinct link_no, cis_marker, 
            min(AGE_IN_YEARS) OVER (PARTITION BY link_no, cis_marker) age,
            FIRST_VALUE(sex) OVER (PARTITION BY link_no, cis_marker 
                ORDER BY ", sort_var, ") sex_grp,
            FIRST_VALUE(dr_postcode) OVER (PARTITION BY link_no, cis_marker 
            ORDER BY ", sort_var, ") pc7,
            max(extract(year from discharge_date)) OVER (PARTITION BY link_no, cis_marker) year, 
            min(admission_date) OVER (PARTITION BY link_no, cis_marker) doadm
   FROM ANALYSIS.SMR01_PI 
   WHERE discharge_date between '1 January 2002' and '31 December 2024'
      AND sex not in ('9', '0')
      AND AGE_IN_YEARS is not null 
      AND (admission_type between '20' and '22' or admission_type between '30' and '39') 
   ORDER BY link_no, cis_marker"))) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Bringing geography info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2026_1.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011, intzone2011, ca2019, hb2019, hscp2019)

geo_lookup <- readRDS(file.path(profiles_data_folder, 'Lookups/Geography/DataZone11_All_Geographies_Lookup.rds')) %>% 
  select(datazone2011, hscp_locality) #as locality not present in the postcode one

## Matching with geography lookups.
data_adm <- left_join(x=data_adm, y=postcode_lookup, c("pc7")) #first with postcode 

data_adm <- left_join(x=data_adm, y=geo_lookup, c("datazone2011")) %>% 
  mutate(scotland = "S00000001") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) %>% # converting variables into factors
  create_agegroups() %>% # Creating age groups for standardization.
  arrange(year, link_no, doadm) %>% #sorting needed for part 2
  select(-pc7, -cis_marker, -age)

# save temp file 
write_parquet(data_adm, file.path(profiles_data_folder, 'Prepared Data/smr01_emergency_basefile.parquet'), compression = "zstd")


###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.

# read back in temp file created in part 1
basefile <- read_parquet(file.path(profiles_data_folder, 'Prepared Data/smr01_emergency_basefile.parquet'))
basefile$age_grp <- as.numeric(basefile$age_grp) # set age group col to numeric - required for age filtering

# set to data.table format
setDT(basefile) 

# remove datazone columns 
basefile[ , (c("datazone2001", "datazone2011")) := NULL ]

# pivot data longer (faster method than dplyr::pivot_longer())
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
basefile_long <- data.table::melt(
  basefile, 
  measure.vars = c("scotland", "hb2019", "ca2019", "hscp2019", "hscp_locality", "intzone2011"),
  variable.name = "areatype", 
  value.name = "code"
)

# aggregate data to count number of admissions per patient/code/year
# taking age and sex from 1st admission in grouping 
# (faster method than dplyr group_by() with summarise())
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
basefile_agg <- basefile_long[, .(
  # anything inside this first .( ) is same as dplyr summarise()
  admissions = .N, # .N is the data.table equivalent of of dplyr::n()
  age_grp = data.table::first(age_grp), # using data.table first() instead of dplyr first()
  sex_grp = data.table::first(sex_grp)
), 
# i.e. group_by()
by = .(link_no, year, code)
]


# number of patients w/ an emergency admission per year/code/age/sex
data_ea <- basefile_agg[, .(numerator = .N), by = .(year, code, sex_grp, age_grp)]


# number of patients w/ more than 1 emergency admission (aged 65+ only) per year/code/age/sex group 
data_ma <- basefile_agg[age_grp >=14 & admissions >=2, .(numerator = .N), by = .(year, code, sex_grp, age_grp)]


# save files ready to be used in analysis function
saveRDS(data_ea, file.path(profiles_data_folder, 'Prepared Data/ea_raw.rds'))
saveRDS(data_ma, file.path(profiles_data_folder, 'Prepared Data/ma_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

# Note geography arg is set to 'multiple' as each geo level already created in Part 2.


# Emergency admissions 

# Note on QA:
# When checking QA report it will flag that the sum of locality numerators don't match
# their parent HSCP/CA numerator (i.e. sum of localities is HIGHER). This is to be expected.
# E.g. A patient may have multiple emergency admissions within same year, but the locality 
# they reside in has changed (within the same council/hscp).
# This means they'll be counted once at the HSCP/CA level, and once for EACH locality they've resided in.


main_analysis(filename = "ea", ind_id = 20305, geography = "multiple", measure = "stdrate", 
              year_type = "calendar", yearstart = 2002, yearend = 2024, time_agg = 3,
              pop = "DZ11_pop_allages", epop_age = "normal", epop_total = 200000)


# Multiple emergency admissions >65

# Note on QA:
# When checking QA report you will see similar situation to above, where locality numerators
# don't match parent HSCP/CA numerator but in this instance the sum of localities is likely to be LOWER.
# E.g. patient has 2 emergency admissions within same year, but has lived in 2 diff localities (in same council/HSCP)
# They'd therefore be classed as a patient with multiple emergency admissions at the HSCP/CA level
# but wouldn't be counted as having multiple emergency admissions at locality level

main_analysis(filename = "ma", ind_id = 20306, geography = "multiple", measure = "stdrate", 
              year_type = "calendar", yearstart = 2002, yearend = 2024, time_agg = 3,
              pop = "DZ11_pop_65+", epop_age = "normal", epop_total = 39000)


##END
