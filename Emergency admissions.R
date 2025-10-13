# ScotPHO indicators: emergency admissions and multiple emergency admissions(+65). 
# Parts 1 and 2 take about ~20 minutes to run

#   Part 1 - Extract data from SMRA
#   Part 2 - Prepare the data
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R")
source("./functions/deprivation_analysis.R")

library(data.table) #for summarising large datasets

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

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
   WHERE discharge_date between '1 January 2002' and '31 December 2023'
      AND sex not in ('9', '0')
      AND AGE_IN_YEARS is not null 
      AND (admission_type between '20' and '22' or admission_type between '30' and '39') 
   ORDER BY link_no, cis_marker"))) |> 
  janitor::clean_names()  #variables to lower case

###############################################.
## Part 2 - Prepare the data ----
###############################################.

# Bringing geography info
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_2.rds') |> 
  clean_names() |>    #variables to lower case
  select(pc7, datazone2011)

## Matching with geography lookups
data_joined <- left_join(data_adm, postcode_lookup, c("pc7")) |>  #first with postcode 
  filter(!(is.na(datazone2011))) |> #select out non-scottish
  create_agegroups() |> # Creating age groups for standardization.
  arrange(year, link_no, doadm) |>  #sorting needed for part 2
  select(-pc7, -cis_marker, -age)
  
# Summarise the data using data.table package as dataset very large (~11 million rows)
dt <- as.data.table(data_joined) #convert to data table type object

#Aggregates episodes within an admission, keeping the first value for sex_grp and age_grp 
dt_cleaned <- dt[, .(
  sex_grp = first(sex_grp),
  age_grp = first(age_grp),
  admissions = .N), #summing all admissions
  by = .(link_no, year, datazone2011)] #grouping variables

#Create emergency admissions indicator
dt_ea <- dt_cleaned[, .N, by = .(year, datazone2011, sex_grp, age_grp)]  #aggregate on number of admissions per year split by sex, age and datazone
setnames(dt_ea, "N", "numerator") #data.table syntax for renaming "N" to numerator

#Create multiple emergency admissions indicator where age = 65+
dt_mea <- dt_cleaned[age_grp >= 14 & admissions >= 2, .N, by = .(year, datazone2011, sex_grp, age_grp)] 
setnames(dt_mea, "N", "numerator") #data.table syntax for renaming "N" to numerator

#Save files to Prepared Data folder
saveRDS(dt_ea, file.path(profiles_data_folder, "/Prepared Data/emergency_admissions_raw.rds"))
saveRDS(dt_mea, file.path(profiles_data_folder, "/Prepared Data/multiple_emergency_admissions_raw.rds"))


###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#Emergency Admissions
main_analysis(filename = "emergency_admissions", measure = "stdrate", 
              geography = "datazone11", year_type = "calendar", ind_id = 20305,
              time_agg = 3, yearstart = 2002, yearend = 2023, epop_total = 200000, 
              epop_age = "normal", pop = "DZ11_pop_allages")

deprivation_analysis(filename = "emergency_admissions", yearstart = 2014, yearend = 2023,
                     time_agg = 3, year_type = "calendar", measure = "stdrate", pop_sex = "all",
                     epop_age = "normal", epop_total = 200000, pop_age = "normal",
                     ind_id = 20305)


#Multiple Emergency Admissions in 65+
main_analysis(filename = "multiple_emergency_admissions", measure = "stdrate", 
              geography = "datazone11", year_type = "calendar", ind_id = 20306,
              time_agg = 3, yearstart = 2002, yearend = 2023, epop_total = 39000, 
              epop_age = "normal", pop = "DZ11_pop_65+")

deprivation_analysis(filename = "emergency_admissions", yearstart = 2014, yearend = 2023,
                     time_agg = 3, year_type = "calendar", measure = "stdrate", pop_sex = "all",
                     epop_age = "normal", epop_total = 39000, pop_age = "normal",
                     ind_id = 20305)

################################################################################
##END

#To do
#Figure out how to add 65+ deprivation age group to deprivation analysis function
#Figure out why numerators are not matching published data, especially over 65s.
#Probably the multiple aspect not being handled properly
#Create 65+ deprivation basefile
#Figure out if there's another script floating about somewhere