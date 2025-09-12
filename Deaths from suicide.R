# ScotPHO indicators: 

# 30008 Adult deaths from suicide 
# 13033 Deaths from suicide in young people

# To do:
# Get crude rate calc in main_analysis to take splits by sex (as is possible for standardised rates). Then update code for the YP suicides indicator. 

# Note that this script uses the functions to generate shiny output files however due to presence of some small counts for certain areas
# and potential sensitivities around this data when looking at small numbers in small areas we apply some post-function suppression of counts
# we may also run the functions and generate the data but we then choose not to include in the live profiles tool. Ensure that you have run this script
# to the end before copying data files from checking area to folder for deployment in live shiny app.


#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions
#   Part 4 - Final processing steps, suppression and saving files


###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("functions/main_analysis.R") #Normal indicator functions
source("functions/deprivation_analysis.R") # deprivation function


###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extracting data on deaths by excluding records with unknown sex and
# with any icd10 code of suicide in any cause (includes non-Scottish residents).
# NB. Restricted to 2002 to match the population lookup as of June 2025. Could revisit in future if population lookup extended back further.
deaths_suicide <- as_tibble(dbGetQuery(channel, statement=
                                         "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
      WHERE  year_of_registration between '2002' and '2024' 
      AND sex <> 9
      AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'X[67]|X8[01234]|Y1|Y2|Y3[01234]|Y870|Y872')" )) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.


# Bringing LA and datazone info. (as not all records have datazone info attached initially)
# latest postcode directory from lookups folder
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case


# join the data sets with postcode info
deaths_suicide <- left_join(deaths_suicide, postcode_lookup, "pc7") %>% 
  # suicide indicator lowest geography is council area so dz level only needed for deprivation splits
  # this is why we can apply filter on dz this early in the script
  mutate(datazone = case_when(year < 2014 ~ datazone2001,
                              year > 2013 ~ datazone2011)) %>%
  select(year, age_grp, age, sex_grp, datazone, ca = ca2019) %>% 
  subset(!(is.na(datazone))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

#############################################################################################.
## Part 2 - Create numerator files for the council area, sex, and deprivation basefiles ----
#############################################################################################.

# Council area data ----

# Suicides in any aged 16+ 
# (this indicator is only published at local authority level even though it would be possible to generate IZ
# ca inserted into name as a reminder)

suicides_16plus <- deaths_suicide %>%
  filter(age>15) %>%
  group_by(year, age_grp, sex_grp, ca) %>%  
  summarize(numerator = n()) %>% 
  ungroup() 
saveRDS(suicides_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_ca_raw.rds'))

# FEMALE 16+
suicides_16plus_F <- suicides_16plus %>% subset(sex_grp==2) 
saveRDS(suicides_16plus_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_F_ca_raw.rds'))

# MALE 16+
suicides_16plus_M <- suicides_16plus %>% subset(sex_grp==1) 
saveRDS(suicides_16plus_M , file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_M_ca_raw.rds'))


# Suicides in young people (11 to 25 year olds)
suicides_young <- deaths_suicide %>%
  filter(age>10 & age<26) %>%
  group_by(year, sex_grp, ca) %>%  
  summarize(numerator = n()) %>% 
  ungroup() 
saveRDS(suicides_young, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_raw.rds'))

# FEMALE 11 to 25 year olds
suicides_young_F <- suicides_young %>% subset(sex_grp==2) 
saveRDS(suicides_young_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_F_raw.rds'))

# MALE 11 to 25 year olds
suicides_young_M <- suicides_young %>% subset(sex_grp==1) 
saveRDS(suicides_young_M, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_M_raw.rds'))


##############################################.
# Deprivation basefiles ----

# ALL 16+ deprivation basefile
suicides_16plus_depr <- deaths_suicide %>%
  mutate(sex_grp == "Total") %>%
  filter(age>15) %>% group_by(year, age_grp, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()

saveRDS(suicides_16plus_depr, file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_depr_raw.rds'))

# FEMALE 16+
suicides_16plus_depr_F <- deaths_suicide %>%
  filter(age>15 & sex_grp==2) %>% group_by(year, age_grp, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()

saveRDS(suicides_16plus_depr_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_F_depr_raw.rds'))

# MALE 16+
suicides_16plus_depr_M <- deaths_suicide %>%
  filter(age>15 & sex_grp==1) %>% group_by(year, age_grp, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()

saveRDS(suicides_16plus_depr_M, file=paste0(profiles_data_folder, '/Prepared Data/suicides_16plus_M_depr_raw.rds'))


# Suicides in young people (11 to 25 year olds) deprivation basefile
suicides_young_depr <- deaths_suicide %>%
  mutate(sex_grp == "Total") %>%
  filter(age>10 & age<26) %>% 
  group_by(year, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()
saveRDS(suicides_young_depr, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_depr_raw.rds'))

# FEMALE 11 to 25 year olds
suicides_young_depr_F <- deaths_suicide %>%
  filter(age>10 & age<26 & sex_grp==2) %>% group_by(year, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()
saveRDS(suicides_young_depr_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_F_depr_raw.rds'))

# MALE 11 to 25 year olds
suicides_young_depr_M <- deaths_suicide %>%
  filter(age>10 & age<26 & sex_grp==1) %>% group_by(year, sex_grp, datazone) %>%
  summarise(numerator = n()) %>% ungroup()
saveRDS(suicides_young_depr_M, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_M_depr_raw.rds'))



###############################################.
## Part 3 - Run analysis functions ----


###############################################.

# Suicides in 16+ population
# calendar years
# epops and epop_totals = 16+

# Total adult suicides

# main analysis function 
main_analysis(filename = "suicides_16plus_ca", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2024,
              time_agg = 5, epop_age = "16+", epop_total = 165800, year_type = "calendar", police_div=TRUE)


# deprivation analysis function
# can only go up to 2023 until 2024 SAPE populations are released
deprivation_analysis(filename="suicides_16plus_depr", measure="stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 165800, epop_age = "16+",
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "all")



###############################################.

# Female suicides 16+ : not a standalone indicator
# data run through main function then combined with other gender to create popgroup file later in script
# you can toggle on/off the QA report by switch false to true if you want to inspect numbers

# main analysis function 
main_analysis(filename = "suicides_16plus_F_ca", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2024,
              time_agg = 5, epop_age = "16+", epop_total = 82900, year_type = "calendar", police_div = TRUE, QA=FALSE)


# deprivation analysis function : not a standalone indicator
# generating data to allow assessment for suitability to publish
deprivation_analysis(filename = "suicides_16plus_F_depr", measure = "stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 82900, epop_age = "16+",
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "female", QA=FALSE)

###############################################.

# Male suicides 16+ : not a standalone indicator
# data run through main function then combined with other gender to create popgroup file later in script

# main analysis function not a standalone indicator
main_analysis(filename = "suicides_16plus_M_ca", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2024,
              time_agg = 5, epop_age = "16+", epop_total = 82900, year_type = "calendar",police_div = TRUE, QA=FALSE)


# deprivation analysis function : not a standalone indicator
# generating data to allow assessment for suitability to publish
deprivation_analysis(filename = "suicides_16plus_M_depr", measure = "stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 82900, epop_age = "16+",
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "male", QA=FALSE)



############################################################.
# Young person suicides (11 to 25 years) (crude rate)

# main analysis function 
main_analysis(filename = "suicides_young", ind_id = 13033, geography = "council", measure = "crude", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2024,
              time_agg = 5, crude_rate = 100000, year_type = "calendar", police_div = TRUE)
# differences compared with previous all with low CI, which is now constrained to be zero (rather than allowed to be negative)

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_depr", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "all")

###############################################.
# Young persons - Female

# # No M or F rates calculated as the crude rate calc doesn't currently split by sex
# # main analysis function
# main_analysis(filename = "suicides_young_F", ind_id = 13033, geography = "council", measure = "crude",
#               pop = "CA_pop_11to25", yearstart = 2002, yearend = 2023,
#               time_agg = 5, crude_rate = 100000, year_type = "calendar")

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_F_depr", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "female")

###############################################.
# Young persons - Male

# # main analysis function
# main_analysis(filename = "suicides_young_M", ind_id = 13033, geography = "council", measure = "crude",
#               pop = "CA_pop_11to25", yearstart = 2002, yearend = 2023,
#               time_agg = 5, crude_rate = 100000, year_type = "calendar")

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_F_depr", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "male")




###############################################.
## Part 4 - Final processing ----
## Inspecting data files to look at numbers of cases
## Generating pop group data files (if required)
## Applying addition data suppression if required - this indicator potentially produced small numbers 
## especially when considering specific age cuts by sex and by gender

###############################################.


## Suicides in 16plus ----
# Get all 16+ data, add relevant columns, suppress as required, and combine

# keep all even counts of 1-4 as NRS publish these for Scotland and LAs
main_16plus <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_ca_shiny.rds")) 

# keep all, even counts of 1-4 as NRS publish these for Scotland and LAs by sex (and all aggregated geogs: HB, PD, ADP, HSCP)
female <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_F_ca_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
male <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_M_ca_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
popgrp_16plus <- rbind(female, male)


# NRS publish SIMD counts split by sex at Scotland level but not at LA level. 
# Decision: Keep the deprivation sex split only at Scotland level, lower geogs have lots of low numerators giving unstable rates and trends
total_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_depr_ineq.rds")) %>% mutate(sex="Total")
f_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_F_depr_ineq.rds")) %>% mutate(sex="Female")%>% filter(substr(code, 1, 3) == "S00")
m_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_M_depr_ineq.rds")) %>% mutate(sex="Male")%>% filter(substr(code, 1, 3) == "S00")
dep_16plus <- rbind(total_dep, f_dep, m_dep) 


## Suicides in young people ----
# Get all young person data, add relevant columns, suppress as required, and combine

# NRS publish counts of 1-4 for Scottish data, but not for lower geogs
# ScotPHO has previously published young suicide rates for lower geogs without their counts, so do this.
# Decision: Drop the counts for lower geogs
main_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_shiny.rds")) %>%
  mutate(numerator = case_when(substr(code, 1, 3) != "S00" & numerator > 0 & numerator < 5 ~ as.numeric(NA),
                               TRUE ~ numerator))

# # N.B. No pop group tab for this profile (CYP) yet, so come back to this if/when that is added
# # NRS publish counts of 1-4 for Scottish data by sex, but not for lower geogs
# # ScotPHO has not previously published young suicide rates by sex.
# # Decision: Drop the counts for lower geogs
# female_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_F_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
# male_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_M_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
# popgrp_yp <- rbind(female_yp, male_yp) %>%
#   mutate(numerator = case_when(substr(code, 1, 3) != "S00" & numerator > 0 & numerator < 5 ~ as.numeric(NA),
#                                TRUE ~ numerator))

# NRS publish no SIMD data by age group, so there is no precedent to follow.
# ~5% of the Scotland level quintile numerators are >0 and <5
# Decision: keep only Scotland level SIMD quintile data for young person suicides, and suppress any counts <5
total_dep_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_depr_ineq.rds")) %>% mutate(sex="Total")
f_dep_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_depr_F_ineq.rds")) %>% mutate(sex="Female")
m_dep_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_depr_M_ineq.rds")) %>% mutate(sex="Male")
dep_yp <- rbind(total_dep_yp, f_dep_yp, m_dep_yp) %>% 
  filter(substr(code, 1, 3) == "S00" & quint_type == "sc_quin") %>% # drop lower geogs, and the deciles (which had smaller counts)
  mutate(numerator = case_when(numerator > 0 & numerator < 5 ~ as.numeric(NA),
                               TRUE ~ numerator))

#################################################################################.
# Save files final (ie files post suppression that are to be used in live tool
#################################################################################.

# Suicides 16+: main data
write.csv(main_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_shiny.csv"), row.names = FALSE)
write_rds(main_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_shiny.rds"))

# check final output
run_qa(filename ="suicides_16plus", type = "main", test=FALSE )

# Suicides 16+: popgroups data
write.csv(popgrp_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_shiny_popgrp.csv"), row.names = FALSE)
write_rds(popgrp_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_shiny_popgrp.rds"))

# check final output
run_qa(filename ="suicides_16plus", type = "popgrp", test=FALSE )

# Suicides 16+: inequalities data
write.csv(dep_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_ineq.csv"), row.names = FALSE)
write_rds(dep_16plus, paste0(profiles_data_folder, "/Data to be checked/suicides_16plus_ineq.rds"))

# check final output
run_qa(filename ="suicides_16plus", type = "deprivation", test=FALSE )



# Suicides: 11-25y: main data
write.csv(main_yp, paste0(profiles_data_folder, "/Data to be checked/suicides_young_shiny.csv"), row.names = FALSE)
write_rds(main_yp, paste0(profiles_data_folder, "/Data to be checked/suicides_young_shiny.rds"))
# check final output
run_qa(filename ="suicides_young", type = "main", test=FALSE )


# Suicides: 11-25y: popgroups data - we could generate CYP pop groups data but the numbers are small & less useful so up to now we do not produce it, although we 
# can look at it to check for feasibility in future.
# write.csv(popgrp_yp, paste0(profiles_data_folder, "Data to be checked/suicides_young_shiny_popgrp.csv"), row.names = FALSE)
# write_rds(popgrp_yp, paste0(profiles_data_folder, "Data to be checked/suicides_young_shiny_popgrp.rds"))

# Suicides: 11-25y: inequalities data
write.csv(dep_yp, paste0(profiles_data_folder, "/Data to be checked/suicides_young_ineq.csv"), row.names = FALSE)
write_rds(dep_yp, paste0(profiles_data_folder, "/Data to be checked/suicides_young_ineq.rds"))

# check final output
run_qa(filename ="suicides_young_ineq", type = "deprivation", test=FALSE )

##END