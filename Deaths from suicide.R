# ScotPHO indicators: 

# 30008 Adult deaths from suicide 
# 13033 Deaths from suicide in young people


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
      WHERE  year_of_registration between '2002' and '2023' 
      AND sex <> 9
      AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'X[67]|X8[01234]|Y1|Y2|Y3[01234]|Y870|Y872')" )) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.


# Bringing LA and datazone info. 
# latest postcode directory from lookups folder
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case


# join the data sets with postcode info
deaths_suicide <- left_join(deaths_suicide, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create numerator files for the council area, sex, and deprivation basefiles ----
###############################################.

# Council areas

# ALL 16+
suicides_ca_16plus <- deaths_suicide %>%
  filter(age>15) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicides_ca_16plus_raw.rds'))

# FEMALE 16+
suicides_ca_F_16plus <- suicides_ca_16plus %>% subset(sex_grp==2) 

saveRDS(suicides_ca_F_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicides_ca_F_16plus_raw.rds'))

# MALE 16+
suicides_ca_M_16plus <- suicides_ca_16plus %>% subset(sex_grp==1) 

saveRDS(suicides_ca_M_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicides_ca_M_16plus_raw.rds'))


# 11 to 25 year olds
suicides_young <- deaths_suicide %>%
  filter(age>10 & age<26) %>%
  group_by(year, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_young, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_raw.rds'))

# FEMALE 11 to 25 year olds
suicides_young_F <- suicides_young %>% subset(sex_grp==2) 

saveRDS(suicides_young_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_F_raw.rds'))

# MALE 11 to 25 year olds
suicides_young_M <- suicides_young %>% subset(sex_grp==1) 

saveRDS(suicides_young_M, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_M_raw.rds'))

###############################################.
# Deprivation basefile

# ALL 16+
# DZ 2011
suicides_dz11_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
suicides_dz01_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_16plus <- rbind(suicides_dz01_16plus, suicides_dz11_16plus %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicide_depr_16plus_raw.rds'))

# FEMALE 16+
dep_file_F_16plus <- dep_file_16plus %>% subset(sex_grp==2)

saveRDS(dep_file_F_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicide_depr_F_16plus_raw.rds'))

# MALE 16+
dep_file_M_16plus <- dep_file_16plus %>% subset(sex_grp==1)

saveRDS(dep_file_M_16plus, file=paste0(profiles_data_folder, '/Prepared Data/suicide_depr_M_16plus_raw.rds'))


# 11 to 25 year olds
# DZ 2011
suicides_dz11_11to25 <- deaths_suicide %>% 
  filter(age>10 & age<26) %>%
  group_by(year, sex_grp, datazone2011) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
suicides_dz01_11to25 <- deaths_suicide %>% 
  filter(age>10 & age<26) %>%
  group_by(year, sex_grp, datazone2001) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

suicides_young_depr <- rbind(suicides_dz01_11to25, suicides_dz11_11to25 %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(suicides_young_depr, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_depr_raw.rds'))

# FEMALE 11 to 25 year olds
suicides_young_depr_F <- suicides_young_depr %>% subset(sex_grp==2)

saveRDS(suicides_young_depr_F, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_depr_F_raw.rds'))

# MALE 11 to 25 year olds
suicides_young_depr_M <- suicides_young_depr %>% subset(sex_grp==1)

saveRDS(suicides_young_depr_M, file=paste0(profiles_data_folder, '/Prepared Data/suicides_young_depr_M_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

# 16+ population
# calendar years
# epops and epop_totals = 16+

# Total adult suicides

# main analysis function 
main_analysis(filename = "suicides_ca_16plus", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2023,
              time_agg = 5, epop_age = "16+", epop_total = 165800, year_type = "calendar")


# deprivation analysis function
deprivation_analysis(filename="suicide_depr_16plus", measure="stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 165800, epop_age = "16+",
                    yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "all")



###############################################.

# Female suicides

# main analysis function 
main_analysis(filename = "suicides_ca_F_16plus", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2023,
              time_agg = 5, epop_age = "16+", epop_total = 82900, year_type = "calendar")


# deprivation analysis function
deprivation_analysis(filename = "suicide_depr_F_16plus", measure = "stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 82900, epop_age = "16+",
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "female")

###############################################.

# Male suicides

# main analysis function 
main_analysis(filename = "suicides_ca_M_16plus", ind_id = 30008, geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2002, yearend = 2023,
              time_agg = 5, epop_age = "16+", epop_total = 82900, year_type = "calendar")


# deprivation analysis function
deprivation_analysis(filename = "suicide_depr_M_16plus", measure = "stdrate", time_agg = 5,
                     pop_age = c(16, 150), epop_total = 82900, epop_age = "16+",
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 30008, pop_sex = "male")



############################################################

# Young person suicides (crude rate)

# main analysis function 
main_analysis(filename = "suicides_young", ind_id = 13033, geography = "council", measure = "crude", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2023,
              time_agg = 5, crude_rate = 100000, year_type = "calendar")
# differences compared with previous all with low CI, which is now constrained to be zero (rather than allowed to be negative)

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_depr", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "all")

###############################################.

# Female 

# main analysis function 
main_analysis(filename = "suicides_young_F", ind_id = 13033, geography = "council", measure = "crude", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2023,
              time_agg = 5, crude_rate = 100000, year_type = "calendar")

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_depr_F", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "female")

###############################################.

# Male 

# main analysis function 
main_analysis(filename = "suicides_young_M", ind_id = 13033, geography = "council", measure = "crude", 
              pop = "CA_pop_11to25", yearstart = 2002, yearend = 2023,
              time_agg = 5, crude_rate = 100000, year_type = "calendar")

# deprivation analysis function
deprivation_analysis(filename = "suicides_young_depr_M", measure = "crude", time_agg = 5,
                     pop_age = c(11, 25), crude_rate = 100000,
                     yearstart = 2002, yearend = 2023, year_type = "calendar", ind_id = 13033, pop_sex = "male")




###############################################.
## Part 4 - Final processing ----
###############################################.

# Get all 16+ data, add relevant columns, suppress as required, and combine

# keep all even counts of 1-4 as NRS publish these for Scotland and LAs
main <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_ca_16plus_shiny.rds")) 

# keep all even counts of 1-4 as NRS publish these for Scotland and LAs by sex
female <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_ca_F_16plus_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
male <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_ca_M_16plus_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
popgrp <- rbind(female, male)

# NRS publish SIMD counts for Scotland by sex, but not LAs. 
# Decision: Drop the lower geogs as there are lots of low numerators here, giving unstable rates and trends
total_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicide_depr_16plus_ineq.rds")) %>% mutate(sex="Total")
f_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicide_depr_F_16plus_ineq.rds")) %>% mutate(sex="Female")
m_dep <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicide_depr_M_16plus_ineq.rds")) %>% mutate(sex="Male")
dep <- rbind(total_dep, f_dep, m_dep) %>% filter(substr(code, 1, 3) == "S00")


# Get all young person data, add relevant columns, suppress as required, and combine

# NRS publish counts of 1-4 for Scottish data, but not for lower geogs
# ScotPHO has previously published young suicide rates for lower geogs without their counts, so do this.
# Decision: Drop the counts for lower geogs
main_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_shiny.rds")) %>%
  mutate(numerator = case_when(substr(code, 1, 3) != "S00" & numerator > 0 & numerator < 5 ~ as.numeric(NA),
                               TRUE ~ numerator))

# NRS publish counts of 1-4 for Scottish data by sex, but not for lower geogs
# ScotPHO has not previously published young suicide rates by sex.
# Decision: Drop the counts for lower geogs
female_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_F_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
male_yp <- readRDS(paste0(profiles_data_folder, "/Data to be checked/suicides_young_M_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
popgrp_yp <- rbind(female_yp, male_yp) %>%
  mutate(numerator = case_when(substr(code, 1, 3) != "S00" & numerator > 0 & numerator < 5 ~ as.numeric(NA),
                               TRUE ~ numerator))

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

############################################################
# Save files
############################################################

# Suicides 16+
# main data
write.csv(main, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_shiny.csv"), row.names = FALSE)
write_rds(main, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_shiny.rds"))

# popgroups data
write.csv(popgrp, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_shiny_popgrp.csv"), row.names = FALSE)
write_rds(popgrp, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_shiny_popgrp.rds"))

# inequalities data
write.csv(dep, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_ineq.csv"), row.names = FALSE)
write_rds(dep, paste0(profiles_data_folder, "/Shiny Data/suicides_16plus_ineq.rds"))

# Suicides: 11-25y
# main data
write.csv(main_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_shiny.csv"), row.names = FALSE)
write_rds(main_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_shiny.rds"))

# popgroups data
write.csv(popgrp_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_shiny_popgrp.csv"), row.names = FALSE)
write_rds(popgrp_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_shiny_popgrp.rds"))

# inequalities data
write.csv(dep_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_ineq.csv"), row.names = FALSE)
write_rds(dep_yp, paste0(profiles_data_folder, "/Shiny Data/suicides_young_ineq.rds"))

##END
