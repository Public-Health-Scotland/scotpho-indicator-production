# ScotPHO indicators: Adult deaths from suicide #

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions

# Data for all-age rates are also processed for comparison with the adult rates.

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Extracting data on deaths by excluding records with unknown sex and
# with any icd10 code of suicide in any cause (includes non-Scottish residents).
deaths_suicide <- as_tibble(dbGetQuery(channel, statement=
  "SELECT year_of_registration year, age, SEX sex_grp, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
      WHERE  year_of_registration between '2000' and '2022'
      AND sex <> 9
      AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'X[67]|X8[01234]|Y1|Y2|Y3[01234]|Y870|Y872')" )) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  create_agegroups() # Creating age groups for standardization.


# Bringing LA and datazone info. 
# 2024 postcode directory now (was 2022)
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.rds') %>% 
  setNames(tolower(names(.)))  #variables to lower case


# join the data sets with postcode info
deaths_suicide <- left_join(deaths_suicide, postcode_lookup, "pc7") %>% 
  select(year, age_grp, age, sex_grp, datazone2001, datazone2011, ca2019) %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create numerator files for the different geographies basefiles ----
###############################################.

# Council areas

# 16+
suicides_ca_16plus <- deaths_suicide %>%
  filter(age>15) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_16plus, file=paste0(data_folder, 'Prepared Data/suicides_ca_16plus_raw.rds'))

# all ages
suicides_ca_all <- deaths_suicide %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_all, file=paste0(data_folder, 'Prepared Data/suicides_ca_all_raw.rds'))

###############################################.
# Deprivation basefile

# Datazone2011

# 16+
suicides_dz11_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_16plus, file=paste0(data_folder, 'Prepared Data/suicides_dz11_16plus_raw.rds'))


# all ages
suicides_dz11_all <- deaths_suicide %>% 
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_all, file=paste0(data_folder, 'Prepared Data/suicides_dz11_all_raw.rds'))

# DZ 2001 data needed up to 2013 to enable matching to advised SIMD

# 16+
suicides_dz01_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_16plus <- rbind(suicides_dz01_16plus, suicides_dz11_16plus %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_16plus, file=paste0(data_folder, 'Prepared Data/suicide_depr_16plus_raw.rds'))

# all ages
suicides_dz01_all <- deaths_suicide %>% 
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_all <- rbind(suicides_dz01_all, suicides_dz11_all %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_all, file=paste0(data_folder, 'Prepared Data/suicide_depr_all_raw.rds'))


###############################################.
# REPEAT BY SEX
###############################################.

# FEMALE

# Council areas
suicides_ca_F_16plus <- deaths_suicide %>%
  filter(age>15) %>%
  subset(sex_grp==2) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_F_16plus, file=paste0(data_folder, 'Prepared Data/suicides_ca_F_16plus_raw.rds'))

suicides_ca_F_all <- deaths_suicide %>%
  subset(sex_grp==2) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_F_all, file=paste0(data_folder, 'Prepared Data/suicides_ca_F_all_raw.rds'))


###############################################.
# Deprivation basefile

# Datazone2011

# 16+
suicides_dz11_F_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  subset(sex_grp==2) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_F_16plus, file=paste0(data_folder, 'Prepared Data/suicides_dz11_F_16plus_raw.rds'))


# all ages
suicides_dz11_F_all <- deaths_suicide %>% 
  subset(sex_grp==2) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_F_all, file=paste0(data_folder, 'Prepared Data/suicides_dz11_F_all_raw.rds'))


# DZ 2001 data needed up to 2013 to enable matching to advised SIMD

# 16+
suicides_dz01_F_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  subset(sex_grp==2) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_F_16plus <- rbind(suicides_dz01_F_16plus, suicides_dz11_F_16plus %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_F_16plus, file=paste0(data_folder, 'Prepared Data/suicide_depr_F_16plus_raw.rds'))

# all ages
suicides_dz01_F_all <- deaths_suicide %>% 
  subset(sex_grp==2) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_F_all <- rbind(suicides_dz01_F_all, suicides_dz11_F_all %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_F_all, file=paste0(data_folder, 'Prepared Data/suicide_depr_F_all_raw.rds'))






###############################################.
# MALE

# Council areas
suicides_ca_M_16plus <- deaths_suicide %>%
  filter(age>15) %>%
  subset(sex_grp==1) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_M_16plus, file=paste0(data_folder, 'Prepared Data/suicides_ca_M_16plus_raw.rds'))


# all ages
suicides_ca_M_all <- deaths_suicide %>%
  subset(sex_grp==1) %>%
  group_by(year, age_grp, sex_grp, ca2019) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%   
  rename(ca = ca2019)

saveRDS(suicides_ca_M_all, file=paste0(data_folder, 'Prepared Data/suicides_ca_M_all_raw.rds'))


###############################################.
# Deprivation basefile

# Datazone2011

# 16+
suicides_dz11_M_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  subset(sex_grp==1) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_M_16plus, file=paste0(data_folder, 'Prepared Data/suicides_dz11_M_16plus_raw.rds'))


# all ages
suicides_dz11_M_all <- deaths_suicide %>% 
  subset(sex_grp==1) %>%
  group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>%  
  rename(datazone = datazone2011)

saveRDS(suicides_dz11_M_all, file=paste0(data_folder, 'Prepared Data/suicides_dz11_M_all_raw.rds'))


# DZ 2001 data needed up to 2013 to enable matching to advised SIMD

# 16+
suicides_dz01_M_16plus <- deaths_suicide %>% 
  filter(age>15) %>%
  subset(sex_grp==1) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_M_16plus <- rbind(suicides_dz01_M_16plus, suicides_dz11_M_16plus %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_M_16plus, file=paste0(data_folder, 'Prepared Data/suicide_depr_M_16plus_raw.rds'))

# all ages
suicides_dz01_M_all <- deaths_suicide %>% 
  subset(sex_grp==1) %>%
  group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% 
  ungroup() %>% 
  rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file_M_all <- rbind(suicides_dz01_M_all, suicides_dz11_M_all %>% subset(year>=2014)) #joining dz01 and dz11

saveRDS(dep_file_M_all, file=paste0(data_folder, 'Prepared Data/suicide_depr_M_all_raw.rds'))



###############################################.
## Part 3 - Run analysis functions ----
###############################################.

# changed depr years to calendar (were FY, don't know why)
# 16+ population
# change epops and epop_totals
# these pop files are both sexes combined. Is this right?

# 16+ suicides
analyze_first(filename = "suicides_ca_16plus", geography = "council", measure = "stdrate", 
              pop = "CA_pop_16+", yearstart = 2000, yearend = 2022,
              time_agg = 5, epop_age = "16+")
analyze_second(filename = "suicides_ca_16plus", measure = "stdrate", time_agg = 5, 
               epop_total = 165800, ind_id = 30008, year_type = "calendar")

# All suicides
analyze_first(filename = "suicides_ca_all", geography = "council", measure = "stdrate", 
              pop = "CA_pop_allages", yearstart = 2000, yearend = 2022,
              time_agg = 5, epop_age = "normal")
analyze_second(filename = "suicides_ca_all", measure = "stdrate", time_agg = 5, 
               epop_total = 200000, ind_id = 30108, year_type = "calendar")

#Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =165800, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)

# all age suicides
analyze_deprivation(filename="suicide_depr_all", measure="stdrate", time_agg=5,
                    pop = "depr_pop_allages", epop_total =200000, epop_age = "normal",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30108)


###############################################.

# Female  suicides

# 16+ pop
analyze_first(filename = "suicides_ca_F_16plus", 
              geography = "council", measure = "stdrate", pop = "CA_pop_16+", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "16+")
analyze_second(filename = "suicides_ca_F_16plus", measure = "stdrate", time_agg = 5, 
               epop_total = 82900, ind_id = 30008, year_type = "calendar")

# all ages
analyze_first(filename = "suicides_ca_F_all", 
              geography = "council", measure = "stdrate", pop = "CA_pop_allages", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "normal")
analyze_second(filename = "suicides_ca_F_all", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 30108, year_type = "calendar")
# Warning: Duplicate levels detected

# #Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_F_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =82900, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)
# all age suicides
analyze_deprivation(filename="suicide_depr_F_all", measure="stdrate", time_agg=5,
                    pop = "depr_pop_allages", epop_total =100000, epop_age = "normal",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30108)


# Male  suicides

# 16+ pop
analyze_first(filename = "suicides_ca_M_16plus", 
              geography = "council", measure = "stdrate", pop = "CA_pop_16+", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "16+")
analyze_second(filename = "suicides_ca_M_16plus", measure = "stdrate", time_agg = 5, 
               epop_total = 82900, ind_id = 30008, year_type = "calendar")

# all ages
analyze_first(filename = "suicides_ca_M_all", 
              geography = "council", measure = "stdrate", pop = "CA_pop_allages", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "normal")
analyze_second(filename = "suicides_ca_M_all", measure = "stdrate", time_agg = 5, 
               epop_total = 100000, ind_id = 30108, year_type = "calendar")
# Warning: Duplicate levels detected

# #Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_M_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =82900, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)
# all age suicides
analyze_deprivation(filename="suicide_depr_M_all", measure="stdrate", time_agg=5,
                    pop = "depr_pop_allages", epop_total =100000, epop_age = "normal",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30108)


# compare all age w 16+

all <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_all_shiny.rds"))
adults <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_16plus_shiny.rds"))

compare <- rbind(all, adults)

compare %>%
  filter(code=="S00000001") %>%
  ggplot(aes(x=year, y=rate, color=as.factor(ind_id), group=as.factor(ind_id))) +
  geom_line() +
  expand_limits(y=0)
  
# Get all 16+ data, check coverage, trim, and combine

total <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_16plus_shiny.rds")) 
female <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_F_16plus_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
male <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_M_16plus_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
total_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_16plus_ineq.rds")) %>% mutate(sex="Total")
f_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_F_16plus_ineq.rds")) %>% mutate(sex="Female")
m_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_M_16plus_ineq.rds")) %>% mutate(sex="Male")

dep <- rbind(total_dep, f_dep, m_dep) %>%
  filter(substr(code, 1, 3) %in% c("S00"))
main <- total %>%
  filter(substr(code, 1, 3) %in% c("S00", "S08", "S12"))
popgrp <- rbind(female, male) %>%
  filter(substr(code, 1, 3) %in% c("S00", "S08", "S12"))

ftable(dep$sex, dep$code, dep$quint_type, dep$year)
ftable(main$code, main$year)

main %>%
  filter(code=="S00000001") %>%
  ggplot(aes(x=year, y=rate)) +
  geom_line() +
  expand_limits(y=0)

popgrp %>%
  filter(code=="S00000001") %>%
  ggplot(aes(x=year, y=rate, colour=split_value, group=split_value)) +
  geom_line() +
  expand_limits(y=0)

dep %>%
  ggplot(aes(x=year, y=rate, color=quintile, group=quintile)) +
  geom_line() +
  expand_limits(y=0) +
  facet_wrap(~sex)


# write to the shiny aoo data file
saveRDS(main, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny.rds")) 
write_csv(main, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny.csv"))
saveRDS(dep, paste0(data_folder, "Test Shiny Data/suicides_16plus_ineq.rds")) 
write_csv(dep, paste0(data_folder, "Test Shiny Data/suicides_16plus_ineq.csv"))
saveRDS(popgrp, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny_popgrp.rds")) 
write_csv(popgrp, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny_popgrp.csv"))

##END
