# ScotPHO indicators: Adult deaths from suicide #

# Oct 2024 new adult deaths from suicide script created to produce indicators reporting suicide rates in those age 16+
# To-do : Could ammend the processing to allow generation of suicides for all (ie both male and female) at HSCP & HSCP locality (but not IZ as numbers too small)

#   Part 1 - Extract data from SMRA.
#   Part 2 - Create the different geographies basefiles
#   Part 3 - Run analysis functions


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


#Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =165800, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)



###############################################.

# Female  suicides

# 16+ pop
analyze_first(filename = "suicides_ca_F_16plus", 
              geography = "council", measure = "stdrate", pop = "CA_pop_16+", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "16+")
analyze_second(filename = "suicides_ca_F_16plus", measure = "stdrate", time_agg = 5, 
               epop_total = 82900, ind_id = 30008, year_type = "calendar")


# #Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_F_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =82900, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)


###############################################.

# Male  suicides

# 16+ pop
analyze_first(filename = "suicides_ca_M_16plus", 
              geography = "council", measure = "stdrate", pop = "CA_pop_16+", 
              yearstart = 2000, yearend = 2022, time_agg = 5, epop_age = "16+")
analyze_second(filename = "suicides_ca_M_16plus", measure = "stdrate", time_agg = 5, 
               epop_total = 82900, ind_id = 30008, year_type = "calendar")


# #Deprivation analysis function

# 16+ suicides
analyze_deprivation(filename="suicide_depr_M_16plus", measure="stdrate", time_agg=5,
                    pop = "depr_pop_16+", epop_total =82900, epop_age="16+",
                    yearstart= 2000, yearend = 2022, year_type = "calendar", ind_id = 30008)


# Get all 16+ data, add relevant columns, filter to robust measures (based on QA), and combine

total <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_16plus_shiny.rds")) 
female <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_F_16plus_shiny.rds")) %>% mutate(split_value="Female", split_name="Sex")
male <- readRDS(paste0(data_folder, "Data to be checked/suicides_ca_M_16plus_shiny.rds")) %>% mutate(split_value="Male", split_name="Sex")
total_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_16plus_ineq.rds")) %>% mutate(sex="Total")
f_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_F_16plus_ineq.rds")) %>% mutate(sex="Female")
m_dep <- readRDS(paste0(data_folder, "Data to be checked/suicide_depr_M_16plus_ineq.rds")) %>% mutate(sex="Male")


# prepare main dataset
main <- total %>%
  filter(substr(code, 1, 3) %in% c("S00", "S08", "S12")) # Scot, HB and CA data have sufficient numerators
popgrp <- rbind(female, male) %>%
  filter(substr(code, 1, 3) %in% c("S00", "S08", "S12")) # Scot, HB and CA data have sufficient numerators
dep <- rbind(total_dep, f_dep, m_dep) %>%
  filter(substr(code, 1, 3) %in% c("S00")) # only Scotland data have sufficient numerators

# frequency tables and charts to cheack data appearance
ftable(dep$sex, dep$code, dep$quint_type, dep$year) # 6 records in each cell (5 quintiles plus total)
ftable(main$code, main$year) # single records

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


# Save files

# main data
write.csv(main, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny.csv"), row.names = FALSE)
write_rds(main, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny.rds"))

# popgroups data
write.csv(popgrp, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny_popgrp.csv"), row.names = FALSE)
write_rds(popgrp, paste0(data_folder, "Test Shiny Data/suicides_16plus_shiny_popgrp.rds"))

# inequalities data
write.csv(dep, paste0(data_folder, "Test Shiny Data/suicides_16plus_ineq.csv"), row.names = FALSE)
write_rds(dep, paste0(data_folder, "Test Shiny Data/suicides_16plus_ineq.rds"))


##END
