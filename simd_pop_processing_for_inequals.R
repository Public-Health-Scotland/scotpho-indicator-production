# Import NRS mid year population estimates
# and process for inequals calcs

## datazone populations to end 2022 (needed for SIMD calculations) due Summer 2024.

pacman::p_load(
  tidyverse, here, arrow
)

## Get SIMD, LA and HB lookups ----
###############################################.

# SIMD quintiles for datazones
dz01_simd <- readRDS("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2001_all_simd.rds") %>%
  select(datazone2001=DataZone2001, 
         ca2019=CA2019,
         hb2019=HB2019,
         simd2004=simd2004_sc_quintile,
         simd2006=simd2006_sc_quintile,
         simd2009=simd2009v2_sc_quintile,
         simd2012=simd2012_sc_quintile) %>%
  mutate(simd2004 = 6 - simd2004, #reverse the quintiles for SIMD 2004 and 2006
         simd2006 = 6 - simd2006)
#6505 obs

dz11_simd16 <- readRDS("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2016.rds") %>%
  select(datazone2011=DataZone2011, simd2016=simd2016_sc_quintile)
dz11_simd20 <- readRDS("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2020v2.rds") %>%
  select(datazone2011, hb2019name, ca2019name, hb2019, ca2019, simd2020=simd2020v2_sc_quintile)
dz11_simd <- dz11_simd16 %>% 
  merge(y=dz11_simd20, by="datazone2011")
# 6976 obs

# make HB and CA lookups 
hb_lut <- dz11_simd20 %>%
  group_by(hb2019, hb2019name) %>%
  summarise() %>%
  ungroup()
ca_lut <- dz11_simd20 %>%
  group_by(ca2019, ca2019name) %>%
  summarise() %>%
  ungroup()



## Get population data ----
###############################################.

# Population for datazones

# 2001 datazones
pop_2001_2013 <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2001_pop_est_2001_2014.rds") %>%
  filter(Year %in% c(2001:2013)) %>% # use DZ2011 for data from 2014, so can assign SIMD2016 to these
  select(year = Year, 
         datazone2001 = DataZone2001,
         sex=SEX,
         starts_with("AGE")) %>%
  pivot_longer(cols = starts_with("AGE"), names_to="age", names_prefix="AGE", values_to = "pop" ) %>%
  mutate(age = case_when(age=="90PLUS" ~ "90",
                         TRUE ~ age),
         age = as.integer(age)) %>%
  filter(age>=16) %>%
  group_by(year, datazone2001, sex) %>%
  summarise(adultpop = sum(pop, na.rm=T)) %>% 
  ungroup()


pop_1996_2000 <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2001_pop_est_1996_2000.rds") %>%
  select(year, 
         datazone2001 = DataZone2001,
         starts_with(c("f", "m"))) %>%
  pivot_longer(cols = starts_with(c("f", "m")), names_to=c("sex", "age"), names_pattern="(.)(.*)", values_to = "pop" ) %>%
  mutate(sex=case_when(sex=="f" ~ "F",
                       sex=="m" ~ "M")) %>%
  filter(!(age %in% c("0", "1_4", "5_9", "10_12", "13_14","15"))) %>%
  group_by(year, datazone2001, sex) %>%
  summarise(adultpop = sum(pop, na.rm=T)) %>% 
  ungroup()

# add SIMD to DZ
DZ01_1996_2013_pop_simd <- rbind(pop_2001_2013, pop_1996_2000) %>%
  merge(y=dz01_simd, by="datazone2001", all.x=T) %>%
  mutate(simd = case_when(year %in% c(1996:2003) ~ as.integer(simd2004), # as per ISD GPD SIMD guidance: https://www.isdscotland.org/Products-and-Services/GPD-Support/Deprivation/_docs/PHS-Deprivation-Guidance-version-3-4.pdf
                          year %in% c(2004:2006) ~ as.integer(simd2006),
                          year %in% c(2007:2009) ~ as.integer(simd2009),
                          year %in% c(2010:2013) ~ as.integer(simd2012))) %>%
  select(datazone2001, year, sex, adultpop, simd, ca2019, hb2019)

# 2011 datazones
pop_2014_2021 <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2021.rds") %>%
  select(year, datazone2011, sex,
         starts_with("age")) %>%
  pivot_longer(cols = starts_with("age"), names_to="age", names_prefix="age", values_to = "pop" ) %>%
  mutate(age = case_when(age=="90plus" ~ "90",
                         TRUE ~ age),
         age = as.integer(age)) %>%
  filter(year>=2014) %>%
  filter(age >=16) %>%
  group_by(year, datazone2011, sex) %>%
  summarise(adultpop = sum(pop, na.rm=T)) %>% 
  ungroup() 

DZ11_2014_2021_pop_simd <- pop_2014_2021 %>%
  merge(y=dz11_simd, by="datazone2011", all.x=T) %>%
  mutate(simd = case_when(year %in% c(2014:2016) ~ as.integer(simd2016),
                          year >=2017 ~ as.integer(simd2020))) %>%
  select(datazone2011, year, sex, adultpop, simd, ca2019, hb2019)
  
# combine into full time series (still at dz level, tho can lose dz reference from here on)
area_pops_unagg <- DZ01_1996_2013_pop_simd %>%
  rename(datazone2011 = datazone2001) %>%
  rbind(DZ11_2014_2021_pop_simd) %>%
  select(-datazone2011) %>% #not needed for aggregations
  mutate(sex = case_when(sex=="M" ~ "Male",
                         sex=="F" ~ "Female"),
         simd = case_when(simd==1 ~ "1st - Most deprived",
                          simd==2 ~ "2nd",
                          simd==3 ~ "3rd",
                          simd==4 ~ "4th",
                          simd==5 ~ "5th - Least deprived")) %>%
  merge(y=ca_lut, by="ca2019", all=T) %>%
  merge(y=hb_lut, by="hb2019", all=T)

# do aggregations: 

######################################################
# total pop 16plus by SIMD (for inequals calcs)
######################################################
SIMD_pops_16plus <- area_pops_unagg %>%
  mutate(sex="Total") %>%
  rbind(area_pops_unagg) %>%
  group_by(year, simd, sex) %>%
  summarise(adultpop = sum(adultpop, na.rm=T)) %>%
  ungroup() 

SIMD_pops_16plus_2 <- SIMD_pops_16plus %>%
  group_by(year, sex) %>%
  summarise(adultpop = sum(adultpop, na.rm=T)) %>% #total pop for year/sex
  ungroup() %>%
  mutate(simd = "Total") %>%
  rbind(SIMD_total_pops_16plus) %>% # add quintile pops back in
  mutate(spatial.scale = "SIMD") %>%
  rename(spatial.unit = simd) %>%
  group_by(year, sex) %>%
  mutate(proportion_pop = adultpop/adultpop[spatial.unit=="Total"]) %>% # ratio needed for inequals calc
  ungroup()

# arrow::write_parquet(SIMD_pops_16plus_2,
#                      sink = here('data', 'derived', "SIMD_pops_16plus_2.parquet"),
#                      compression = "zstd")

SIMD_total_pops_16plus <- SIMD_pops_16plus_2 %>%
  mutate(year_label = as.character(year)) %>%
  select(-proportion_pop) %>%
  arrange(spatial.unit, sex, year)

# Calculate rolling average populations and year_labels to match the different year_label formats in the data:
# year_label formats included in the data: 
# single year: 1997 to 2022            
# FY: 2002/03 to 2021/22
# 2 year: 1999-2000 to 2009-2010
# 2 FY: 2016/18  and 2018/20 
# 3 FY: 1994/95-1996/97 to 2019/20-2021/22 
# 3 year: 2010-2012 to 2018-2020 
# 4 year: 2008-2011 to 2016-2019       
# 5 year: 2000-2004 to  2017-2021 


# FY: (format: 2002/03) (use pop from 1st year as it's the biggest portion of the FY)
simd_pops_FY <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(year_label = paste0(year, "/", substr(year+1, 3, 4))) %>%
  ungroup()

# 2y: (format 1999-2000)      
simd_pops_2y <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 2, fill=NA),
         year_label = paste0(year, "-", as.character(year+1)), #original numeric year is year 1 of the 2y rolling mean
         # year = (year + year + 1)/2 # would have added 0.5 to the numeric year: don't want this now
         year = year+1  #to reflect ScotPHO year labelling (midpoint rounded UP)
  ) %>%
  ungroup()

# 2 FY (format = 2016/18)  (counts should be the same as 2y version, just with different year_label)            
simd_pops_2FY <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 2, fill=NA),
         year_label = paste0(year, "/", as.character(substr(year+2, 3, 4))), #original numeric year is year 1 of the 2y rolling mean
         year = year+1  #to reflect ScotPHO year labelling (midpoint rounded UP)
  ) %>% 
  ungroup()

# 3y (format = 2010-2012)             
simd_pops_3y <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 3, fill=NA),
         year_label = paste0(year-1, "-", as.character(year+1))) %>% #original numeric year is the centre of the 3y rolling mean
  ungroup()


# 3 FY range (format = 1994/95-1996/97)   (counts should be the same as 3y version, just with different year_label)          
simd_pops_3FY <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 3, fill=NA),
         year_label = paste0(year-1, "/", as.character(substr(year, 3, 4)), "-",
                             year+1, "/", as.character(substr(year+2, 3, 4)))) %>% # numeric year remains unchanged as is the centre
  ungroup()

# 4y: (format = 2008-2011)   
simd_pops_4y <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 4, fill=NA),
         year_label = paste0(year-1, "-", as.character(year+2)), #original numeric year is year 2 of the 4y rolling mean
         #   year = (year-1 + year+2)/2 # would have added 1.5 to the numeric year: don't want this now
         # year = year+1 # keep it as integer (yr 2 of the range)
         year = year+2  #to reflect ScotPHO year labelling (midpoint rounded UP)
  ) %>%
  ungroup()

# 5y: (format = 2000-2004)
simd_pops_5y <- SIMD_total_pops_16plus %>%
  group_by(sex, spatial.unit) %>%
  mutate(adultpop = rollmean(adultpop, 5, fill=NA),
         year_label = paste0(year-2, "-", as.character(year+2))) %>% #original numeric year is the centre of the 5y rolling mean
  ungroup()

# Combine all and calculate population proportions
simd_16plus_pops <- rbind(SIMD_total_pops_16plus,
                   simd_pops_FY,
                   simd_pops_2y,
                   simd_pops_2FY,
                   simd_pops_3y,
                   simd_pops_3FY,
                   simd_pops_4y,
                   simd_pops_5y) %>%
  filter(!is.na(adultpop)) %>%
  group_by(year_label, sex) %>%
  mutate(proportion_pop = adultpop/adultpop[spatial.unit=="Total"]) %>% # proportion of the population in each SIMD out of the total population.
  ungroup()


# Save the populations for the inequals calcs
arrow::write_parquet(simd_16plus_pops, "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/simd_16plus_pops_to_2021.parquet")


