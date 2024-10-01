# Update of ScotPHO profiles indicator:
# MVPA Guidelines (MVPA - moderate to vigorous physical activity) 

# Need to determine which categories of the MVPA guidelines should be reported in profiles tool
# There are four options avialable, meets guideline, some activity, low activity, no activity (having 4 indicators may not be necessary)

# This script currently only generates indicator "99107_meeting_mvpa"

# Source of data Scottish Health Survey (SHeS) dashboard (https://scotland.shinyapps.io/sg-scottish-health-survey/_w_85e637b9/#tab-6590-2)
# Two distinct csv files downloaded from dashboard.

# Percentage of population by various categories of physical activity split by age, sex, urban/rural, SIMD, lli (limiting longstanding illness) (annual figures, scotland only)
# Percentage of population by various categories of physical activity split by sex (4 year figures, scotland & nhs board & local authority)


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1_Setup.R") #Normal indicator functions

# Function that will manipulate data files extracted from SHeS dashboard and reshape in preparation for scotpho shiny app

# function to streamline reformatting of age/sex/lli data only 
generate_split_data <- function(input_data, 
                                filename, 
                                category,
                                split_name=c("age","sex","lli","simd"),
                                split_value_total,
                                ind_id
) {
  
  input_data %>%
    filter(categories==category) |> #select only category (e.g. meets mpva, low activity etc)
    mutate(indicator_name=filename,
           code= "S00000001", 
           geography="Scotland",
           numerator="",
           def_period=paste(year, "calendar year"),
           trend_axis=year,
           ind_id=ind_id,
           split_name=split_name,
           split_value=case_when(split_value=="All" ~ split_value_total, TRUE ~ split_value)) |>
    #rename fields to match expected names for scotpho app
    rename(rate = percent,
           lowci=lower_ci, 
           upci=upper_ci) |>
    select(ind_id,indicator_name,code,geography,year,numerator,rate,lowci,upci,def_period,trend_axis,split_name,split_value)
  
} 



##########################################################################################################################.
# Scotland, NHS board & Council 4 year rolling average data ----
# Generates indicator data for shiny app main data file & partial data for population groups tab (only split for subnation data is sex)


mvpa_areas <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","shs_activity_levels_rank_data_2012_15_2018_22_v2.csv")) |>
  clean_names()

# bring in LA dictionary and include LA codes
la_lookup <- readRDS(paste0(lookups, "Geography/CAdictionary.rds"))%>%
  mutate(geographylevel="Local Authority")
hb_lookup <- readRDS(paste0(lookups, "Geography/HBdictionary.rds"))%>%
  mutate(geographylevel="Health Board")

area_lookup <-rbind(la_lookup,hb_lookup)

rm(hb_lookup,la_lookup)

mvpa_areas <- mvpa_areas |>
  select(-indicator) |>
  mutate(location=case_when(geographylevel=="Health Board" ~ paste0("NHS ",location), #nhs board names need to match
                            location=="Edinburgh City" ~"City of Edinburgh", #city of edinburgh council spelling mismatch
                            TRUE ~ location)) |> # nhs 
  left_join(area_lookup, by = c("geographylevel","location" = "areaname"))|>
  mutate(code=case_when(geographylevel=="Scotland" ~ "S00000001", TRUE~code))

#check all areas matched to a code
freq_table <- mvpa_areas %>%
  group_by(location,code) %>%
  summarise(frequency = n()) 

# reformat fields to expected format for shiny app
mvpa_areas <- mvpa_areas |>
  mutate(
    numerator="",
    def_period=paste(year, "4 year aggregate"),
    trend_axis=year,
    year=substr(year,1,4),
    ind_id=99107) |>
  rename(rate = percent,
         lowci=lower_ci, 
         upci=upper_ci) 

#filter fields needed in shiny app
ID99107_mvpa_meets_recommendations_maindata <-mvpa_areas |>
  filter(categories=="Meets recommendations") |> #make indicator on meeting mvpa guidelines
  filter(sex=="All") |> # only need all sex for main profiles data
  select(ind_id,code,year,numerator,rate,lowci,upci,def_period,trend_axis)


# Save out files to network drive which can be fed into shiny app
write.csv(ID99107_mvpa_meets_recommendations_maindata, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny.csv"), row.names = FALSE)
write_rds(ID99107_mvpa_meets_recommendations_maindata, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny.rds"))


# create data for sub national geographies split by sex - this df will get bound to other splits generated below.
mvpa_areas_split <- mvpa_areas %>%
  filter(categories=="Meets recommendations") %>%
  filter(geographylevel != "Scotland") |> #scotland already in splits data for 4 year aggregate rate
  mutate(split_name="Sex",
         split_value=sex) |>
  select(ind_id,code,year,numerator,rate,lowci,upci,def_period,trend_axis, split_name,split_value)




##########################################################################################################################.
# Scotland annual data ----
# Creating indicator data for Scotland level splits by  age, sex, long-term condition available
# (deprivation also available however this is presented in different tab in profiles tool and requires more work)

## Age splits ----
mvpa_age <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","trend_data_age_2012_2022.csv")) |>
  clean_names() |>
  rename(split_value=age) #rename field to generic field name contains the values within split group 

mvpa_age <-mvpa_age %>%
  generate_split_data(filename="mvpa_meets_recommendation", category="Meets recommendations",
                      split_name="age", split_value_total="All ages", ind_id = 99107)

## Sex splits ----
mvpa_sex <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","trend_data_sex_2012_2022.csv")) |>
  clean_names() |>
  rename(split_value=sex) #rename field to generic field name contains the values within split group 

mvpa_sex <-mvpa_sex %>%
  generate_split_data(filename="mvpa_meets_recommendation", category="Meets recommendations",
                      split_name="sex", split_value_total="All sexes", ind_id = 99107)


## Long-term condition splits ----
## Note that there are 3 groups No long-term conditions, limiting long-term conditions and non-limiting long-term conditions - there is no all category
## these 3 groups do not appear to be mutually exclusive for some reason (sum of percentages does not equall 100)
mvpa_ltc <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","trend_data_ltc_2012_2022.csv")) |>
  clean_names() |>
  rename(split_value=lti) %>% #rename field to generic field name contains the values within split group 
  generate_split_data(filename="mvpa_meets_recommendation", category="Meets recommendations",
                      split_name="longterm_condition", split_value_total="", ind_id = 99107)

## PAUSE SIMD SPLIT AS DEPRIVATION TAB DISTINCT FROM POPULATION GROUP
## SIMD splits ----
# mvpa_simd <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","trend_data_simd_2012_2022.csv")) |>
#   clean_names() |>
#   rename(split_value=simd) #rename field to generic field name contains the values within split group 
# 
# mvpa_sex2 <-mvpa_sex %>%
#   generate_split_data(filename="mvpa_meets_recommendation", category="Meets recommendations",
#                       split_name="simd", split_value_total="All sexes", ind_id = 88007)

# Combine the scotland age,sex,long-term conditions datasets

mvpa_meets_recommendations_scotland <-rbind(mvpa_age,mvpa_sex,mvpa_ltc)

mvpa_meets_recommendations_scotland<-mvpa_meets_recommendations_scotland |>
  mutate(split_name=case_when(split_name=="age" ~ "Age",
                              split_name=="sex" ~ "Sex",
                              split_name=="longterm_condition" ~ "Longterm conditions",
                              TRUE ~ "other")) |>
  select(-indicator_name,-geography)


rm(mvpa_age,mvpa_sex,mvpa_ltc)


ID99107_mvpa_meets_recommendations_popgrp <-rbind(mvpa_meets_recommendations_scotland,mvpa_areas_split)

write.csv(ID99107_mvpa_meets_recommendations_popgrp, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny_popgrp.csv"), row.names = FALSE)
write_rds(ID99107_mvpa_meets_recommendations_popgrp, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny_popgrp.rds"))




#STILL WORKING ON SCRIPT:

## need to filter data to select only those meeting mvpa guidelines/not meeting etc
## need to create an output the deprivation tab
# finalise script to save outputs to non-test netowrk locations



