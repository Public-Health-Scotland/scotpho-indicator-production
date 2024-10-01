# Update of ScotPHO profiles indicator:
# MVPA Guidelines (MVPA - moderate to vigorous physical activity) 

# Need to determine which categories of the MVPA guidelines should be reported in profiles tool
# There are four options avialable, meets guideline, some activity, low activity, no activity (having 4 indicators may not be necessary)

# This script currently only generates indicator "99107_meeting_mvpa"

# Source of data Scottish Health Survey (SHeS) dashboard (https://scotland.shinyapps.io/sg-scottish-health-survey/_w_85e637b9/#tab-6590-2)
# Two distinct csv files downloaded from dashboard.

# Percentage of population by various categories of physical activity split by age, sex, urban/rural, SIMD, lli (limiting longstanding illness) (annual figures, scotland only)
# Percentage of population by various categories of physical activity split by sex (4 year figures, scotland & nhs board & local authority)


#TO DO:

## need to filter data to select only those meeting mvpa guidelines/not meeting etc (ER: done I think? or have I misunderstood...?)
## need to create an output for the deprivation tab (ER: done)
# finalise script to save outputs to non-test network locations



###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R") #Normal indicator functions

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

# Main data = Scotland, LA, HB; meets MVPA; 4-yr aggregates from 2012-15 to 2018-22 (2020 not included in last 2, hence span 5 yrs)

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
                      split_name="longterm_condition", split_value_total="", ind_id = 99107) # adults meeting MVPA recommendation

# Combine the scotland age,sex,long-term conditions datasets

mvpa_meets_recommendations_scotland <-rbind(mvpa_age,mvpa_sex,mvpa_ltc)

mvpa_meets_recommendations_scotland<-mvpa_meets_recommendations_scotland |>
  mutate(split_name=case_when(split_name=="age" ~ "Age",
                              split_name=="sex" ~ "Sex",
                              split_name=="longterm_condition" ~ "Longterm conditions",
                              TRUE ~ "other")) |>
  select(-indicator_name,-geography)


ID99107_mvpa_meets_recommendations_popgrp <-rbind(mvpa_meets_recommendations_scotland,mvpa_areas_split)

write.csv(ID99107_mvpa_meets_recommendations_popgrp, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny_popgrp.csv"), row.names = FALSE)
write_rds(ID99107_mvpa_meets_recommendations_popgrp, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations_shiny_popgrp.rds"))

# Popgrp data = Scotland (single year), LA & HB (4y agg from 2012-15 to 2018-22); meets MVPA; 
# splits:
# agegp: 7 plus total
# sex: 2 plus total
# ltc: 3 (no total)



# SIMD splits ----
mvpa_simd <-read.csv(paste0(data_folder,"Received Data/Physical Activity/MVPA/","trend_data_simd_2012_2022.csv")) |>
  clean_names() |>
  rename(split_value=simd) #rename field to generic field name contains the values within split group

mvpa_simd <- mvpa_simd %>%
  generate_split_data(filename="mvpa_meets_recommendation", category="Meets recommendations",
                      split_name="simd", split_value_total="", ind_id = 99107) %>%
  mutate(split_value = gsub("-", " - ", split_value))

mvpa_simd_total <- mvpa_sex %>%
  filter(split_value == "All sexes") %>%
  mutate(split_name = "simd",
         split_value = "Total")

mvpa_simd_w_total <- rbind(mvpa_simd, mvpa_simd_total)

###############################################.
## Calculate inequalities ----
###############################################.

# get populations for the relevant years, adult pop (16+), Scotland.
# (ER previously calculated these in script simd_pop_processing_for_inequals.R)
# Currently to 2021. Datazone populations to end 2022 (needed for SIMD calculations) are due ....

simd_pops <- arrow::read_parquet("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/simd_16plus_pops_to_2021.parquet") %>%
  filter(sex=="Total") %>%
  rename(trend_axis = year_label,
         split_value = spatial.unit) %>%
  select(-c(spatial.scale, sex))


## Calculate inequalities metrics

##################################################.
##  Inequalities summary data  ----
##################################################.

inequals1 <- mvpa_simd_w_total %>% 
  group_by(year, trend_axis, def_period, ind_id) %>%
  mutate(count = n()) %>% #will be 1 if have only Scotland data
  ungroup() %>% # some might have 2, 3, 4 or 5, when should have 6: if so they should be excluded from inequals calc.
  filter(count==6) %>% 
  select(-count) %>%
  merge(y=simd_pops, 
        by=c("trend_axis", "year", "split_value")) %>% # 2022 dropped as no pops yet
  group_by(year, trend_axis, def_period, ind_id) %>%
  mutate(most_depr_value = rate[split_value=="1st - Most deprived"], # not summarising here as want to keep data for each quintile, for now
         least_depr_value = rate[split_value=="5th - Least deprived"],
         overall_value = rate[split_value=="Total"]) %>% 
  ungroup() %>%
  # Calculate ranges 
  mutate(abs_range = most_depr_value - least_depr_value,
         rel_range = most_depr_value / least_depr_value) %>%
  mutate(quintile = case_when(split_value == "1st - Most deprived" ~ "1 - most deprived",
                              split_value == "2nd" ~ "2",
                              split_value == "3rd" ~ "3",
                              split_value == "4th" ~ "4",
                              split_value == "5th - Least deprived" ~ "5 - least deprived",
                              TRUE ~ split_value
  ),
  quint_type = "sc_quin", # scotland-level quintiles
  code = "S00000001") # all data are for whole of Scotland currently

# get the Scotland-wide data
inequals2 <- inequals1 %>%
  filter(split_value=="Total")

# get the quintile-level data
inequals3 <- inequals1 %>%
  filter(split_value!="Total")

inequals4 <- inequals3 %>%
  mutate(par_rr = (rate/least_depr_value - 1) * proportion_pop) %>%
  group_by(year, trend_axis, def_period, ind_id) %>%
  summarise(
    #calculating PAR. PAR of incomplete groups to NA
    #CI calculation missing, this can help https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.2779
    #https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
    count= n(),
    par = case_when(count != 5 ~ NA_real_,
                    count == 5 ~ sum(par_rr)/(sum(par_rr) + 1) * 100)) %>%
  ungroup() %>%
  select(-count)

# Add Scotland data back in
inequals5 <- rbind(inequals2, inequals3) %>%
  merge(y=inequals4, by=c("year", "trend_axis", "def_period", "ind_id"))


###############################################.
## Slope of index of inequality (SII) ----
###############################################.
# The calculations below are those of the linear SII, you will have to amend the
# model if you wanted to calculate the Poisson SII
# This code will produce the results of the model, including confidence intervals

sii_model <- inequals1 %>%  
  filter(split_value != "Total") %>%
  group_by(year, trend_axis, ind_id) %>% 
  mutate(non_na = sum(!is.na(rate))) %>%
  filter(non_na==5) %>% # remove the data with NA (otherwise the model will break)
  arrange(split_value, by_group=T) %>%
  mutate(cumulative_pro = cumsum(proportion_pop),  # cumulative proportion population for each area
         relative_rank = case_when(split_value=="1st - Most deprived" ~ 0.5*proportion_pop,
                                   split_value!="1st - Most deprived" ~ lag(cumulative_pro) + 0.5*proportion_pop),
         sqr_proportion_pop = sqrt(proportion_pop), #square root of the proportion of the population in each SIMD
         relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
         value_sqr_proppop = sqr_proportion_pop * rate) %>% #value based on population weights
  nest() %>% #creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
         sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")), # *-1 to fix interpretation
         lowci_sii = -1 * as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[2])),
         upci_sii = -1 * as.numeric(map(model, ~confint(., parm = "relrank_sqr_proppop")[1]))) %>%
  select(ind_id, year, trend_axis, sii, lowci_sii, upci_sii)



###############################################.
## Relative index of inequality (RII) ----
###############################################.
# This is the calculation of the linear RII which is based on the SII values
# Can reduce the data to one row per group now
mvpa_depr_ineq <- inequals5 %>%  
  merge(y=sii_model, by = c("ind_id", "year", "trend_axis"), all=TRUE) %>% 
  mutate(rii = sii / overall_value,
         lowci_rii = lowci_sii / overall_value,
         upci_rii = upci_sii / overall_value,
         #Transforming RII into %. This way is interpreted as "most deprived areas are
         # xx% above the average" For example: Cancer mortality rate is around 55% higher
         # in deprived areas relative to the mean rate in the population
         rii_int = rii * 0.5 *100,
         lowci_rii_int = lowci_rii * 0.5 *100,
         upci_rii_int = upci_rii * 0.5 *100,
         numerator = NA_real_) %>%
  select(year, code, quintile, quint_type, ind_id, 
         trend_axis, def_period,
         numerator, denominator = adultpop,
         rate, lowci, upci, 
         par, abs_range,rel_range,
         sii,lowci_sii,upci_sii,
         rii,lowci_rii,upci_rii,
         rii_int,lowci_rii_int,upci_rii_int)

# save to ScotPHO
saveRDS(mvpa_depr_ineq, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations__depr_ineq.rds"))
write.csv(mvpa_depr_ineq, paste0(data_folder, "Test Shiny Data/ID99107_mvpa_meets_recommendations__depr_ineq.csv"))



