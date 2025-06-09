################################################################################.
#####           Children in low income families                            #####.
################################################################################.


# This script analyses HMRC data on the Children in (relative) Low Income Families
# Data located under 'children in low income families' > 'relative low income'

# Relative low-income is defined as a family in low income Before Housing Costs (BHC) in the reference year.
# A family must have claimed Child Benefit and at least one other household benefit (Universal Credit, tax credits, or Housing Benefit)
# at any point in the year to be classed as low income in these statistics. Gross income measure is Before Housing Costs (BHC) and includes 
# contributions from earnings, state support and pensions.
# Figures are calibrated to the Households Below Average Income (HBAI) survey UK of children in low income


# Data sourced from StatXplore Tool
# https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml
# Extracting data requires registration with statxplore (ie username and password) to create custom tables and extract data



###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
library(readxl)


################################################################################.
## PART 1: Read in data downloaded from StatXplore ----                  
################################################################################.

################################################################################.
# Extract 1 : Scotland and local authority level data.
# Contains clif for local authorities and scotland split by age band - note that totals for age splits in area don't match reported total (ie 0-4 + 5-10 + 11-15 dones't always equal under 16 total)
# DWP apply some kind of cell swapping or small number disclosure control that prevent accurate summing.
# Figures are for total children (i.e. not split according to parental status e.g. lone parent)
raw_la <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Children in low income families/children_in_low_income_families_LA2025.xlsx"),sheet="all_data") %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  mutate(code=case_when(code=="S92000003" ~ "S00000001", TRUE ~ code))|> #reset scotland code to that used by scotpho
  select(-total)|> #don't need total column as it should equal the under 16 column
  pivot_longer(cols=c(under16:age11_15), names_to="age")|>
  mutate(age2 = case_when(age=="age0_4" ~"0-4", 
                          age=="age5_10"~ "5-10",
                          age=="age11_15"~ "11-15",
                          age=="under16" ~ "under16",TRUE ~ "other"))|>
  select(-age) |>
  rename(numerator=value)


################################################################################.
# Extract 2 : Intermediate Zone level data.
# Figures are for total children (i.e. not split according to parental status e.g. lone parent)
# Figures are not split by age band since this split not available for small geographies
# Note that some IZ figures are suppressed by DWP as they are too small (this can generate warnings when you read in the data where cells have "..." instead of a figure
# e.g.
# Warning messages:                                                                                                                                                               
# 1: Expecting numeric in C6247 / R6247C3: got)

raw_iz <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Children in low income families/children_in_low_income_families_IZ2025.xlsx"),sheet="iz") %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  filter(code != "S92000003") %>% # don't need scotland level again
  rename(numerator=u16) %>%
  mutate(age2="under16") # year from stat explore is FYE but scotpho year needs


# Join the local authority and iz level data
clif_data <-bind_rows(raw_la,raw_iz) |>
  mutate(split_name = case_when(age2=="under16" ~ "main",TRUE ~ "age"),
         split_value = age2)
  


## EXTRACTS 3 and 4 will form breakdowns presented in the populations group tab only.

################################################################################.
# Extracts 3  Scotland and local authority level data : LONE PARENTS
# Contains cilif for local authorities and scotland where parents are lone parents
# Only keeping the total for under16s (although age splits are available not sure how we would present in profiles tool and may not be useful)

lone_la <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Children in low income families/children_in_low_income_families_LA2025.xlsx"),sheet="loneparent") %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  mutate(code=case_when(code=="S92000003" ~ "S00000001", TRUE ~ code),#reset scotland code to that used by scotpho
         split_name="Parental Status",
         split_value="Lone Parent")|>
  select(-total, -age0_4,-age5_10, -age11_15) #only interested in total under 16 for parental split

################################################################################.
# Extracts 4  Scotland and local authority level data : Couple PARENTS
# Contains cilif for local authorities and scotland where parents are lone parents
# Only keeping the total for under16s (although age splits are available not sure how we would present in profiles tool and may not be useful)

couple_la <- read_xlsx(paste0(profiles_data_folder, "/Received Data/Children in low income families/children_in_low_income_families_LA2025.xlsx"),sheet="couple") %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  mutate(code=case_when(code=="S92000003" ~ "S00000001", TRUE ~ code),#reset scotland code to that used by scotpho
         split_name="Parental Status",
         split_value="Couple Parent")|>
  select(-total, -age0_4,-age5_10, -age11_15)  #only interested in total under 16 for parental split

# Join the parental status local authority and iz level data
parent_status_data <-bind_rows(lone_la,couple_la)|>
  mutate(age2="under16")|>
  rename(numerator=under16)
                            
rm(lone_la,couple_la) #remove files no longer required

#add parental status data to IZ and LZ dataset
clif_data <-bind_rows(clif_data,parent_status_data) |>
 mutate(trend_axis = year, #create trend axis field required in shiny app (this is the full financial year description)
       year = as.numeric(substr(year,1,4))) |> #generate year field which is single numeric year (start year of the financial year)
  rename(age=age2)


################################################################################.
## PART 2 : Generate NHS board and HSCP geographies datasets ----
#
# Construct NHS board and HSCP level data based on aggregations of LA data
# StatXplore tool does not report NHS based organisations like such as NHS board
# however best fit of these these can be constructed by aggregating constituent LA.
# Constructing these geographies will provide imperfect estimates that are affected
# by DWP statistical disclosure methodology - it may result in slight differences between
# sum of age splits and totals for all under 16.
################################################################################.

# Read in a geography lookup that allows linkage from local authority to NHS board or HSCP
geo_lookup <- readRDS(paste0(profiles_data_folder, "/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")) %>% 
  select(ca2019, hb2019, hscp2019) |>
  unique() #restrict lookup so each LA only 

# add geo lookup to indicator data
clif_data <- left_join(clif_data,geo_lookup, by = c("code"="ca2019"))

# filter out NHS board data and summarise count of children
hb_data <- clif_data |>
filter (!is.na(hb2019)) |>
  group_by(hb2019, year, age, trend_axis, split_name, split_value) |>
  summarise(numerator=sum(numerator)) |>
  ungroup() |>
  rename(code=hb2019)

# filter out HSCP data and summarise count of children
hscp_data <-clif_data |>
  filter (!is.na(hscp2019)) |>
  group_by(hscp2019, year, age, trend_axis, split_name, split_value) |>
  summarise(numerator=sum(numerator)) |>
  ungroup()|>
  rename(code=hscp2019)

# Join all the geography data files together (resultant file is long format which includes number of children by age groups for
# Scotland, LA, IZ and the new NHS board and HSCP - this file is what will be used to generate main indicator dataset
clif_data <-bind_rows(clif_data, hb_data, hscp_data) |>
  select(-hb2019,-hscp2019)

rm(hb_data, hscp_data) #remove no longer required




################################################################################.
## PART 3 : Create population lookup that can be used as denominator ----
################################################################################.  

# Need population aged 0-15 population - here we use the scotphop dz populations basefile which already contains all geography levels (ie IZ,HSCP,HB,LA,Scotland)
population <- readRDS(file=paste0(profiles_data_folder, "/Lookups/Population/basefile_DZ11.rds")) %>%
  filter(year>=2014,  #select from 2014 onwards
         age<16) |> #only want under 16s
  mutate(age2 = case_when(age<= 4 ~"0-4",age>=5 & age<=10 ~ "5-10",age>=11 & age<=15 ~ "11-15", TRUE ~ "other")) |>
  group_by(year,code,age2) %>%
  summarise(pop=sum(denominator)) %>% # remove sex column from population as indicator doesn't have sex split
  ungroup()

#add rows containing total under 16 populations by year and geography (note this step can take a few minutes to run)
population <-population |>
  group_by(year,code) %>%
  group_modify(~ .x |> adorn_totals(name = "under16")) %>% #adorn_total from janitor package adds totals row into data set but can take a while to run
  ungroup()


population <- population |>
  # create flag column that can be used as a filter 
  # we only need overall under 16 pop at IZ level (numbers too small to publish age splits at this geographic level)
  # exclude when area code is IZ and population not the total for 'under16' 
  mutate (filter= case_when(substr(code,1,3)=="S02" & age2 !="under16" ~ "exclude", TRUE ~ "keep")) |>
  filter (filter=="keep") |> # keep total under 16 pop for IZs and all age groups and total pop for scotland and LA level
  select(-filter) |>
  rename (age=age2) 


################################################################################.
## PART 4 : Join populations to CLIF data and calculate rates ----
################################################################################.  


indicator_data <- left_join(x = population, y = clif_data, by = c("year","code","age")) |>
  rename(denominator=pop)|>
  filter(substr(code,1,3) %in% c( "S00","S08","S12","S37","S02")) |> #filter so that HSCO locality and ADP excluded as these are not presented for this indicator
  calculate_percent() |> #call scotpho helper function to add percentage and confidence intervals
  create_def_period_column(year_type="financial",agg=1)



###########################################################################.
# Generate main dataset.
main_data <-indicator_data %>%
  filter(split_value=="under16") |> #main data only requires 
  select(-age,-denominator, -split_name,-split_value) |>
  mutate(ind_id=99142) |>
  arrange(year,code)


# Save
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/children_low_income_shiny.rds"))
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/children_low_income_shiny.csv"), row.names = FALSE)

run_qa(filename="children_low_income", type="main",test=FALSE)



###########################################################################.
# Generate population group dataset .
pop_grp_data <-indicator_data %>%
  filter(substr(code,1,3) %in% c( "S00","S08","S12","S37")) |> # include only higher level geographies
  select(-denominator, -age) |>
  mutate(ind_id=99142,
         split_name=case_when(split_name=="age" ~ "Age", split_name=="main" ~ "Age", TRUE ~ split_name),
         split_value=case_when(split_value=="under16" ~"Under 16", TRUE ~ split_value)) |>
  arrange(year,code)

#add a total series for the parental status split
pop_grp_total <-pop_grp_data |>
  filter(split_value=="Under 16") |>
  mutate(split_name="Parental Status",
         split_value="All")

#join that total series to pop group dataset
pop_grp_data <-bind_rows(pop_grp_data,pop_grp_total)

# Save
write_rds(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/children_low_income_shiny_popgrp.rds"))
write.csv(pop_grp_data, paste0(profiles_data_folder, "/Data to be checked/children_low_income_shiny_popgrp.csv"), row.names = FALSE)

run_qa(filename="children_low_income", type="popgrp",test_file=FALSE)

