# ScotPHO indicators: Children in low income families

# Low income families are defined using relative poverty estimates

# Relative low-income is defined as a family in low income Before Housing Costs (BHC) in the reference year. 
# A family must have claimed Child Benefit and at least one other household benefit (Universal Credit, tax credits, or Housing Benefit) 
# at any point in the year to be classed as low income in these statistics. Gross income measure is Before Housing Costs (BHC) and
# includes contributions from earnings, state support and pensions.


#########################################################################.
## Notes on data source and calculation methodology
#########################################################################.

# In August 2023 update the source of children in low income families changed from Scottish Government website to the DWP opendata tool (StatXplore) as SG website no long being updated.
# At this time we replaced the whole data series to ensure consistency in the indicator source/time periods over time.
# This may well result in some changes to historically published figures - any differences likely to be small and due to the fact historical dataset was built up from DZ aggregations whereas
# from 2023 onwards the CA data series used to derive HB and HSCP figures.

# Data Source : StatXplore 
#               https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml
# You will need to register with the site to enable you build and download the data required.
# Once you have created a table you can save it to your account and use this

# SHOULD BE ABLE TO USE API TO ACCESS THIS DATA BUT NEED HELP TO WRITE THE SCRIPT.

# DWP official publications page
# https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-2014-to-2022

#   Part 1 - Open lookup files
#   Part 2 - Read in CLIF data
#   Part 3 - Run analysis function (only the second function is required)


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("1.indicator_analysis.R")


###############################################.
## Part 1 Open look-up files  ----
###############################################.

# DWP data only includes Scotland, LA and IZ level data - scotpho can derive CLIF number for NHS board and HSCP by assuming values are same for corresponding councils (or aggregations of coucnil)
# This method makes the assumption that Council and HSCP partnership boundaries are the same and that NHS board are sum of constituent council areas
# Not as accurate as aggregating data up from individual datazones but method should be close enough.
geo_lookup <- readRDS(paste0(lookups, "Geography/DataZone11_All_Geographies_Lookup.rds")) %>%
  group_by(ca2019, hb2019, hscp2019) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  select(-count)

# DWP StatXplore does not include populations at IZ level so need to use the ScotPHO populations files to generate denominators.
pop_lookup <- readRDS(paste0(lookups, "Population/DZ11_pop_allages_SR.rds")) %>% 
  subset(year >= 2014 & age_grp <5) %>% # Reading pop file and selecting only 2014 onwards where age is 0-19 years
  group_by(year, code) %>%
  summarise(pop=sum(denominator))%>%
  ungroup()


#statxplorer provides numbers of children (<19) for IZ, LA, Scotland - but missing NHS board and HSCP - datazone level info is suppressed so can't aggregate up
# maybe we could contact and request from Stat.Xplore@dwp.gov.uk
#need a population denominator file with required geography levels


###############################################.
## Part 2 - Read in CLIF data ----
## sourced from StatXplore open data platform
## Population should be 0-19 population for Scotland
###############################################.

# This excel file has been prepared by manually copying IZ level and council level exports from StatXplore into a single Excel sheet.
clif_data<- read_excel(paste0(data_folder, "Received Data/Children in low income families/CLIF_formatted source data_082023.xlsx")) 

#restructure data to long format
clif_data <- clif_data %>%
  pivot_longer(!code,names_to = "year",values_to = "numerator") %>%
  mutate(year=as.numeric(substr(year,1,4)))
  
# Match on population with data
clif_data  <- left_join(x=clif_data, y=pop_lookup, by = c("year", "code"))

# Match on geography look-up to allow identification of parent geographies of IZ
# This step allows us to derive nhs board and hscp partnership geography data from the CA level information
# This triggered warning about many-to-many matches as there are multiple years of data for each CA
# the relationship is set to many-to-many so warning is ignored
clif_data  <- left_join(x=clif_data, y=geo_lookup, by = c("code" = "ca2019"), relationship="many-to-many")


# aggregate numerator and denominators table for NHS boards
clif_nhsboard <-clif_data %>%
  filter(!(is.na(hb2019))) %>%
  group_by(hb2019, year) %>%
  summarise(numerator=sum(numerator),
            pop=sum(pop)) %>%
  ungroup() %>%
  rename(code=hb2019)

# aggregate numerator and denominators table for hscp partnership
clif_hscp <-clif_data %>%
  filter(!(is.na(hscp2019))) %>%
  group_by(hscp2019, year) %>%
  summarise(numerator=sum(numerator),
            pop=sum(pop)) %>%
  ungroup() %>%
  rename(code=hscp2019)

#select only required fields from base file
clif_data <- clif_data %>%
  select(code, year, numerator, pop)

# combined IZ (which also contains scotland and LA), NHS board, HSCP data files
clif_allgeo <-rbind(clif_data,clif_nhsboard,clif_hscp) %>%
  rename(denominator=pop)
  

saveRDS(clif_allgeo, file=paste0(data_folder, "Temporary/children_low_income_formatted.rds"))


###############################################.
## Part 3 Run analysis functions  ----
###############################################.

# prepared data already contains numerator and denominator for all geographies so only need analyse second function.

analyze_second(filename = "children_low_income", measure = "percent", time_agg = 1,
               ind_id = 13027, year_type = "financial")




##END
