# Adults (16-64 years) in working households in relative poverty

# Indicator designed to provide insights into those living in poverty despite there being those in paid employment
## Number and proportions of working-age adults in relative poverty (after housing costs) living in households where someone in the household is in paid work
### Author: Vicky Elliott, 31 Oct 2025

## Sourced from Statistics.gov.scot
## https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpoverty-working-age-adults


## This indicator ties in with statistics published in the SG poverty report
## https://data.gov.scot/poverty/
## data tables: https://data.gov.scot/poverty/download.html (3 year averages)
## remember this indicator only considers working age populations and excludes the number of children living in these household which 
## is why you can find similar sounding statistics but with differing percentage values.

# Relative poverty: Individuals living in households whose equivalised income is below 60% of UK median income in the same year. 
#     This is a measure of whether those in the lowest income households are keeping pace with the growth of incomes in the economy as a whole.
# The income measure used is equivalised net disposable income before and after housing costs. 
# The before housing costs measure is income from all sources (including earnings, benefits, tax credits, pensions, and investments) after deductions for 
# income tax, national insurance contributions, council tax, pension contributions and maintenance payments. 
# The after housing costs measure further deducts housing costs such as rent and/or mortgage payments.
# The data source is the Department for Work and Pensions' Family Resources Survey (Households Below Average Income dataset).
# This indicator considers only working age adults (so it will exclude children and pensionable age adults).

# Coverage:
# 1994/95-1996/97 to 2021/22-2023/24 (some splits don't go back this far though).
# N.B. The pandemic severely affected data collection and as a result, data from 2020/21 was not used to produce any of the averaged estimates. 
# This means, for example, that the three-year periods 2018-21, 2019-22 and 2020-23 only contain data from two financial years each. 
# From 2011 the dataset includes both 3-year and 5-year rolling averages. 5-year aggregations used for religion breakdown.



###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
library(opendatascot) # to extract from statistics.gov


###############################################.
## Part 1 - Extract data from statistics.gov ----
###############################################.


# Optional for exploring fields available in working age poverty dataset
#ods_structure("poverty-working-age-adults") # see structure and variables of this dataset


# extract data
poverty_data <- opendatascot::ods_dataset("poverty-working-age-adults",
                                          workStatus = "someone-in-household-in-paid-work",
                                          indicatorpoverty = "relative-poverty",
                                          housingCosts="after-housing-costs")|>
  clean_names() |>
  mutate(value=as.numeric(value),
         code = case_when(ref_area=="S92000003"~"S00000001", TRUE ~ ref_area))|>
  rename("def_period" = ref_period)|>
  select(code,def_period,measure_type,value)|>
  pivot_wider(names_from="measure_type" ,values_from="value") |>
  rename(rate = ratio,
         numerator = count)|>
  mutate(trend_axis=def_period,
         year =substr(def_period,1,4),
         def_period=paste0(def_period,"; 3 year average"))|>
  filter(year>"2000/01") |>
  rename(sample='sample-size') |># confidence intervals
  mutate(ci_wald = 100 * (1.96*sqrt(((rate/100)*(1-(rate/100)))/sample)), # Wald method. 
       lowci = rate - ci_wald,
       upci = rate + ci_wald,
       ind_id =  99147) |>
  select(code, ind_id, year, numerator, rate, upci,lowci, def_period, trend_axis)

# save out main data file
write.csv(poverty_data, paste0(profiles_data_folder, "/Data to be checked/poverty_working_age_shiny.csv"), row.names = FALSE)
write_rds(poverty_data, paste0(profiles_data_folder, "/Data to be checked/poverty_working_age_shiny.rds"))

run_qa(filename="poverty_working_age" , type="main")
