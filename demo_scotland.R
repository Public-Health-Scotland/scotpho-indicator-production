#Scotland 

# what is the size of the population for each of the within scotland SIMD deciles - 
# tell me how many datazones, what is the size/proportion of the population in each of the deprivation deciles

# taken from population deprive script but edited to generate deciles not quintiles


# ~~~~~~~~~~~~~~~~~~~~~~~~
# Dependencies ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
source("functions/main_analysis.R")
library(phslookups) # for get_simd_datazone() function to read in SIMD versions from cl-out

# uncomment and run below to install phslookups package:
# remotes::install_github("Public-Health-Scotland/phslookups")



# ~~~~~~~~~~~~~~~~~~~~~~~
# SIMD versions ----
# ~~~~~~~~~~~~~~~~~~~~~~~

# Details about each version of SIMD - matches Table 4 of PHS deprivation guidance:
# https://publichealthscotland.scot/media/24056/2023-12-phs-deprivation-guidance-v35.pdf

# dz_year = whether datazones are 2001/2011 based
# pop_year = year of population used in the index
# trend_years = period each version of SIMD should be applied to

simd_info <- list(
  # SIMD 2004 pop_year should be 2001 but our pop lookups only start at 2002
  "2004" = list(dz_year = "2001", pop_year = 2002, trend_years = 2002:2003),
  "2006" = list(dz_year = "2001", pop_year = 2004, trend_years =  2004:2006),
  "2009v2" = list(dz_year = "2001", pop_year = 2007, trend_years = 2007:2009),
  "2012" = list(dz_year = "2001", pop_year = 2010, trend_years = 2010:2013),
  "2016" = list(dz_year = "2011", pop_year = 2014, trend_years = 2014:2016),
  "2020v2" = list(dz_year = "2011", pop_year = 2017, trend_years = 2017:2023)
)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population estimates data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Early versions of SIMD are based on 2001 DZs and more recent versions are based on 2011 DZs
# Therefore use pop estimates based on both 2001 and 2011 DZs for different years of this indicators time series
pop_data <- list(
  "2001" = "DZ01_pop_basefile.rds",
  "2011" = "DZ11_pop_basefile.rds"
)


# read in both pop files, aggregate to get totals per DZ and year for different age groups 
# might take a few secs as files are massive!
pop_data <- imap(pop_data, ~ {
  
  # x. refers to the filename from 'pop_data' list above
  readRDS(file.path(profiles_data_folder, "Lookups/Population/", .x)) |>
    group_by(across(contains("datazone")), year) |>
    summarise(
      all_ages = sum(denominator), # total pop
      under26 = sum(denominator[age >= 0 & age <= 25]), # 0-25 pop (i.e. 'young people')
      working_age = sum(denominator[age >= 16 & age <= 64]), # 16-64 pop (i.e. 'working age')
      .groups = "drop"
    ) |>
    rename(datazone = 1)
})


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get HSCP localities -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# localities are not an  official geography with set of standard codes 
# so are not part of any of the SIMD lookups - have to add them on as a column 
localities <- readRDS(file.path(profiles_data_folder, "Lookups", "Geography", "DataZone11_HSCLocality_Lookup.rds")) |>
  select(datazone = datazone2011, hscp_locality)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in SIMD lookups  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Read in each SIMD version and save into a named list of dataframes
# For each version:
simd_data <- imap(simd_info, ~ {
  
  
  ## get SIMD lookup from cl-out
  # '.y' refers to the SIMD version e.g. "2020v2"
  phslookups::get_simd_datazone(simd_version = .y) |>
    select(
      # select geography cols
      datazone = 1,
      any_of(c(
        "intzone2011" = matches("^intzone2011$", ignore.case = TRUE),
        "ca2019" = matches("^CA2019$", ignore.case = TRUE),
        "hscp2019" = matches("^HSCP2019$", ignore.case = TRUE),
        "hb2019" = matches("^HB2019$", ignore.case = TRUE)
      )),
      # select overall SIMD rank col and individual domain rank cols
      any_of(c(
        "overall" = matches(paste0("^simd", .y, "_?rank$")),
        "access" = ends_with("_access_rank"),
        "income" = ends_with("_inc_rank"),
        "crime" = ends_with("_crime_rank"),
        "employment" = ends_with("_emp_rank"), 
        "education" = ends_with("educ_rank"), # not currently being used
        "housing" = ends_with("house_rank"), # not currently being used
        "health" = ends_with("hlth_rank")
      ))
    ) |>
    # add localities column 
    left_join(localities, by = "datazone") |>
    # add scotland column 
    mutate(scotland = "S00000001") |>
    # move columns so all geo cols next to eachother
    relocate(c("hscp_locality", "scotland"), .after = "hb2019")
  
})




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate population weighted quintiles: -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We calculate population weighted quintiles separately for each SIMD version, domain and age group.
# For example, in SIMD2020v2 the 'access' domain will have quintiles calculated 3 different ways:
# 20% of the total population are each in each quintile
# 20% of 0-64 population are in each quintile
# 20% of 0-25 are in each quintile

## Step 1 ----
# For each SIMD version, get pop estimates for the year that SIMD is based on 
# e.g. for SIMD2020v2: 
# a. take pop estimates based on 2011 datazones (.x$dz_year == 2011)
# b. filter on 2017 (.x$pop_year == 2017)
simd_index_pop <- imap(simd_info, ~ {
  
  pop_data[[.x$dz_year]] |>
    filter(year == .x$pop_year) |>
    rename(index_year = year)
})




## Step 2 ----
# For each SIMD, join with pop index year data
# e.g. for SIMD 2020v2
# join simd data (.x == "2020v2") with the index pop for that simd version (.y == "2020v2")
# so that the 3 population columns are added on to the SIMD 
simd_data <- imap(simd_data, ~ {
  left_join(.x, simd_index_pop[[.y]], by = "datazone")
})


## Step 3 ----
# For each SIMD pop index data, divide the total population into 5 equal intervals for 3 different age groups:
# These are used as break points when calculating quintiles based on different age groups

# e.g. for SIMD2020v2:
# create intervals between 0 and 5,424,800 (total population in 2017)
# create intervals between 0 and 3,494,791 (total 16-64 population in 2017)
# create intervals between 0 and 1,589,458 (total 0-25 population in 2017)
decile_breaks <- map(simd_data, ~ {
  list(
    "all_ages" = seq(from = 0, to = sum(.x$all_ages), length.out = 11),
    "working_age" = seq(from = 0, to = sum(.x$working_age), length.out = 11),
    "under26" = seq(from = 0, to = sum(.x$under26), length.out = 11)
  )
})



## Step 4 ----
# For each SIMD version, calculate quintiles for each domain using breaks created above for different age groups

# e.g for SIMD2020v2, go through each of the 5 domain rank cols one at a time and:
# order data from most (rank number 1) to least deprived for that particular domain
# calculate quintiles based on total population:
# calculate quintiles based on 0-25 population:
# calculate quintiles based on 16-64 population:
simd_deciles <- imap(simd_data, ~ {
  
  # take each domain column in turn and:
  reduce(
    c("overall", "access", "income", "crime", "employment","health","education","housing"
    ),
    function(df, domain) {
      
      # check if domain exists first (e.g. SIMD2004 doesn't have crime domain)
      # if the column exists then:
      if (domain %in% names(df)) {
        df |>
          
          # a. order domain col from most-least deprived 
          arrange(!!sym(domain)) |> 
          
          # b. calculate that domains quintiles based on all ages 
          mutate("{domain}_centile_all_ages" := cut(cumsum(all_ages),
                                                  breaks = decile_breaks[[.y]]$all_ages,
                                                  labels = 1:10,
                                                  include.lowest = TRUE)) |>
          
          # calculate that domains quintiles based on working age population (16-64)
          mutate("{domain}_centile_working_age" := cut(cumsum(working_age),
                                                     breaks = decile_breaks[[.y]]$working_age,
                                                     labels = 1:10,
                                                     include.lowest = TRUE)) |>
          
          # calculate that domains quintiles based on young population (0-25)
          arrange(!!sym(domain)) |>
          mutate("{domain}_centile_under26" := cut(cumsum(under26),
                                                 breaks = decile_breaks[[.y]]$under26,
                                                 labels = 1:10,
                                                 include.lowest = TRUE)) |>
          # remove domain rank col
          select(-!!sym(domain))
      } else {
        df 
      }
      
    },
    .init = .x
  ) |>
    select(-c(under26, all_ages, working_age))
  
})


## Step 5 ----
# apply correct versions of SIMD to correct population data
# e.g. e.g. for SIMD2020v2, take 2011 pop estimates and filter from years 2017 - 2023
# and join pop data with that version of SIMD
simd_pop_data <- imap(simd_info, ~ {
  
  pop_data[[.x$dz_year]] |>
    filter(year %in% .x$trend_years) |>
    left_join(simd_deciles[[.y]], by = "datazone")
  
})



# Uncomment code below for a run through of steps 1-5:
# Example run through for SIMD2020v2:
# simd2020v2_index_pop <- simd_index_pop[["2020v2"]] # Step 1 result: get 2017 pop estimates (DZ11 based)
# simd2020v2_data <- simd_data[["2020v2"]] # Step 2 result: join SIMD2020v2 with 2017 pop estimates
# simd2020v2_quint_breaks <- quint_breaks[["2020v2"]] # Step 3 result: calculate population breakpoints 
# simd2020v2_quintiles <- simd_quintiles[["2020v2"]] # Step 4 result: calculate quintiles
# simd2020v2_pop_data <- simd_pop_data[["2020v2"]] # Step 5 result: use new SIMD2020v2 lookup with pop estimates from 2017-2023 (DZ11 based)
# rm(list = ls(pattern = 'simd2020v2'))


# tidy global env:
rm(localities, pop_data, decile_breaks, simd_data, simd_index_pop, simd_deciles, simd_info)


## Step 6: Convert data list into a single dataframe
# and add a column to signify which SIMD version has been applied
# note there will be NAs for SIMD2004 crime domain as doesn't exist
# and NAs for IZs/localities until 2015 as they cannot be mapped to 2001 dzs
result_decile <- imap_dfr(simd_pop_data, ~ mutate(.x, simd_version = .y))




# pivot data longer to create 1 geography col
result_decile2 <- result_decile |>
  pivot_longer(
    cols = c("datazone", "intzone2011", "hscp_locality", "ca2019", "hscp2019", "hb2019", "scotland"), 
    names_to = "geo_type", 
    values_to = "code")
# doesn't filter



# Function to create indicator data for different domains/populations 
create_data <- function(data = data, 
                        domain = c("overall", "access", "income", "crime", "employment","health","education","housing")
                        # population = c("all_ages", "under26", "working_age")
                        #ind_id, yearstart, yearend
){
  
  # name of domain col (e.g. 'access_quint_all_ages)
  domain_col <- paste0(domain, "_centile_all_ages")
  
  # list of columns to include in group_by (this takes parameter value so domain can be varied e.g. 'access_quint_all_ages)
  grouping_var <-c("code", "year", {{domain_col}}, "geo_type", "simd_version")
  
  data <- data |>
    # select cols
    select(code, year, {{domain_col}}, all_ages,under26, working_age, index_year, geo_type, simd_version)|>
    #group and calculate populations for each of the simd quintiles
    group_by(pick(all_of(grouping_var))) |>
    summarise(all_ages_pop=sum(all_ages),
              u26_pop=sum(under26),
              working_pop=sum(working_age),
              dz_count=n())|>
    ungroup() |> #remove initial grouping
    #re-group data to add overall population totals irrespective of SIMD quintile
    group_by(code,year) |>
    mutate(total_pop_all_ages=sum(all_ages_pop),
           total_pop_u26 = sum(u26_pop),
           total_pop_working=sum(working_pop))|>
    ungroup()|>
    # calculate % of total (age-specific) population within each quintile & add columns to identify which simd domain
    mutate(all_ages_percent=round(all_ages_pop/total_pop_all_ages*100,1),
           u26_percent=round(u26_pop/total_pop_u26*100,1),
           working_percent=round(working_pop/total_pop_working*100,1),
           simd_domain={{domain}},
           centile_type="decile")
}

# result is the file generated in the population deprived script at line 299



overall_domain <-result_decile2|>
  create_data(domain="overall") |>
  rename(centile=overall_centile_all_ages)

income_domain <-result_decile2|>
  create_data(domain="income")|> 
  rename(centile=income_centile_all_ages)

access_domain <-result_decile2|>
  create_data(domain="access") |> 
  rename(centile=access_centile_all_ages)

employment_domain <-result_decile2|>
  create_data(domain="employment") |>
  rename(centile=employment_centile_all_ages)

health_domain <-result_decile2|>
  create_data(domain="health") |>
  rename(centile=health_centile_all_ages)

education_domain <-result_decile2|>
  create_data(domain="education") |>
  rename(centile=education_centile_all_ages)

housing_domain <-result_decile2|>
  create_data(domain="housing") |>
  rename(centile=housing_centile_all_ages)


#combine all domains
populations_by_domain <- rbind(overall_domain,income_domain,access_domain,employment_domain,health_domain,education_domain,housing_domain) 

#clean up a bit
rm(access_domain,education_domain,overall_domain,health_domain,housing_domain,income_domain, employment_domain)
rm(simd_pop_data)

#apply filters to some years as the dataset is so large (in excess if 1.2 million rows)
#maybe limit to years which SIMD produced and latest year? 
populations_by_domain <-populations_by_domain   |>
  filter(geo_type != "intzone2011") |> #not sure we really need iz level for this presentation & will help reduce row numbers 
  filter(year %in% c(2004,2006,2009,2012,2016,2020,2023)) #brings down t0 380,000 ish rows


# likely stage that indicator data file saved out (this would then be picked up in app data prep) 
#saveRDS(populations_by_domain, file=paste0(profiles_data_folder, '/Test Shiny Data/testfile_population_by_simd_centiles.rds'))



## START MESSING AROUND WITH TABLE/CHART DESIGNS ----

library(reactable)


# table could present by quintile or decile
# which population (all ages, under 25, working age)
# show total population or number of dz within the quintile  (are the centile threholds defined using population weighting)
# only shows 1 year 
# how do we show areas that are persistently deprived - how many dz wher 

#open profiles tool geo lookup (this step should go in data prep when the full process is clearer and i know what i'm trying to generate)
geography_lookup <- readRDS(file = ("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/opt_geo_lookup.rds"))
demo_simd<- populations_by_domain|> 
  left_join(geography_lookup, by = "code") |>
  select(-geo_type,-simd_version)


# draw a simple table with just scotland data as an example 
demo_table<- demo_simd |>
  select(-c(total_pop_all_ages,total_pop_u26,total_pop_working))|>
  #need to consider that not all geo will have all deciles present
  complete(centile = c("1","2","3","4","5","6","7","8","9","10"), fill = list(pop_all_ages = 0))|> # ensure all areas have 5 quintiles and fill gaps with zero
  mutate(centile=paste0("D",centile))|> #this field becomes a column name and numeric column names can be problematic
  filter(centile_type=="decile")|> #not strictly necessary here but maybe the groupings should be quintile & decile
  pivot_longer(cols = c("all_ages_pop", "u26_pop", "working_pop","all_ages_percent","u26_percent","working_percent","dz_count"),
                                                            names_to = "population_group",
                                                            values_to = "population")|>
  pivot_wider(names_from = "centile",
              values_from = "population")|>
  relocate(D10,.after=D9)|> # relocate decile 10 which wants to sort after decile 1 not 9
  mutate(Total = rowSums(across(D1:D10), na.rm = TRUE),
         measure_type=case_when(grepl("pop", population_group) ~ "count",
                                grepl("percent", population_group) ~ "(%)", TRUE ~ "other"))


#filter data desired for 1st table
# Summary of how many datazones within each decile for a selected year and selected geography

demo_table1<-demo_table |>
  filter(areatype == "Scotland" & areaname =="Scotland")|>
  filter(year==2023) |>
  filter(population_group=="dz_count")|>
  select(simd_domain,D1:Total)
  #filter(simd_domain=="overall")|>
  #select(simd_domain,measure_type,population_group,D1:Total)



#count of datazones by decile for a particular geography
reactable(demo_table1,          
          #defaultExpanded = TRUE, #set default expanded groups (not required if table isn't grouped)
          pagination = FALSE,
          defaultPageSize = nrow(data),
         # groupBy = c("simd_domain"),
          columns = list(
            simd_domain = colDef(name = "SIMD Domain",      
                              style = list(borderRight = "1px solid #eee"),
                              headerStyle = list(borderRight = "1px solid #eee")),
            D1=colDef(name = "1", format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
            D2=colDef(name = "2",format = colFormat(digits = 0,separators = TRUE)),
            D3=colDef(name = "3",format = colFormat(digits = 0,separators = TRUE)),
            D4=colDef(name = "4",format = colFormat(digits = 0,separators = TRUE)),
            D5=colDef(name = "5",format = colFormat(digits = 0,separators = TRUE)),
            D6=colDef(name = "6",format = colFormat(digits = 0,separators = TRUE)),
            D7=colDef(name = "7",format = colFormat(digits = 0,separators = TRUE)),
            D8=colDef(name = "8",format = colFormat(digits = 0,separators = TRUE)),
            D9=colDef(name = "9",format = colFormat(digits = 0,separators = TRUE)),
            D10=colDef(name = "10",format = colFormat(digits = 0,separators = TRUE)),
            Total=colDef(format = colFormat(digits = 0,separators = TRUE))),
          columnGroups = list(
            colGroup("Count of 2011 Datazones by deprivation decile (population weighted)", columns = c("D1", "D2", "D3","D4","D5","D6","D7","D8","D9","D10"))
            ))
          




# filter for one particular

demo_table2<-demo_table |>  
  #filter(areatype == "Council area" & areaname =="Aberdeen City")|>
#  filter(areatype == "Council area" & areaname =="Inverclyde")|>
  filter(areatype == "HSC locality" & areaname =="Airdrie")|>
  #filter(areatype == "Scotland" & areaname =="Scotland")|>
  filter(measure_type != "other") |>
  filter(year==2023) |>
  select(simd_domain,measure_type,population_group,D1:Total)


# 1. Define the styling function - want to highlight when the % of population deviates from 10%
# suggesting that a particular decile is over/under represented for a certain geography
my_style <- function(value) {
  bg_color <- case_when(
    value < 6              ~ "#9fc5e8", # Red range
    #value >= 5 & value < 9 ~ "#cfe2ff", # Yellow range
    value >= 6 & value < 14 ~ "#eeeeee", # Green range
    #value >= 11 & value < 14 ~ "#fff3cd", # Green range
    TRUE                   ~ "#fce4c0"  # Blue (everything else)
  )
  list(background = bg_color)
}



#don't really want the colour scheme on the population count

#count of datazones by decile for a particular geography
reactable(demo_table2,          
          defaultExpanded = TRUE, #set default expanded groups (not required if table isn't grouped)
          pagination = FALSE,
          defaultPageSize = nrow(data),
          groupBy = c("population_group"),
          columns = list(
            simd_domain = colDef(name = "SIMD Domain",      
                                 style = list(borderRight = "1px solid #eee"),
                                 headerStyle = list(borderRight = "1px solid #eee")),
            D1=colDef(name = "1", style=my_style, format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
            D2=colDef(name = "2",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D3=colDef(name = "3",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D4=colDef(name = "4",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D5=colDef(name = "5",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D6=colDef(name = "6",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D7=colDef(name = "7",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D8=colDef(name = "8",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D9=colDef(name = "9",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            D10=colDef(name = "10",style=my_style,format = colFormat(digits = 0,separators = TRUE)),
            Total=colDef(format = colFormat(digits = 0,separators = TRUE))),
          columnGroups = list(
            colGroup("Deprivation Decile (population weighted)", columns = c("D1", "D2", "D3","D4","D5","D6","D7","D8","D9","D10"))
          ))



















#table showing population count and percent of population by each simd quintile grouped by simd domain
reactable(data=demo_table2,
          defaultExpanded = TRUE,
          defaultPageSize = nrow(data),
          groupBy = "simd_domain",
              columns = list(
            simd_domain = colDef(name = "SIMD Domain",      
                                 style = list(borderRight = "1px solid #eee"),
                                 headerStyle = list(borderRight = "1px solid #eee")),
            D1=colDef(format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
            D2=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D3=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D4=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D5=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D6=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D7=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D8=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D9=colDef(format = colFormat(digits = 0,separators = TRUE)),
            D10=colDef(format = colFormat(digits = 0,separators = TRUE)),
            Total=colDef(format = colFormat(digits = 0,separators = TRUE))
          ),
          columnGroups = list(
          #  colGroup("", columns = c("population_group", "simd_domain")),
            colGroup("Deprivation Decile (population weighted)", columns = c("1", "2", "3","4","5","6","7","8","9","10"))
          ))













pop_simd_centile_iz<- demo_simd_scot |>
  #filter(areatype == "Intermediate zone" & areaname =="Culter")|>
  filter(areatype == "Council area" & areaname =="Aberdeen City")|>
  #filter(year==2020) |>
  filter(centile_type=="quintile")|>
  group_by(year,code,simd_domain, areatype, areaname,centile_type,parent_area,areaname_full) |>
  ungroup()|>
  complete(quintile = c("1","2","3","4","5"), fill = list(pop_all_ages = 0))|> # ensure all areas have 5 quintiles and fill gaps with zero
  mutate(quintile=paste0("Q",quintile))

pop_simd_centile_iz_rearrange <-pop_simd_centile_iz |>
  # select(-dz_count)|>
  pivot_longer(cols = c("pop_all_ages", "pop_u26", "pop_working","percent_all","percent_u26","percent_working","dz_count"),
               names_to = "population_group",
               values_to = "population")|>
  pivot_wider(names_from = "quintile",
              values_from = "population") |>
  #mutate(across(c(Q1:Q5), ~ replace_na(.x, 0)))|>
  #ungroup() |>
  mutate(Total = rowSums(across(c(Q1,Q2,Q3,Q4,Q5)), na.rm = TRUE),
         measure_type=case_when(substr(population_group,1,3) =="pop" ~ "count",
                                substr(population_group,1,3) =="per" ~ "(%)",
                                TRUE ~ "other"))




# draw a simple table with just scotland data as an example 
table_data<-pop_simd_centile_iz_rearrange |>
  filter(year==2020)|>
  filter(population_group=="percent_all") |>
  select(simd_domain,measure_type,population_group,Q1,Q2,Q3,Q4,Q5,Total)






#table showing population count and percent of population by each simd quintile grouped by simd domain
reactable(data=table_data,
          defaultExpanded = TRUE,
          #defaultPageSize = nrow(data),
          groupBy = "measure_type",
          columns = list(
            simd_domain = colDef(name = "SIMD Domain",      
                                 style = list(borderRight = "1px solid #eee"),
                                 headerStyle = list(borderRight = "1px solid #eee")),
            Q1=colDef(format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
            Q2=colDef(format = colFormat(digits = 0,separators = TRUE)),
            Q3=colDef(format = colFormat(digits = 0,separators = TRUE)),
            Q4=colDef(format = colFormat(digits = 0,separators = TRUE)),
            Q5=colDef(format = colFormat(digits = 0,separators = TRUE)),
            Total=colDef(format = colFormat(digits = 0,separators = TRUE))
          ),
          columnGroups = list(
            #colGroup("", columns = c("population_group", "simd_domain")),
            colGroup("Deprivation Quintile (population weighted)", columns = c("Q1", "Q2", "Q3","Q4","Q5"))
          ))





#charting  

# would need to create input fitlers for population group and simd domain  
trend_chart_data<-pop_simd_centile_iz_rearrange |>
  filter(population_group=="pop_all_ages") |>
  filter(simd_domain=="overall")

data<-trend_chart_data

hc <- 
  
  hchart()|>
  hc_chart(object = data,type = "line") %>%
  #hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]], group = .data[[grouping_col]]), 
  hc_xAxis(categories = data$year)|>
  hc_add_series(data = data$Q1, name = "Q1 Most Deprived",color = "#1f77b4") %>%
  # Add the second column
  hc_add_series(data = data$Q2, color = "#DFDDE3") |> 
  hc_add_series(data = data$Q3, color = "#DDDDE3") |>
  hc_add_series(data = data$Q4, color = "#DFDDE3") |> 
  hc_add_series(data = data$Q5, name = "Q5 Least Deprived",color = "#9b4393")


marker = list(enabled = TRUE)) |>
  hc_colors(colours) |>
  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>
  hc_chart(marginRight = 15) |>
  hc_legend(verticalAlign = legend_position, align = "left") |>
  hc_tooltip(crosshairs = TRUE, shared = TRUE)



highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = data$date_column) %>%
  # Add the first column
  hc_add_series(
    data = data$column1, 
    name = "Series A", 
    color = "#1f77b4"
  ) %>%
  # Add the second column
  hc_add_series(
    data = data$column2, 
    name = "Series B", 
    color = "#ff7f0e"
  ) %>%
  hc_tooltip(shared = TRUE





