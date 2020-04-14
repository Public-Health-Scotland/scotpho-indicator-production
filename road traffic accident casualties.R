# ScotPHO indicators: Road traffic accident casualties #

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

#Extracting data on deaths of Scottish residents with a diagnosis of road traffic accident (RTA) 
#and admissions with admission type of RTA(32), excluding records with unknown sex and age.
road_accidents <- tbl_df(dbGetQuery(channel, statement=
  "SELECT link_no, year_of_registration year, age, SEX sex_grp, POSTCODE pc7, null as cis_marker 
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE date_of_registration between '1 January 2002' and '31 December 2019'
    AND country_of_residence='XS'
    AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'V[0-8]')
    AND age is not NULL
    AND sex <> 9
  UNION ALL
    SELECT link_no, extract(year from admission_date) year, AGE_IN_YEARS age, SEX sex_grp, 
        DR_POSTCODE pc7, cis_marker
    FROM ANALYSIS.SMR01_PI z
    WHERE admission_date between '1 January 2002' and '31 December 2019'
      AND exists(select * from ANALYSIS.SMR01_PI 
          WHERE link_no=z.link_no and cis_marker=z.cis_marker
            AND admission_type=32
            AND admission_date between '1 January 2002' and '31 December 2019')
  ORDER BY link_no, cis_marker, year")) %>% 
  setNames(tolower(names(.)))  #variables to lower case

# Aggregating to select only one case per admission and year
road_accidents <- road_accidents %>% 
  group_by(link_no, cis_marker, year) %>% 
  summarise_at(c("age", "sex_grp", "pc7"), funs(first)) %>% 
  ungroup() %>% 
  create_agegroups() # Creating age groups for standardization.

# Bringing datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2001, datazone2011)

road_accidents <- left_join(road_accidents, postcode_lookup, "pc7") %>% 
  subset(!(is.na(datazone2011))) %>%  #select out non-scottish
  mutate_if(is.character, factor) # converting variables into factors

###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.

# Datazone2011
roadaccidents_dz11 <- road_accidents %>% group_by(year, datazone2011, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>%  rename(datazone = datazone2011)

saveRDS(roadaccidents_dz11, file=paste0(data_folder, 'Prepared Data/roadaccidents_dz11_raw.rds'))

#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
roadaccidents_dz01_dep <- road_accidents %>% group_by(year, datazone2001, sex_grp, age_grp) %>%  
  summarize(numerator = n()) %>% ungroup() %>% rename(datazone = datazone2001) %>% 
  subset(year<=2013)

dep_file <- rbind(roadaccidents_dz01_dep, roadaccidents_dz11 %>% subset(year>=2014)) #joing dz01 and dz11

saveRDS(dep_file, file=paste0(data_folder, 'Prepared Data/roadaccidents_depr_raw.rds'))

###############################################.
## Part 3 - Run analysis functions ----
###############################################.

#All patients asthma
analyze_first(filename = "roadaccidents_dz11", geography = "datazone11", measure = "stdrate", 
              pop = "DZ11_pop_allages", yearstart = 2002, yearend = 2019,
              time_agg = 3, epop_age = "normal")

analyze_second(filename = "roadaccidents_dz11", measure = "stdrate", time_agg = 3, 
               epop_total = 200000, ind_id = 20307, year_type = "calendar")

#Deprivation analysis function
analyze_deprivation(filename="roadaccidents_depr", measure="stdrate", time_agg=3, 
                    yearstart= 2002, yearend=2019,   year_type = "calendar", 
                    pop = "depr_pop_allages", epop_age="normal",
                    epop_total =200000, ind_id = 20307)

##END