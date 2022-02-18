# ScotPHO indicators: child healthy weight in primary 1

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
# Reading data provided by child health team for datazones 2011
child_weight11 <- read.spss( paste0(data_folder, "Received Data/IR2022-00007_DZ2011-child-weight.sav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% #variable names to lower case
  rename(datazone = datazone2011, numerator = healthy_weight, denominator = tot) %>% 
  # reformat the year variable.
  mutate(year = as.numeric(paste0("20", substr(schlyr_exam, 1, 2)))) %>% 
  # aggregate to get the count
  group_by(year, datazone) %>% 
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) %>% ungroup()

saveRDS(child_weight11, file=paste0(data_folder, 'Prepared Data/child_healthyweight_raw.rds'))

###############################################.
# Datazone2001 
child_weight01 <- read.spss( paste0(data_folder, "Received Data/IR2022-00007_DZ2001-child-weight.sav"), 
                             to.data.frame=TRUE, use.value.labels=FALSE)%>% 
  setNames(tolower(names(.))) %>%  #variable names to lower case
  rename(datazone = datazone2001, numerator = healthy_weight, denominator = tot) %>% 
  # reformat the year variable.
  mutate(year = as.numeric(paste0("20", substr(schlyr_exam, 1, 2)))) %>% 
  # aggregate to get the count
  group_by(year, datazone) %>% 
  summarise_at(c("numerator", "denominator"), list(sum), na.rm =T) %>%  ungroup()

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
child_weight_dep <- rbind(child_weight01 %>% subset(year<=2013), 
                          child_weight11 %>% subset(year>=2014)) 

saveRDS(child_weight_dep, file=paste0(data_folder, 'Prepared Data/child_healthyweight_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "child_healthyweight", geography = "datazone11", 
              measure = "percent", yearstart = 2002, yearend = 2020, time_agg = 1)

# There are several boards and councils for which we only have incomplete data 
# for certain years as they join to the CHSPS later on.
# See annual publication to find out when each board/ca started to record their 
# data and its completeness.
# Check the numbers(e.g. pivot table excel) and the annual publication completeness 
# estimates to ensure you are excluding the incomplete periods/areas. 
# Most of them very low numbers so they can be taken out in mass.
child_formatted <- readRDS(file=paste0(data_folder, "Temporary/child_healthyweight_formatted.rds")) %>%
  filter(numerator>4 &  # Excluding years/areas where the data is very incomplete or absent
           # This excludes all the incomplete years at council and hb levels.
          !(numerator < 50 & substr(code,1,3) %in% c('S12', 'S08', 'S37')))

saveRDS(child_formatted, file=paste0(data_folder, "Temporary/child_healthyweight_formatted.rds")) 
  
analyze_second(filename = "child_healthyweight", measure = "perc_pcf", time_agg = 1,
               pop="DZ11_pop_5", ind_id = 21106, year_type = "financial")

# Excluding data for boards, hscps, las, localities and izs with incomplete data
# Merging final data with parent geographies lookup and then filtering
geo_parents <- readRDS(paste0(lookups, "Geography/IZtoPartnership_parent_lookup.rds")) %>% 
  #TEMPORARY FIX. dealing with change in ca, hb and hscp codes
  mutate(hscp_partnership = recode(hscp_partnership, "S37000014"='S37000032', 
                                   "S37000023"='S37000033')) %>% 
  gather(geotype, code, c(intzone2011, hscp_locality)) %>% distinct() %>% 
  select(-geotype) %>% rename(parent_area = hscp_partnership)

data_shiny <- left_join(readRDS(file = paste0(data_folder, "Data to be checked/child_healthyweight_shiny.rds")),
                        geo_parents, by = "code") %>%
  subset(!(((code %in% c('S37000001', 'S37000002', "S12000033", "S12000034", 'S12000020',
                         'S37000019', 'S08000020') | # Moray, Aberdeen, Aberdeenshire
               parent_area %in% c('S37000001', 'S37000002', 'S37000019')) & year <2009) |
             ((code %in% c('S12000035', 'S37000004', 'S12000017', 'S37000016', "S12000027", 'S37000026',
                           'S12000040', 'S37000030', 'S08000022', 'S08000026')  |
                 # "Argyll & Bute", 'Shetland Islands', 'West Lothian', 'Highland'
                 parent_area %in% c("S37000004", 'S37000016', 'S37000026', 'S37000030')) & 
                year %in% c('2007') ) |
             ((code %in% c('S12000039', 'S37000029', 'S12000011', 'S37000011', 'S12000049', 'S37000034') |
                 #"East Renfrewshire", 'Glasgow City', 'West Dunbartonshire'
                 parent_area %in% c("S37000029", 'S37000011', 'S37000034')) & 
                year %in% c('2007', "2008", "2010")) | # East Dunbartonshire
             ((code %in% c('S12000045', 'S37000009') | parent_area %in% c("S37000009")) & 
                year %in% c('2007', "2008", "2010", "2016")) | # "Inverclyde"
             ((code %in% c('S12000018', 'S37000017') | parent_area %in% c('S37000017')) & 
                year %in% c('2007', "2008", "2009", "2010")) |
             ((code %in% c('S12000023', 'S37000022', 'S08000025') |#Orkney Islands
                 parent_area %in% c("S37000022") ) & year %in% c('2007', "2008", "2009"))
           ) #negation
  ) %>% #subset
  select(-parent_area)

saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/child_healthyweight_shiny.rds"))
write_csv(data_shiny, path = paste0(data_folder, "Data to be checked/child_healthyweight_shiny.csv"))

###############################################.
# # Deprivation analysis function 
# analyze_deprivation(filename="child_healthyweight_depr", measure="perc_pcf", time_agg=1,
#                     yearstart= 2002, yearend=2018,   year_type = "financial",
#                     pop = "depr_pop_allages", pop_pcf = "DZ11_pop_5", ind_id = 21106)

#need to add exclusions to this too. Is that the right population file? No.

##END

