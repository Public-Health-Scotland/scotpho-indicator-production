# ScotPHO indicators: child healthy weight in primary 1

#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
server_desktop <- "server" # change depending if you are using R server or R desktop
source("./1.indicator_analysis.R") #Normal indicator functions
source("./2.deprivation_analysis.R") # deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.
# Reading data provided by child health team for datazones 2011
child_weight11 <- read.spss( paste0(data_folder, "Received Data/IR2018-02013_DZ2011 Child weight.sav"), 
                         to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>% #variable names to lower case
  rename(datazone = datazone2011, numerator = healthy_weight, denominator = tot) %>% 
  # reformat the year variable.
  mutate(year = as.numeric(paste0("20", substr(schlyr_exam, 1, 2)))) %>% 
  # aggregate to get the count
  group_by(year, datazone) %>% 
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>% ungroup()

saveRDS(child_weight11, file=paste0(data_folder, 'Prepared Data/child_healthyweight_raw.rds'))

###############################################.
# Datazone2001 
child_weight01 <- read.spss( paste0(data_folder, "Received Data/IR2018-02013_DZ2001 Child weight.sav"), 
                             to.data.frame=TRUE, use.value.labels=FALSE)%>% 
  setNames(tolower(names(.))) %>%  #variable names to lower case
  rename(datazone = datazone2001, numerator = healthy_weight, denominator = tot) %>% 
  # reformat the year variable.
  mutate(year = as.numeric(paste0("20", substr(schlyr_exam, 1, 2)))) %>% 
  # aggregate to get the count
  group_by(year, datazone) %>% 
  summarise_at(c("numerator", "denominator"), funs(sum), na.rm =T) %>%  ungroup()

###############################################.
#Deprivation basefile
# DZ 2001 data needed up to 2013 to enable matching to advised SIMD
child_weight_dep <- rbind(child_weight01 %>% subset(year<=2013), 
                          child_weight11 %>% subset(year>=2014)) 

saveRDS(child_weight_dep, file=paste0(data_folder, 'Prepared Data/childweight_depr_raw.rds'))

###############################################.
## Part 2 - Run analysis functions ----
###############################################.
analyze_first(filename = "child_healthyweight", geography = "datazone11", 
              measure = "percent", yearstart = 2002, yearend = 2017, time_agg = 1)

# There are several boards and councils for which we only have incomplete data 
# for certain years as they join to the CHSPS later on.
# See annual publication to find out when each board/ca started to record their 
# data and its completeness.
# Check the numbers(e.g. pivot table excel) and the annual publication completeness 
# estimates to ensure you are excluding the incomplete periods/areas. 
# Most of them very low numbers so they can be taken out in mass.
child_formatted <- readRDS(file=paste0(data_folder, "Temporary/child_healthyweight_formatted.rds")) %>%
  #excluding years for which we don't have populations for pcf
  filter(!(substr(code,1,3) %in% c('S02','S99', 'S37') & year < 2011)) %>% 
  filter(numerator>4 &  # Excluding years/areas where the data is very incomplete or absent
           # This excludes all the incomplete years at council and hb levels.
          !(numerator < 50 & substr(code,1,3) %in% c('S12', 'S08')))

saveRDS(child_formatted, file=paste0(data_folder, "Temporary/child_healthyweight_formatted.rds")) 
  
analyze_second(filename = "child_healthyweight", measure = "perc_pcf", time_agg = 1,
               pop="DZ11_pop_5", ind_id = 21106, year_type = "financial", 
               profile = "HN", min_opt = 1471290)

#Deprivation analysis function 
# analyze_deprivation(filename="child_healthyweight_depr", measure="perc_pcf", time_agg=1,
#                     yearstart= 2002, yearend=2017,   year_type = "financial",
#                     pop = "depr_pop_allages", pop_pcf = pop="DZ11_pop_5", ind_id = 21106)

##END