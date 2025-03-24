# ScotPHO indicators: Quit attempts at 4 weeks (including quintile indicators)

#   Part 1 - Create basefile for general indicator
#   Part 2 - Create basefiles for quintile indicators
#   Part 3 - Run analysis functions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
library(janitor) # for cleaning column names



###############################################.
## Part 1 - Create basefile for general indicator ----
###############################################.

#Reading data extracted from table from Smoking cessation annual publication
quit_4weeks <- read_csv(paste0(data_folder, "Received Data/quit_attempts_4weeks_2022.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "numerator", -council) %>% #from wide to long format
  mutate(year = substr(year,1,4))

#the total number of quit attempts is the denominator 
quit_total <- read_csv(paste0(data_folder, "Received Data/quit_attempts_total_2022.csv")) %>% 
  setNames(tolower(names(.))) %>%    #variables to lower case
  gather("year", "denominator", -council) %>% #from wide to long format
  mutate(year = substr(year,1,4))

# merging numerator and denominator
quit_4weeks <- left_join(quit_4weeks, quit_total, by = c("year", "council"))

# converting council names into codes. First bring lookup.
ca_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/CAdictionary.rds") %>% 
  setNames(tolower(names(.))) %>% rename(ca=code)

quit_4weeks <- left_join(quit_4weeks, ca_lookup, by = c("council" = "areaname")) %>% 
  select(-council)

saveRDS(quit_4weeks, file=paste0(data_folder, 'Prepared Data/quitattempts_4weeks_raw.rds'))

# # ###############################################.
# # ## Part 2 - Create basefiles for quintile indicators ----
# # ###############################################.
# 
# # Reading council quintile data requested to smoking cessation team
# quit4w_quint <- read.spss(paste0(data_folder, "Received Data/Smoking_Cessation_Council_SIMD_FY2009-10 to FY2019-20.sav"),
#                           to.data.frame=TRUE, use.value.labels=FALSE) %>%
#   setNames(tolower(names(.))) %>%    #variables to lower case
#   filter(scsimdquintile != 99) %>% #excluding unknown values
#   rename(ca = ca2011, numerator = four_week_quit, denominator = quit_attempt, year = finyear) %>%
#   mutate(year = substr(year,1,4))
# 
# for (quint in 1:5) { #creating files for each one of the quintiles
#   quit4w_quint_raw <- quit4w_quint %>% filter(scsimdquintile == quint) %>%
#     select(-scsimdquintile)
# 
#   saveRDS(quit4w_quint_raw,
#           paste0(data_folder, "Prepared Data/quitattempts_4weeks_quint", quint, "_raw.rds"))
# 
# }

###############################################.
## Part 3 - Run analysis functions ----
###############################################.
analyze_first(filename = "quitattempts_4weeks", geography = "council", hscp = T,
              measure = "percent", yearstart = 2009, yearend = 2021, time_agg = 1)

analyze_second(filename = "quitattempts_4weeks", measure = "percent", time_agg = 1, 
               ind_id = 1536, year_type = "financial")

# # For quintile indicators
# # Names of the files used in the next two functions
# filenames <- c("quitattempts_4weeks_quint1", "quitattempts_4weeks_quint2",
#                "quitattempts_4weeks_quint3","quitattempts_4weeks_quint4",
#                "quitattempts_4weeks_quint5")
# 
# mapply(analyze_first, filename = filenames, geography = "council",
#        measure = "percent", yearstart = 2009, yearend = 2021, time_agg = 1)
# 
# mapply(analyze_second, filename = filenames, measure = "percent", time_agg = 1, qa = F,
#        ind_id = c(1539:1543), year_type = "financial")
# 

##END

##########################.
## MM testing ----
#creating deprivation data file using data already aggregated by quintile (scottish quintiles) at CA level
##########################.

# note first need to open deprivation_analysis.R and run the create_quintile_data() and inequality_measures() functions
# to make available in global script as they're created within deprivation_analysis function so don't appear when sourcing deprivation function script

### 1. read in smoking quit attempts at 4 weeks data ----
data_extract <- read.spss(paste0(data_folder, "Received Data/Smoking_Cessation_Council_SIMD_FY2009-10 to FY2019-20.sav"),
                          to.data.frame=TRUE, use.value.labels=FALSE)


### 2. clean the data ----
clean_data <- data_extract |>
  clean_names() |>
  rename(ca = ca2011, numerator = four_week_quit, denominator = quit_attempt, year = fin_year, sc_quin = scsim_dquintile) |>
  filter(sc_quin != 99) |> # excluding unknown values
  mutate(year = as.numeric(substr(year, 1, 4)))


### 3. join data with deprivation lookup ----
depr_lookup <- readRDS(paste0(lookups, "Geography/deprivation_geography.rds")) |>
  select(-datazone) |>
  unique() |>
  mutate(scotland = "S00000001")

data_depr <- left_join(clean_data, depr_lookup, by = c("year", "sc_quin", "ca"))


### 4. create quintiles for different geo levels ----
measure = "percent" # variable required to run create_quintile_data() function 

data_depr <- rbind( 
  #Scotland 
  create_quintile_data(geo = "scotland", quint = "sc_quin", 
                       group_vars =  c("scotland", "year", "sc_quin")),
  #Health boards using national quintiles
  create_quintile_data(geo = "hb", quint = "sc_quin", 
                       group_vars =  c("hb", "year", "sc_quin")),
  #Health boards using health board quintiles
  create_quintile_data(geo = "hb", quint = "hb_quin", 
                       group_vars =  c("hb", "year", "hb_quin")),
  #Council area using national quintiles
  create_quintile_data(geo = "ca", quint = "sc_quin", 
                       group_vars =  c("ca", "year", "sc_quin")),
  #Council area using council quintiles
  create_quintile_data(geo = "ca", quint = "ca_quin",
                       group_vars =  c("ca", "year", "ca_quin"))
)


### 5. create totals ----
data_depr_totals <- data_depr %>% group_by(code, year, quint_type)

data_depr_totals <- data_depr_totals %>% summarise_all(sum, na.rm = T) %>% 
  mutate(quintile = "Total") %>% ungroup()


### 6. merge totals with quintiles ----
data_depr <- rbind(data_depr, data_depr_totals) %>% 
  # To exclude cases without quintile or code. Caused when raw files have 
  # records withoout datazone needed for get the right Scotland level
  filter(!(is.na(code) | is.na(quintile)))

data_depr$numerator[is.na(data_depr$numerator)] <- 0 # Converting NA's to 0s


### 7. Calculating moving average for denominator and numerator ----
## Data needs to be sorted to calculate the right figures
time_agg = 1
time_fix <- ifelse(time_agg < 3, 0, ifelse(time_agg == 3, 1,
                                           ifelse(time_agg == 5, 2, "ERROR")))

data_depr <- data_depr %>% 
  arrange(code, quintile, quint_type, year) %>% 
  group_by(code, quintile, quint_type) %>%
  mutate(numerator = roll_meanr(numerator, time_agg), 
         denominator = roll_meanr(denominator, time_agg)) %>% 
  subset(!is.na(denominator)) %>%  #excluding NA rows 
  mutate(year = as.numeric(year)-time_fix) %>%  # year minus to adjust to center year
  ungroup()

# calculate percentage and confidence intervals
data_depr <- data_depr %>%
  mutate(rate = numerator/denominator*100,
         lowci=(2*numerator+1.96*1.96-1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100))) 
         / (2*(denominator+1.96*1.96))*100,
         upci=(2*numerator+1.96*1.96+1.96*sqrt(1.96*1.96+4*numerator*(1-rate/100)))
         /  (2*(denominator+1.96*1.96))*100,
         # if over 100 or under 0, set to these values as it is a percentage
         upci = case_when(upci>100 ~ 100, TRUE ~ upci),
         lowci = case_when(lowci<0 ~ 0, TRUE ~ lowci))


### 8. calculate inequalities measures ----
data_depr <- data_depr %>% inequality_measures()


### 9. final prep for shiny file ----
ind_id = 1536

#Indicator code
data_depr <- data_depr %>% mutate(ind_id = ind_id) %>% 
  # fill in missing values and if any have negative lower CI change that to zero.
  mutate_at(c("rate", "lowci", "upci"), ~replace(., is.na(.), 0)) 

data_depr$lowci <- ifelse(data_depr$lowci<0, 0, data_depr$lowci)


# create date columns 
year_type = "financial"

data_depr <- data_depr %>% 
  mutate(trend_axis = paste0(year, "/", substr(year+1, 3, 4)),
         def_period = paste0(trend_axis, " ", year_type, " year"))


#Preparing data for Shiny tool
data_shiny <- data_depr %>% 
  select(-c(overall_rate, total_pop, proportion_pop, most_rate, 
            least_rate, par_rr, count))



### 10. Saving file ----
filename <- "smoking_quit_4weeks"
saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/", filename, "_ineq.rds"))


### 11. spot check ----
# spot check against original indicator 
# Aberdeen City council area: 2015

# new deprivation file
test <- data_shiny |>
  filter(year == 2015 & quintile == 1 & code == "S12000033")

# original quintile 1 indicator 
original_quint1_indicator <- readRDS(paste0(data_folder, "/Shiny Data/quitattempts_4weeks_quint1_shiny.rds")) |>
  filter(year == 2015 & code == "S12000033")

  