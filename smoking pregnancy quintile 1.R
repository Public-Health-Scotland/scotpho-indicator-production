
#Incomplete script - still work in progress ###################################

# ScotPHO indicators: smoking during pregnancy quintile 1

## Part 1 - Format raw data ready for analysis functions 
## Part 2 - calling the analysis functions 
## Part 3 - Editing shiny data file to exclude data for regions where data is known to be incomplete

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #Normal indicator functions
source("2.deprivation_analysis.R") # deprivation function


smoking_pregnant <- read_excel(paste0(data_folder, "Received Data/IR2019-00231_smoking_pregnancy.xlsx"), 
                          sheet = "IR2019-00231_smoking_pregnancy", range = cell_limits(c(1, 1), c(NA, 4))) %>%
  setNames(tolower(names(.))) %>%
  rename(year = finyear, datazone = datazone2011) %>%
  filter(!is.na(datazone)) # exclude rows with no datazone.

dep_lookup <- read_rds(paste0(data_folder, "Lookups/Geography/deprivation_geography.rds")) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  select(hb2014, ca2011, datazone, sc_quin) %>%
  rename(hb = hb2014, ca = ca2011, quintile = sc_quin)


###ALL QUINTILES##

smoking_pregnant <- left_join(smoking_pregnant, dep_lookup, "datazone") %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  select(year, hb, ca, datazone, quintile, smoker, known_status) %>%
  mutate_if(is.character, factor) # converting variables into factors

##SMOKING DURING PREGNANCY QUINTILE 1##

create_quintile_data <- function(quint_number) {
  smoking_preg_quint <- left_join(smoking_pregnant, dep_lookup) %>% 
    setNames(tolower(names(.))) %>% #variables to lower case
    subset(quintile == quint_number) %>%
    select(year, datazone, smoker, known_status) %>%
    rename(numerator = smoker, denominator = known_status) %>%
    mutate_if(is.character, factor) # converting variables into factors
  
  
  # olddata <- readRDS(paste0(data_folder, "Prepared Data/smoking_preg_q1_raw.rds"))
  saveRDS(smoking_preg_quint, file=paste0(data_folder, 'Prepared Data/smoking_preg_q', quint_number, '_raw.rds'))
}

mapply(create_quintile_data, quint_number = c(1,2,3,4,5))



###############################################.
## ANALYSE
###############################################.

mapply(analyze_first, filename = c("smoking_preg_q1", "smoking_preg_q2", "smoking_preg_q3", 
                                   "smoking_preg_q4", "smoking_preg_q5"), 
       geography = "datazone11", measure = "percent", 
              yearstart = 2002, yearend = 2016, time_agg = 3)

mapply(analyze_second, filename = c("smoking_preg_q1", "smoking_preg_q2", "smoking_preg_q3", 
                                    "smoking_preg_q4", "smoking_preg_q5"),
       measure = "percent", time_agg = 3, 
               ind_id = c(1521, 1522, 1523, 1524, 1525), 
       year_type = "financial", profile = "TP", min_opt = 1013120)
