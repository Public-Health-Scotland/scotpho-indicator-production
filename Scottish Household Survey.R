# Scottish Household Survey data import

# Update ScotPHO indicators for mental health profile:

# 30022	Adults agreeing they can influence local decisions
# 30043	Households managing well financially (household level: sex==Total)
# 30024	Adults feeling they belong to their local neighbourhood
# 30046	Adults rating neighbourhood as a very good place to live
# 30027	Adults trusting most people in their neighbourhood
# 30047	Adults regularly using or passing through local open space
# 30020	Adults volunteering in past year
# 30038	Adults experiencing discrimination in past year
# 30041	Adults experiencing harassment in past year
# 30025	Adults feeling lonely in past week
# 30049	Adults experiencing noisy neighbours in past year
# 30045	Adults using high-risk loans in past year (household level: sex==Total)

# Source: bespoke analysis from SHoS team, SG. 

# NOTES:
# Data for 2020 and 2021 are not included in the timeseries as they are not comparable 
# with other years due to changes in the mode and delivery of the survey for those years because of the pandemic. 
# This means that for some of the biennial variables, the latest year available is 2019.
# Where the base is under 50, the associated figures have been left blank.
# For the credit tables specifically, the percentages are very low as the actual 
# numbers for the credit309 options are low. As such, they will likely not be useful for your 
# purposes and it may be worth excluding them from your final dashboards.

#   Part 1 - Read in data
#   Part 2 - Run analysis function (for inequals analysis)

### functions/packages ----

source("1.indicator_analysis.R") 
source("2.deprivation_analysis.R") 

library(stringr)
library(rio)


### Lookups

# bring in LA dictionary and include LA codes
la_lookup <- readRDS(paste0(lookups, "Geography/CAdictionary.rds"))%>%
  mutate(geographylevel="Local Authority")
hb_lookup <- readRDS(paste0(lookups, "Geography/HBdictionary.rds"))%>%
  mutate(geographylevel="Health Board")

area_lookup <-rbind(la_lookup,hb_lookup)

rm(hb_lookup,la_lookup)



### 1 - Read in SHoS data from spreadsheet ----

file <- "/Scottish Household Survey/mental_health_indicators_2024.xlsx"

vars <- c("SERV1H", "HK2", "COMMBEL", "RB1", "SOCIAL32", 
          "GREENUSE13", "VOLUN", "SOCIAL2",
          "ASB2A", "CREDIT", "DISCRIM", "HARASS")

# Function to read in worksheets and perform common processing
import_shos_xlsx <- function(suffix, filename) {
  import_list(paste0(data_folder,"Received Data", filename),  
              setclass = "tbl", rbind = TRUE, 
              which = paste0(vars, suffix), 
              rbind_label = "indicator") %>%
    #rename fields to match expected names for scotpho app
    rename(year = F_DYEAR,
           rate = RowPercent,
           lowci = RowLowerCL,
           upci = RowUpperCL) %>%
    mutate(indicator = gsub(suffix, "", indicator),
           numerator="",
           def_period=paste(year, "survey year"),
           trend_axis=year) %>%
    mutate(year = as.integer(substr(trend_axis, 1, 4))) %>%
    # Create new indicator id column
    mutate(ind_id = case_when(indicator == "SERV1H" ~ 30022,
                              indicator == "HK2" ~ 30043,
                              indicator == "COMMBEL" ~ 30024,
                              indicator == "RB1" ~ 30046,
                              indicator == "SOCIAL32" ~ 30027, 
                              indicator == "GREENUSE13" ~ 30047, 
                              indicator == "VOLUN" ~ 30020, 
                              indicator == "SOCIAL2" ~ 30025,
                              indicator == "ASB2A" ~ 30049,
                              indicator == "CREDIT" ~ 30045, 
                              indicator == "DISCRIM" ~ 30038, 
                              indicator == "HARASS" ~ 30041)) %>%
    select(-Base, -RowStdErr, -indicator) 
}


shos_scot <- import_shos_xlsx("_1", file) %>%
  mutate(geographylevel="Scotland",
         areaname = "Scotland") 

shos_ca <- import_shos_xlsx("_2", file) %>%
  rename(areaname = council) %>%
  mutate(geographylevel="Local Authority")

shos_hb <- import_shos_xlsx("_3", file) %>%
  rename(areaname = hb2014) %>%
  mutate(geographylevel="Health Board")

shos_simd <- import_shos_xlsx("_4", file) %>%
  rename(split_value = mdquin_ts) %>%
  mutate(geographylevel="Scotland",
         areaname = "Scotland",
         split_name= "Deprivation (SIMD)") 

shos_sex <- import_shos_xlsx("_5", file) %>%
  rename(split_value = randgender_ts) %>%
  mutate(geographylevel="Scotland",
         areaname = "Scotland",
         split_name= "Sex") %>%
  filter(is.na(hihgender_ts)) %>% # drop the indicators measured at hhd level (sex=meaningless)
  select(-hihgender_ts)


### 1 - Prepare data for shiny ----


# Combine main data (no splits apart from geographies)
shos_main <- bind_rows(shos_ca, shos_hb, shos_scot) %>%
  filter(!(ind_id==30045 & geographylevel %in% c("Health Board", "Local Authority"))) %>% # loans = too few cases for splits (many are suppressed)
  mutate(areaname = gsub("&", "and", areaname)) %>%
  mutate(areaname = ifelse(areaname=="Edinburgh, City of", "City of Edinburgh", areaname)) %>%
  mutate(areaname = case_when(geographylevel=="Health Board" ~ paste("NHS", areaname),
                              TRUE ~ areaname)) %>%
  mutate(areaname = ifelse(areaname=="NHS Orkney Islands", "NHS Orkney", areaname)) %>%
  mutate(areaname = ifelse(areaname=="NHS Shetland Islands", "NHS Shetland", areaname)) %>%
  left_join(area_lookup, by=c("geographylevel","areaname")) %>%
  mutate(code=case_when(geographylevel=="Scotland" ~ "S00000001", TRUE~code)) %>%
  select(-areaname, -geographylevel)


# Combine popgroup data (splits = sex and SIMD, National level)
shos_popgrp <- bind_rows(shos_simd, shos_sex) %>%
  filter(!(ind_id==30045)) %>% # too few cases for splits (many are suppressed) (loans)
  filter(!(split_value %in% c("Identify in another way", "Prefer not to say"))) %>% # keep M and F sex only
  mutate(code=case_when(geographylevel=="Scotland" ~ "S00000001", TRUE~"")) %>%
  select(-areaname, -geographylevel)

# get and add in totals for the splits
shos_total_sex <- shos_popgrp %>%
  select(code, trend_axis) %>%
  distinct() %>%
  merge(y=shos_main, by=c("code", "trend_axis")) %>%
  mutate(split_name="Sex",
         split_value = "Total") %>%
  filter(!(ind_id==30045))  # too few cases for splits (many are suppressed) (loans)


# Add in to popgrps file
shos_popgrp_sex <- bind_rows(shos_popgrp, shos_total_sex) %>%
  filter(split_name=="Sex")

# Add in to inequals file (raw, before running deprivation analysis)
shos_total_simd <- shos_total_sex %>%
  mutate(split_name="Deprivation (SIMD)")

shos_popgrp_simd <- bind_rows(shos_popgrp, shos_total_simd) %>%
  filter(split_name=="Deprivation (SIMD)") %>%
  mutate(quintile = case_when(split_value=="Quintile 1- 20% most deprived" ~ "1",
                              split_value=="Quintile 2" ~ "2",
                              split_value=="Quintile 3" ~ "3",
                              split_value=="Quintile 4" ~ "4",
                              split_value=="Quintile 5 - 20% least deprived" ~ "5",
                              TRUE ~ split_value)) %>%
  mutate(quint_type="sc_quin") %>%
  select(-split_name, -split_value)
# Data are provided at quintile level (sc_quin)  
# Match to quintile pops
# code       year quintile numerator quint_type


# Save files

# Main
saveRDS(shos_main, file = paste0(data_folder, "Data to be checked/shos_main_shiny.rds"))
write_csv(shos_main, file = paste0(data_folder, "Data to be checked/shos_main_shiny.csv"))

# Pop groups
saveRDS(shos_popgrp_sex, file = paste0(data_folder, "Data to be checked/shos_shiny_popgrp.rds"))
write_csv(shos_popgrp_sex, file = paste0(data_folder, "Data to be checked/shos_shiny_popgrp.csv"))

# Ineq file
saveRDS(shos_popgrp_simd, file = paste0(data_folder, "Prepared Data/shos_shiny_depr_raw.rds"))
write_csv(shos_popgrp_simd, file = paste0(data_folder, "Prepared Data/shos_shiny_depr_raw.csv"))


shos_popgrp_simd <- readRDS(file = paste0(data_folder, "Prepared Data/shos_shiny_depr_raw.rds"))
# 2. Run analysis functions ----
# all data are in the same input file
# function splits them out and saves separately

# create lookup to pass to function:
ind_lookup <- list("30022" =	"influence_local_decisions",
                   "30043"	=	"managing_well_financially",
                   "30024"	=	"neighbourhood_belonging",
                   "30046"	=	"neighbourhood_good_place",
                   "30027"	=	"neighbourhood_trust",
                   "30047"	=	"open_space_use",
                   "30020"	=	"volunteering",
                   "30038"	=	"discrimination",
                   "30041"	=	"harassment",
                   "30025"	=	"feeling_lonely",
                   "30049"	=	"noisy_neighbours",
                   "30045"	=	"high_risk_loans") 

analyze_deprivation_aggregated(filename="shos_shiny_depr", pop = "depr_pop_16+", ind = 30022, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30043, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30024, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30046, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30027, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30047, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30020, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30038, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30041, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30025, lookup = ind_lookup)
analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30049, lookup = ind_lookup)
#analyze_deprivation_aggregated("shos_shiny_depr", pop = "depr_pop_16+", ind = 30045, lookup = ind_lookup) # no SIMD for this indicator





# # Some QA charts:
# ####Charting rate by quintile ----
# 
# chart <- data_depr %>%
#   filter(code=="S00000001" & quintile !="Total")
# 
# p <- plot_ly(data=chart , x=~trend_axis) %>%
#   #Comparator line
#   add_lines(y = ~rate, name = "Life Ex", type = 'scatter', mode = 'lines',
#             color=~quintile, text= ~quintile, hoverinfo="text") %>%
#   layout(bargap = 0.1, margin=list(b = 140), #to avoid labels getting cut out
#          showlegend = FALSE) %>%
#   config(displayModeBar = F, displaylogo = F, editable =F) # taking out toolbar
# 
# p
# 
# 
# ####Charting sii ----
# 
# chart2 <- data_depr %>%
#   filter(code=="S00000001" & quintile =="Total")
# 
# sii_plot <- plot_ly(data=chart2, x=~trend_axis,hoverinfo="text") %>%
#   add_lines(y = ~sii, name = "Absolute inequality (SII)", type = 'scatter', mode = 'lines',
#             line = list(color = '#74add1'))  %>% 
#   #Layout
#   layout(showlegend = FALSE,
#          margin = list(b = 140)) %>% 
#   config(displayModeBar = FALSE, displaylogo = F,  editable =F) # taking out toolbar
# 
# sii_plot
# 
# ####Charting rii ----
# 
# rii_plot <- plot_ly(data=chart2, x=~trend_axis)%>%
#   add_lines(y = ~rii,name = "Relative gap", type = 'scatter', mode = 'lines',
#             line = list(color = '#313695')) %>% 
#   #Layout
#   layout(showlegend = FALSE, margin = list(b = 140))%>%
#   config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
# 
# rii_plot
# 
# ####Charting paf ----
# 
# #preparing data needed, creates two dummy variables for stacked bar chart
# chart3<- data_depr %>%
#   filter(code=="S00000001" & year=="2018" & quintile !="Total") %>%
#   mutate(baseline = rate[quintile == "5"],
#          diff_baseline = rate - rate[quintile == "5"]) %>% 
#   droplevels()
# 
# par_bar_plot <- plot_ly(data = chart3, x = ~quintile, 
#                         textposition="none", hoverinfo="text") %>%
#   add_bars(y = ~baseline, name= "", marker = list(color = "#4da6ff"), showlegend = FALSE) %>%   
#   add_bars(y = ~diff_baseline, name = "Attributable to deprivation", 
#            marker = list(color = "#ffa64d"), showlegend = FALSE) %>% 
#   layout(bargap = 0.1, barmode = 'stack', showlegend = T, 
#          legend = list(x = 0.9, y = 0.9),
#          margin = list(b = 140)) %>% #to avoid labels getting cut out
#   config(displayModeBar = FALSE, displaylogo = F, editable =F) # taking out toolbar
# 
# par_bar_plot 
# 
