#########################################################
# Scottish Household Survey data import
#########################################################

# Update ScotPHO indicators for mental health profile:

# 30022	Adults agreeing they can influence local decisions
# 30043	Households managing well financially (household level: sex==Total)
# 30024	Adults feeling they belong to their local neighbourhood
# 30046	Adults rating neighbourhood as a very good place to live
# 30027	Adults trusting most people in their neighbourhood
# 30047	Adults regularly using or passing through local open space
# 30020	Adults volunteering in past year
# 30038	Adults experiencing discrimination in past year (use CWB ind_id of 99134)
# 30041	Adults experiencing harassment in past year (use CWB ind_id of 99135)
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
#   Part 2 - Prepare data for shiny
#   Part 3 - Prepare final files for checking

### functions/packages ----

source("1.indicator_analysis.R") 
source("2.deprivation_analysis.R") 

library(stringr) #for string manipulation
library(rio) #for importing data


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
                              indicator == "DISCRIM" ~ 99134, 
                              indicator == "HARASS" ~ 99135)) %>%
    mutate(ind_name = case_when(ind_id == 30022  ~	"influence_local_decisions",
                                ind_id == 30043	~	"managing_well_financially",
                                ind_id == 30024	~	"neighbourhood_belonging",
                                ind_id == 30046	~	"neighbourhood_good_place",
                                ind_id == 30027	~	"neighbourhood_trust",
                                ind_id == 30047	~	"open_space_use",
                                ind_id == 30020	~	"volunteering",
                                ind_id == 99134	~	"discrimination",
                                ind_id == 99135	~	"harassment",
                                ind_id == 30025	~	"feeling_lonely",
                                ind_id == 30049	~	"noisy_neighbours",
                                ind_id == 30045	~	"high_risk_loans")) %>%
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
  mutate(code="S00000001", # Scotland only
         split_name= "Deprivation (SIMD)") 

shos_sex <- import_shos_xlsx("_5", file) %>%
  rename(split_value = randgender_ts) %>%
  mutate(code="S00000001", # Scotland only
         split_name= "Sex") %>%
  filter(is.na(hihgender_ts)) %>% # drop the indicators measured at hhd level (sex=meaningless)
  select(-hihgender_ts)


### 2 - Prepare data for shiny ----

# a) main data

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

# b) popgroup data

# popgroup data (split = sex, National level)
shos_popgrp <- shos_sex %>%
  filter(!(split_value %in% c("Identify in another way", "Prefer not to say")))  # keep M and F sex only

# get totals for the splits (because no 'all' in the sex data)
shos_total_sex <- shos_popgrp %>%
  select(code, trend_axis) %>% 
  distinct() %>% # the groups we need totals for
  merge(y=shos_main, by=c("code", "trend_axis")) %>% # the totals for those groups
  mutate(split_name="Sex",
         split_value = "Total") 

# Add totals in
shos_popgrp <- bind_rows(shos_popgrp, shos_total_sex) 

# c) inequals data (raw, before running deprivation analysis)
# National level only 
shos_simd <- shos_simd %>%
  mutate(quintile = case_when(split_value=="Quintile 1- 20% most deprived" ~ "1",
                              split_value=="Quintile 2" ~ "2",
                              split_value=="Quintile 3" ~ "3",
                              split_value=="Quintile 4" ~ "4",
                              split_value=="Quintile 5 - 20% least deprived" ~ "5")) %>%
  mutate(quint_type="sc_quin") %>%
  select(-split_name, -split_value)

# get totals for the splits (because no 'all' in the simd data)
shos_total_simd <- shos_simd %>%
  select(code, trend_axis) %>% 
  distinct() %>% # the groups we need totals for
  merge(y=shos_main, by=c("code", "trend_axis")) %>% # the totals for those groups
  mutate(quint_type="sc_quin",
         quintile = "Total") 

# Add totals in
shos_simd <- bind_rows(shos_simd, shos_total_simd) 


### 3. Prepare final files -----

# Function to prepare final files: main_data, popgroup, and ineq
prepare_final_files <- function(indicator_name){
  
  # 1 - main data (ie data behind summary/trend/rank tab)
  # Contains Scotland, LA and HB data (single years, or 2yr aggregates)
  main_data_final <- shos_main %>% 
    filter(ind_name == indicator_name) %>% 
    unique() %>%
    select(-ind_name)
  
  # Save files
  write.csv(main_data_final, paste0(data_folder, "Test Shiny Data/", indicator_name, "_shiny.csv"), row.names = FALSE)
  write_rds(main_data_final, paste0(data_folder, "Test Shiny Data/", indicator_name, "_shiny.rds"))
  # save to folder that QA script accesses:
  write_rds(main_data_final, paste0(data_folder, "Data to be checked/", indicator_name, "_shiny.rds"))
  
  # Make data created available outside of function so it can be visually inspected if required
  main_data_result <<- main_data_final

  
  if(indicator_name!="high_risk_loans") { # don't run for the loans indicator, as too few obs
    
  # 2 - population groups data (ie data behind population groups tab)
  # Contains Scotland data by sex (single year or 2y aggregate)
    pop_grp_data_final <- shos_popgrp %>% 
      filter(ind_name == indicator_name) %>% 
      unique() %>%
      select(-ind_name)

  # Save
  write.csv(pop_grp_data_final, paste0(data_folder, "Test Shiny Data/", indicator_name, "_shiny_popgrp.csv"), row.names = FALSE)
  write_rds(pop_grp_data_final, paste0(data_folder, "Test Shiny Data/", indicator_name, "_shiny_popgrp.rds"))
  # save to folder that QA script accesses: (though no QA for popgroups files?)
  write_rds(pop_grp_data_final, paste0(data_folder, "Data to be checked/", indicator_name, "_shiny_popgrp.rds"))
  
  # 3 - SIMD data (ie data behind deprivation tab)
  # Contains Scotland data by SIMD quintile (single year or 2y aggregate)
  
  # Process SIMD data
  simd_data <- shos_simd %>% 
    filter(ind_name == indicator_name) %>% 
    unique() %>%
    select(-ind_name)
  
  
  # Save intermediate SIMD file
  write_rds(simd_data, file = paste0(data_folder, "Prepared Data/", indicator_name, "_shiny_depr_raw.rds"))
  write.csv(simd_data, file = paste0(data_folder, "Prepared Data/", indicator_name, "_shiny_depr_raw.csv"), row.names = FALSE)
  
  #get ind_id argument for the analysis function 
  ind_id <- unique(simd_data$ind_id)

  # Run the deprivation analysis (saves the processed file to 'Data to be checked')
  analyze_deprivation_aggregated(filename = paste0(indicator_name, "_shiny_depr"), 
                                 pop = "depr_pop_16+", # these are adult (16+) indicators, with no sex split for SIMD
                                 ind_id, 
                                 indicator_name
                                 )
  
  # Make data created available outside of function so it can be visually inspected if required
  pop_grp_data_result <<- pop_grp_data_final
  simd_data_result <<- simd_data
  
  
  }
  
}



# Prepare the final files 
prepare_final_files(indicator_name = "influence_local_decisions")
prepare_final_files(indicator_name = "managing_well_financially")
prepare_final_files(indicator_name = "neighbourhood_belonging")
prepare_final_files(indicator_name = "neighbourhood_good_place")
prepare_final_files(indicator_name = "neighbourhood_trust")
prepare_final_files(indicator_name = "open_space_use")
prepare_final_files(indicator_name = "volunteering")
prepare_final_files(indicator_name = "discrimination")
prepare_final_files(indicator_name = "harassment")
prepare_final_files(indicator_name = "feeling_lonely")
prepare_final_files(indicator_name = "noisy_neighbours")
prepare_final_files(indicator_name = "high_risk_loans") 


# Run QA reports 
# main data
run_qa(filename = "influence_local_decisions")
run_qa(filename = "managing_well_financially")
run_qa(filename = "neighbourhood_belonging")
run_qa(filename = "neighbourhood_good_place")
run_qa(filename = "neighbourhood_trust")
run_qa(filename = "open_space_use")
run_qa(filename = "volunteering")
run_qa(filename = "discrimination")
run_qa(filename = "harassment")
run_qa(filename = "feeling_lonely")
run_qa(filename = "noisy_neighbours")
run_qa(filename = "high_risk_loans") # "Warning: Error in eval: object 'S08' not found"


# ineq data: failing because the data aren't available at HB level (fix the .rmd later) "Warning: Error in eval: object 'S08' not found"
run_ineq_qa(filename = "influence_local_decisions") # Warning: Error in eval: object 'S08' not found"
run_ineq_qa(filename = "managing_well_financially")
run_ineq_qa(filename = "neighbourhood_belonging")
run_ineq_qa(filename = "neighbourhood_good_place")
run_ineq_qa(filename = "neighbourhood_trust")
run_ineq_qa(filename = "open_space_use")
run_ineq_qa(filename = "volunteering")
run_ineq_qa(filename = "discrimination")
run_ineq_qa(filename = "harassment")
run_ineq_qa(filename = "feeling_lonely")
run_ineq_qa(filename = "noisy_neighbours")
run_ineq_qa(filename = "high_risk_loans") 


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
