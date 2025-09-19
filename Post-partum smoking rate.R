# ScotPHO indicator: post partum smoking rate 1552

# Produces indicators looking at number & percentage of mothers/main carers recorded as current smoker at first antenatal visit.
# The main indicator datset is based on 3 year rolling averages but the deprivation split uses 5 year rolling averages as numbers are 
# too small to produce robust rates when split into quintiles 

# raw data requested from child health team and is processed using analysis function however there is some post function processing to remove
# unwanted geographies and suppress numerators where counts are small.
# note that although small counts are suppressed we still release percentages based on small counts given there is ambiguity introduced by using 3/5 year rolling rates
# and for deprivation data self identifition wouldl also require knowledge ofwhich datazones were in which deprivation quintile


#   Part 1 - Prepare basefile
#   Part 2 - Run analysis functions
#   Part 3 - Apply post function suppressions

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("./functions/main_analysis.R") #Normal indicator functions
source("./functions/deprivation_analysis.R") #Deprivation function

###############################################.
## Part 1 - Prepare basefile ----
###############################################.

#Data comes from child health team
postpartum <- read.csv(file.path(profiles_data_folder, '/Received Data/Post-partum Smoking/IR2025-00008.csv')) |>  
  janitor::clean_names() |> 
  mutate(year=case_when(nchar(fin_year)==3 ~ paste0("200",substr(fin_year,1,1)), 
                        TRUE ~ paste0("20",substr(fin_year,1,2)))) #format year to display financial year

postpartum <- postpartum |> 
  rename(code = datazone2011, 
         numerator = smoker,
         denominator = total_valid_status) |> 
  select(-fin_year) |> 
  mutate(year = as.numeric(year))

# remove this code chunk when everyone happy with option to present main 3yrs and depr 5 years
# #Aggregating over three-year period
# postpartum_test <- postpartum |> 
#   group_by(code) |> 
#   mutate(year2 = lead(year, 1),
#          year3 = lead(year, 2),
#          num2 = lead(numerator, 1),
#          num3 = lead(numerator, 2),
#          denom2 = lead(denominator, 1),
#          denom3 = lead(denominator, 2)) |> 
#   filter(!is.na(year3)) |> 
#   reframe(numerator = numerator + num2 + num3,
#             denominator = denominator + denom2 + denom3,
#           year = year2)

saveRDS(postpartum, file.path(profiles_data_folder, '/Prepared Data/postpartum_smoking_raw.rds'))



###############################################.
## Part 2 - Run analysis functions ----
###############################################.

main_analysis(filename = "postpartum_smoking", geography = "datazone11", measure = "percent",
              yearstart = 2002, yearend = 2023, time_agg = 3, ind_id = 1552, year_type = "financial")



deprivation_analysis(filename = "postpartum_smoking", yearstart = 2002, yearend = 2023,
                     time_agg = 5, year_type = "financial", measure = "percent", pop_sex = "all",
                     ind_id = 1552)


###############################################.
## Part 3 - Consider dislcosure & apply suppressions ----
###############################################.

# MAIN DATA (3 year rolling average)----
# After inspecting output data consider robustness of estimates, especially areas where majority of counts are less than 5
# decision: remove IZ level data and suppress counts (but leave %) when 3 year rolling average less than 5 
main_data <- readRDS(file.path(profiles_data_folder, '/Data to be checked/postpartum_smoking_shiny.rds'))|>
  filter(substr(code,1,3) != "S02")|> # Remove the IZ level gepgraphy as numbers are not sufficient to release robust rates at this geography level
  mutate(numerator = case_when(numerator > 0 & numerator < 5 ~ as.numeric(NA), TRUE ~ numerator)) #suppress numerator where count less than 5 - rate is preserved

# write main data after suppression (should overwrite )
write.csv(main_data, paste0(profiles_data_folder, "/Data to be checked/postpartum_smoking_shiny.csv"), row.names = FALSE)
write_rds(main_data, paste0(profiles_data_folder, "/Data to be checked/postpartum_smoking_shiny.rds"))

run_qa(filename="postpartum_smoking",type="main",test=FALSE)


#DEPRIVATION DATA (5 year rolling average)----
# After inspecting output data consider robustness of estimates,
# decision: suppress numerators where average counts over 5 years less than 5
# leaving in HB and CA deprivation splits even though some island/higland boards not all quintiles present - this might be revisited at some point
depr_data <- readRDS(file.path(profiles_data_folder, '/Data to be checked/postpartum_smoking_ineq.rds'))|>
  mutate(numerator = case_when(numerator > 0 & numerator < 5 ~ as.numeric(NA),TRUE ~ numerator))

run_qa(filename="postpartum_smoking",type="deprivation",test=FALSE)


##END
