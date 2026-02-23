


# Deprivation data

nrs_hle_simd <- read_excel((paste0(source_network,"NRS data/healthy-life-expectancy-22-24-data_(NRS publication Feb 2026).xlsx")), sheet = "Table 5",skip = 3) |>
  clean_names() |>
  filter(age_group=="<1")|>
  mutate(trend_axis =paste0(substr(period,1,4),"-",substr(period,9,12)),
         def_period = paste(trend_axis, "(3 year aggregate)"),
         year = as.numeric(substr(trend_axis,1,4))+1, # year for LE is mid-point of aggregate years (this helps line up data series when comparing le & hle which aren't always same periods)
         code = "S00000001",
         numerator = NA, #required by shiny app but is null for HLE
         ind_id = case_when(sex == "Females" ~ 99101,
                            sex == "Males" ~ 99102),
         quintile=as.character(area_code))|>
  rename("rate" = "healthy_life_expectancy",
         "lowci" = "lower_95_percent_confidence_interval",
         "upci" = "upper_95_percent_confidence_interval") |>
  select(-age_group,-area_type,-area_code, -area_name, -period)

nrs_hle_scot_total <-nrs_hle |>
  filter(code=="S00000001" & period != "2013 to 2015") |>
  select(-split_value) |>
  mutate(quintile="Total") |>
  select(-age_group,-area_type, -area_code, -area_name,-period)

depr_data_all <-rbind(nrs_hle_simd,nrs_hle_scot_total) |>
  mutate(quint_type = "sc_quin")


rm(nrs_hle_simd,nrs_hle_scot_total)



