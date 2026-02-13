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





simd_data <- imap(simd_info, ~ {
  
  
  ## get SIMD lookup from cl-out
  # '.y' refers to the SIMD version e.g. "2020v2"
  phslookups::get_simd_datazone(simd_version = .y)})


##the result file from monicas script has a lot of what i need
#it tells you for each iz which quintile it falls into 



iz<-result |>
  filter(code =="S02001866")|>
  filter(year==2022) 

|>
  filter(code =="S020012362")