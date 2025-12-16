install.packages("PHEindicatormethods")


###############################################.
## Packages/Filepaths/Functions ----
###############################################.

source("functions/main_analysis.R") # source functions & libraries to run script
source("functions/deprivation_analysis.R") # source functions & libraries to run script


# Extracts for Life Expectancy data saved in life expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"

# Open Health Board LE data
scotland_decile <- read_excel(sheet="decile",paste0(source_network,"Scotland Life Expectancy by SIMD 2009 to 2024.xlsx")) |>
  mutate(quint_type="sc_decile") |>
  rename(quintile=decile)


scotland_quin <- read_excel(sheet="quintile",paste0(source_network,"Scotland Life Expectancy by SIMD 2009 to 2024.xlsx")) |>
  mutate(quint_type="sc_quin") 

data_depr <- bind_rows(scotland_decile,scotland_quin) |>
  mutate(code="S00000001") |> 
  rename(rate=le)


#need population denominators - is it populations aged <0 or is it total population? i think the population aged <1

# maybe contact ons about methodology for calculating sii
