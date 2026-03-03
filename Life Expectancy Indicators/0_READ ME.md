## Guide life expectancy/healthy life expectancy indicators & ScotPHO profiles tool: 

# ONS and NRS are the main producers of LE and HLE estimates for UK nations and Scotland.
# Often these agencies work in tandem to produce estimates and release publications so publications come out on the same day (but should be consistent)

# ScotPHO are the only agency that produce and publish life expectancy data for small areas (ie IZ and HSCP locality)

# Its important to be aware there are various different methodologies/tweaks to methodology that can be used when producing LE/HLE estimates. Ideally 
# comparisons of LE or HLE between different time periods/geographies should only be done when consistent methodology applied.
# Within the ScotPHO tool we do allow comparisons of the NRS LE estimates for larger geographies (scotland/NHS board/Council) with the ScotPHO small area
# estimates - these comparisions should be carried out with caution as the methodologies are not strictly equivalent and small area LE estimates are likely to be
# more volatile since they are based on limited data.

# Headline life expectancy estimates are produced annually and will be 3 year period estimates compiled using unabrdiged (single-age) lifetables, 
# these estimates are only produced for the UK nations (ie Scotland) level.   
# Headline life expectancy figures are considered the official estimates for Scotland but there are no equivalent estimates for sub-national areas produced by exactly the same methodology.
# Headline LE estimates are typically published to 2 decimal places (this is a quick and dirty way to check if you are looking at headline or abridged LE).

# Sub-national (ie NHS board/Council) 3-year period life expectancy estimates are released but these are produced using abridged (5 year ageband) life tables (these are often labelled abridged life expectancy estimates)
# To ensure fair comparisons between national & subnation LE estimates another set of 3-year LE estimates for scotland are also published for Scotland using abridged life tables. These are typically published to 1 decimal place.

# It is advised that comparisons between LE estimated produced using abridged and unabridge life tables is avoided, this is part of the reason why ScotPHO profiles includes several different (but very similar looking) indicators.

# There are multiple indicator series relating to life expectancy in the ScotPHO profiles tool (not all geography levels are available for all indicators)

*LIFE EXPECTANCY:*

Indicators: *20101/20102* (mixture of 3 & 5 year depending on geography level)
-main_data: life expectancy (Male & Female) : 3yr for NHS scotland/NHS Board/Council/HSCP sourced from NRS and 5yr for HSC locality and IZ generted by ScotPHO
-popgroup_data:  life expectancy (male/female) by sex (all areas, mixture of 3 and 5 yr) and by urban/rural for Scotland only
-deprivation_data life expectancy inequalities (male & female) *5 YEAR* period (Scotland/NHSboard/Councils), even scotland 5 year to ensure comparability

Indicators: *99142/99142* (3 yr SCOTLAND Only)
-deprivation_data:  life expectancy (male/female) by SIMD quintile/decile 3yr estimates for Scotland only (this is used in SHI profile only)

*HEALTHY LIFE EXPECTANCY*

Indicators: *99101/99102* (all 3 year)
-main_data: healthy life expectancy (male & female) 3yr Scotland/NHSboard/Councils
-popgrp_data: healthy life expectancy (male & female) 3yr Scotland/NHSboard/Councils
-deprivation_data: healthy life expectancy inequalities (male & female) 3yr SCOTLAND only 


In theory each indicator should be updated once per year but the timings of these updates throughout the year vary due to availability of source data.
We may update LE estimates for larger geographies before the HCSP locality and IZ updates (as the smaller geography data relies on release of SAPE populations when the larger geographies do not)

#There should be numerous scripts within the life expectancy sub-folder of profiles tool indicator production

#Scripts that source LE/HLE data from SG open data platorm/external:
0_Sourcing NRS 3yr Sub-national abridged LE.R [prepares raw data for larger geographies of indicators 20101&20102]
0_Sourcing NRS 3 year Scotland deprivation.R [this script is only used for long term monitoring of health inequalities only at Scotland level]

#Scripts that include declare filepaths/R libraries/functions used by other scripts in this project
 1.Functions_life_expectancy.R [needed for generating small area LE]

#Scripts that prepare data files for indicators that appear in the ScotPHO profiles tool
 2.LE_generate small area estimates.R [creates le for small areas]
 3.Profiles Indicators_LE_male and female.R [combined le for large and small geographies and prepares indicator files for shiny]
 4.Profiles Indicators_LE deprivation.R [prepares 5 year inequalities indicator data]
 5.Profiles Indicators_HLE_2025 methodology [prepares main and popgroup for HLE indicators]
 6.Profiles Indicators_HLE_2025 methodology deprivation [prepares deprivation data for HLE indicators]

#scripts used in the past for updating indicators but not actively in use for future updates (keep in case we need to revert) 
 Archived_5. Profiles Indicators_healthy life expectancy.R

#NRS are the main producers of life expectancy (LE) and HLE estimates for Scotland, NHS boards and council areas.  

Only PHS ScotPHO team generate LE for small geographies - ie 2011 datazone and HSCP locality.  The scripts in this folder mostly source LE data from the SG open data platform/NRS website or require 
a bespoke request for data from NRS. It is important to be aware that LE estimates for smaller geographies are calculated over 5 year periods rather than then NRS official estimates which are based on
3 year periods. This longer time period for smaller geographies is required to increase sample size and attempt to improve robustness of the estimates however there are some small geographies where the
is not sufficient sample size to permit calculation - for these geographies ScotPHO will suppress/not calculate LE.
Another difference between LE calculations for IZ/HSCP locality is that the maximum age group used in life tables is 85+ years rather than 90+ years - again this is due to issues with small numbers
(some small areas have zero population in the 90+ age group however some deaths are apparent which causes calculation errors).

To generate new LE figures for 2011 DZ & HSCP locality (including refreshing locality data when boundary changes take place) you will need to run script  
"2.LE_generate small area estimates.R" (which calls functions from the first script " 1.Functions_life_expectancy.R"), then run " 3.Profiles Indicators_LE male & female.R"

The source data for LE inequalities indicator is provided as a distinct file by NRS following a bespoke request to NRS (askinig for 5 year LE figures for Scotland, NHS board and LA by SIMD quintile)
Typically this data is provided at a different time to the official NRS publication and so SIMD splits for LE can't always be run at the same time as other LE indicators.

Healthy Life Expectancy figure are also sourced from NRS but are only available at Scotland, NHS board, Local Authority level - this is because there is no robust information on
self-assessed health for smaller geographies. 

#To update LE for small areas (ie IZ or HSCP locality)
Skip straight to script "2.LE_generate small area estimates.R"

#To update LE for Scotland, NHS board, Councils & HSCP:
Skip straight to script "3.Profiles Indicators_LE male & female.R"

#To update inequalities LE indicator for Scotland, NHS board, Councils:
Skip straight to  "4.Profiles Indicators_LE deprivation.R"

#To update inequalities HLE indicator for Scotland, NHS board, Councils:
Skip straight to  " 5.Profiles Indicators_HLE.R"



