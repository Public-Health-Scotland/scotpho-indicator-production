Brief guide to generating life expectancy indicators for ScotPHO profiles tool:

There are 3 indicators relating to life expectancy in the ScotPHO profiles tool

1) life expectancy (Male & Female) - Scotland/NHSboard/Councils/HSCP - HSCP locality & IZ
2) life expectancy inequalities (male & female) Scotland/NHSboard/Councils/HSCP
3) healthy life expectancy (male & female) Scotland/NHSboard/Councils/HSCP

In theory each indicator should be updated once per year but the timings of these updates throughout the year vary due to availability of source data.
Sometimes we may update LE estimates for larger geographies before the HCSP locality and IZ updates (as the smaller geography data relies on release of SAPE populations however the larger geographies do not)

There should be five scripts within the life expectancy sub-folder of profiles tool indicator production

 1.Functions_life_expectancy.R
 2.LE_generate small area estimates.R
 3.Profiles Indicators_male & female life expectancy.R
 4.Profiles Indicators_life expectancy inequality.R
 5.Profiles Indicators_healthy life expectancy.R

NRS are the main producers of life expectancy (LE) estimates for Scotland.  
Only PHS ScotPHO team generate LE for small geographies - ie 2011 datazone and HSCP locality.  The scripts in this folder mostly source LE data from the NRS website or require a bespoke request for data from NRS. It is important to be aware that LE estimates for smaller geographies are calculated over 5 year periods rather than then NRS official estimates which are based on 3 year periods. This longer time period for smaller geographies is required to increase sample size and attempt to improve robustness of the estimates however there are some small geographies where the is not sufficient sample size to permit calculation - for these geographies ScotPHO will suppress/not calculate LE.
Another difference between LE calculations for IZ/HSCP locality is that the maximum age group used in life tables is 85+ years rather than 90+ years - again this is due to issues with small numbers (some small areas have zero population in the 90+ age group however some deaths are apparent which causes calculation errors).

To generate new LE figures for 2011 DZ & HSCP locality (including refreshing locality data when boundary changes take place) you will need to run script  "2.LE_generate small area estimates.R" (which calls functions from the first script), then run " 3.Profiles Indicators_male & female life expectancy.R"

The source data for LE inequalities indicator is separate provided by NRS following a bespoke request, typically this data is provided at a different time to the official NRS publication and so SIMD splits for LE can't always be run at the same time as other LE indicators.

Healthy Life Expectancy figure are also sourced from NRS but are only available at Scotland, NHS board, Local Authority level - this is because there is no robust information on self-assessed health for smaller geographies. 




To update LE for Scotland, NHS board, Councils:

