brief guide to generating life expectancy indicators for ScotPHO profiles tool:

There are 3 indicators relating to life expectancy in the ScotPHO profiles tool

1) life expectancy (Male & Female)
2) life expectancy inequalities (male & female)
3) healthy life expectancy (male & female)

In theory each indicator should be updatable once per year but the time of the year that updates can take place may differ between indicators
There should be five scripts within the life expectancy sub-folder of profiles tool indicator production

 1.Functions_life_expectancy.R
 2.LE_generate small area estimates.R
 3.Profiles Indicators_male & female life expectancy.R
 4.Profiles Indicators_life expectancy inequality.R
 5.Profiles Indicators_healthy life expectancy.R

NRS are the main producers of life expectancy (LE) estimates for Scotland.  ScotPHO only generate LE for small geographies - ie 2011 datazone and HSCP locality.  The scripts in this folder mostly source LE data from the NRS website or require a bespoke request for data from NRS.

To generate new LE figures for 2011 DZ & HSCP locality (including refreshing locality data when boundary cahnges take place) you will need to run script  "2.LE_generate small area estimates.R" (which calls functions from the frist script), then run " 3.Profiles Indicators_male & female life expectancy.R"

LE inequalities indicator data is separate from the main profiles tool indicators on LE- data needs to be sourced from NRS and updates can't always be run at the same time as other LE indicators.

Healthy Life Expectancy figure are also sourced from NRS but are only available at Scotland, NHS board, Local Authority level - this is becuase there is no robust information on self-assessed health for smaller geographies. 
