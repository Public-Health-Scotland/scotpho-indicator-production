# indicator-production
Code used to prepare data for indicators in the [ScotPHO's profiles](https://scotland.shinyapps.io/ScotPHO_profiles_tool/).

Input files for the various indicators are sourced either direct from datasources such as SMRA (SMR01/NRS deaths), opendata  portals or via bespoke requests to data owners.
Final output files need to have the same format and follow a set of common transformations.
Most input files are processed using scripts within the functions folder (pre 2025 functions found within scripts "1.indicator_analysis"/"2.deprivation_analysis"). 
For details on how to use these functions please read the instruction in the script.
These functions can prepare data for intermediate zones, HSC localities, HSC partnerships, alcohol and drug partnerships, council areas, health boards and Scotland.

All indicators have a 'main' output file (this is what populate summary/trend and rank tabs within online profiles tool).
For some indicators we also prepare data by socioeconomic deprivation or population group categories.

Geography and population lookups used in the creation of indicator output files are created with the code from [this repository](https://github.com/Public-Health-Scotland/scotpho-lookups).

##Licensing
Source code: Licensed under the MIT License. 
Underlying Data: Licensed under the Open Government Licence (OGL) v3.0.
