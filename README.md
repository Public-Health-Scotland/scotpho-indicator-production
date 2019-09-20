# indicator-production
Code used to prepare data for indicators in the [ScotPHO's profiles](https://scotland.shinyapps.io/ScotPHO_profiles_tool/).

Final files need to have the same format and follow a set of common transformations. This way most of the indicators can be processed using the functions from "1.indicator_analysis". For details on how to use these functions please read the instruction in the script.
These functions can prepare data for intermediate zones, HSC localities, HSC partnerships, alcohol and drug partnerships, council areas, health boards and Scotland.

For some indicators we also prepare data by socioeconomic deprivation categories. To do this use the functions from "2.deprivation_analysis". Their approach is very similar to the other ones, but you can find more detailed instructions in the function script.

Both functions use the geography and population lookups created with the code from [this repository](https://github.com/ScotPHO/lookups).
