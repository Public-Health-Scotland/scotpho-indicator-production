#Create lookup which details the parent areas of smaller geographies (ie which IZ belong to which territories, locality to HSCP and ca to NHS board etc)
# This lookup is used in QA reporting to check small area rates/numerators agree with figures presented for larger geographies.
# Issues in the past when HSCP locality lookup incorrect or if null/missing cases for some age/sex groups

create_geo_parents <- function() {

#read in geography lookup which allows mapping of parent/child areas (e.g. which CA are within which NHS board etc)
geography_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")

parents_iz<-geography_lookup |>
  select(intzone2011, hscp_locality, hscp2019, ca2019, hb2019,adp,pd) |>
  unique() |>
  rename(code=intzone2011)
  
parents_locality<-geography_lookup |>
  select(hscp_locality, hscp2019, ca2019, hb2019,adp,pd) |>
  unique()|>
  rename(code=hscp_locality)

parents_ca<-geography_lookup |>
  select(ca2019, hb2019,adp,pd) |>
  unique()|>
  rename(code=ca2019)

master_parent_geos<<- bind_rows(parents_iz,parents_locality, parents_ca)

}
