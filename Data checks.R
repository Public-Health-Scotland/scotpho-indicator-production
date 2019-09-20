#library(foreign) #to read SPSS data.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
#library(RcppRoll) #for moving averages
library(readr) # writing csv's
#library(odbc) # for reading oracle databases
library(readxl) #for reading excel
library(reshape2) # for dcasting/reshape
library(flextable) # for output tables


# Varies filepaths depending on if using server or not.
if (sessionInfo()$platform == "x86_64-redhat-linux-gnu (64-bit)") {
  data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
  lookups <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/" 
} else {
  data_folder <- "//stats/ScotPHO/Profiles/Data/"
  lookups <- "//stats/ScotPHO/Profiles/Data/Lookups/" 
}

#tokens required

filename <- "breastfed"

IZ = TRUE  # Does indicator include IZ level data (#might need token for ADP or HSCP?)
#IZ = FALSE

check_codes =c("S00000001","S08000015","S12000010","S02001236") #selection of some gegraphies to check

locality = TRUE





#Reading file
data_indicator <- readRDS(paste0(data_folder, "Data to be checked/", filename, "_shiny.rds")) 


data_indicator <- data_indicator %>%
  mutate(numerator=case_when(code=='S99000044' & year=="2003"~500.00, TRUE ~numerator))


# test file for what happens if no IZ level data
# data_indicator <- data_indicator %>%
# subset(substr(code,1,3) != "S02")



#Reading last shiny data file loaded for comparisons.
# ?need error message if mathcing file name can't be found?

old_data_indicator <- readRDS(paste0(data_folder, "Shiny Data/", filename, "_shiny.rds")) 



##Manual manipulations to an indicator file to simulate what happens if data point looks odd
old_data_indicator_test <- readRDS(paste0(data_folder, "Shiny Data/", filename, "_shiny.rds")) %>%
  mutate(rate=case_when(code=='S00000001' & year=="2003"~50.00, TRUE ~rate))
old_data_indicator <-old_data_indicator_test



#####################################################################################
##Data checks ----


##Check 1 : What geotypes & years are present,how many unique geographies appear

#Aggegrate file to detect which geographies and years are present.
geo_checks <- data_indicator %>%
  mutate(geo_type=substr(code,1,3)) %>%
  group_by(geo_type, trend_axis) %>%
  summarise(count=n()) %>%
  dcast(trend_axis ~ geo_type)

#Output 1 - summary table of years and geogrpahy types outputted - conditional formatting on counts

# need to make flextable formatting responsive to which geographies are present
# ft_geo_check <- flextable(geo_checks) %>%
#   color(~ S08!=14, ~S08, color = "red") %>% #NHS board should be 14
#   color(~ S12!=32, ~S12, color = "red") %>% #S12 council should be 32
#   color(~ S37!=31, ~S37, color = "red")  #HSCP should be 31
  
  
  if (IZ == TRUE) {
    ft_geo_check <- flextable(geo_checks) %>%
      color(~ S08!=14, ~S08, color = "red") %>% #NHS board should be 14
      color(~ S12!=32, ~S12, color = "red") %>% #S12 council should be 32
      color(~ S37!=31, ~S37, color = "red") %>% #HSCP should be 31
      color(~ S02!=1279,~S02, color = "red") #There should be 1279 IZ
  } else if (IZ==FALSE) {
     ft_geo_check <- flextable(geo_checks) %>%
       color(~ S08!=14, ~S08, color = "red") %>% #NHS board should be 14
       color(~ S12!=32, ~S12, color = "red") %>% #S12 council should be 32
       color(~ S37!=31, ~S37, color = "red")  #HSCP should be 31
  }
  
#print flextable
ft_geo_check 


#Check 2 : How many new rows added in latest update 
new_row <- nrow(data_indicator)
old_row <- nrow(old_data_indicator)
compare_row <-paste0(c(new_row-old_row)," rows of data have been added since last shiny data file")

#print text
compare_row


##Check 3 : How do Scotland counts compare to figures in new & old files

new_Scot <-data_indicator %>% subset(code %in% check_codes)
old_Scot <- old_data_indicator %>% subset(code %in% check_codes)%>%
  select (code, year, numerator, rate, lowci, upci)

#calculate percentage difference between old and new figures
matched <- merge(x = new_Scot, y = old_Scot, by=c("code", "year")) %>%
  mutate(numerator_match =  (numerator.x - numerator.y)/numerator.x,
         rate_match = (rate.x-rate.y)/rate.x,
         lowci_match = (lowci.x-lowci.y)/lowci.x,
         upci_match = (upci.x-upci.y)/upci.x)

ft_geography_check <- flextable(matched,
  col_keys = c("code","year","numerator.x", "numerator.y","numerator_match",
               "rate.x", "rate.y","rate_match",
               "lowci.x","lowci.y", "lowci_match",
               "upci.x","upci.y","upci_match")) %>%
  color(~ numerator_match !=0,~numerator_match, color = "red") %>% #
  color(~ rate_match !=0,~rate_match, color = "red") %>% 
  color(~ lowci_match !=0,~lowci_match, color= "red") %>% 
  color(~ upci_match !=0,~upci_match, color = "red") 

ft_geography_check  #reoder fields so those that should match are next to each other?




##Check 4 : 
##Is Scotland figure approximately central compared to NHS Boards

scot_value <-data_indicator %>% filter((code=="S00000001")& year==max(year)) %>%select(rate)

#Selecting Health boards and Scotland for latest year in dataset
ggplot(data = data_indicator %>% filter((substr(code, 1, 3)=="S08" | code=="S00000001") 
                                        & year== max(year)), aes(code, rate) ) +
  geom_point(stat = "identity") +
  geom_hline(yintercept= scot_value$rate[1], linetype="dashed", color = "red")+
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##Check 5 : 
# ?check on which CA and HSCP locality codes present?
#pick 2 hscp and see how numerators/rates for the constituent localities compare to hscp total
 
#Selecting Council area, HSCP and its localities for latest year in dataset
#test case#1
#city of edinburgh council = S1200036
#city of edinburgh partnership = S37000012
#Edinburgh localities = S99000044,45,46,47

edinburgh_data <- data_indicator %>% 
  filter(code =="S12000036" | code=="S37000012"|code=="S99000044"|code=="S99000045"|code=="S99000046"|code=="S99000047") %>%
  mutate(geotype=substr(code,1,3)) %>%
  group_by(year, geotype) %>%
  summarise(numerator=sum(numerator)) %>%
  dcast(year ~ geotype) %>%
  mutate(ca_partnership=round(S12-S37,2),
         partnership_localities=round(S99-S37,2),
         localities_ca=round(S99-S12,2),
         check_tot=sum(ca_partnership,partnership_localities,localities_ca))


ft_edinburgh_check <- flextable(edinburgh_data) %>%
  color(~ check_tot!=0, ~check_tot, color = "red") %>%
  color(~ ca_partnership!=0, ~ca_partnership, color = "red") %>%
  color(~ partnership_localities!=0, ~partnership_localities, color = "red") %>%
  color(~ localities_ca!=0, ~localities_ca, color = "red") %>%
  autofit()


ft_edinburgh_check 

edinburgh_summary <- 
if(sum(data$check_tot==0)){
 paste0("Yay...looking good")  
  } else{paste0("Uh-oh,something looks a little strange with the numerat Edinburgh council/partnerships/localties")}

edinburgh_summary

ggplot(data = data_indicator %>% 
         filter((code =="S12000036" | code=="S37000012"
                |code=="S99000044"|code=="S99000045"|code=="S99000046"|code=="S99000047") & year== max(year)), aes(code, rate)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#test case#2
#Falkirk = S1200036
#Falkirk = S37000012
#Falkirk(central,east, west) = S99000049,50,51

data <- data_indicator %>% 
  filter(code=="S12000014" | code=="S37000013"|code=="S99000049"|code=="S99000050"|code=="S99000051") %>%
  mutate(geotype=substr(code,1,3)) %>%
  group_by(year, geotype) %>%
  summarise(numerator=sum(numerator)) %>%
  dcast(year ~ geotype) %>%
  mutate(check1=round(S12-S37,2),
         check2=round(S99-S37,2),
         check3=round(S99-S12,2)) %>%
  summarise(council_partnership_match=sum(check1),
            partnership_locality_match=sum(check2),
            council_locality_match=sum(check3))


ggplot(data = data_indicator %>% 
         filter((code=="S12000014" | code=="S37000013"|code=="S99000049"|code=="S99000050"|code=="S99000051") 
                & year== max(year)), aes(code, rate)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymax=upci, ymin=lowci), width=0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





##Check 5 : Look at numerators - is the indicator likely to be robust?
##Assess how many rows contain cell counts of <5 or <10

small_count_data <- data_indicator  %>% 
  mutate(geo_type=substr(code,1,3)) %>%
  group_by(geo_type, year) %>%
  summarise(count=n(),
            u5=sum(numerator<5),
            u10=sum(numerator<10)) %>%
  mutate(percent_U5=u5/count*100,
         percent_U10=u10/count*100)
  

ft_small_count <- flextable(small_count_data)

ft_small_count



rmarkdown::render("Data checks.Rmd")