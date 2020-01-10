# ScotPHO indicators: Maternities with drug use

#   Part 1 - Create basefile
#   Part 2 - Computing rates and adding labels

###############################################.
## Packages/Filepaths/Functions ----
###############################################.
source("1.indicator_analysis.R") #doesn't use the functions, but quick way of getting packages and folders

###############################################.
## Part 1 - Create basefile ----
###############################################.
#Bringing old data not present in open data platform. CHECK that in open data platform 
#they keep the whole trend too. If not incorporate the oldest data to this file.
#Data available at Health Board level prior 2008/09-2010/11 based on pre 2014 health board boundaries. 
drugmat_old <- readRDS(file=paste0(data_folder, 'Prepared Data/maternity_drug_old_do_not_delete.rds'))

#Now extract data from open data platform
drugmat_ca <- read_csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/3e96277a-9029-4390-ab90-ec600f9926a5/download/11.6_ca_drugmisuse.csv") %>%
  setNames(tolower(names(.))) %>%   #variables to lower case
  rename(code = ca2011) #to allow merging

drugmat_hb <- read_csv("https://www.opendata.nhs.scot/dataset/df10dbd4-81b3-4bfa-83ac-b14a5ec62296/resource/8c8377e1-b1c7-48e7-b313-79eb5ac3c110/download/11.6_hb_drugmisuse.csv") %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  rename(code = hbr2014) %>%  select(-hbr2014qf) #to allow merging

#Merging together ca and hb
data_drugmat <- rbind(drugmat_ca, drugmat_hb) %>% 
  #selecting only totals and hb, ca and scotland
  filter(simdquintileqf == "d" &
           substr(code, 1, 3) %in% c("S92", "S08", "S12")) %>% 
  rename(trend_axis = financialyears, numerator = drugmisuse, denominator = maternities) %>% 
  select(trend_axis, code, numerator, denominator) %>% 
  #reformatting year to style needed
  mutate(year = as.numeric(paste0(substr(trend_axis, 1, 2), substr(trend_axis, 6, 7))))

data_drugmat <- rbind(data_drugmat, drugmat_old) 

#Now, we need ADP level, so selecting councils and recoding codes.
# both lanarkshires CA are one ADP and Mid and East lothian are one ADP
drugmat_adp <- data_drugmat %>% filter(substr(code,1,3) == "S12") %>% 
  mutate(code = case_when(
    code == "S12000005" ~ "S11000005", code == "S12000006" ~ "S11000006", code == "S12000008" ~ "S11000008", 
    code == "S12000010" ~ "S11000051", code == "S12000011" ~ "S11000011", code == "S12000014" ~ "S11000013", 
    code == "S12000017" ~ "S11000016", code == "S12000018" ~ "S11000017", code == "S12000019" ~ "S11000051", 
    code == "S12000020" ~ "S11000019", code == "S12000021" ~ "S11000020", code == "S12000026" ~ "S11000025", 
    code == "S12000028" ~ "S11000027", code == "S12000029" ~ "S11000052", code == "S12000030" ~ "S11000029", 
    code == "S12000033" ~ "S11000001", code == "S12000034" ~ "S11000002", code == "S12000035" ~ "S11000004", 
    code == "S12000036" ~ "S11000012", code == "S12000038" ~ "S11000024", code == "S12000039" ~ "S11000030", 
    code == "S12000040" ~ "S11000031", code == "S12000041" ~ "S11000003", code == "S12000042" ~ "S11000007", 
    code == "S12000050" ~ "S11000052", code == "S12000045" ~ "S11000009", code == "S12000049" ~ "S11000015", 
    code == "S12000047" ~ "S11000014", code == "S12000048" ~ "S11000023", code == "S12000013" ~ "S11000032", 
    code == "S12000027" ~ "S11000026", code == "S12000023" ~ "S11000022", TRUE ~ "Error")) %>% 
  group_by(year, code, trend_axis) %>% summarise_all(sum, na.rm=T) %>% ungroup()

data_drugmat <- rbind(data_drugmat, drugmat_adp) %>% 
  mutate(code = recode(code, "S92000003" = "S00000001")) # recoding Scotland

###############################################.
## Part 2 - Computing rates and adding labels ----
###############################################.
data_drugmat <- data_drugmat %>% 
  #create 3-year average values.
  mutate(numerator = numerator/3,
         denominator = denominator/3,
  # calculate the rate and the confidence intervals (Byars method)
         rate = numerator/denominator*1000,
         o_lower = numerator *(1-1/9/numerator-1.96/3/sqrt(numerator))^3,
         o_upper = (numerator+1) *(1-1/9/(numerator+1)+1.96/3/sqrt(numerator+1))^3,
         lowci = o_lower/(denominator)*1000,
         upci = o_upper/(denominator)*1000) %>% 
  select(-o_upper,- o_lower, -denominator) %>% 
  # add in the definition period label.
  mutate(def_period = paste0(substr(trend_axis, 1, 7), " to ", substr(trend_axis, 9, 15),
                    " ", "financial years; 3-year aggregates"),
        ind_id = 4129) #indicator number

#Including both rds and csv file for now
saveRDS(data_drugmat, file = paste0(data_folder, "Data to be checked/maternity_druguse_shiny.rds"))
write_csv(data_drugmat, path = paste0(data_folder, "Data to be checked/maternity_druguse_shiny.csv"))

##END