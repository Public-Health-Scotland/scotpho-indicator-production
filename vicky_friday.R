
#a<-phslookups::get_simd_datazone(simd_version = "2020v2")

result <- imap_dfr(simd_pop_data, ~ mutate(.x, simd_version = .y))


# pivot data longer to create 1 geography col
result <- result |>
  pivot_longer(
    cols = c("datazone", "intzone2011", "hscp_locality", "ca2019", "hscp2019", "hb2019", "scotland"), 
    names_to = "geo_type", 
    values_to = "code")




# Function to create indicator data for different domains/populations 
create_data <- function(data = data, 
                        domain = c("overall", "access", "income", "crime", "employment","health","education","housing")
                        # population = c("all_ages", "under26", "working_age")
                        #ind_id, yearstart, yearend
){
  
  # name of domain col (e.g. 'access_quint_all_ages)
  domain_col <- paste0(domain, "_quint_all_ages")
  
  # list of columns to include in group_by (this takes parameter value so domain can be varied e.g. 'access_quint_all_ages)
   grouping_var <-c("code", "year", {{domain_col}}, "geo_type", "simd_version")
   
   data <- data |>
     # select cols
     select(code, year, {{domain_col}}, all_ages,under26, working_age, index_year, geo_type, simd_version)|>
     #group and calculate populations for each of the simd quintiles
     group_by(pick(all_of(grouping_var))) |>
     summarise(pop_all_ages=sum(all_ages),
               pop_u26=sum(under26),
               pop_working=sum(working_age),
               dz_count=n())|>
     ungroup() |> #remove initial grouping
     #re-group data to add overall population totals irrespective of SIMD quintile
     group_by(code,year) |>
     mutate(total_pop_all_ages=sum(pop_all_ages),
            total_pop_u26 = sum(pop_u26),
            total_pop_working=sum(pop_working))|>
     ungroup()|>
     # calculate % of total (age-specific) population within each quintile & add columns to identify which simd domain
     mutate(percent_all=round(pop_all_ages/total_pop_all_ages*100,1),
            percent_u26=round(pop_u26/total_pop_u26*100,1),
            percent_working=round(pop_working/total_pop_working*100,1),
            simd_domain={{domain}},
            centile_type="quintile")
}

# result is the file generated in the population deprived script at line 299



overall_domain <-result|>
  create_data(domain="overall") |>
  rename(quintile=overall_quint_all_ages)

income_domain <-result|>
  create_data(domain="income")|> 
  rename(quintile=income_quint_all_ages)

access_domain <-result|>
  create_data(domain="access") |> 
rename(quintile=access_quint_all_ages)
       
employment_domain <-result|>
  create_data(domain="employment") |>
  rename(quintile=employment_quint_all_ages)

health_domain <-result|>
  create_data(domain="health") |>
  rename(quintile=health_quint_all_ages)

education_domain <-result|>
  create_data(domain="education") |>
  rename(quintile=education_quint_all_ages)

housing_domain <-result|>
  create_data(domain="housing") |>
  rename(quintile=housing_quint_all_ages)

#combine all years
populations_by_simd_centiles <- rbind(overall_domain,income_domain,access_domain,employment_domain,health_domain,education_domain,housing_domain)

#apply filters to some years as the dataset is so large (in excess if 1.3 million rows)
#maybe limit to years which SIMD produced and latest year? 
populations_by_simd_centiles <- populations_by_simd_centiles |>
  filter(year %in% c(2004,2006,2009,2012,2016,2020,2023))


populations_by_simd_centiles2 <-populations_by_simd_centiles |>
  select(-total_pop_all_ages,-total_pop_u26,-total_pop_working) |>
  pivot_longer(cols = c("pop_all_ages", "pop_u26", "pop_working","percent_all","percent_u26","percent_working"),
               names_to = "population_group",
               values_to = "population")


populations_by_simd_centiles3 <-populations_by_simd_centiles2 |>
  pivot_wider(names_from = "quintile",
              values_from = "population")




#if we needed rows for all quintiles even if the rows are zero then this can be used
#result3$overall_quint_all_ages <- factor(result3$overall_quint_all_ages, levels = c("1", "2","3","4","5"))
# result4<-result3 |>
#   group_by(year,code,geo_type,simd_version)|>
#   complete(overall_quint_all_ages = c("1","2","3","4","5"), fill = list(pop_all_ages = 0))
# rm(result4)
# result3 %>% complete(overall_quint_all_ages)


populations_by_simd_centiles2 <-populations_by_simd_centiles |>
  select(-total_pop_all_ages,-total_pop_u26,-total_pop_working) |>
  pivot_wider(names_from = quintile,
              values_from = )

|> # drop the total populations columns which are repeated
  # pivot_longer(cols = c("pop_all_ages", "pop_u26", "pop_working"), 
  #              names_to = "population_group", 
  #              values_to = "population")
  pivot_longer(
    cols = c(starts_with("pop"),starts_with("percent")),
             names_to = c("pop_group", ".value"),
             names_sep = "_",
             values_to = "pop")
    
  
  
pivot_longer(cols  = -ids, names_to = c(".value", "group"), 

             df_long <- df %>%
               pivot_longer(
                 cols = c(starts_with("pop"),starts_with("percent"),
                 names_to = c("pop_group", ".value"),
                 names_sep = "_",
                 values_to = "pop"
               )
             
             
|>
  pivot_longer(cols = c("percent_all", "percent_u26", "percent_working"), 
                names_to = "population_group", 
                values_to = "population") 


saveRDS(populations_by_simd_centiles, file=paste0(profiles_data_folder, '/Test Shiny Data/testfile_population_by_simd_centiles.rds'))








df <- tibble(
  student = c("A", "A", "B", "B"),
  subject = c("Math", "English", "Math", "English"),
  score   = c(90, 85, 78, 88)
)

df %>% pivot_wider(
  names_from = subject, 
  values_from = score
)
