run_qa <- function(filename, old_file="default", check_extras=c(), type = c("main", "deprivation", "popgrp"), test_file){
  if(type == "main"){
  run("3.Data Quality Checks.Rmd")
  } else if (type=="deprivation") {
    run("4.Data Quality Checks_inequalities indicators.Rmd")  
  } else if (type=="popgrp") {
    run("5.Data Quality Checks_population groups.Rmd")
  }
    
    
}  