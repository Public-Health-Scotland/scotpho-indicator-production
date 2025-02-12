run_qa <- function(filename, old_file="default", check_extras=c(), type = c("main", "deprivation"), test_file){
  if(type == "main"){
  run("../scotpho-indicator-production/3.Data Quality Checks.Rmd")
  } else {
    run("../scotpho-indicator-production/4.Data Quality Checks_inequalities indicators.Rmd")  
  }
}  