run_qa <- function(filename, old_file="default", check_extras=c(), type = c("main", "deprivation"), test_file = FALSE){
  if(type == "main"){
  run("3.Data Quality Checks.Rmd")
  } else {
    run("4.Data Quality Checks_inequalities indicators.Rmd")  
  }
}  