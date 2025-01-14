check_file_exists <- function(folder, filename){
  
  # check user has access to folder 
  if(!file.access(folder, mode = 4) >= 0L) {
    cli::cli_abort(
      c(
        "No folder access",
        "x" = "You don't have the appropriate file permissions to {folder}",
        "i" = "Please contact a member of the ScotPHO team."
      )
    )
  }
  
  # get full path to the data file
  path <- file.path(folder, filename)
  
  
  # check file exists
  if (!file.exists(path)) {
    cli::cli_abort(
      c(
        "Prepared data file not found",
        "x" = "File '{filename}' not saved {folder}",
        "i" = "There should be a step within your indicator production R script that saves 
               a prepared 'rds' file ending in '_raw' to be further processed in this function."
      )
    )
  }
  
}

