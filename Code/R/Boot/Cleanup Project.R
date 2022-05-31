# DemeteR ----
# Written by Chris Snyder, MPH (Roqarr)
## Documentation ----
# Adds some quality of life cleanup, reset, and last functions
## Arguments ----
# na
## Returns ----
# na

# Clean Out the R Directory ----
.cleanup_R_dir = function() {
  unlink("./Database/Temp", recursive = T) # remove the temp directory from database 
  unlink("./Shape Files/Temp", recursive = T) # remove the temp directory from shape files
}

# Reset Project ----
.reset = function() {
  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # removes all objects in global enviornment,
  rm(list=ls(a = T), envir = .GlobalEnv) # removes all hidden objects in global enviornment, 
  if(!is.null(dev.list())) graphics.off() # removes all Plots from viewer
  source("./.Rprofile") # reruns the Boot Code
}

# Exit Function, run on program close ----
.Last = function() {
  .cleanup_R_dir()
}
