# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies, Green Team Internship Program
# Documentation ----
# When R is run, it will first look for any file named '.Rprofile' inside the directory it was started in.
# When you load this R Project, R will start inside of this project directory, and will first look for this file.
# This file is automatically run at R Project start-up.
# You can use this file to run any additional files that you want at project start-up, to have a consistent R Environment each time.
# Check the ./Code/R/Boot directory for the functions that are loaded on project boot.
# Arguments ----
# .devmode   Internal Flag to run development startup while editing or rendering documents.
# Returns ----
# na

# Project Start-up ----
.devmode = T

if (.devmode == T) {
  source("./Code/R/Boot/Prepare Project.R") # Load the Project  
} 

if (.devmode == F) {
  writeLines("Welcome to the Green_Teams Sample R Project.
  

If you need additional assistance, or have any questions, please contact:
Chris Snyder, snyderc@montclair.edu
             ")
}

# If this is your first time using the project, and you have not installed the recommended packages yet, please do so by executing the following scripts based on operating system.
# 
# If you are on Windows, please enter the following command into the R console:
#   source('./Code/R/Boot/Check Packages Windows.R')
# 
# If you are on Mac or Linux, please enter the following command into the R console:
#   source('./Code/R/Boot/Check Packages Unix.R')
