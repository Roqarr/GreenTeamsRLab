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
  source("./Code/R/Boot/Prepare Project.R") # Load the Project in Development mode, for use by CSS
} 

if (.devmode == F) {
  writeLines(sprintf("Welcome to the Green Teams R Lab, a sample R project.
  
You are currently working out of the Green Teams R Lab Directory, inside of an R Project.
Your current working directory is: %s

The behavior of our R project is influenced by the contents of the .Rprofile file.
Each time R is started, R will check for the existence of a .Rprofile file in the working directory.
If R finds the .Rprofile file, it will run the contents of that file.
In this case, the .Rprofile is presenting this help information to you at startup.

If you need additional assistance, or have any questions, please contact:
Chris Snyder, snyderc@montclair.edu
             ", getwd()))
}

