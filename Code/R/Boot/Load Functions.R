# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies
# Documentation ----
# This is the script called by the .Rprofile, and the .reset() function.
# This script handles the loading of all user-defined functions.
# User-Defined Functions are functions that are created by us, the user, to combine existing functionality to create repeatable code.
# In this case, we only have one function to load at startup; the function that will render our PPT files
# Arguments ----
# na
# Returns ----
# na

# Report Generation Functions ----
source("./Code/R/Render/Render Lessons.R")
source("./Code/R/Render/Render GTR Lab.R")
writeLines(sprintf("%s LOADED: Report Rendering Tools", Sys.time()))

# Finish ----
