# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies
# Documentation ----
# This funciton builds the interactive web lab for the GT R Lesson
# Arguments ----
# na
# Returns ----
# na

# GT-Lab ----
# Render the bookdown dashboard for Cornerstone Family Programs Internal Use
render_gtr_lab = function() {
  # Set the website directory inside the project dir
  rendir = sprintf("%s/Documentation/Lab/GTR-Lab", getwd())
  
  # Render the website using bookdown
  render_book("./Code/Rmd/GTR-Lab", 
              output_dir = rendir
              )
  
}
