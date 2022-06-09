# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies
# Documentation ----
# This funciton builds the interactive web lab for the GT R Lesson
# Arguments ----
# na
# Returns ----
# na

# GTR Lab ----
# Render the bookdown dashboard for Cornerstone Family Programs Internal Use
# All rmd docs not prefaced by `_` are rendered and place in the output directory:  `./Reports/Lab/`
render_gtr_lab = function() {
  # Set the website directory inside the project dir
  rendir = sprintf("%s/Reports/Lab/GTR-Lab", getwd())
  docdir = sprintf("%s/docs", getwd())
  
  # Render the website using bookdown
  render_book("./Code/Rmd/GTR-Lab",
              output_dir = rendir
              )
  
  # Render the website to git docs using bookdown
  render_book("./Code/Rmd/GTR-Lab", 
              output_dir = docdir
  )
  
}

# GTR Docs ----
render_gtr_docs = function() {
  # Set the website directory inside the project dir
  docdir = sprintf("%s/docs", getwd())
  
  # Render the website to git docs using bookdown
  render_book("./Code/Rmd/GTR-Lab", 
              output_dir = docdir
  )
  
}

# GTR Powerpoints ----
# Renders the Powerpoint documents containing the R Lessons
# N number of R lessons are rendered and placed in the output directory: `./Reports/Lessons/`
render_gtr_lessons = function() {
  # Determine the list of lessons to render
  lessons = grep(pattern = "xx.", # omit lessons in progress, numbered as a 'xx.'
                 x = dir("./Code/Rmd/Lessons/"),
                 value = T,
                 ignore.case = T,
                 invert = T
  )
  
  for (i in lessons) {
    writeLines(dir(sprintf("./Code/Rmd/Lessons/%s", i), 
                   pattern = ".rmd", 
                   full.names = T
    )
    )
    
    s = dir(sprintf("./Code/Rmd/Lessons/%s", i), 
            pattern = ".rmd", 
            full.names = T
    )
    
    if (i == lessons[1]) {
      # First Loop
      slides = s
    } else {
      # Nth Loop  
      slides = c(slides, s)
    }
  }
  
  docs = data_frame(lessons, 
                    slides
  )
  
  for (i in 1:nrow(docs)) {
    render(input = docs$slides[i], 
           output_dir = "./Reports/Lessons/",
           output_file = ,
           # params = list("state" = "nj",
           #               "county" = as.character(i)),
           knit_root_dir =  getwd())
  }
  
  
}

# Render All ----
render_gtr = function() {
  render_gtr_lab()
  render_gtr_docs()
  render_gtr_lessons()
}