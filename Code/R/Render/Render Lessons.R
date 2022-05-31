# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies
# Documentation ----
# Renders the Powerpoint documents containing the R Lessons
# Arguments ----
# na
# Returns ----
# na
# N number of R lessons are rendered and placed in the output directory: `./Documentation/Lessons/`

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
           output_dir = "./Documentation/Lessons/",
           output_file = ,
           # params = list("state" = "nj",
           #               "county" = as.character(i)),
           knit_root_dir =  getwd())
  }
  
  
}
