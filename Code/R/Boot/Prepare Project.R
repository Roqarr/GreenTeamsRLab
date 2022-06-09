# Green_Teams ----
# Written by Chris Snyder, MPH (Roqarr)
# Written for the Montclair State University PSEG Institute for Sustainability Studies
# Documentation ----
# This is the script called by the .Rprofile, and the .reset() function.
# This script handles the loading of all required packages, user-defined functions, and environment variables.
# Arguments ----
# na
# Returns ----
# na

# Prepare the R Project Workspace ----
source("./Code/R/Boot/Load Packages.R") # Loads all of the packages used in this project. New packages to load should be added here. Any missing packages will be installed as needed.
source("./Code/R/Boot/Load Functions.R") # Loads User-Defined Functions. Newly written functions should be added here.
source("./Code/R/Boot/Cleanup Project.R") # Loads end-of-session and quality-of-life functions.

# Assign Rendering Quality of Life Variables ----
.proj_dir = getwd()

# Assign Options ----
# options("bitmapType" = "cairo") # force cairo rendering engine in R / RStudio. New default is unstable with Unix. Toggle as needed.
writeLines(sprintf("%s SET: Graphics Engine - %s", 
                   Sys.time(),
                   getOption("bitmapType")
))

# Report on Project Load ----
r = sessionInfo()

r$R.version$version.string # Full R Version String
r$platform # Platform
r$running # Local OS
r$basePkgs
r$otherPkgs
r$loadedOnly
r$R.version$nickname

## Session Version and Platform ----
writeLines(sprintf("%s READY: %s running on %s, %s",
                   Sys.time(),
                   r$R.version$version.string,
                   r$running,
                   r$platform
))

## Number of loaded Packages ----
writeLines(sprintf("%s READY: %s Loaded Packages (%s Base, %s Attached, %s Via Namespace)",
                   Sys.time(),
                   length(loadedNamespaces()),
                   length(r$basePkgs),
                   length(r$otherPkgs),
                   length(r$loadedOnly)
))
rm(r) # cull the session info object, before reporting on objects

## Number of User-Defined Functions Loaded ----
writeLines(sprintf("%s READY: %s Defined Functions (%s Hidden)",
                   Sys.time(),
                   length(ls(all.names = T)[sapply(mget(ls(all.names = T), .GlobalEnv), is.function)]), # Length of objects in the global environment that are a function
                   length(ls(all.names = T)[sapply(mget(ls(all.names = T), .GlobalEnv), is.function)])-length(ls(all.names = F)[sapply(mget(ls(all.names = F), .GlobalEnv), is.function)]) # Difference in length of all functions and visible functions
))
writeLines(sprintf("%s READY: %s Defined Objects (%s Hidden)",
                   Sys.time(),
                   length(ls(all.names = T)) - length(ls(all.names = T)[sapply(mget(ls(all.names = T), .GlobalEnv), is.function)]), # Length of all non-function values
                   (length(ls(all.names = T)) - length(ls(all.names = T)[sapply(mget(ls(all.names = T), .GlobalEnv), is.function)])) - # Length of all hidden non-function values
                     (length(ls(all.names = F)) - length(ls(all.names = F)[sapply(mget(ls(all.names = F), .GlobalEnv), is.function)]))
))

## Memory Allocation ----
writeLines(sprintf("%s READY: %s Mb Memory Used for R Objects",
                   Sys.time(),
                   sum(gc()[1:2,2]) # GC will also conveniently run the garbage collection, helpful during the .reset() call
))


# Finish ----
# Writes the message that is seen on project start-up, and provides helpful reminders of the absolute paths to useful project folders.
writeLines(sprintf("%s %s READY:

You may reset this sample project at any time using the user-defined function: '.reset()'.
This function will remove all currently loaded data, plots, and functions. 
It will then re-run the start-up script, returning the project to its original state.

The absolute file path to the current project directory is located below:
%s

The Code samples inside this project are located inside the following directory:
%s/Code/R

The Document samples inside this project are located inside the following directory:
%s/Code/Rmd

Documentation and resources about this project are located inside the following directory:
%s/Reports

To regenerate the Powerpoint Slides used in this project, use the following function:
render_gtr()

If you need additional assistance, or have any questions, please contact:
Chris Snyder, snyderc@montclair.edu

", 
  Sys.time(), 
  .proj_dir,
  .proj_dir,
  .proj_dir,
  .proj_dir,
  .proj_dir
  )
) # Write a ready message with time stamp.

# At this point, all of the setup scripts have finished running, and the interactive R session is ready.
# These steps are also run during all R scripting processes that occur when launched from the perspective of the project directory, such as R Markdown rendering.

