# DemeteR ----
# Written by Chris Snyder, MPH (Roqarr)
## Documentation ----
# Loads all of the libraries used by DemeteR
# Depricated by Check Packages
## Arguments ----
# na
## Returns ----
# na

# Packages for Development and Github Installs ----
library(devtools)

# Packages for Multi Core Processing ----
library(parallel)
library(foreach)
library(doParallel)

# Packages for creation of additional graphics or statistical tests ----
# Loaded first, as future tests will dominate namesspaces
library(psych) # Social Science Statistics Package
library(ggpubr) # Publication support for ggplot2
library(ggplot2) # Graphics workhorse
library(scales) # Support for scale text in ggplot2
library(wordcloud) # Word Clouds
library(corrplot) # Correlation Plots
library(ggbiplot)
library(multcomp) # Multiple Comparisons
library(UsingR) # linear modeling support
library(gplots) # Legacy stats plots
library(tmap) # Choropleth Mapping
library(plotly)

# Packages for reading from and writing to .dbf; xls, xlsx, ; shape file, file formats ----
library(yaml)
library(foreign)
library(shapefiles)
library(openxlsx)
library(readr)
library(sf)
library(sp)

# Packages for sorting, filtering, manipulating, Data and Data Frames ----
library(plyr)
library(dplyr)
library(tidyr)
library(numform)
library(tidyselect)
library(data.table) # What is this used for? Possible Legacy Entry
library(stringr)

# Packages for API and WWW interfaces, web scraping ----
library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(tidycensus)
library(RCurl)
library(rsconnect)
library(qrcode)
library(askpass)

# Packages for report generation ----
library(kableExtra)
library(knitr)
library(rmarkdown)
library(report)
library(bookdown)
library(shiny)
library(flexdashboard)
library(DiagrammeR)



# Finish ----
cat("\014") # clears console
writeLines(sprintf("%s LOADED: R Packages", Sys.time()))