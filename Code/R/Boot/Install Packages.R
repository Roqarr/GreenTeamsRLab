# DemeteR ----
# Written by Chris Snyder, MPH (Roqarr)
## Documentation ----
# Installs the anticipated needed packages for DemeteR.
# Used for first install
# Selection tree provided for Windows and Linux specific dependencies.
## Arguments ----
# na
## Returns ----
# na

writeLines(sprintf("%s STARTED: Package Install Script", Sys.time()))

file.rename("./.Rprofile", "./.offRprofile")

# Ask for OS
os = readline("Which OS are packages being installed for? This process is OS specific.
1: Linux
2: Windows
")

# Code OS to number
os = as.numeric(os)

# Begin Install ----
if (os == 1) {
  ## Linux ----
  writeLines("Be sure to run Install Packages function in ./Code/Sh to install systemwide dependencies.\nSudo access is required.")
  Sys.sleep(3) # 5 second delay. Enough time to read the message, but not be annoying.
  
} else if (os == 2) {
  ## Windows ----
  writeLines("Beginning Installation of Windows Specific Dependencies.")
  # Windows specific Prerequisite package for handling the compiling and installaiton of unix-like packages / code compiling.
  if ("installr" %in% rownames(installed.packages()) == F) {
    install.packages("installr") # Install the InstallR package for extra windows tools, as needed
  }
  
  installr::install.Rtools() # Install the Rtools as needed. The function checks for presence and updates as needed.
  installr::install.MikTeX() # Install full mixtex for windows, the recommended full LaTeX engine
  
  if ("tinytex" %in% rownames(installed.packages()) == F) {
    install.packages("tinytex") # Install the TinyTex package for extra windows tools, as needed
  }
  
  tinytex::install_tinytex() # Install tinytex for windows, which handles the management of LaTeX packages
} else {stop("Enter a valid numeric selection for the desired OS.")} # If not a valid selection, do nothing.


## Package Definition ----
pk = c(
  ### Packages for Development and Github Installs ----
  'devtools',
  
  ### Packages for creation of additional graphics or statistical tests ----
  'psych', # Social Science Statistics Package
  'ggplot2', # Graphics workhorse
  'ggpubr', # Publication support for ggplot2
  'scales', # Support for scale text in ggplot2
  'wordcloud', # Word Clouds
  'corrplot', # Correlation Plots
  # 'ggbiplot', # Git Package, below, used for PCA
  'multcomp', # Multiple Comparisons
  'UsingR', # linear modeling support
  'gplots', # Legacy stats plots
  'tmap', # Choropleth Mapping
  'plotly', # Interactive graphics using js library
  
  ### Packages for reading from and writing to .dbf; xls, xlsx, ; shape file, file formats ----
  'yaml', # yaml format reader
  'foreign', # SPSS, SAS, .dbf read support
  'shapefiles', # ESRI shape file format reader
  'openxlsx', # open and write excel files
  'readr', # read additional data into R
  'sf', # Support for Shape files
  'sp', # Support for Shape files
  
  ### Packages for sorting, filtering, manipulating, Data and Data Frames ----
  'plyr',
  'dplyr',
  'tidyr',
  'numform',
  'tidyselect',
  'data.table',
  'stringr',
  
  ### Packages for API and WWW interfaces, web scraping ----
  'httr', # http reading
  'jsonlite', # JSON file parsing
  'xml2', # xml reading, for APIs
  'rvest',
  'tidycensus', # Support for accessing the US Census Data
  'RCurl', # Web Page Get Requests
  'rsconnect',
  'qrcode', # Generate QR codes
  
  ### Packages for report generation ----
  'kableExtra', # Tables
  'knitr', # Document rendering
  'rmarkdown', # R Markdown document rendering
  'report',
  'bookdown', # Book rendering, specifically the R Lab Web Pages
  'shiny', # Interactive Web App Package, for dashboarding
  'flexdashboard', # Uninteractive Web Dashboarding
  'DiagrammeR' # Suppor for making GraphViz flow charts
  
)

# For each package in the list above, R will install that package and its dependencies.
for (p in pk) {
  # Determine if the package is in the list of installed packages.
  # If it is missing, we will install the package
  if (p %in% rownames(installed.packages()) == T) {
    writeLines(sprintf("%s HAVE: %s", Sys.time(), p))
  } else {
    writeLines(sprintf("%s NEED: %s", Sys.time(), p))
    install.packages(p, dependencies = T)
  }
}


##Github Package Definition ----
# To install a package that is not available on CRAN, we need `devtools`, installed from above.
# Load the package devtools, which is required for installing packages from GitHub.
library(devtools) # Requirement for Github Installs
gpk = data.frame(
  pkg = c(
    ### Github Package Names used by library() ----
    'ggbiplot',
    'report'
  ),
  git = c(
    ### Github Packages for creation of additional graphics or statistical tests ----
    'vqv/ggbiplot',
    'easystats/report'
  )
  
)

# For each package in the list above, R will install that package and its dependencies.
for (gp in 1:nrow(gpk)) {
  # Determine if the package is in the list of installed packages.
  # If it is missing, we will install the package
  if (gpk$pkg[gp] %in% rownames(installed.packages()) == T) {
    writeLines(sprintf("%s HAVE: %s", Sys.time(), gpk$pkg[gp]))
  } else {
    writeLines(sprintf("%s NEED: %s", Sys.time(), gpk$pkg[gp]))
    install_github(gpk$git[gp])
  }
}

rm(p, pk, gp, gpk, os)

file.rename("./.offRprofile", "./.Rprofile")

writeLines(sprintf("%s FINISHED: Package Install Script", Sys.time()))
