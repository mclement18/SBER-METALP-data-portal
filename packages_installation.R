install.packages(
  pkgs = c("shinyjs", 
           "shinyWidgets", 
           "shinybusy", 
           "shinycssloaders", 
           "sass", 
           "jsonlite", 
           "readr", 
           "stringr", 
           "ggplot2", 
           "Cairo", 
           "data.table", 
           "lubridate", 
           "forcats", 
           "tidyr", 
           "magrittr", 
           "dplyr",
           "DBI",
           "RMySQL",
           "pool",
           "sodium",
           "DT",
           "rhandsontable"
  ),
  repos = 'https://cran.rstudio.com/'
)

# Install specific versions
# dbplyr 2 is adding a `` to the SELECT query which make the query invalid
devtools::install_version('dbplyr', version='1.4.4', repos='https://cran.rstudio.com/', upgrade = 'never')
