## Load Libraries #################################################################
library(shiny)
library(shinyjs)
library(sass)
library(jsonlite)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(lubridate)
library(tidyr)
library(dplyr)


## Compile CSS from Sass ##########################################################
sass(
  sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass_options(output_style = 'compressed')
)

## Load data ######################################################################

# Load Grab Samples
grabSampleDf <- read.csv('./data/Metalp_grab_20200717_ND.csv', header = TRUE, na.strings=c(""," ","NA", "<0.05"))

## Convert Date to Date data type and create a DATETIME_GMT column
grabSampleDf$DATETIME_GMT <- paste(grabSampleDf$DATE_reading, grabSampleDf$TIME_reading_GMT) %>% dmy_hms(tz = 'GMT')
grabSampleDf$DATE_reading <- dmy(grabSampleDf$DATE_reading)

 

## Source needed files ############################################################

# Load Shiny extensions functions
source('./utils/shiny_extensions.R')
# Load tabs modules
source('./modules/visualisation_tab/visualisation_tab.R')

## Create main UI #################################################################

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(href = 'main.css', rel = 'stylesheet', type = 'text/css'),
    includeScript('./assets/js/sidebar_actions.js'),
    includeScript('./assets/js/shiny_custom_events.js')
  ),
  tags$body(class = 'footer-to-bottom-container'),
  navbarPageWithWrapper(
    navbarPage(
      htmlTemplate('html_components/logo.html'),
      windowTitle = 'METALP DATA PORTAL',
      tabPanel(
        tags$span(icon('home'),tags$span('Home', class = 'navbar-menu-name')),
        htmlTemplate(
          'html_components/home.html',
          chartIcon = icon('chart-bar'),
          dataIcon = icon('database'),
          toolboxIcon = icon('toolbox'),
          downloadIcon = icon('download')
        )
      ),
      tabPanel(
        tags$span(icon('chart-bar'),tags$span('Visualisation', class = 'navbar-menu-name')),
        visualisationTabUI('1', grabSampleDf)
      ),
      tabPanel(tags$span(icon('database'),tags$span('Data Management', class = 'navbar-menu-name'))),
      tabPanel(tags$span(icon('toolbox'),tags$span('Toolbox', class = 'navbar-menu-name'))),
      tabPanel(tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')))
    ),
    footer = htmlTemplate('html_components/footer.html')
  )
)

## Create server function #########################################################

server <- function(input, output, session) {
  callModule(visualisationTab, '1', grabSampleDf)
}


## Launch App #####################################################################

shinyApp(ui, server)
