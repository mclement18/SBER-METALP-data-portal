## Load Libraries #################################################################
library(shiny)
library(sass)
library(stringr)


## Compile CSS from Sass ##########################################################
sass(
  sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass_options(output_style = 'compressed')
)

## Source needed files ############################################################

# Load Shiny extensions functions
source('./utils/shiny_extensions.R')

## Create main UI #################################################################

ui <- tagList(
  tags$head(
    tags$link(href = 'main.css', rel = 'stylesheet', type = 'text/css')
  ),
  tags$body(class = 'footer-to-bottom-container'),
  navbarPageWithWrapper(
    navbarPage(
      htmlTemplate('html_components/logo.html'),
      windowTitle = 'METALP DATA PORTAL',
      tabPanel('Home'),
      tabPanel('Visualisation'),
      tabPanel('Data Input'),
      tabPanel('Tools')
    ),
    footer = htmlTemplate('html_components/footer.html')
  )
)

## Create server function #########################################################

server <- function(input, output, session) {}


## Launch App #####################################################################

shinyApp(ui, server)
