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
# Load tabs moduels
source('./modules/visualisation_tab/visualisation_tab.R')

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
        visualisationTabUI('1')
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
  callModule(visualisationTab, '1')
}


## Launch App #####################################################################

shinyApp(ui, server)
