library(shiny)
library(sass)

sass(
  sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass_options(output_style = 'compressed')
)


ui <- tagList(
  tags$head(
    tags$link(href = 'main.css', rel = 'stylesheet', type = 'text/css')
  ),
  navbarPage(
    htmlTemplate('html_components/logo.html'),
    windowTitle = 'METALP DATA PORTAL',
    tabPanel('Home'),
    tabPanel('Visualisation'),
    tabPanel('Data Input'),
    tabPanel('Tools')
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
