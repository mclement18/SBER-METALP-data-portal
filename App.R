library(shiny)
library(sass)
library(stringr)

sass(
  sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass_options(output_style = 'compressed')
)

navbarPageWithWrapper <- function(navbarPageOutput, footer = NULL) {
  navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class} content-wrapper')
  navbarPageOutput[[3]][[2]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[2]]$attribs$class} content-wrapper')
  
  if(!is.null(footer)) {
    navbarPageOutput[[3]][[length(navbarPageOutput[[3]]) + 1]] <- footer
  }
  
  return(navbarPageOutput)
}

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
    htmlTemplate('html_components/footer.html')
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
