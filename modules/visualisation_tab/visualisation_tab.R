# This module contains the UI and server code for the Visualisation tab

## Create module UI ###############################################################

visualisationTabUI <- function(id) {
  ns <- NS(id)
    
  return(
    tagList(
      tabsetPanel(
        navbarMenu('Grab Samples',
                   tabPanel('Time Series'),
                   tabPanel('Parameters comparisons')
        ),
        navbarMenu('Sensors',
                   tabPanel('Time Series'),
                   tabPanel('VS Grab samples')
        )
      )
    )
  )
}

## Create module server function ##################################################

visualisationTab <- function(input, output, session) {
  
}
