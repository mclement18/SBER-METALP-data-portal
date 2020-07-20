## This module contains the UI and server code for the Visualisation tab ##########

## Source needed files ############################################################

# Load plotting functions
source('./utils/plotting_functions.R')
# Load visualisation modules
source('./modules/visualisation_tab/timeseries_plotting.R')
source('./modules/visualisation_tab/grab_samples_time_series.R')

## Create module UI ###############################################################

visualisationTabUI <- function(id, grabSampleDf) {
  ns <- NS(id)
  
  return(
    tagList(
      tabsetPanel(
        
        tabPanel(
          'Grab Samples',
          grabSamplesTimeSeriesUI(
            ns('1'),
            minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE), 
            maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE)
          )
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

visualisationTab <- function(input, output, session, grabSampleDf) {
  callModule(grabSamplesTimeSeries, '1', grabSampleDf)
}
