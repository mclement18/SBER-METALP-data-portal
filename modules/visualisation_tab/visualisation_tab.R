## This module contains the UI and server code for the Visualisation tab ##########

## Source needed files ############################################################

# Load plotting functions
source('./utils/plotting_functions.R')
# Load visualisation modules
source('./modules/visualisation_tab/sidebar_input_layout.R')
source('./modules/visualisation_tab/grab_samples_timeseries.R')

## Create module UI ###############################################################

visualisationTabUI <- function(id, grabSampleDf) {
  ns <- NS(id)
  
  return(
    tagList(
      tabsetPanel(
        tabPanel(
          'Grab Samples',
          sidebarInputLayoutUI(
            ns('grab-samples-timeseries'),
            minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE), 
            maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
            innerModuleUI = grabSamplesTimeSeriesUI
          )
        ),
        tabPanel('Sensors'),
        tabPanel('Parameters comparisons')
      )
    )
  )
}

## Create module server function ##################################################

visualisationTab <- function(input, output, session, grabSampleDf) {
  callModule(sidebarInputLayout, 'grab-samples-timeseries',
             grabSamplesTimeSeries, grabSamplesTimeSeriesUI,
             list('inputs' = 'time-serie-plot-input', 'plots' = 'time-serie-plots'),
             grabSampleDf)
}
