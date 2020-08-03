## This module contains the UI and server code for the Visualisation tab

## Source needed files ############################################################

# Load plotting functions
source('./utils/plotting_functions.R')

# Load visualisation modules
source('./modules/visualisation_tab/sidebar_input_layout.R')
source('./modules/visualisation_tab/grab_samples_timeseries.R')



## Create module UI ###############################################################

visualisationTabUI <- function(id, grabSampleDf, hfDfList, sites, grabSampleParameters, hfParameters) {
# Create the UI for the visualisationTab module
# Parameters:
#  - id: String, the module id
#  - grabSampleDf: Data.frame, the grab samples data
#  - hfDfList: Named list of the sensors high frequency data of each station
#  - sites: Named list of sites info, cf data_preprocessing.R
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    # Create the grab samples timeserie visualisation tab
    tabPanel(
      # Tab title
      'Grab Samples',
      # Tab content
      # Create a sidebarInputLayout UI with for the grabSamplesTimeSeries module 
      sidebarInputLayoutUI(
        ns('grab-samples-timeseries'),
        minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE), 
        maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
        innerModuleUI = grabSamplesTimeSeriesUI,
        sites = sites,
        parameters = grabSampleParameters
      )
    ),
    # Create the Sensors timeserie visualisation tab
    tabPanel(
      # Tab title
      'Sensors',
      # Tab content
      # Create a sidebarInputLayout UI with for the highFreqTimeSeries module
      # sidebarInputLayoutUI(
      #   ns('sensors-timeseries'),
      #   minDate = min(hfDfList$AND$date, na.rm = TRUE), 
      #   maxDate = max(hfDfList$AND$date, na.rm = TRUE),
      #   innerModuleUI = highFreqTimeSeriesUI
      # )
    ),
    # Create the exploratory analysis visualisation tab with dropdown menu
    navbarMenu(
      'Exploratory analysis',
      # Create the grab samples comparison tab
      tabPanel('Grab samples comparison'),
      # Create the sensors vs grab samples comparison tab
      tabPanel('Sensors vs Grab samples comparison')
    )
  )
}



## Create module server function ##################################################

visualisationTab <- function(input, output, session, grabSampleDf, hfDfList, sites, grabSampleParameters, hfParameters) {
# Create the logic for the visualisationTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - grabSampleDf: Data.frame, the data of the grab samples
#                 (to pass to the grabSamplesTimeSeries, grabSamplesComparison and sensorsVsGrabSamplesComparison modules)
#  - hfDfList: Named list of the sensors high frequency data of each station
#  - sites: Named list of sites info, cf data_preprocessing.R
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns NULL
  
  # Load the server logic for the grabSamplesTimeSeries module inside the sidebarInputLayout module
  callModule(sidebarInputLayout, 'grab-samples-timeseries',
             grabSamplesTimeSeries, grabSamplesTimeSeriesUI,
             list('inputs' = 'time-serie-plot-input', 'plots' = 'time-serie-plots'),
             df = grabSampleDf, sites = sites, parameters = grabSampleParameters)
  # # Load the server logic for the highFreqTimeSeries module inside the sidebarInputLayout module
  # callModule(sidebarInputLayout, 'sensors-timeseries',
  #            highFreqTimeSeries, highFreqTimeSeriesUI,
  #            list('inputs' = 'time-serie-plot-input', 'plots' = 'time-serie-plots'),
  #            hfDfList)
}
