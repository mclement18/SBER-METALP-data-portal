## This module contains the UI and server code for the Visualisation tab

## Source needed files ############################################################

# Load plotting functions
source('./utils/plotting_functions.R')

# Load visualisation modules
source('./modules/visualisation_tab/sidebar_input_layout.R')
source('./modules/visualisation_tab/grab_samples_timeseries.R')
source('./modules/visualisation_tab/high_frequency_timeseries.R')
source('./modules/visualisation_tab/grab_samples_comparison.R')
source('./modules/visualisation_tab/sensor_grab_comparison.R')



## Create module UI ###############################################################

visualisationTabUI <- function(id, pool, grabSampleDf, hfDf, grabSampleParameters, hfParameters) {
# Create the UI for the visualisationTab module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - grabSampleDf: Data.frame, the grab samples data
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    id = ns('visuTabs'),
    # Create the grab samples timeserie visualisation tab
    tabPanel(
      # Tab title
      'Grab sample data',
      # Tab content
      # Create a sidebarInputLayout UI with for the grabSamplesTimeSeries module 
      sidebarInputLayoutUI(
        ns('grabSamplesTimeseries'),
        minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE), 
        maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
        innerModuleUI = grabSamplesTimeSeriesUI,
        pool = pool,
        parameters = grabSampleParameters
      ),
      value = ns('grabSamplesTimeseries')
    ),
    # Create the Sensors timeserie visualisation tab
    tabPanel(
      # Tab title
      'Sensor data',
      # Tab content
      # Create a sidebarInputLayout UI with for the highFreqTimeSeries module
      sidebarInputLayoutUI(
        ns('sensorsTimeseries'),
        minDate = min(hfDf$`24H`$Date, na.rm = TRUE),
        maxDate = max(hfDf$`24H`$Date, na.rm = TRUE),
        innerModuleUI = highFreqTimeSeriesUI,
        pool = pool,
        parameters = hfParameters
      ),
      value = ns('sensorsTimeseries')
    )
  )
}



## Create module server function ##################################################

visualisationTab <- function(input, output, session, pool, user, grabSampleDf, hfDf, grabSampleParameters, hfParameters) {
# Create the logic for the visualisationTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - user: Reactive values, the current user
#  - grabSampleDf: Data.frame, the data of the grab samples
#                 (to pass to the grabSamplesTimeSeries, grabSamplesComparison and sensorsVsGrabSamplesComparison modules)
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns NULL
  
  # Load the server logic for the grabSamplesTimeSeries module inside the sidebarInputLayout module
  callModule(sidebarInputLayout, 'grabSamplesTimeseries',
             grabSamplesTimeSeries, grabSamplesTimeSeriesUI,
             list('inputs' = 'time-serie-plot-input', 'plots' = 'time-serie-plots'),
             df = grabSampleDf,
             minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE),
             maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
             pool = pool,
             parameters = grabSampleParameters)
  
  # Load the server logic for the highFreqTimeSeries module inside the sidebarInputLayout module
  callModule(sidebarInputLayout, 'sensorsTimeseries',
             highFreqTimeSeries, highFreqTimeSeriesUI,
             list('inputs' = 'hf-time-serie-plot-input', 'plots' = 'hf-time-serie-plots'),
             df = hfDf,
             minDate = min(hfDf$`24H`$Date, na.rm = TRUE),
             maxDate = max(hfDf$`24H`$Date, na.rm = TRUE),
             pool = pool,
             parameters = hfParameters)

  # ## Check for authorization #######################################################
  # 
  # # Check for user update
  # observeEvent(user$role, {
  #   if (user$role %in% c('intern', 'sber', 'admin')) {
  #     # Create the grab samples comparison tab
  #     appendTab(
  #       'visuTabs',
  #       tabPanel(
  #         # Tab title
  #         'Grab sample comparison',
  #         # Tab content
  #         # Create a sidebarInputLayout UI with for the grabSamplesComparison module
  #         sidebarInputLayoutUI(
  #           session$ns('grabVsGrab'),
  #           minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE), 
  #           maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
  #           innerModuleUI = grabSamplesComparisonUI,
  #           pool = pool,
  #           parameters = grabSampleParameters
  #         ),
  #         value = session$ns('grabVSgrab')
  #       )
  #     )
  #     
  #     
  #     # Create the sensors vs grab samples comparison tab
  #     appendTab(
  #       'visuTabs',
  #       tabPanel(
  #         # Tab title
  #         'Sensor vs Grab sample comparison',
  #         # Tab content
  #         # Create a sidebarInputLayout UI with for the sensorGrabComparison module
  #         sidebarInputLayoutUI(
  #           session$ns('sensorVsGrab'),
  #           minDate = min(hfDf$`24H`$Date, na.rm = TRUE),
  #           maxDate = max(hfDf$`24H`$Date, na.rm = TRUE),
  #           innerModuleUI = sensorGrabComparisonUI,
  #           pool = pool,
  #           parameters = list('hf' = hfParameters)
  #         ),
  #         value = session$ns('sensorVsGrab')
  #       )
  #     )
  #     
  #     
  #     # Load the server logic for the grabSamplesComparison module inside the sidebarInputLayout module
  #     callModule(sidebarInputLayout, 'grabVsGrab',
  #                grabSamplesComparison, grabSamplesComparisonUI,
  #                list('inputs' = 'grab-vs-grab-plot-input', 'plots' = 'grab-vs-grab-plots'),
  #                df = grabSampleDf,
  #                plotDateRangeSelection = FALSE,
  #                minDate = min(grabSampleDf$DATE_reading, na.rm = TRUE),
  #                maxDate = max(grabSampleDf$DATE_reading, na.rm = TRUE),
  #                pool = pool,
  #                parameters = grabSampleParameters)
  #     
  #     # Load the server logic for the sensorGrabComparison module inside the sidebarInputLayout module
  #     callModule(sidebarInputLayout, 'sensorVsGrab',
  #                sensorGrabComparison, sensorGrabComparisonUI,
  #                list('inputs' = 'sensor-vs-grab-plot-input', 'plots' = 'sensor-vs-grab-plots'),
  #                df = list('hf' = hfDf, 'grab' = grabSampleDf),
  #                minDate = min(hfDf$`24H`$Date, na.rm = TRUE),
  #                maxDate = max(hfDf$`24H`$Date, na.rm = TRUE),
  #                pool = pool,
  #                parameters = list('hf' = hfParameters, 'grab' = grabSampleParameters))
  #   }
  # })
}
  
