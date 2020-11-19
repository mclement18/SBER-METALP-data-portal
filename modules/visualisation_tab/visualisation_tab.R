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
source('./modules/visualisation_tab/global_grab_sample_comparison.R')



## Create module UI ###############################################################

visualisationTabUI <- function(id, pool, hfDf) {
# Create the UI for the visualisationTab module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Get grab data min and max values
  grabMinMaxDates <- getMinMaxValues(pool, 'data', DATE_reading) %>%
    mutate(across(everything(), ymd))
  
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
        minDate = grabMinMaxDates$min, 
        maxDate = grabMinMaxDates$max,
        innerModuleUI = grabSamplesTimeSeriesUI,
        pool = pool
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
        pool = pool
      ),
      value = ns('sensorsTimeseries')
    )
  )
}



## Create module server function ##################################################

visualisationTab <- function(input, output, session, pool, user, hfDf) {
# Create the logic for the visualisationTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - user: Reactive values, the current user
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
# 
# Returns NULL
  
  ## Get min and max values ##############################################
  
  # For grab sample
  grabMinMaxDates <- getMinMaxValues(pool, 'data', DATE_reading) %>%
    mutate(across(everything(), ymd))
  
  # For sensors
  hfMinMaxDates <- list(
    min = min(hfDf$`24H`$Date, na.rm = TRUE),
    max = max(hfDf$`24H`$Date, na.rm = TRUE)
  )
  
  
  
  # Load the server logic for the grabSamplesTimeSeries module inside the sidebarInputLayout module
  callModule(sidebarInputLayout, 'grabSamplesTimeseries',
             innerModule = grabSamplesTimeSeries,
             innerModuleUI = grabSamplesTimeSeriesUI,
             innerModulePrefixIds = list(
               'inputs' = 'time-serie-plot-input',
               'plots' = 'time-serie-plots'
             ),
             minDate = grabMinMaxDates$min,
             maxDate = grabMinMaxDates$max,
             pool = pool)
  
  # Load the server logic for the highFreqTimeSeries module inside the sidebarInputLayout module
  callModule(sidebarInputLayout, 'sensorsTimeseries',
             innerModule = highFreqTimeSeries,
             innerModuleUI = highFreqTimeSeriesUI,
             innerModulePrefixIds = list(
               'inputs' = 'hf-time-serie-plot-input',
               'plots' = 'hf-time-serie-plots'
             ),
             minDate = hfMinMaxDates$min,
             maxDate = hfMinMaxDates$max,
             df = hfDf,
             pool = pool)

  ## Check for authorization #######################################################

  # Check for user update
  observeEvent(user$role, {
    if (user$role %in% c('intern', 'sber', 'admin')) {
      # Create the grab samples comparison tab
      appendTab(
        'visuTabs',
        tabPanel(
          # Tab title
          'Grab sample comparison',
          # Tab content
          # Create a sidebarInputLayout UI with for the grabSamplesComparison module
          sidebarInputLayoutUI(
            session$ns('grabVsGrab'),
            minDate = grabMinMaxDates$min,
            maxDate = grabMinMaxDates$max,
            innerModuleUI = grabSamplesComparisonUI,
            pool = pool
          ),
          value = session$ns('grabVSgrab')
        )
      )
      
      
      # Create the global grab samples comparison tab
      appendTab(
        'visuTabs',
        tabPanel(
          # Tab title
          'Global grab sample comparison',
          # Tab content
          # Create a sidebarInputLayout UI with for the grabSamplesComparison module
          sidebarInputLayoutUI(
            session$ns('globalGrabVsGrab'),
            minDate = grabMinMaxDates$min,
            maxDate = grabMinMaxDates$max,
            innerModuleUI = globalGrabSamplesComparisonUI,
            pool = pool
          ),
          value = session$ns('globalGrabVSgrab')
        )
      )


      # Create the sensors vs grab samples comparison tab
      appendTab(
        'visuTabs',
        tabPanel(
          # Tab title
          'Sensor vs Grab sample comparison',
          # Tab content
          # Create a sidebarInputLayout UI with for the sensorGrabComparison module
          sidebarInputLayoutUI(
            session$ns('sensorVsGrab'),
            minDate = hfMinMaxDates$min,
            maxDate = hfMinMaxDates$max,
            innerModuleUI = sensorGrabComparisonUI,
            pool = pool
          ),
          value = session$ns('sensorVsGrab')
        )
      )


      # Load the server logic for the grabSamplesComparison module inside the sidebarInputLayout module
      callModule(sidebarInputLayout, 'grabVsGrab',
                 innerModule = grabSamplesComparison,
                 innerModuleUI = grabSamplesComparisonUI,
                 innerModulePrefixIds = list(
                   'inputs' = 'grab-vs-grab-plot-input',
                   'plots' = 'grab-vs-grab-plots'
                 ),
                 minDate = grabMinMaxDates$min,
                 maxDate = grabMinMaxDates$max,
                 plotDateRangeSelection = FALSE,
                 pool = pool)
      
      # Load the server logic for the globalGrabSamplesComparison module inside the sidebarInputLayout module
      callModule(sidebarInputLayout, 'globalGrabVsGrab',
                 innerModule = globalGrabSamplesComparison,
                 innerModuleUI = globalGrabSamplesComparisonUI,
                 innerModulePrefixIds = list(
                   'inputs' = 'global-grab-vs-grab-plot-input',
                   'plots' = 'global-grab-vs-grab-plots'
                 ),
                 minDate = grabMinMaxDates$min,
                 maxDate = grabMinMaxDates$max,
                 plotDateRangeSelection = FALSE,
                 pool = pool)

      # Load the server logic for the sensorGrabComparison module inside the sidebarInputLayout module
      callModule(sidebarInputLayout, 'sensorVsGrab',
                 innerModule = sensorGrabComparison,
                 innerModuleUI = sensorGrabComparisonUI,
                 innerModulePrefixIds = list(
                   'inputs' = 'sensor-vs-grab-plot-input',
                   'plots' = 'sensor-vs-grab-plots'
                 ),
                 minDate = hfMinMaxDates$min,
                 maxDate = hfMinMaxDates$max,
                 df = hfDf,
                 pool = pool)
    }
  })
}
  
