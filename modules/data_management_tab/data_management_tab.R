## This module contains the UI and server code for the Data Management tab

## Source needed files ############################################################

source('./modules/data_management_tab/stations_management.R')
source('./modules/data_management_tab/grab_sample_plotting_options.R')
source('./modules/data_management_tab/sensor_plotting_options.R')
source('./modules/data_management_tab/grab_param_categories.R')


## Create module UI ###############################################################

dataManagementTabUI <- function(id) {
# Create the UI for the dataManagementTab module
# Parameters:
#  - id: String, the module id
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    id = ns('dataTab'),
    # Create the grab samples data management tab
    tabPanel(
      # Tab title
      'Grab sample data',
      # Tab content
      
      value = ns('grabData')
    ),
    # Create the stations management tab
    tabPanel(
      # Tab title
      'Stations',
      # Tab content
      stationsManagementUI(ns('stationsMan')),
      value = ns('stationsMan')
    ),
    # Create the Grab sample categories management tab
    tabPanel(
      # Tab title
      'Grab sample categories',
      # Tab content
      grabParamCategoriesUI(ns('grabCat')),
      value = ns('grabCat')
    ),
    # Create the Grab sample plotting options management tab
    tabPanel(
      # Tab title
      'Grab sample plotting options',
      # Tab content
      gbPlotOptionsUI(ns('grabPlot')),
      value = ns('grabPlot')
    ),
    # Create the Sensor plotting options management tab
    tabPanel(
      # Tab title
      'Sensor plotting options',
      # Tab content
      sensorPlotOptionsUI(ns('sensorPlot')),
      value = ns('sensorPlot')
    )
  )
}



## Create module server function ##################################################

dataManagementTab <- function(input, output, session, pool) {
# Create the logic for the dataManagementTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  callModule(stationsManagement, 'stationsMan', pool)
  
  callModule(grabParamCategories, 'grabCat', pool)
  
  callModule(gbPlotOptions, 'grabPlot', pool)
  
  callModule(sensorPlotOptions, 'sensorPlot', pool)
}

