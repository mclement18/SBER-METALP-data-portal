## This module contains the UI and server code for the Portal Management tab

## Source needed files ############################################################

# Load visualisation modules
source('./modules/portal_management/users_tab.R')
source('./modules/portal_management/stations_management.R')
source('./modules/portal_management/grab_param_categories.R')
source('./modules/portal_management/grab_sample_plotting_options.R')
source('./modules/portal_management/sensor_plotting_options.R')
source('./modules/portal_management/portal_actions.R')



## Create module UI ###############################################################

portalManagementUI <- function(id, pool) {
# Create the UI for the portalManagement module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    id = ns('portalTabs'),
    # Create users tab
    tabPanel(
      # Tab title
      'Users',
      # Tab content
      usersTabUI(ns('users')),
      value = ns('usersTab')
    ),
    # Create the stations management tab
    tabPanel(
      # Tab title
      'Stations',
      # Tab content
      stationsManagementUI(ns('stationsMan')),
      value = ns('stationsTab')
    ),
    # Create the Grab sample categories management tab
    tabPanel(
      # Tab title
      'Grab sample categories',
      # Tab content
      grabParamCategoriesUI(ns('grabCat'), pool),
      value = ns('grabCatTab')
    ),
    # Create the Grab sample plotting options management tab
    tabPanel(
      # Tab title
      'Grab sample plotting options',
      # Tab content
      gbPlotOptionsUI(ns('grabPlot'), pool),
      value = ns('grabPlotTab')
    ),
    # Create the Sensor plotting options management tab
    tabPanel(
      # Tab title
      'Sensor plotting options',
      # Tab content
      sensorPlotOptionsUI(ns('sensorPlot')),
      value = ns('sensorPlotTab')
    ),
    # Create the portal actions
    tabPanel(
      # Tab title
      'Portal actions',
      # Tab content
      portalActionsUI(ns('actions')),
      value = ns('actionsTab')
    )
  )
}



## Create module server function ##################################################

portalManagement <- function(input, output, session, pool) {
# Create the logic for the portalManagement module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  ## Call modules functions ########################################################
  
  # Users tab
  callModule(usersTab, 'users', pool)
  
  # Staions tab
  callModule(stationsManagement, 'stationsMan', pool)
  
  # Grab param categories
  callModule(grabParamCategories, 'grabCat', pool)
  
  # Grab plotting options
  callModule(gbPlotOptions, 'grabPlot', pool)
  
  # Sensor plotting options
  callModule(sensorPlotOptions, 'sensorPlot', pool)
  
  # Sensor plotting options
  callModule(portalActions, 'actions')
}

