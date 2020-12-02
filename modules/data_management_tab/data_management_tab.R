## This module contains the UI and server code for the Data Management tab

## Source needed files ############################################################

source('./modules/data_management_tab/grab_data.R')
source('./modules/data_management_tab/sensor_inventory.R')
source('./modules/data_management_tab/parameter_calculations.R')
source('./utils/calculation_functions.R')



## Create module UI ###############################################################

dataManagementTabUI <- function(id, pool, userRole) {
# Create the UI for the dataManagementTab module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - userRole: String, the role of the currently logged in user
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanelWithNULL(
    id = ns('dataTabs'),
    # If the user is at least a sber member
    if (userRole %in% c('sber', 'admin')) {
      # Add the grab samples data management tab
      tabPanel(
        # Tab title
        'Grab sample data',
        # Tab content
        grabDataUI(ns('grabData'), pool),
        value = ns('grabDataTab')
      )
    } else NULL,
    # If the user is at least a sber member
    if (userRole %in% c('sber', 'admin')) {
      # And the sensor inventory tab
      tabPanel(
        # Tab title
        'Sensor inventory',
        # Tab content
        sensorInventoryUI(ns('sensorsTab'), pool),
        value = ns('sensorsTab')
      )
    } else NULL,
    # If the user is at least a sber member
    if (userRole %in% c('sber', 'admin')) {
      # And the sensor inventory tab
      tabPanel(
        # Tab title
        'Parameter calculations',
        # Tab content
        parameterCalculationsUI(ns('calculationsTab'), pool),
        value = ns('calculationsTab')
      )
    } else NULL,
    # And the tools tab
    navbarMenu(
      # Menu title
      'Tools',
      tabPanel(
        # Tab title
        'Tools',
        # Tab content
        value = ns('toolsTab')
      ),
      # Menu reference
      menuName = 'toolsTabs',
      icon = icon('tools')
    )
  )
}



## Create module server function ##################################################

dataManagementTab <- function(input, output, session, pool, userRole) {
# Create the logic for the dataManagementTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - userRole: String, the role of the currently logged in user
# 
# Returns NULL
  
  ## Check authorizations #########################################################
  
  # If the user is at least a sber member
  if (userRole %in% c('sber', 'admin')) {
    # Call the grab data module
    callModule(grabData, 'grabData', pool)
    # Call the sensor inventory module
    callModule(sensorInventory, 'sensorsTab', pool)
    # Call the parameter calculations module
    callModule(parameterCalculations, 'calculationsTab', pool)
  }
}

