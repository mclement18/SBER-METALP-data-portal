## This module contains the UI and server code for the Data Management tab

## Source needed files ############################################################

source('./modules/data_management_tab/grab_data.R')
source('./modules/data_management_tab/parameter_calculations.R')
source('./modules/data_management_tab/constants_management.R')
source('./modules/data_management_tab/standard_curves.R')



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
    # Add the grab samples data management tab
    tabPanel(
      # Tab title
      'Database',
      # Tab content
      grabDataUI(ns('grabData'), pool),
      value = ns('grabDataTab')
    ),
    # Add the standard curves management tab
    tabPanel(
      # Tab title
      'Standard curves',
      # Tab content
      standardCurvesUI(ns('standardCurvesTab'), pool),
      value = ns('standardCurvesTab')
    ),
    # If the user is an admin
    if (userRole == 'admin') {
      # And the sensor inventory tab
      tabPanel(
        # Tab title
        'Parameter calculations',
        # Tab content
        parameterCalculationsUI(ns('calculationsTab'), pool),
        value = ns('calculationsTab')
      )
    } else NULL,
    # If the user is an admin
    if (userRole == 'admin') {
      # And the constants management tab
      tabPanel(
        # Tab title
        'Constants',
        # Tab content
        constantsManagementUI(ns('constantsTab')),
        value = ns('constantsTab')
      )
    } else NULL
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
  
  ## Call modules #########################################################
  
  # Call the grab data module
  callModule(grabData, 'grabData', pool)
  # Call the standard curves module
  callModule(standardCurves, 'standardCurvesTab', pool)
  
  ## Check authorizations #########################################################
  
  # If the user is an admin
  if (userRole == 'admin') {
    # Call the parameter calculations module
    callModule(parameterCalculations, 'calculationsTab', pool)
    # Call the parameter calculations module
    callModule(constantsManagement, 'constantsTab', pool)
  }
}

