## This module contains the UI and server code for the Data Management tab

## Source needed files ############################################################

source('./modules/data_management_tab/grab_data.R')
source('./modules/data_management_tab/sensor_inventory.R')
source('./modules/data_management_tab/parameter_calculations.R')
source('./modules/data_management_tab/constants_management.R')
source('./modules/data_management_tab/tools/tool_layout.R')
source('./modules/data_management_tab/tools/entry_layout.R')
source('./modules/data_management_tab/tools/tool_table.R')
source('./modules/data_management_tab/tools/field_data_tool.R')
source('./modules/data_management_tab/tools/doc_tool.R')
source('./modules/data_management_tab/tools/dom_tool.R')
source('./modules/data_management_tab/tools/alkalinity_tool.R')
source('./modules/data_management_tab/tools/ions_tool.R')
source('./modules/data_management_tab/tools/nutrients_tool.R')
source('./modules/data_management_tab/tools/tss_afdm_tool.R')
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
    } else NULL,
    # And the tools tab
    navbarMenu(
      # Menu title
      'Tools',
      tabPanel(
        # Tab title
        'Field data',
        # Tab content
        toolsLayoutUI(ns('fieldDataTool'), 'Field data'),
        value = ns('fieldDataTool')
      ),
      tabPanel(
        # Tab title
        'DOC',
        # Tab content
        toolsLayoutUI(ns('docTool'), 'DOC'),
        value = ns('docTool')
      ),
      tabPanel(
        # Tab title
        'DOM',
        # Tab content
        toolsLayoutUI(ns('domTool'), 'DOM'),
        value = ns('domTool')
      ),
      tabPanel(
        # Tab title
        'Alkalinity',
        # Tab content
        toolsLayoutUI(ns('alkalinityTool'), 'Alkalinity'),
        value = ns('alkalinityTool')
      ),
      tabPanel(
        # Tab title
        'Ions',
        # Tab content
        toolsLayoutUI(ns('ionsTool'), 'Ions'),
        value = ns('ionsTool')
      ),
      tabPanel(
        # Tab title
        'Nutrients',
        # Tab content
        toolsLayoutUI(ns('nutrientsTool'), 'Nutrients'),
        value = ns('nutrientsTool')
      ),
      tabPanel(
        # Tab title
        'TSS & AFDM',
        # Tab content
        toolsLayoutUI(ns('tssAfdmTool'), 'TSS & AFDM'),
        value = ns('tssAfdmTool')
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
  
  ## Call tools modules ###########################################################
  
  # Call the tools layout module with the field data tool
  callModule(toolsLayout, 'fieldDataTool', fieldDataTool, fieldDataToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = TRUE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the DOC tool
  callModule(toolsLayout, 'docTool', docTool, docToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the DOC tool
  callModule(toolsLayout, 'domTool', domTool, domToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the Alkalinity tool
  callModule(toolsLayout, 'alkalinityTool', alkalinityTool, alkalinityToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the Ions tool
  callModule(toolsLayout, 'ionsTool', ionsTool, ionsToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the nutrients tool
  callModule(toolsLayout, 'nutrientsTool', nutrientsTool, nutrientsToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the TSS & AFDM tool
  callModule(toolsLayout, 'tssAfdmTool', tssAfdmTool, tssAfdmToolUI, pool,
             updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  
  
  
  ## Check authorizations #########################################################
  
  # If the user is at least a sber member
  if (userRole %in% c('sber', 'admin')) {
    # Call the grab data module
    callModule(grabData, 'grabData', pool)
    # Call the sensor inventory module
    callModule(sensorInventory, 'sensorsTab', pool)
  }
  
  # If the user is an admin
  if (userRole == 'admin') {
    # Call the parameter calculations module
    callModule(parameterCalculations, 'calculationsTab', pool)
    # Call the parameter calculations module
    callModule(constantsManagement, 'constantsTab', pool)
  }
}

