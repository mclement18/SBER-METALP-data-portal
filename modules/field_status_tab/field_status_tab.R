## This module contains the UI and server code for the Field Status tab

## Source needed files ############################################################

source('./modules/field_status_tab/sensor_inventory.R')
source('./modules/field_status_tab/notes.R')



## Create module UI ###############################################################

fieldStatusTabUI <- function(id, pool) {
# Create the UI for the fieldStatusTab module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    id = ns('fieldStatusTabs'),
    # And the sensor inventory tab
    tabPanel(
      # Tab title
      'Sensor inventory',
      # Tab content
      sensorInventoryUI(ns('sensorsTab'), pool),
      value = ns('sensorsTab')
    ),
    # And the notes tab
    tabPanel(
      # Tab title
      'Notes',
      # Tab content
      notesUI(ns('notesTab')),
      value = ns('notesTab')
    )
  )
}



## Create module server function ##################################################

fieldStatusTab <- function(input, output, session, pool) {
# Create the logic for the fieldStatusTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  ## Call modules #########################################################
  
  # Call the sensor inventory module
  callModule(sensorInventory, 'sensorsTab', pool)
  
  # Call the notes module
  callModule(notes, 'notesTab', pool)
}

