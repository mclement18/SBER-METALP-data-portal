## This module contains the UI and server code for the instruction panel

## Create module UI ###############################################################

instructionsPanelUI <- function(id, instructionsUI, initStateHidden = FALSE) {
# Create the UI for the instruction panel module
# Parameters:
#  - id: String, the module id
#  - instructionsUI: Some Shiny UI elements to add to the instruction panel
#  - initStateHidden: Logical, indicate if the instruction must be hidden initially, default: FALSE
# 
# Returns a div containing the instruction panel
  
  # Create namespace
  ns <- NS(id)
  
  # Create instructions
  intructions <-  div(
    id = ns('instructions'),
    instructionsUI
  )
  
  # Create panel
  div(
    class = 'instructions',
    h4(class = 'title', 'Instructions', actionLink(ns('toggle'), 'show', class = 'instruction-toggle custom-links')),
    if (initStateHidden) {
      hidden(
        intructions
      )
    } else {
      intructions
    }
  )
}




## Create module server function ##################################################

instructionsPanel <- function(input, output, session, initStateHidden = FALSE) {
# Create the logic for the instructionsPanel module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - initStateHidden: Logical, indicate if the instruction must be hidden initially, default: FALSE
# 
# Returns NULL
  
  # Set initial state
  instructionHidden <- reactiveVal(initStateHidden)
  
  # Toggle instructions visibility
  observeEvent(input$toggle, ignoreInit = TRUE, {
    # Inverse value
    instructionHidden(!instructionHidden())
    
    # Toggle visibility
    toggleElement(
      id = 'instructions',
      condition = !instructionHidden(),
      anim = TRUE
    )
    
    # Update button label
    updateActionLink(session, 'toggle', label = if (instructionHidden()) 'show' else 'hide')
  })
}
  