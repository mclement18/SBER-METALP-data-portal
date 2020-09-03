## This module contains the UI and server code for the Download Data feature


## Create module UI ###############################################################

downloadDataUI <- function(id) {
# Create the UI for the downloadData module
# Parameters:
#  - id: String, the module id
# 
# Returns a downloadButton
  
  # Create namespace
  ns <- NS(id)
  
  # Create and eturn a downloadButton disabled by default
  disabled(
    # Add "onclick = 'return false;'" additional attribute to disable the button which is in reality a hyper link
    downloadButton(ns('download'), class = 'custom-style custom-style--primary', onclick = 'return false;')
  )
}



## Create module server function ##################################################

downloadData <- function(input, output, session, selectedData) {
# Create the logic for the downloadData module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - selectedData: Reactive expression, the data selected with the form in the outer module
# 
# Returns NULL
  
  ## Download button state logic ###########################################################
  
  # Create an observeEvent that react to data change to set downloadButton state
  observeEvent(selectedData(), ignoreInit = TRUE, {
    # Create message to send to client in as a list containing:
    #  - id: the downloadButton id defined in the UI
    #  - disable: boolean, indicate is the button is disabled
    messageList <- list(
      'id' = session$ns('download'),
      'disable' = FALSE
    )
    
    
    if (nrow(selectedData()) >= 1) {
      # Style the button
      enable('download')
      # Inform UI that button needs to be enabled
      messageList$disable <- FALSE
    } else {
      # Style the button
      disable('download')
      # Inform UI that button needs disabled
      messageList$disable <- TRUE
    }
    
    # Convert the list message to JSON
    messageJSON <- toJSON(messageList, auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle downloadButton state
    # Linked to some JavaScript defined in './assets/js/download_button_state.js'
    session$sendCustomMessage('toggleDownloadButton', messageJSON)
  })
  
  
  
  
  ## Data Download logic ###########################################################
  
  # Create a download handler that takes care of the download process
  # Use the output created by the downloadButton
  output$download <- downloadHandler(
    filename = 'metalp_data.csv',
    content = function(file) {
      write.csv(selectedData(), file, row.names = FALSE)
    }
  )
}
