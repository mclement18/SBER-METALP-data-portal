## This module contains the UI and server code for the Data Request feature


## Create module UI ###############################################################

requestDataUI <- function(id) {
# Create the UI for the requestData module
# Parameters:
#  - id: String, the module id
# 
# Returns a actionButton
  
  # Create namespace
  ns <- NS(id)
  
  # Return a list with a request button and a disclaimer
  list(
    # Create an actionButton disabled by default
    'button' = disabled(actionButton(ns('requestData'), 'Request Data', class = 'custom-style custom-style--primary')),
    # Load the HTML template with the disclaimer
    'disclaimer' = htmlTemplate('./html_components/data_privacy.html')
  )
}



## Create module server function ##################################################

requestData <- function(input, output, session, pool, selectedData, dataSelectionInput) {
# Create the logic for the requestData module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - selectedData: Reactive expression, the data selected with the form in the outer module
#  - dataSelectionInput: Reactive expression, return a list of the data selection inputs
# 
# Returns NULL
  
  ## Request Data button state logic ###########################################################
  
  # Create an observeEvent that react to data change to set downloadButton state
  observeEvent(selectedData(), ignoreInit = TRUE, {
    # Enable button when df is not empty
    toggleState('requestData', condition = nrow(selectedData()) >= 1)
  })
  
  
  
  
  ## Request Data modal form logic ###########################################################
  
  # Create an observeEvent that react to the requestData button press
  observeEvent(input$requestData, ignoreInit = TRUE, {
    # Display a modal containing the form asking for the requisitor information
    showModal(
      # Create the modal form
      modalDialog(
        title = 'Requisitor Informations',
        # Create the form
        div(
          class = 'modal-form',
          div(
            class = 'requisitor-info',
            # Name
            withAttributes(
              textInput(session$ns('requisitorName'), 'Name*', placeholder = 'max 50 char...'),
              maxlength = 50
              ),
            # Email
            withAttributes(
              textInput(session$ns('requisitorEmail'), 'Email*', placeholder = 'john.doe@epfl.com'),
              maxlength = 75
            ),
            # Institution
            withAttributes(
              textInput(session$ns('requisitoInstitution'), 'Institution / Company*', placeholder = 'max 50 char...'),
              maxlength = 50
            ),
            # Reasons
            withAttributes(
              textAreaInput(session$ns('requisitorReason'), 'Motivations*', resize = 'vertical', placeholder = 'max 250 char...'),
              maxlength = 250
            ),
          ),
          div(
            class = 'data-terms',
            htmlTemplate('./html_components/data_terms_use.html'),
            checkboxInput(
              inputId = session$ns('dataTermsUse'),
              label = '* I agree with the Terms of use for the METALP data and Privacy policy.',
              value = FALSE)
          ),
          p('* mandatory fields')
        ),
        # Create footer buttons
        footer = tagList(
          # Send request button disabled by default
          disabled(
            actionButton(session$ns('sendRequest'), 'Send Request', class = 'custom-style custom-style--primary')
          ),
          # Cancel button
          modalButtonWithClass('Cancel', class = 'custom-style')
        )
      )
    )
  })
  
  
  
  
  ## Send Request Button state logic ##########################################################
  
  # Create a reactive expression returning a boolean vector of checks
  checkInputs <- reactive(c(
    input$requisitorName != '',
    nchar(input$requisitorName) <= 50,
    isValidEmail(input$requisitorEmail),
    nchar(input$requisitorEmail) <= 75,
    input$requisitoInstitution != '',
    nchar(input$requisitoInstitution) <= 50,
    input$requisitorReason != '',
    nchar(input$requisitorReason) <= 250,
    input$dataTermsUse
  ))
  
  # Debounce checks to avoid it to rerun for every single character typed
  checkInputs_d <- debounce(checkInputs, 400)
  
  # Create an observeEvent that react to the inputs check and toggle the send request button state accordingly
  observeEvent(checkInputs_d(), ignoreInit = TRUE, toggleState('sendRequest', condition = all(checkInputs_d())))
  
  
  
  
  ## Email logic ##############################################################################
  
  # Create an observeEvent that react to sendRequest button
  observeEvent(input$sendRequest, ignoreInit = TRUE, {
    # Run only if all fields are filled and correct
    req(checkInputs())
    
    # Create the requested data
    data <- paste(
      # Add the date range and data type requested
      paste0('Date Range: ', dataSelectionInput()$min, ' - ', dataSelectionInput()$max),
      paste0('Data Type: ', dataSelectionInput()$data),
      sep = '\n'
    )
    
    # If sensor data are requested
    if (dataSelectionInput()$data == 'sensor') {
      # Append the data interval frequency and modeled data
      data <- paste(
        data,
        paste0('Data frequency: ', dataSelectionInput()$dataFreq),
        paste0('Modeled Data: ', dataSelectionInput()$modeled),
        sep = '\n'
      )
    }
    

    # The selected parameters and stations
    data <- paste(
      data,
      paste0('Stations: [', paste(dataSelectionInput()$sites, collapse = ', '), ']'),
      paste0('Parameters: [', paste(dataSelectionInput()$parameters, collapse = ', '), ']'),
      sep = '\n')
    
    # Save request
    error <- createRequest(
      pool = pool,
      name = input$requisitorName,
      email = input$requisitorEmail,
      institution = input$requisitoInstitution,
      data = data,
      reason = input$requisitorReason
    )
    
    # Show success or error
    if (error == '') {
      showNotification('Request sent successfully!', type = 'message')
      removeModal()
    } else {
      showNotification(
        paste(
          'Request could not be sent...',
          error,
          sep = '\n'
        ),
        duration = NULL,
        type = 'error'
      )
    }
  })
}
