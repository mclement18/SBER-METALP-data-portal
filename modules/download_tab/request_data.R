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

requestData <- function(input, output, session, selectedData, dataSelectionInput) {
# Create the logic for the requestData module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
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
            textInput(session$ns('requisitorName'), 'Name*', placeholder = 'Otto Octavius'),
            # Email
            textInput(session$ns('requisitorEmail'), 'Email*', placeholder = 'otto.octavius@octaviusindustries.com'),
            # Institution
            textInput(session$ns('requisitoInstitution'), 'Institution / Company*', placeholder = 'Octavius Industries'),
            # Reasons
            textAreaInput(session$ns('requisitorReason'), 'Motivations*', resize = 'vertical', placeholder = 'Kill Spider-Man, Destroy Oscorp, Take revenge on Norman Osborn'),
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
    isValidEmail(input$requisitorEmail),
    input$requisitoInstitution != '',
    input$requisitorReason != '',
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
    req(
      input$requisitorName,
      isValidEmail(input$requisitorEmail),
      input$requisitoInstitution,
      input$requisitorReason,
      input$dataTermsUse
    )
    
    # Set the sender email address
    from <- paste0('From: <', NOREPLY_ADDRESS, '>')
    # Set the recipient email address
    to <- paste0('To: <', TO_ADDRESS, '>')
    # Set the reply to email address, i.e. the requisitor
    replyTo <- paste0('Reply-To: ', input$requisitorName, ' <', str_trim(input$requisitorEmail), '>')
    # Create the email subject
    subject <- paste0('Subject: Data Request from ', input$requisitorName, ' <', str_trim(input$requisitorEmail), '>')
    # Create the email body
    body <- paste(
      # Add the requisitor's name and institution
      paste0(input$requisitorName, ' from ', input$requisitoInstitution, ' would like to get the following data:'),
      '',
      # Add the date range and data type requested
      paste0('Date Range: ', dataSelectionInput()$min, ' - ', dataSelectionInput()$max),
      paste0('Data Type: ', dataSelectionInput()$data),
      sep = '\n'
    )
    
    # If sensor data are requested
    if (dataSelectionInput()$data == 'sensor') {
      # Append to the body
      body <- paste(
        body,
        # The data interval frequency and modeled data
        paste0('Data frequency: ', dataSelectionInput()$dataFreq),
        paste0('Modeled Data: ', dataSelectionInput()$modeled),
        sep = '\n'
      )
    }
    
    # Finally append to the body
    body <- paste(
      body,
      # The selected parameters and stations
      paste0('Stations: [', paste(dataSelectionInput()$sites, collapse = ', '), ']'),
      paste0('Parameters: [', paste(dataSelectionInput()$parameters, collapse = ', '), ']'),
      '\n',
      # And the reasons for the request
      'For the following reasons:',
      '',
      input$requisitorReason,
      sep = '\n')
    
    # Create email
    email <- paste(
      to,
      from,
      replyTo,
      subject,
      '',
      body,
      sep = '\n'
    )
    
    # Send email with UNIX sendmail command
    tryCatch({
      # Send email
      system2('sendmail', args = c('-t'), input = email)
      # Close modal
      removeModal()
      # Show success notification
      showNotification('Request sent successfully!', type = 'message')
    },
    # Show error notification
      error = function(e) showNotification(
        paste(
          'Request could not be sent...',
          e$message,
          sep = '\n'
        ),
        type = 'error'
      )
    )
    
    # Cleanup email
    rm(email)
  })
}
