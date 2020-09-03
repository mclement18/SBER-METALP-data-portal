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
  
  # Create and return an actionButton disabled by default
  disabled(
    actionButton(ns('requestData'), 'Request Data', class = 'custom-style custom-style--primary')
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
    showModal(
      modalDialog(
        title = 'Requisitor Informations',
        div(
          class = 'modal-form',
          textInput(session$ns('requisitorName'), 'Name*', placeholder = 'Otto Octavius'),
          textInput(session$ns('requisitorEmail'), 'Email*', placeholder = 'otto.octavius@octaviusindustries.com'),
          textInput(session$ns('requisitoInstitution'), 'Institution / Company*', placeholder = 'Octavius Industries'),
          textAreaInput(session$ns('requisitorReason'), 'Motivations*', resize = 'vertical', placeholder = 'Kill Spider-Man, Destroy Oscorp, Take revenge on Norman Osborn'),
          p('* mendatory fields')
        ),
        footer = tagList(
          disabled(
            actionButton(session$ns('sendRequest'), 'Send Request', class = 'custom-style custom-style--primary')
          ),
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
    input$requisitorReason != ''
  ))
  
  # Debounce checks to avoid it to rerun for every single character typed
  checkInputs_d <- debounce(checkInputs, 250)
  
  # Create an observeEvent that react to the inputs check and toggle the send request button state accordingly
  observeEvent(checkInputs_d(), ignoreInit = TRUE, toggleState('sendRequest', condition = all(checkInputs_d())))
  
  
  
  
  ## Email logic ##############################################################################
  
  # Create an observeEvent that react to sendRequest button
  observeEvent(input$sendRequest, ignoreInit = TRUE, {
    req(input$requisitorName, isValidEmail(input$requisitorEmail), input$requisitoInstitution, input$requisitorReason)
    
    from <- 'yourdummysenderemail@epfl.ch'
    to <- 'metalp-poeple@epfl.ch'
    replyTo <- paste0('Reply to ', input$requisitorName, ' <', str_trim(input$requisitorEmail), '>')
    subject <- paste0('Data Request from ', input$requisitorName, ' <', str_trim(input$requisitorEmail), '>')
    body <- paste(
      paste0(input$requisitorName, ' from ', input$requisitoInstitution, ' would like to get the following data:'),
      '',
      paste0('Date Range: ', dataSelectionInput()$min, ' - ', dataSelectionInput()$max),
      paste0('Data Type: ', dataSelectionInput()$data),
      sep = '\n'
    )
    
    if (dataSelectionInput()$data == 'sensor') {
      body <- paste(
        body,
        paste0('Data frequency: ', dataSelectionInput()$dataFreq),
        paste0('Modeled Data: ', dataSelectionInput()$modeled),
        paste0('Single Points Info: ', dataSelectionInput()$singelPoint),
        sep = '\n'
      )
    }
    
    body <- paste(
      body,
      paste0('Stations: [', paste(dataSelectionInput()$sites, collapse = ', '), ']'),
      paste0('Parameters: [', paste(dataSelectionInput()$parameters, collapse = ', '), ']'),
      '\n',
      'For the following reasons:',
      '',
      input$requisitorReason,
      sep = '\n')
    smtp <- list(host.name = "smtp.server.ch", port = 25, 
                 user.name = "********",            
                 passwd = "******", ssl = TRUE)
  
    
    send.mail(
      from = from,
      to = to,
      replyTo = replyTo,
      subject = subject,
      body = body,
      smtp = smtp,
      authenticate = TRUE,
      send = TRUE
    )
  })
}
