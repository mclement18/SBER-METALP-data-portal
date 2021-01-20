## This module contains the UI and server code for the tools layout

## Create module UI function ######################################################

toolsLayoutUI <- function(id, toolName, instructionsPanelUIArgs = NULL, ...) {
# Create the UI for the toolsLayout module
# Parameters:
#  - id: String, the module id
#  - toolName: String, the name of the tool
#  - instructionsPanelUIArgs: Named list, arguments to pass to the instructionsPanelUI function, should not contain the id argument.
#                             Set to NULL, to do not create a panel. Default: NULL
#  - ...: All other arguments needed by the inner module function
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  tagList(
    h1(toolName, class = 'global-header'),
    if (!is.null(instructionsPanelUIArgs)) do.call(instructionsPanelUI, c(list(id = ns('info')), instructionsPanelUIArgs)),
    div(
      class = 'tool-layout',
      div(
        class = 'action-with-error',
        div(
          class = 'errors-and-warnings',
          uiOutput(ns('updateError'))
        ),
        div(
          class = 'action btn-group',
          actionButton(ns('update'), 'Update', class = 'custom-style custom-style--primary'),
          disabled(
            actionButton(ns('removeAll'), 'Remove All', icon = icon('trash-alt'), class = 'custom-style')
          )
        )
      ),
      div(
        id = ns('entries'),
        class = 'entries'
      ),
      div(
        class = 'add-entry btn-group',
        actionButton(ns('plus'), 'Add Entry', icon = icon('plus'), class = 'custom-style'),
        disabled(
          actionButton(ns('minus'), 'Remove Last', icon = icon('minus'), class = 'custom-style')
        )
      )
    )
  )
}



## Create module server function ##################################################

toolsLayout <- function(input, output, session,
                        toolModule, toolModuleUI, pool,
                        instructionPanel = NULL, updateVerification = FALSE, ...) {
# Create the logic for the toolsLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - toolModule: Function, the tool module server function
#  - toolModuleUI: Function, the tool module UI function
#  - pool: The pool connection to the database
#  - instructionPanel: Boolean or NULL, if it is NULL, the instructionsPanel module is not called.
#                      If it is a Boolean, indicates the instruction panel initial visibility. TRUE == hidden, FALSE == visible
#  - updateVerification: Boolean, whether to perform verification before update, default: FALSE
#  - ...: All other arguments needed by the inner module function
# 
# Returns NULL
  
  ## Call instruction panel module if needed #######################################################
  
  if (!is.null(instructionPanel)) callModule(instructionsPanel, 'info', initStateHidden = instructionPanel)
  
  
  
  
  ## Create update reactive value and update verification ############################################
  
  # Create the update reactive value to pass to entry module
  update <- reactiveVal(0)
  
  # Create an observeEvent that react to the update button
  observeEvent(input$update, ignoreInit = TRUE, {
    # If a verification is needed, show verification modal
    if (updateVerification) {
      showModal(modalDialog(
        title = 'Update confirmation', size = 's',
        div(
          class = 'login-form',
          # Info
          p('Enter your credential to confirme update.'),
          p('You need to have the specific authorization to perform the update.'),
          # Add an text output to log the errors
          textOutput(session$ns('authorizationError')),
          # Username and password inputs
          textInput(session$ns('username'), 'Username'),
          passwordInput(session$ns('password'), 'Password'),
        ),
        # Action buttons
        footer = tagList(
          actionButton(session$ns('authorize'), 'Authorize', class = 'custom-style custom-style--primary'),
          actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
        )
      ))
    } else {
      # Otherwise increase update value
      update(update() + 1)
      
      # Show notifiaction that the update was sent
      showNotification('Update sent!\nCheck individual results.', type = 'message')
    }
  })
  
  # Save authorization errors
  authorizationError <- reactiveVal('')
  
  # Create an observe event that react to the authorize button
  observeEvent(input$authorize, ignoreInit = TRUE, {
    # Get user from db
    userResult <- loginUser(pool, input$username) %>%
      mutate(intern_confirmation = as.logical(intern_confirmation))
    
    # If a user was found and he has the authorization, verify password
    if (nrow(userResult) == 1 & userResult$intern_confirmation) {
      # Increase update value if correct password
      if (sodium::password_verify(userResult$password, input$password)) {
        update(update() + 1)
        
        # Remove authorization modal
        removeModal()
        
        # Show notifiaction that the update was sent
        showNotification('Update sent!\nCheck individual results.', type = 'message')
        
        # Stop here
        return()
      }
      authorizationError('Incorrect username / password combination!')
    } else {
      authorizationError('You do not have the right to authorize the update.')
    }
  })
  
  # Create an observeEvent that react to the cancel button
  observeEvent(input$cancel, ignoreInit = TRUE, {
    # Clear user log in error
    authorizationError('')
    
    # Close modal
    removeModal()
  })
  
  # Render the authorization error
  output$authorizationError <- renderText(shiny::validate(
    errorClass = 'form',
    need(FALSE, message = authorizationError())
  ))
  
  
  
  
  
  
  ## Error display logic ##########################################################
  
  # Track errors, warnings and successes NB
  errors <- reactiveValues(errors = 0, warnings = 0, success = 0)
  
  # Reset it before update
  observeEvent(input$update, {
    errors$errors <- 0
    errors$warnings <- 0
    errors$success <- 0
  })
  
  # Display errors
  output$updateError <- renderUI(
    htmlTemplate(
      './html_components/tool_error_summary.html',
      errorNb = errors$errors,
      errorIcon = icon('times-circle'),
      warningNb = errors$warnings,
      warningIcon = icon('exclamation-triangle'),
      successNb = errors$success,
      successIcon = icon('check-circle')
    )
  )
  
  
  
  ## Entries Nb tracking ##########################################################
  
  # Create a reactive value that keep track of the number of displayed entries
  entryNb <- reactiveVal(0)
  
  # Track entries observers
  entriesObservers <- reactiveValues()
  
  
  
  
  ## Entry adding logic ############################################################
  
  # Add an observeEvent that will run upon click on the add entry button
  observeEvent(input$plus, ignoreInit = TRUE, {
    # If there is currently no entry enable remove unit button
    if (entryNb() == 0) {
      enable('minus')
      enable('removeAll')
    }
    
    # Increment the number of entries
    entryNb(entryNb() + 1)
    
    # Insert the new entry UI elements
    insertUI(
      selector = paste0('#', session$ns('entries')),
      where = 'beforeEnd',
      ui = div(
        id = session$ns(paste0('entry', entryNb())),
        class = 'entry',
        entryLayoutUI(session$ns(entryNb()), pool, toolModuleUI, ...)
      ),
      immediate = TRUE
    )
    
    # Call new entry module function and retrieve, if any, the named list containing:
    result <- callModule(entryLayout, entryNb(),
                         pool, toolModule, update, ...)
    
    # Save entry observers
    entriesObservers[[as.character(entryNb())]] <- result$observers
    
    # Update errors number using module result
    entriesObservers[[as.character(entryNb())]]$layoutErrorLogic <- observeEvent(result$errors(), {
      errors$errors <- errors$errors + result$errors()$errors
      errors$warnings <- errors$warnings + result$errors()$warnings
      errors$success <- errors$success + result$errors()$success
    })
  })
  
  
  
  ## Entry removing logic ##########################################################
  
  # Add an observeEvent that will run upon click on the remove entry button
  observeEvent(input$minus, ignoreInit = TRUE, {
    req(entryNb() > 0)
    # Remove last entry
    removeUI(
      paste0('#', session$ns(paste0('entry', entryNb()))),
      immediate = TRUE
    )
    
    # Destroy observers
    destroyObservers(entriesObservers[[as.character(entryNb())]])
    
    # Decrement entry nb
    entryNb(entryNb() - 1)
    
    # If no entry left, disable remove entry button
    if (entryNb() == 0) {
      disable('minus')
      disable('removeAll')
    }
  })
  
  
  # Add an observeEvent that will run upon click on the remove all entries button
  observeEvent(input$removeAll, ignoreInit = TRUE, {
    req(entryNb() > 0)
    
    # Rome all entries
    for (i in 1:entryNb()) {
      removeUI(
        paste0('#', session$ns(paste0('entry', i))),
        immediate = TRUE
      )
      
      # Destroy observers
      destroyObservers(entriesObservers[[as.character(i)]])
    }
    
    # Reset entry number
    entryNb(0)
    
    # Disable remove buttons
    disable('minus')
    disable('removeAll')
    
    # Reset errors
    errors$errors <- 0
    errors$warnings <- 0
    errors$success <- 0
  })
}
