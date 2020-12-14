## This module contains the UI and server code for the tools layout

## Create module UI function ######################################################

toolsLayoutUI <- function(id, toolName, ...) {
# Create the UI for the toolsLayout module
# Parameters:
#  - id: String, the module id
#  - toolName: String, the name of the tool
#  - toolModuleUI: Function, the tool module UI function
#  - ...: All other arguments needed by the inner module function
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  tagList(
    h1(toolName, class = 'global-header'),
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
                               toolModule, toolModuleUI, pool, ...) {
# Create the logic for the toolsLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - toolModule: Function, the tool module server function
#  - toolModuleUI: Function, the tool module UI function
#  - pool: The pool connection to the database
#  - ...: All other arguments needed by the inner module function
# 
# Returns NULL
  
  ## Create update reactive expression ############################################
  
  update <- reactive(input$update)
  
  
  
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
