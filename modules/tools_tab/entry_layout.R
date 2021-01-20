## This module contains the UI and server code for the entry layout

## Create module UI function ######################################################

entryLayoutUI <- function(id, pool, toolModuleUI, createNew = FALSE, ...) {
# Create the UI for the entryLayout module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - toolModuleUI: Function, the tool module UI function
#  - createNew: Boolean, create or not the new button
#  - ...: All other arguments needed by the inner module function
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  div(
    class = 'entry-layout',
    div(
      class = 'header',
      div(
        class = 'entry-actions',
        if (createNew) actionButton(ns('new'), 'New Entry', class = 'custom-style') else NULL,
        # Site selection
        selectInput(
          inputId = ns('site'),
          label = 'Station',
          choices = c(
            'Choose a station ...' = '',
            parseOptions(
              getRows(pool, 'stations', columns = c('order', 'name')) %>%
              arrange(order) %>% select(-order),
              'name'
            )
          )
        ),
        # Date selection
        selectInput(
          ns('date'),
          'Date',
          c(
            'Choose a station ...' = ''
          )
        )
      ),
      # Show/Hide tables
      actionButton(ns('show'), icon('chevron-up'), class = 'icon-btn show-hide')
    ),
    div(
      id = ns('content'),
      class = 'entry-content',
      # Creation and update dates
      div(
        class = 'dates',
        div(
          class = 'date',
          span('Created at:'),
          textOutput(ns('creationDate'), inline = TRUE)
        ),
        div(
          class = 'date',
          span('Updated at:'),
          textOutput(ns('updateDate'), inline = TRUE)
        )
      ),
      # Tool UI
      toolModuleUI(ns('tool'), pool, ...),
      # Update errors
      div(
        class = 'entry-errors',
        uiOutput(ns('entryErrors'))
      )
    )
  )
}



## Create module server function ##################################################

entryLayout <- function(input, output, session, pool,
                        toolModule, update, createNew = FALSE, ...) {
# Create the logic for the entryLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - toolModule: Function, the tool module server function
#  - update: Reactive value, used as trigger for update
#  - createNew: Boolean, create or not the new button
#  - ...: All other arguments needed by the inner module function
# 
# Returns a reactive expression containing a list with number of errors, warnings or success
  
  ## Track observer ##############################################################
  
  # Reactive values that contain the observers output
  observersOutput <- reactiveValues()
  
  
  
  
  
  ## Date input logic ##############################################################
  
  # Create a reactive value that track if the selection date must be updated
  updateDate <- reactiveVal(TRUE)
  
  # Create an observeEvent that react to site change to update the date selection
  observersOutput$dateLogic <- observeEvent(input$site, ignoreInit = TRUE, {
    req(input$site)
    
    # If the date must be updated then updated the date
    if (updateDate()) {
      updateSelectInput(session, 'date', choices = c(
        'Pick a date ...' = '',
        as.character(getDates(pool, station == local(input$site), descending = TRUE))
      ))
    } else {
      # Set the updateDate to true to react to the next site change
      updateDate(TRUE)
    }
  })
  
  # Create a reactive expression that parse the datetime
  datetime <- reactive({
    req(input$date)
    # Parse datetime
    datetime <- ymd_hms(input$date, tz = 'GMT')
    
    # If the input is a date
    if (!is.na(datetime)) {
      # Return a list containing the datetime, date and time components
      list(
        datetime = datetime,
        date = date(datetime),
        time = paste(
          hour(datetime),
          minute(datetime),
          ifelse(second(datetime) < 10, paste0(0, second(datetime)), second(datetime)),
          sep = ':'
        )
      )
    } else NULL
  })
  
  
  
  
  
  ## New entry logic ##############################################################
  
  if (createNew) {
    # Show modal for new data creation
    observersOutput$newLogic <- observeEvent(input$new, ignoreInit = TRUE, {
      # Create and show modal with inputs
      showModal(modalDialog(
        title = 'New Data Entry', size = 's',
        div(
          class = 'table-edit-form',
          textOutput(session$ns('form_error')),
          selectInput(
            session$ns('station'),
            label = 'station',
            choices = getRows(pool, 'stations', columns = c('order', 'name')) %>%
              arrange(order) %>% pull(name)
          ),
          textInput(session$ns('DATE_reading'), 'DATE_reading', placeholder = 'YYYY-MM-DD'),
          textInput(session$ns('TIME_reading'), 'TIME_reading', placeholder = 'HH:MM:SS'),
          textInput(session$ns('Convert_to_GMT'), 'Convert_to_GMT', placeholder = 'HH:MM:SS'),
          textInput(session$ns('TIME_reading_GMT'), 'TIME_reading_GMT', placeholder = 'HH:MM:SS')
        ),
        footer = tagList(
          actionButton(session$ns('create'), 'Create', class = 'custom-style custom-style--primary'),
          actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
        )
      ))
    })
    
    # Date and time input validation
    dateTimeValidation <- reactive({
      all(
        grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$', input$DATE_reading),
        grepl('^[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}$', c(input$TIME_reading, input$Convert_to_GMT, input$TIME_reading_GMT))
      )
    })
    
    # Create a reactive value to save error happening in the modal
    modalError <- reactiveVal('')
    
    # Create an observeEvent that react to the modal cancel button
    observersOutput$cancelLogic <- observeEvent(input$cancel, ignoreInit = TRUE, {
      # Clear error
      modalError('')
      
      # Close modal
      removeModal()
    })
    
    # Create new row in the DB
    # Create an observeEvent that react to the modal create button
    observersOutput$createLogic <- observeEvent(input$create, ignoreInit = TRUE, {
      req(dateTimeValidation())
      
      # Create new row
      error <- createData(
        pool = pool,
        station = input$station,
        DATE_reading = input$DATE_reading,
        TIME_reading = input$TIME_reading,
        Convert_to_GMT = input$Convert_to_GMT,
        TIME_reading_GMT = input$TIME_reading_GMT
      )
      
      # Save error
      modalError(error)
      
      # If there is no error, remove the modal and populate the site and date inputs
      if (error == '') {
        # Remove modal and show success notif
        removeModal()
        showNotification('Row successfully created!', type = 'message')
        
        # Indicate that the date should not update upon the next site selection
        updateDate(FALSE)
        
        # Update inputs
        updateSelectizeInput(session, 'site', selected = input$station)
        updateSelectInput(session, 'date',
                          choices = c(
                            'Pick a date ...' = '',
                            as.character(getDates(pool, station == local(input$station), descending = TRUE))
                          ),
                          selected = paste(input$DATE_reading, input$TIME_reading_GMT))
      }
      
      # Render the error, if any
      output$form_error <- renderText(shiny::validate(
        errorClass = 'form',
        need(FALSE, message = modalError()),
        need(dateTimeValidation(), message = 'You need to fill all the inputs in the correct format.')
      ))
    })
  }
  
 
  
  
  
  
  
  ## Call tool module #############################################################
  
  # Call module and retrieve data
  result <- callModule(toolModule, 'tool', pool, reactive(input$site), datetime, ...)
  
  # Save observers
  observersOutput$toolModuleObservers <- result$observers
  
  # Get errors and warnings and save observers
  observersOutput$errorLogic <- observeEvent(result$errors(), {
    # Add errors
    errors$errors <- c(
      errors$errors,
      result$errors()$errors
    )
    # Add warnings
    errors$warnings <- c(
      errors$warnings,
      result$errors()$warnings
    )
  })
  
  # Display creation and update dates
  output$creationDate <- renderText(result$df() %>% pull(created_at))
  
  output$updateDate <- renderText(result$df() %>% pull(updated_at))
  
  
  
  
  
  ## Update logic #################################################################
  
  # Track if update is done
  updated <- reactiveVal(FALSE)
  
  # Update the row when update button is pressed
  observersOutput$updateLogic <- observeEvent(update(), ignoreInit = TRUE, {
    req(result$df(), update() != 0)
    # Get updated row
    row <- result$df()
    # Check that it is a single row
    if (nrow(row) == 1) {
      # Get updates
      updates <- row %>% 
        select(
          -c(id, station, DATE_reading, TIME_reading, Convert_to_GMT, TIME_reading_GMT, ends_with('_at'))
        ) %>% as.list()
      
      # Remove NAs
      updates[sapply(updates, is.na)] <- NULL
      
      # Send updates
      error <- updateData(
        pool = pool,
        id = row$id,
        columns = names(updates),
        values = updates
      )
      
      # Parse update errors
      if (error != '') {
        errors$errors <- c(
          errors$errors,
          paste('Error: Could not update row.', error, sep = '\n')
        )
      }
    } else {
      # Add multiple rows error
      errors$errors <- c(
        errors$errors,
        'Error: Cannot update an empty row or more than one row.'
      )
    }
    
    # Set updated to TRUE, to display errors
    updated(TRUE)
  })
  
  # Reset updated to false when site or date is changed
  observersOutput$resetUpdatedLogic <- observe({
    input$site;input$date
    updated(FALSE)
  })
  
  
  
  
  
  ## Error display logic ##########################################################
  
  # Track errors
  errors <- reactiveValues(errors = c(), warnings = c())
  
  # Render errors and warnings or success
  output$entryErrors <- renderUI({
    req(updated())
    # If no error or warning present, show success
    if (length(errors$errors) == 0 & length(errors$warnings) == 0) {
      p(class = 'success', 'Successfully updated row!')
    } else {
      # Error and warning layout
      tagList(
        # If any error, display it
        if (length(errors$errors) > 0){
          pre(
            class = 'error',
            paste(
              'Errors:',
              '----------',
              '',
              paste(errors$errors, collapse = '\n\n'),
              sep = '\n'
            )
          )
        } else NULL,
        # If any warning, display it
        if (length(errors$warnings) > 0) {
          pre(
            class = 'warning',
            paste(
              'Warnings:',
              '----------',
              '',
              paste(errors$warnings, collapse = '\n\n'),
              sep = '\n'
            )
          )
        } else NULL
      )
    }
  })
  
  
  
  
  
  
  ## Show/Hide content logic ######################################################
  
  # Track content visibility
  showContent <- reactiveVal(TRUE)
  
  # Show or hide content
  observersOutput$contentLogic <- observeEvent(input$show, ignoreInit = TRUE, {
    # Toggle showContent
    showContent(!showContent())
    
    # Toggle content visibility
    toggleElement('content', anim = TRUE, condition = showContent())
    
    # Update link icon
    if (showContent()) icon <- icon('chevron-up') else icon <- icon('chevron-down')
    updateActionButton(session, 'show', icon = icon)
  })
  
  
  
  
  
  ## Return errors nb #############################################################
  
  # Return a reactive expression
  return(
    list(
      # All observers to be able to destroy them from the outer module
      observers = observersOutput,
      # Errors, warning and success Nb to updated layout summary
      errors = reactive({
        if (updated()) {
          # Rerun on update
          update()
          # Return a list with error, warning and success Nb
          list(
            errors = if (length(errors$errors) > 0) 1 else 0,
            warnings = if (length(errors$warnings) > 0) 1 else 0,
            success = if (length(errors$errors) == 0 & length(errors$warnings) == 0) 1 else 0
          )
        }
      })
    )
  )
}
