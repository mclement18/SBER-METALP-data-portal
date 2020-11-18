## This module contains the UI and server code for the grab data management tab

## Create module UI ###############################################################

grabDataUI <- function(id, pool) {
# Create the UI for the grabData module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a tagList with the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return the layout
  tagList(
    instructionsPanelUI(
      ns('info'),
      htmlTemplate('./html_components/grab_data_info.html'),
      initStateHidden = TRUE
    ),
    # Data selection inputs
    div(
      class = 'data-filter',
      # Station selection
      selectInput(
        ns('site'),
        'Station',
        choices = c(
          list(
            All = list(
              All = 'All'
            )
          ),
          parseOptionsWithSections(
            getRows(pool, 'stations', columns = c('name', 'full_name', 'catchment')),
            valueColumn = 'name',
            sectionColumn = 'catchment',
            optionColumn = 'full_name'
          )
        )
      ),
      # Year selection
      textInput(ns('year'), 'Year', placeholder = 'YYYY'),
      # Parameter category selection
      selectInput(
        ns('paramCategory'),
        'Parameter category',
        choices = c(
         'All',
          parseOptions(
            getRows(pool, 'grab_param_categories', columns = 'category'),
            'category'
          )
        )
      )
    ),
    div(
      class = 'rhandsontable-with-controls',
      # Table action
      div(
        class = 'RH-table-actions',
        div(
          class = 'btn-group',
          # New button
          actionButton(ns('new_top'), 'New Row', icon = icon('plus'), class = 'custom-style'),
          # Delete button
          actionButton(ns('delete_top'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary')
        ),
        div(
          class = 'btn-group',
          # Update button
          actionButton(ns('update_top'), 'Update', icon = icon('upload'), class = 'custom-style custom-style--primary'),
          # Refresh button
          actionButton(ns('refresh_top'), 'Refresh', icon = icon('refresh'), class = 'custom-style')
        )
      ),
      # rhandsontable
      rHandsontableOutput(ns('grabData')),
      # Table action
      div(
        class = 'RH-table-actions',
        div(
          class = 'btn-group',
          # New button
          actionButton(ns('new_bottom'), 'New Row', icon = icon('plus'), class = 'custom-style'),
          # Delete button
          actionButton(ns('delete_bottom'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary')
        ),
        div(
          class = 'btn-group',
          # Update button
          actionButton(ns('update_bottom'), 'Update', icon = icon('upload'), class = 'custom-style custom-style--primary'),
          # Refresh button
          actionButton(ns('refresh_bottom'), 'Refresh', icon = icon('refresh'), class = 'custom-style')
        )
      ),
      downloadButton(ns('download'), class = 'custom-style custom-style--primary')
    )
  )
}



## Create module server function ##################################################

grabData <- function(input, output, session, pool) {
# Create the logic for the grabData module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  ## Call instruction panel module ################################################
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  
  
  ## JavaScript callback functions for the Handsontable ###########################
  
  # JavaScript logic in './assets/js/custom_handsontable.js'
  
  # Callback to render the date in 'YYYY-MM-DD' format
  dateRanderer <- 'CustomHandsontable.dateRenderer'
  
  # Callback to validate string in a time format (HH:MM:SS)
  timeValidator <- 'CustomHandsontable.timeFormatValidator'
  
  # Callback that register table hooks
  onTableRender <- 'CustomHandsontable.grabDataOnRenderCallback'
  
  # Callback to render the numbers up to the 4th decimal if any
  numericRenderer <- 'CustomHandsontable.numericRenderer'
  
  
  
  
  ## Data loading logic ############################################################
  
  # Reactive value used to trigger data reload
  reloadData <- reactiveVal(0)
  
  # Reactive expression returning the data
  data <- reactive({
    # Trigger the reload
    reloadData()
    
    # Clear the stored updates
    # Isolate it to avoid issues
    isolate(clearReactiveValues(updates))
    
    # Get selected site
    sites <- input$site
    # Get all available sites
    allSites <- getRows(pool, 'stations', columns = 'name') %>% pull(name)
    # If all sites are selected, sites to allSites
    if (sites == 'All') sites <- allSites
    
    # Get the selected parameter category
    paramCat <- input$paramCategory
    # If all categories are selected
    if (paramCat == 'All') {
      # Get all categories
      columns <- getRows(
        pool,
        'grab_param_categories',
        columns = 'param_name'
      ) %>% 
        pull(param_name)
    } else {
      # Else get only the selected one
      columns <- getRows(
        pool,
        'grab_param_categories',
        category == paramCat,
        columns = 'param_name'
      ) %>% 
        pull(param_name)
    }
    
    # Add the necessary columns
    columns <- c(
      'id', 'station', 'DATE_reading', 'TIME_reading', 'Convert_to_GMT', 'TIME_reading_GMT',
      columns,
      'created_at', 'updated_at'
    )
    
    # Get year
    selectedYear <- input$year
    if (is.null(selectedYear)) selectedYear <- ''
    
    # Get the the data in function of the selectedYear
    if (selectedYear == '') {
      data <- getRows(pool, 'data', station %in% sites, columns = columns)
    } else {
      data <- getRows(pool, 'data',
                      station %in% sites,
                      year(DATE_reading) == selectedYear,
                      columns = columns)
    }
    
    # Parse data
    data %>% mutate(
        station = factor(station, levels = allSites),
        across(matches('DATE', ignore.case = FALSE), ymd),
        across(ends_with('_at'), ymd_hms)
      ) %>% arrange(station, DATE_reading, TIME_reading_GMT)
  })
  
  # Refresh the data when refresh button is pressed
  observeEvent(input$refresh_top | input$refresh_bottom, ignoreInit = TRUE, {
    req(input$refresh_top != 0 | input$refresh_bottom != 0)
    
    reloadData(reloadData() + 1) 
  })
  
  
  
  
  
  ## New data logic ###############################################################
  
  # Create new row when new button is pressed
  observeEvent(input$new_top | input$new_bottom, ignoreInit = TRUE, {
    req(input$new_top != 0 | input$new_bottom != 0)
    
    # Create template for inputs
    inputsTemplate <- data() %>%
      select(station, DATE_reading, TIME_reading, Convert_to_GMT, TIME_reading_GMT) %>%
      head(0)
    
    # Create and show modal with inputs
    showModal(modalDialog(
      title = 'New Data Entry', size = 's',
      div(
        class = 'table-edit-form',
        textOutput(session$ns('form_error')),
        selectInput(session$ns('station'), label = 'station', choices = getRows(pool, 'stations', columns = 'name') %>% pull(name)),
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
  observeEvent(input$cancel, ignoreInit = TRUE, {
    # Clear error
    modalError('')
    
    # Close modal
    removeModal()
  })
  
  
  # Create an observeEvent that react to the modal create button
  observeEvent(input$create, ignoreInit = TRUE, {
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
    
    # If there is no error, remove the modal and reload the table
    if (error == '') {
      removeModal()
      reloadData(reloadData() + 1)
      showNotification('Row successfully created!', type = 'message')
    }
    
    # Render the error, if any
    output$form_error <- renderText(shiny::validate(
      errorClass = 'form',
      need(FALSE, message = modalError())
    ))
  })
  
  
  ## Data update logic ############################################################
  
  # Create reactive values list that will contains the updates
  updates <- reactiveValues()
  
  # Observe the changes
  observeEvent(input$tableChanges, ignoreInit = TRUE, {
    req(input$tableChanges != '[]')
    # Get data, columns and changes
    data <- data()
    columns <- colnames(data)
    changes <- jsonlite::fromJSON(input$tableChanges)
    
    # For each change
    for (i in c(1:nrow(changes))) {
      # Get the change
      change <- changes %>% slice(i)
      
      # Retrieve the data id and column
      dataRow <- data %>% slice(change$row)
      id <- dataRow %>% pull(id) %>% as.character()
      column <- columns[change$column]
      
      # Get the new value
      value <- change$value
      
      # If the value is a character and has only digits and '.'
      # Convert it to numeric data type
      # If it is a date with the 'DD/MM/YYYY' format
      # Convert it to a 'YYYY-MM-DD' format
      if (is.character(value)) {
        if (grepl('^[[:digit:].]+$', value)) {
          value <- as.numeric(value)
        } else if (grepl('^[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}$', value)) {
          value %<>% str_split(value, '/') %>%
            unlist() %>% rev() %>% paste(collapse = '-')
        }
      }
      
      # If the id is NULL, create a list containing some info and the updates as a list
      if (is.null(updates[[id]])) updates[[id]] <- list(
        info = paste(dataRow$station, dataRow$DATE_reading),
        updates = list()
      )
      
      # Add or update the update for the id
      updates[[id]]$updates[[column]] <- value
    }
  })
  
  
  # Update when update button is pressed
  observeEvent(input$update_top | input$update_bottom, ignoreInit = TRUE, {
    req(input$update_top != 0 | input$update_bottom != 0, length(updates) > 0)
    
    # Get the updates as list
    updatesAsList <- reactiveValuesToList(updates)
    
    # For each data id, update its columns
    for (id in names(updatesAsList)) {
      currentId <- updatesAsList[[id]]
      
      # Update currentRow
      error <- updateData(pool, id = as.integer(id), columns = names(currentId$updates), values = currentId$updates)
      
      # Display success or error
      if (error == '') {
        showNotification(
          paste0('Successfully updated ', currentId$info, '!'),
          type = 'message'
        )
      } else {
        showNotification(
          paste0('Error: Cannot update ', currentId$info, '...\n', error),
          duration = NULL,
          type = 'error'
        )
      }
    }
    
    # Clear stored updates
    clearReactiveValues(updates)
    
    # Refresh table
    reloadData(reloadData() + 1)
  })
  
  
  
  
  
  ## Data Deletion logic ###########################################################
  
  # Create a reactive value to store the row ids that will be deleted
  idsToDelete <- reactiveVal()
  
  # Create an observeEvent that react to both delete buttons
  observeEvent(input$delete_top | input$delete_bottom, ignoreInit = TRUE, {
    req(input$delete_top != 0 | input$delete_bottom != 0, input$rowsSelected$start)
    # Get selection and data
    selection <- input$rowsSelected
    data <- data()
    
    # Filter rows to delete
    if (is.null(selection$end)) {
      data %<>% slice(selection$start)
    } else {
      data %<>% slice(selection$start:selection$end)
    }
    
    # Store ids to delete
    idsToDelete(pull(data, id))
    
    # Show confirmation modal
    confirmationModal('You are about to permanently delete rows from this table. Please confirm your action.')
  })
  
  
  # Create an observeEvent linked to the YES button of the confirmation modal
  observeEvent(input$YES, ignoreInit = TRUE, {
    # Remove confirmation modal
    removeModal()
    
    # Delete the rows and retrieve the error
    error <- deleteRows(pool, 'data', idsToDelete())
    
    # Show success or error notification
    if (error == '') {
      showNotification('Rows successfully deleted!', type = 'message')
    } else {
      showNotification(paste('The following error(s) occured:', error, sep = '\n'))
    }
    
    # Reload table
    reloadData(reloadData() + 1)
  })
  
  
  
  
  ## Table rendering ##############################################################
  
  # Render the handsontable
  output$grabData <- renderRHandsontable({
    # Get data and column names
    data <- data()
    colNames <- colnames(data)
    
    # Build the handsontable with the first 3 columns fixed and no context menu
    hot <- rhandsontable(
      data,
      height = if (nrow(data) > 35) 800 else NULL,
      fixedColumnsLeft = 3,
      contextMenu = FALSE
    ) %>%
      # Set the custom numeric renderer globally
      hot_cols(renderer = numericRenderer) %>%
      # Hide and mark as read only the id
      hot_col('id', readOnly = TRUE, colWidths=0.1) %>%
      # Set the dropdown options for the station
      hot_col('station', type = 'dropdown', source = levels(data$station), colWidths=60) %>%
      # Set the date format and renderer for the DATE
      hot_col(
        'DATE_reading',
        type = 'date',
        dateFormat = 'YYYY-MM-DD',
        renderer = dateRanderer
      ) %>%
      # Add a time validator for the always present time columns
      hot_col(
        c('TIME_reading', 'Convert_to_GMT', 'TIME_reading_GMT'),
        type = 'text',
        validator = timeValidator
      ) %>%
      # Mark the created and updated at columns as read only
      hot_col(c('created_at', 'updated_at'), readOnly = TRUE)
    
    # If the two following time columns are present, add a validator to them
    if ('Vaisala_CO2_time' %in% colNames) hot %<>% hot_col('Vaisala_CO2_time', validator = timeValidator)
    if ('unused_WTW_DO_2_time' %in% colNames) hot %<>% hot_col('unused_WTW_DO_2_time', validator = timeValidator)
    
    # Add hooks callback to the table
    hot %>% htmlwidgets::onRender(
      onTableRender,
      data = list(
        onChangeId = session$ns('tableChanges'),
        afterSelectionId = session$ns('rowsSelected')
      )
    )
  })
  

  
  
  ## Download logic ##############################################################
  
  output$download <- downloadHandler(
    filename = function() {
      filename <- paste(input$site, 'grab-data', sub('[ _]', '-', input$paramCategory), Sys.Date(), sep = '_')
      year <- input$year
      if (year != '') filename <- paste(year, filename, sep = '_')
      paste0(filename, '.csv')
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}
