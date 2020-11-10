## This module contains the UI and server code for the Login

## Create module UI ###############################################################

requestsManagementUI <- function(id, pool) {
# Create the UI for the requestsManagement module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Get unread request nb
  unreadNb <- countRows(pool, 'data_requests', read == FALSE)
  
  # Create the div content with the button
  uiContent <- tagList(actionButton(ns('dataRequests'), icon('bell'), class = 'icon-btn'))
  
  # Add the unread request nb if any
  if (unreadNb > 0 & unreadNb < 10) {
    uiContent <- tagList(
      uiContent,
      div(
        class = 'request-nb',
        id = 'request-nb',
        unreadNb
      )
    )
  } else if (unreadNb >= 10) {
    uiContent <- tagList(
      uiContent,
      div(
        class = 'request-nb',
        id = 'request-nb'
      )
    )
  }
  
  # Navbar UI
  navbarUI <- div(
    class = 'data-requests-link',
    id = 'data-requests-nav',
    uiContent
  )
  
  # Tab content
  tabContent <- tagList(
    h1('Data Requests', class = 'global-header'),
    div(
      class = 'table-with-controls',
      div(
        class = 'table-controls',
        div(
          class = 'btn-group',
          actionButton(ns('read_top'), 'Read / Unread', icon = icon('book-open'), class = 'custom-style'),
          actionButton(ns('delete_top'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary')
        ),
        actionButton(ns('refresh_top'), 'Refresh', icon = icon('refresh'), class = 'custom-style')
      ),
      # Create a table of users
      DTOutput(ns('table')),
      div(
        class = 'table-controls',
        div(
          class = 'btn-group',
          actionButton(ns('read_bottom'), 'Read / Unread', icon = icon('book-open'), class = 'custom-style'),
          actionButton(ns('delete_bottom'), 'Delete', icon = icon('trash-alt'), class = 'custom-style custom-style--primary')
        ),
        actionButton(ns('refresh_bottom'), 'Refresh', icon = icon('refresh'), class = 'custom-style')
      )
    )
  )
  
  # Return a list with the UI elements
  list(
    navbarUI = navbarUI,
    tabContent = tabContent
  )
}



## Create module server function ##################################################

requestsManagement <- function(input, output, session, pool, navbarSession, navbarId, requestTabId) {
# Create the logic for the requestsManagement module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - navbarSession: Shiny session object, the session of the navbar to update
#  - navbarId: String, the id of the navbar to update
#  - requestTabId: String, the id of the request tab to select
# 
# Returns NULL
  
  ## Open data request management tab #############################################
  
  # Update main navigation navbar
  observeEvent(input$dataRequests, ignoreInit = TRUE, updateNavbarPage(navbarSession, navbarId, selected = requestTabId))
  
  
  
  
  
  ## Requests loading ################################################################
  
  # Create a reactive value that will be used as a trigger to reload the requests
  reloadRequests <- reactiveVal(0)
  
  # Create a reactive expression that load and return the requests
  loadRequests <- reactive({
    # Call reactive value to trigger a reload if changed
    reloadRequests()
    
    # Retrieve requests
    requests <- getRows(pool, 'data_requests') %>%
      mutate(
        read = as.logical(read),
        across(ends_with('_at'), ymd_hms)
      )
    
    # Update the unread requests indicator
    unreadNb <- requests %>% filter(read == FALSE) %>% summarise(nb = n()) %>% pull(nb)
    
    # Display or hide unread requests nb
    # First remove any UI present
    removeUI('#request-nb', immediate = TRUE)
    
    # Then display the correct UI
    if (unreadNb > 0 & unreadNb < 10) {
      insertUI(
        selector = '#data-requests-nav',
        where = 'beforeEnd',
        ui = div(
          class = 'request-nb',
          id = 'request-nb',
          unreadNb
        ),
        immediate = TRUE
      )
    } else if (unreadNb >= 10) {
      insertUI(
        selector = '#data-requests-nav',
        where = 'beforeEnd',
        ui = div(
          class = 'request-nb',
          id = 'request-nb'
        ),
        immediate = TRUE
      )
    }
    
    # Return the requests
    requests
  })
  
  # Create an observe event that react to both refresh buttons
  observeEvent(input$refresh_top | input$refresh_bottom, ignoreInit = TRUE, {
    req(input$refresh_top != 0 | input$refresh_bottom != 0)
    
    reloadRequests(reloadRequests() + 1)
  })
  
  
  
  
  
  
  ## Request read / unread #################################################################
  
  # Create an observe event that react to both edit buttons
  observeEvent(input$read_top | input$read_bottom, ignoreInit = TRUE, {
    req(input$read_top != 0 | input$read_bottom != 0, length(input$table_rows_selected) > 0)
    
    # Get requests to edit
    requests <- loadRequests()[input$table_rows_selected,]
    
    # Update each request
    for (i in nrow(requests)) {
      # Get request
      request <- slice(requests, i)
      
      # Update request
      error <- updateRequest(pool, request)
      
      # Sow success or error
      if (error == '') {
        showNotification('Successfully updated request.', type = 'message')
      } else {
        showNotification('An error occured. One request could not be updated...', type = 'error')
      }
    }
    
    # Reload table
    reloadRequests(reloadRequests() + 1)
  })
  
  
  
  
  ## Element deletion #############################################################
  
  # Create an observeEvent that react to both delete buttons
  observeEvent(input$delete_top | input$delete_bottom, ignoreInit = TRUE, {
    req(input$delete_top != 0 | input$delete_bottom != 0, length(input$table_rows_selected) > 0)
    confirmationModal('You are about to permanently delete rows from this table. Please confirm your action.')
  })
  
  
  # Create an observeEvent linked to the YES button of the confirmation modal
  observeEvent(input$YES, ignoreInit = TRUE, {
    # Remove confirmation modal
    removeModal()
    
    # If rows are selected
    if (length(input$table_rows_selected) > 0) {
      # Get the selected row ids
      selectedRequestIds <- loadRequests()[input$table_rows_selected,] %>% pull(id)
      
      # Run the element deletion expression and retrieve the output message
      error <- deleteRows(pool, 'data_requests', selectedRequestIds)
      
      # Show success or error notification
      if (error == '') {
        showNotification('Rows successfully deleted!', type = 'message')
      } else {
        showNotification(paste('The following error(s) occured:', error, sep = '\n'))
      }
      
      # Reload table
      reloadRequests(reloadRequests() + 1)
    }
  })
  
  
  
  
  
  ## Table rendering ##############################################################
  
  # Render the DataTable
  output$table <- renderDT({
    loadRequests() %>%
      datatable(rownames = FALSE, options = list(
        dom = 't',
        paging = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )) %>%
      formatDate(c('created_at', 'updated_at'), method = 'toUTCString')
  })
}
