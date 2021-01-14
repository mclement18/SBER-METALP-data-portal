## This module contains the UI and server code for the parameter calculations

## Create module UI ###############################################################

parameterCalculationsUI <- function(id, pool) {
# Create the UI for the parameterCalculations module
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
      htmlTemplate('./html_components/parameter_calculations_info.html'),
      initStateHidden = TRUE
    ),
    div(
      class = 'action-with-error',
      div(
        class = 'errors-and-warnings',
        uiOutput(ns('caculationError'))
      ),
      div(
        class = 'action',
        actionButton(ns('calculate'), 'Make Global Calculation', class = 'custom-style custom-style--primary')
      )
    ),
    h3('Filter by:'),
    div(
      class = 'data-filter',
      selectInput(
        ns('categoryFilter'),
        'Parameter category',
        choices = c(
          'All',
          parseOptions(
            getRows(pool, 'grab_param_categories', columns = c('order', 'category')) %>%
              arrange(order) %>% select(-order),
            'category'
          )
        )
      )
    ),
    editableDTUI(ns('calculations'), canReorder = TRUE)
  )
}



## Create module server function ##################################################

parameterCalculations <- function(input, output, session, pool) {
# Create the logic for the parameterCalculations module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  ## Calculation logic ############################################################
  
  # Create an observeEvent that react to the calculate button
  observeEvent(input$calculate, ignoreInit = TRUE, {
    # Show confirmation modal
    confirmationModal('By clicking YES all the listed columns for the entire grab data table will be recalculated based on the actual state of the table.')
  })
  
  # Create an observeEvent that react to the YES button from the confirmation modal
  observeEvent(input$YES, ignoreInit = TRUE, {
    # Remove confirmation modal
    removeModal()
    
    # Show spinner
    show_modal_spinner(spin = 'cube-grid', color = '#e24727',
                       text = 'Running calculations...')
    
    # Run calculations asynchronously
    future(
      {
        # Source the needed files
        source('./secrets.R')
        source('./utils/calculation_functions.R')
        
        # Open pool connection for the new session
        pool <- connectToDB()
        
        # Get data
        df <- getRows(pool, 'data')
        
        # Make calculations
        result <- runGlobalCalculations(df, pool)
        
        # Close opened pool
        poolClose(pool)
        
        # Return result from calculations
        result
      },
      # Bind needed packages to the new session
      packages = c('DBI', 'pool', 'RMySQL', 'dplyr', 'dbplyr', 'magrittr')
      # Once completed
    ) %...>% (function(result) {
      # Get errors and warnings
      errors <- result$errors
      warnings <- result$warnings
      
      # Parse errors and warnings
      if (length(errors) == 0 & length(warnings) == 0) {
        # If all went good show notif and remove error log
        showNotification('All calculations successfully completed.', type = 'message')
        output$caculationError <- renderUI({})
      } else {
        # Else show notif and error log
        showNotification('Calculations completed with errors and/or warnings.', type = 'error')
        output$caculationError <- renderUI(
          htmlTemplate(
            './html_components/error_with_log.html',
            errorNb = length(errors),
            warningNb = length(warnings),
            showHideButton = actionLink(session$ns('showLog'), 'show log', class = 'custom-links'),
            errors = hidden(
              pre(
                id = session$ns('log'),
                paste(
                  'Errors:',
                  '-----------\n',
                  paste(errors, collapse = '\n\n'),
                  '',
                  'Warnings:',
                  '-----------\n',
                  paste(warnings, collapse = '\n\n')
                ),
                sep = '\n'
              )
            )
          )
        )
      }
      # Remove spinner
    }) %>% finally(remove_modal_spinner)
  })
  
  # Track log visibility
  showErrorLog <- reactiveVal(FALSE)
  
  # Show or hide log
  observeEvent(input$showLog, ignoreInit = TRUE, {
    # Toggle showErrorLog
    showErrorLog(!showErrorLog())
    
    # Toggle error log visibility
    toggleElement('log', anim = TRUE, condition = showErrorLog())
    
    # Update link name
    if (showErrorLog()) label <- 'hide log' else label <- 'show log'
    updateActionLink(session, 'showLog', label = label)
  })
  
  
  
  
  
  
  ## Call instruction panel module ################################################
  
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  
  
  ## Call editableDT module #######################################################
  
  callModule(editableDT, 'calculations', pool = pool, tableName = 'parameter_calculations', element = 'calculation',
             canReorder = TRUE,
             tableLoading = expression({
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$categoryFilter from the current module
               if (categoryFilter() == 'All') {
                 calculations <- getRows(pool, 'parameter_calculations')
               } else {
                 calculations <- getRows(pool, 'parameter_calculations', param_category %in% local(categoryFilter()))
               }
               
               # Cast data types
               calculations %>% mutate(
                 across(ends_with('_at'), ymd_hms)
               )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 param_category, column_calculated, calcul_func, columns_used
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id, param_category, column_calculated, calcul_func, columns_used
               )
             ),
             creationExpr = expression(
               createCalculation(
                 pool = pool,
                 param_category = input$param_category,
                 column_calculated = input$column_calculated,
                 calcul_func = input$calcul_func,
                 columns_used = input$columns_used
               )
             ),
             updateExpr = expression(
               updateCalculation(
                 pool = pool,
                 calculation = editedRow(),
                 param_category = input$param_category,
                 column_calculated = input$column_calculated,
                 calcul_func = input$calcul_func,
                 columns_used = input$columns_used
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'parameter_calculations',
                 ids = selectedRowIds
               )
             ),
             # Pass the current module inputs as an additional reactive expression which are used in the above expressions
             categoryFilter = reactive(input$categoryFilter)
  )
}
