## This module contains the UI and server code for the DOC tool

## Create module UI function ######################################################

docToolUI <- function(id, ...) {
# Create the UI for the docTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'doc-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('replicates'))
    ),
    div(
      class = 'calculation',
      div(
        class = 'calculation-header',
        h4('Calulated columns:'),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('avgSd'))
      )
    )
  )
}



## Create module server function ##################################################

docTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the docTool module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns a reactive expression containing the updated row
  
  ## Track observer ##############################################################
  
  # Reactive values that contain the observers output
  observersOutput <- reactiveValues()
  
  
  
  
  ## Get Row ####################################################################
  
  row <- reactive({
    req(datetime(), site())
    site <- site()
    datetime <- datetime()
    selectedDate <- datetime$date
    selectedTime <- datetime$time
    
    # Get columns
    columns <- c(
      'id', 'station', 'DATE_reading', 'TIME_reading', 'Convert_to_GMT', 'TIME_reading_GMT',
      getRows(
        pool,
        'grab_param_categories',
        category == 'DOC',
        columns = 'param_name'
      ) %>% pull(),
      'created_at', 'updated_at'
    )
    
    # Get data
    getRows(
      pool,
      'data',
      station == site,
      DATE_reading == selectedDate,
      TIME_reading_GMT == selectedTime,
      columns = columns
    )
  })
  
  
  
  
  
  
  ## Render replicates ####################################################################
  
  # Row filtering
  docRep <- reactive({
    row() %>% select(starts_with('DOC_rep_'))
  })
  
  # Call table module and retrieve updates
  docRepUpdated <- callModule(toolTable, 'replicates', docRep, replicates = TRUE, ...)
  
  
  
  
  
  
  
  ## Render Reach calculation #####################################################
  
  # Calculated values
  docAvgSd <- reactive({
    if (useCalculated()) {
      calculations$docAvgSd
    } else {
      row() %>% select(starts_with('DOC_') & ends_with('_ppb'))
    }
  })
  
  # Call table module and retrieve updates
  docAvgSdUpdated <- callModule(toolTable, 'avgSd', docAvgSd, readOnly = TRUE)
  
  
  
  
  
  
  
  ## Calculation logic ############################################################
  
  # Use default or calculated values
  useCalculated <- reactiveVal(FALSE)
  
  # Reset on site or date update
  observersOutput$resetUseCalculated <- observe({
    site();datetime()
    useCalculated(FALSE)
  })
  
  # Store calculation
  calculations <- reactiveValues()
  
  # Calculate upon button click
  observersOutput$calculationLogic <- observeEvent(input$calculate, ignoreInit = TRUE, {
    # Calculate DOC avg and sd
    newDocMean <- calcMean(docRepUpdated())
    newDocSd <- calcSd(docRepUpdated())
    # Set new mean and sd
    # If KEEP OLD, take it from the row()
    calculations$docAvgSd <- data.frame(
      'DOC_avg_ppb' = ifelse(
        newDocMean != 'KEEP OLD',
        newDocMean,
        pull(row(), 'DOC_avg_ppb')
      ),
      'DOC_sd_ppb' = ifelse(
        newDocSd != 'KEEP OLD',
        newDocSd,
        pull(row(), 'DOC_sd_ppb')
      )
    )
    
    # Use calculation
    useCalculated(TRUE)
  })
  
  
  
  
  
  
  ## Return row ####################################################################
  
  # Return a reactive expression
  return(
    list(
      # Returns the row to update
      df = reactive({
        # Re-run when site or date updates
        site();datetime()
        # Return the row
        bind_cols(
          row() %>% select(
            id, station, starts_with('DATE'), starts_with('TIME'), ends_with('GMT'),
            ends_with('_at')
          ),
          docRepUpdated(),
          docAvgSdUpdated()
        )
      }),
      # Returns errors and warnings
      errors = reactive(
        list(
          errors = c(),
          warnings = c()
        )
      ),
      # Return observers to destroy them from the outer module
      observers = observersOutput
    )
  )
}
