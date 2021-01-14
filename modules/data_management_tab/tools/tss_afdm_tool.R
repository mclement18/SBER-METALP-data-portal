## This module contains the UI and server code for the TSS and AFDM tool

## Create module UI function ######################################################

tssAfdmToolUI <- function(id, ...) {
# Create the UI for the tssAfdmTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'tss-afdm-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData'))
    ),
    div(
      class = 'calculation',
      div(
        class = 'calculation-header',
        h4('Calculated columns:'),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('tssAfdm'))
      )
    )
  )
}



## Create module server function ##################################################

tssAfdmTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the tssAfdmTool module
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
        category == 'TSS',
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
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
  
  
  
  
  
  
  ## Render raw data ####################################################################
  
  # Row filtering
  rawData <- reactive({
    row() %>% select(starts_with('lab_'))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, ...)
  
  
  
  
  
  
  
  ## Render TSS and AFDM calculation ##################################################
  
  # Calculated values
  tssAfdm <- reactive({
    if (useCalculated()) {
      calculations$tssAfdm
    } else {
      row() %>% select(TSS_dry_weight_mgL, AFDM_mgL)
    }
  })
  
  # Call table module and retrieve updates
  tssAfdmUpdated <- callModule(toolTable, 'tssAfdm', tssAfdm, readOnly = TRUE)
  
  
  
  
  
  
  
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
    # Calculate TSS and AFDM
    calculations$tssAfdm <- data.frame(
      'TSS_dry_weight_mgL' = calcTSS(rawDataUpdated()),
      'AFDM_mgL' = calcAFDM(rawDataUpdated())
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
          rawDataUpdated(),
          tssAfdmUpdated()
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
