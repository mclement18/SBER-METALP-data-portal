## This module contains the UI and server code for the DOM tool

## Create module UI function ######################################################

domToolUI <- function(id, ...) {
# Create the UI for the domTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'dom-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData'))
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
        toolTableUI(ns('calculated'))
      )
    )
  )
}



## Create module server function ##################################################

domTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the domTool module
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
      # Get DOM columns
      getRows(
        pool,
        'grab_param_categories',
        category == 'DOM',
        columns = 'param_name'
      ) %>% pull(),
      # Add DOC_avg_ppb
      'DOC_avg_ppb',
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
    row() %>% select(-c(
      id, station, starts_with('DATE'), starts_with('TIME'), ends_with('GMT'),
      SUVA, A_T, C_A, C_M, C_T, DOC_avg_ppb, ends_with('_at')
    ))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, ...)
  
  
  
  
  
  
  
  ## Render calculation ##################################################
  
  # Calculated values
  calculated <- reactive({
    if (useCalculated()) {
      calculations$calculated
    } else {
      row() %>% select(SUVA, A_T, C_A, C_M, C_T)
    }
  })
  
  # Call table module and retrieve updates
  calculatedUpdated <- callModule(toolTable, 'calculated', calculated, readOnly = TRUE)
  
  
  
  
  
  
  
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
    # Calculate SUVA and ratios
    calculations$calculated <- data.frame(
      'SUVA' = calcSUVA(
        bind_cols(
          select(rawDataUpdated(), a254),
          select(row(), DOC_avg_ppb)
        )
      ),
      'A_T' = calcRatio(select(rawDataUpdated(), A, T)),
      'C_A' = calcRatio(select(rawDataUpdated(), C, A)),
      'C_M' = calcRatio(select(rawDataUpdated(), C, M)),
      'C_T' = calcRatio(select(rawDataUpdated(), C, T))
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
          calculatedUpdated()
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
