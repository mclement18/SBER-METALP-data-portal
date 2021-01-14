## This module contains the UI and server code for the DOC tool

## Create module UI function ######################################################

alkalinityToolUI <- function(id, ...) {
# Create the UI for the alkalinityTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'alkalinity-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData'))
    ),
    div(
      class = 'calculation',
      div(
        class = 'calculation-header',
        h4('Set missing WTW_pH_1 based on Alk_init_pH:'),
        actionButton(ns('calculate'), 'Set', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('ph'))
      )
    )
  )
}



## Create module server function ##################################################

alkalinityTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the alkalinityTool module
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
      # Alkalinity columns
      getRows(
        pool,
        'grab_param_categories',
        category == 'Alkalinity',
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
      # Add field data column to update
      'WTW_pH_1',
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
    row() %>% select(starts_with('Alk_'))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, ...)
  
  
  
  
  
  
  
  ## Render WTW_pH_1 calculation #####################################################
  
  # Calculated values
  ph <- reactive({
    if (useCalculated()) {
      calculations$ph
    } else {
      row() %>% select(WTW_pH_1)
    }
  })
  
  # Call table module and retrieve updates
  phUpdated <- callModule(toolTable, 'ph', ph, readOnly = TRUE)
  
  
  
  
  
  
  
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
    # Calculate WTW_pH_1
    calculations$ph <- data.frame(
      'WTW_pH_1' = calcEquals(
        bind_cols(
          select(row(), WTW_pH_1),
          select(rawDataUpdated(), Alk_init_pH)
        )
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
          rawDataUpdated(),
          phUpdated()
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
