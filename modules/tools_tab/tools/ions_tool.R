## This module contains the UI and server code for the Ion tool

## Create module UI function ######################################################

ionsToolUI <- function(id, ...) {
# Create the UI for the ionsTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'ions-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('ions'))
    )
  )
}



## Create module server function ##################################################

ionsTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the ionsTool module
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
        category == 'Ions',
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
  ions <- reactive({
    row() %>% select(ends_with('_mgL'))
  })
  
  # Call table module and retrieve updates
  ionsUpdated <- callModule(toolTable, 'ions', ions, ...)
  
  
  

    
  
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
          ionsUpdated()
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
      observers = observersOutput,
      # Return a character vector containing the name of the columns not to check
      noCheckCols = reactive(c()),
      # Return a list containing key-value pairs of columns to check with the regex to get the columns to check against
      checkCols = reactive(list())
    )
  )
}
