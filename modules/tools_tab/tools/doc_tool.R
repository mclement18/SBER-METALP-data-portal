## This module contains the UI and server code for the DOC tool

## Create module UI function ######################################################

docToolUI <- function(id, pool, ...) {
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
        h4('Calculated columns:'),
        checkboxInput(ns('stdCurveCorr'), 'Std curve corr?', value = FALSE),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'std-selection',
        hidden(
          selectInput(
            ns('stdCurveInput'),
            'Standard curve used to correct DOC',
            choices = c(
              'Select a date...',
              parseOptions(
                getRows(
                  pool,
                  'standard_curves',
                  parameter == 'DOC corr',
                  columns = 'date'
                ) %>% arrange(desc(date)),
                'date'
              )
            )
          )
        )
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
  
  
  
  
  ## Hide and show select input ##############################################################
  
  # Observe event that react to chla acid curve selection
  observersOutput$stdCurveVisibility <- observeEvent(input$stdCurveCorr, ignoreInit = TRUE, {
    toggleElement('stdCurveInput', condition = input$stdCurveCorr)
    if (!input$stdCurveCorr) updateSelectInput(session, 'stdCurveInput', selected = 'Select a date...')
  })
  
  
  
  
  
  
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
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
      'doc_std_curve_id',
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
  
  
  
  
  
  ## Select preset std curves ############################################################
  
  # Update them each time the row update
  observersOutput$stdCurvePreset <- observeEvent(row(), {
    req(nrow(row()) > 0)
    # Get curves
    curveId <- row()$doc_std_curve_id
    
    if (!is.na(curveId) & curveId > 0) {
      stdCurve <- getRows(
        pool,
        'standard_curves',
        id == curveId
      )
      
      # Update select input if needed
      curveDate <- stdCurve %>% pull('date') %>% as_date()
      updateSelectInput(session, 'stdCurveInput', selected = curveDate)
    } else {
      updateSelectInput(session, 'stdCurveInput', selected = 'Select a date...')
    }
  })
  
  # Reactive that returns the std curves ids
  stdCurveIds <- reactive({
    stdDate <- input$stdCurveInput
    
    data.frame(
      doc_std_curve_id = ifelse(
        stdDate == 'Select a date...',
        as.numeric(NA),
        getRows(
          pool,
          'standard_curves',
          parameter == 'DOC corr',
          date == stdDate
        ) %>% pull('id')
      )
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
    # Get DOC replicates and std curve id
    docRepUpdated <- bind_cols(
      docRepUpdated(),
      stdCurveIds()
    )
    
    # Calculate DOC avg and sd
    newDocMean <- calcDOCavg(docRepUpdated, pool)
    newDocSd <- calcDOCsd(docRepUpdated, pool)
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
          docAvgSdUpdated(),
          stdCurveIds()
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
      noCheckCols = reactive(row() %>% select(matches('^DOC_(sd|rep)')) %>% colnames()),
      # Return a list containing key-value pairs of columns to check with the regex to get the columns to check against
      checkCols = reactive(list())
    )
  )
}
