## This module contains the UI and server code for the DIC tool

## Create module UI function ######################################################

dicToolUI <- function(id, ...) {
# Create the UI for the dicTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'dic-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData')),
      toolTableUI(ns('labCst'))
    ),
    div(
      class = 'calculation',
      div(
        class = 'calculation-header',
        h4('Calculated columns:'),
        checkboxInput(ns('useCst'), 'Use lab temp constant', value = FALSE),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('dic')),
        toolTableUI(ns('avgSd'))
      )
    )
  )
}



## Create module server function ##################################################

dicTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the dicTool module
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
        category == 'DIC',
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
    row() %>% select(starts_with('lab_dic_'), -c(lab_dic_air_temp, lab_dic_air_pressure))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, replicates = TRUE, ...)
  
  
  
  
  
  
  ## Render lab constant table ####################################################################
  
  # Row filtering
  labCst <- reactive({
    row() %>% select(lab_dic_air_temp, lab_dic_air_pressure)
  })
  
  # Call table module and retrieve updates
  labCstUpdated <- callModule(toolTable, 'labCst', labCst, ...)
  
  
  
  
  
  
  ## Render DIC calculation #####################################################
  
  # Calculated values
  dic <- reactive({
    if (useCalculated()) {
      calculations$dic
    } else {
      row() %>% select(DIC_A, DIC_B, d13C_DIC_A, d13C_DIC_B)
    }
  })
  
  # Call table module and retrieve updates
  dicUpdated <- callModule(toolTable, 'dic', dic, readOnly = TRUE, replicates = TRUE)
  
  
  
  
  
  
  ## Render meand and sd calculation ##################################################
  
  # Calculated values
  avgSd <- reactive({
    if (useCalculated()) {
      calculations$avgSd
    } else {
      row() %>% select(ends_with('_avg') | ends_with('_std'))
    }
  })
  
  # Call table module and retrieve updates
  avgSdUpdated <- callModule(toolTable, 'avgSd', avgSd, readOnly = TRUE, replicates = TRUE)
  
  
  
  
  
  
  
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
    # Calculate DIC replicates
    # Set to NULL for new calculation
    calculations$dic <- NULL
    
    # Get use constant input value
    if (input$useCst) {
      labTemp <- 'cst'
    } else {
      labTemp <- 'db'
    }
    
    # For each parameter and replicate
    for (param in c('DIC', 'd13C_DIC')) {
      for (rep in c('A', 'B')) {
        # Set DIC column
        dicCol <- paste0(param, '_', rep)
        
        # Create input df
        inputDf <- bind_cols(
          labCstUpdated(),
          select(rawDataUpdated(), ends_with(rep))
        )
        
        # Calculate DIC
        df <- setNames(
          data.frame(
            dicCol = ifelse(
              param == 'DIC',
              calcDIC(inputDf, pool, labTemp),
              calcd13DIC(inputDf, pool, labTemp)
            )
          ),
          dicCol
        )
        
        # If NULL, create df else update
        if (is.null(calculations$dic)) {
          calculations$dic <- df
        } else {
          calculations$dic <- bind_cols(
            calculations$dic,
            df
          )
        }
      }
    }
    
    
    # Calculate DIC avg and sd
    # Set to NULL for new calculation
    calculations$avgSd <- NULL
    
    # For each parameter
    for (param in c('DIC', 'd13C_DIC')) {
      # Get replicates
      reps <- select(calculations$dic, starts_with(param))
      
      # Calculate mean and sd
      newMean <- calcMean(reps)
      newSd <- calcSd(reps)
      meanCol <- paste0(param, '_avg')
      sdCol <- paste0(param, '_std')
      
      # Set new mean and sd
      # If KEEP OLD, take it from the row()
      df <- setNames(
        data.frame(
          meanCol = ifelse(
            newMean != 'KEEP OLD',
            newMean,
            pull(row(), meanCol)
          ),
          sdCol = ifelse(
            newSd != 'KEEP OLD',
            newSd,
            pull(row(), sdCol)
          )
        ),
        c(meanCol, sdCol)
      )
      
      # If NULL, create df else update
      if (is.null(calculations$avgSd)) {
        calculations$avgSd <- df
      } else {
        calculations$avgSd <- bind_cols(
          calculations$avgSd,
          df
        )
      }
    }
    
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
          labCstUpdated(),
          dicUpdated(),
          avgSdUpdated()
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
