## This module contains the UI and server code for the nutrients tool

## Create module UI function ######################################################

nutrientsToolUI <- function(id, ...) {
# Create the UI for the nutrientsTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'nutrients-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData')),
      toolTableUI(ns('oldNut'))
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
        toolTableUI(ns('no3')),
        toolTableUI(ns('avgSd')),
        toolTableUI(ns('oldNutAvgSd'))
      )
    )
  )
}



## Create module server function ##################################################

nutrientsTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the nutrientsTool module
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
        category %in% c('Nutrients', 'Old Nutrients'),
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
    row() %>% select(matches('_rep_') & starts_with('NUT'), -starts_with('NUT_NO3_rep_'))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, replicates = TRUE, ...)
  
  
  
  
  
  ## Render old nutrients data ####################################################################
  
  # Row filtering
  oldNut <- reactive({
    row() %>% select(matches('_rep_') & (starts_with('NH4_') | starts_with('SRP_')))
  })
  
  # Call table module and retrieve updates
  oldNutUpdated <- callModule(toolTable, 'oldNut', oldNut, replicates = TRUE, ...)
  
  
  
  
  
  
  ## Render NO3 calculation ####################################################################
  
  # Row filtering
  no3 <- reactive({
    if (useCalculated()) {
      calculations$no3
    } else {
      row() %>% select(starts_with('NUT_NO3_rep_'))
    }
  })
  
  # Call table module and retrieve updates
  no3Updated <- callModule(toolTable, 'no3', no3, readOnly = TRUE, ...)
  
  
  
  
  
  
  ## Render average and sd calculation #####################################################
  
  # Calculated values
  avgSd <- reactive({
    if (useCalculated()) {
      calculations$avgSd
    } else {
      row() %>% select(ends_with('_avg') | ends_with('_sd'))
    }
  })
  
  # Call table module and retrieve updates
  avgSdUpdated <- callModule(toolTable, 'avgSd', avgSd, readOnly = TRUE, replicates = TRUE)
  
  
  
  
  
  ## Render old nutrients avg and sd calculation #####################################################
  
  # Calculated values
  oldNutAvgSd <- reactive({
    if (useCalculated()) {
      calculations$oldNutAvgSd
    } else {
      row() %>% select(matches('_avg_|_sd_'))
    }
  })
  
  # Call table module and retrieve updates
  oldNutAvgSdUpdated <- callModule(toolTable, 'oldNutAvgSd', oldNutAvgSd, readOnly = TRUE)
  
  
  
  
  
  
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
    # Calculate NO3
    # Set to NULL in case of new calculation
    calculations$no3 <- NULL
    # For each replicate
    for (rep in c('A', 'B', 'C')) {
      # Get replicate name
      repName = paste0('NUT_NO3_rep_', rep)
      
      # Calculate new column
      newCols <- setNames(
        data.frame(
          repName = calcMinus(
            select(
              rawDataUpdated(),
              (starts_with('NUT_NOx_rep_') | starts_with('NUT_NO2_rep_')) & ends_with(rep)
            )
          )
        ),
        repName
      )
      
      # If calculations$no3 is NULL, create it else update it
      if (is.null(calculations$no3)) {
        calculations$no3 <- newCols
      } else {
        calculations$no3 <- bind_cols(
          calculations$no3,
          newCols
        )
      }
    }
    
    # Calculate nutrients avg and sd
    # Set to NULL in case of second calculation
    calculations$avgSd <- NULL
    calculations$oldNutAvgSd <- NULL
    
    oldNutrients <- c('NH4', 'SRP')
    
    for (param in c('NUT_P', 'NUT_NH4', 'NUT_NOx', 'NUT_NO2', 'NUT_NO3', 'NUT_TDP', 'NUT_TDN', 'NH4', 'SRP')) {
      # Select data
      if (param == 'NUT_NO3') {
        df <- calculations$no3
      } else {
        if (param %in% oldNutrients) {
          df <- oldNutUpdated() %>% select(starts_with(param))
        } else {
          df <- rawDataUpdated() %>% select(starts_with(param))
        }
      }
      
      # Calculate mean and sd
      newMean <- calcMean(df)
      newSd <- calcSd(df)
      if (param %in% oldNutrients) {
        meanCol <-  paste0(param, '_avg_ugL')
        sdCol <-  paste0(param, '_sd_ugL')  
      } else {
        meanCol <-  paste0(param, '_avg')
        sdCol <-  paste0(param, '_sd')
      }
      
      # Set new mean and sd
      # If KEEP OLD, take it from the row()
      newCols <- setNames(
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
      
      if (param %in% oldNutrients) {
        # If calculations is NULL, create it else update it
        if (is.null(calculations$oldNutAvgSd)) {
          calculations$oldNutAvgSd <- newCols
        } else {
          calculations$oldNutAvgSd <- bind_cols(
            calculations$oldNutAvgSd,
            newCols
          )
        }  
      } else {
        # If calculations is NULL, create it else update it
        if (is.null(calculations$avgSd)) {
          calculations$avgSd <- newCols
        } else {
          calculations$avgSd <- bind_cols(
            calculations$avgSd,
            newCols
          )
        }
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
          no3Updated(),
          avgSdUpdated(),
          oldNutUpdated(),
          oldNutAvgSdUpdated()
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
