## This module contains the UI and server code for the field data tool

## Create module UI function ######################################################

fieldDataToolUI <- function(id, ...) {
# Create the UI for the fieldDataTool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'field-data-tool tools-layout',
    div(
      class ='raw-data',
      toolTableUI(ns('rawData')),
      toolTableUI(ns('depth'))
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
        toolTableUI(ns('co2corr')),
        toolTableUI(ns('bpAltitude')),
        toolTableUI(ns('depthAvg'))
      )
    )
  )
}



## Create module server function ##################################################

fieldDataTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the fieldDataTool module
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
        category == 'Field data',
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
    row() %>% select(-c(
      id, station, starts_with('DATE'), starts_with('TIME'), ends_with('GMT'),
      ends_with('_at'), starts_with('Reach_depth'), Field_BP_altitude, ends_with('_corr')
    ))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, ...)
  
  
  
  
  
  
  ## Render Reach depth table ####################################################################
  
  # Row filtering
  reachDepth <- reactive({
    row() %>% select(starts_with('Reach_depth'), -ends_with('_cm'))
  })
  
  # Call table module and retrieve updates
  reachDepthUpdated <- callModule(toolTable, 'depth', reachDepth, ...)
  
  
  
  
  
  
  ## Render Reach calculation #####################################################
  
  # Calculated values
  reachDepthCalculated <- reactive({
    if (useCalculated()) {
      calculations$reachDepth
    } else {
      row() %>% select(starts_with('Reach_depth') & ends_with('_cm'))
    }
  })
  
  # Call table module and retrieve updates
  reachDepthCalcUpdated <- callModule(toolTable, 'depthAvg', reachDepthCalculated, readOnly = TRUE)
  
  
  
  
  
  
  ## Render CO2 corr calculation ##################################################
  
  # Calculated values
  co2corr <- reactive({
    if (useCalculated()) {
      calculations$co2corr
    } else {
      row() %>% select(ends_with('_corr'))
    }
  })
  
  # Call table module and retrieve updates
  co2corrUpdated <- callModule(toolTable, 'co2corr', co2corr, readOnly = TRUE)
  
  
  
  
  
  
  ## Render BP altitude calculation ##################################################
  
  # Calculated values
  bpAltitude <- reactive({
    if (useCalculated()) {
      calculations$bpAltitude
    } else {
      row() %>% select('Field_BP_altitude')
    }
  })
  
  # Call table module and retrieve updates
  bpAltitudeUpdated <- callModule(toolTable, 'bpAltitude', bpAltitude, readOnly = TRUE)
  
  
  
  
  
  
  
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
    # Calculate Reach depth avg and sd
    newDepthMean <- calcMean(reachDepthUpdated())
    newDepthSd <- calcSd(reachDepthUpdated())
    # Set new mean and sd
    # If KEEP OLD, take it from the row()
    calculations$reachDepth <- data.frame(
      'Reach_depth_avg_cm' = ifelse(
        newDepthMean != 'KEEP OLD',
        newDepthMean,
        pull(row(), 'Reach_depth_avg_cm')
      ),
      'Reach_depth_sd_cm' = ifelse(
        newDepthSd != 'KEEP OLD',
        newDepthSd,
        pull(row(), 'Reach_depth_sd_cm')
      )
    )
    
    # Calculate altitude BP
    calculations$bpAltitude <- data.frame(
      'Field_BP_altitude' = calcAlt2BP(
        bind_cols(
          row() %>% select(station),
          rawDataUpdated() %>% select(WTW_Temp_degC_1)
        ),
        pool
      )
    )
    
    # Calculate CO2 corrections
    co2 <- bind_cols(
      rawDataUpdated() %>% select(
        starts_with('Vaisala_CO2'),
        WTW_Temp_degC_1, starts_with('Field_BP'),
      ),
      calculations$bpAltitude
    )
    
    calculations$co2corr <- data.frame(
      'Vaisala_CO2_min_corr' = calcCO2corr(co2 %>% select(-matches('_avg|_max'))),
      'Vaisala_CO2_avg_corr' = calcCO2corr(co2 %>% select(-matches('_min|_max'))),
      'Vaisala_CO2_max_corr' = calcCO2corr(co2 %>% select(-matches('_avg|_min')))
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
          co2corrUpdated(),
          bpAltitudeUpdated(),
          reachDepthCalcUpdated(),
          reachDepthUpdated()
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
