## This module contains the UI and server code for the field data tool

## Create module UI function ######################################################

fieldDataToolUI <- function(id, pool, ...) {
# Create the UI for the fieldDataTool module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
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
        checkboxInput(ns('vaisalaStdCurveCorr'), 'Vaisala std curve corr?', value = FALSE),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'std-selection',
        hidden(
          selectInput(
            ns('vaisalaStdCurveInput'),
            'Vaisala CO2 standard curve',
            choices = c(
              'Select a date...',
              parseOptions(
                getRows(
                  pool,
                  'standard_curves',
                  parameter == 'Vaisala corr',
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
  
  
  
  
  ## Hide and show select input ##############################################################
  
  # Observe event that react to chla acid curve selection
  observersOutput$vaisalaStdCurveVisibility <- observeEvent(input$vaisalaStdCurveCorr, ignoreInit = TRUE, {
    toggleElement('vaisalaStdCurveInput', condition = input$vaisalaStdCurveCorr)
    if (!input$vaisalaStdCurveCorr) updateSelectInput(session, 'vaisalaStdCurveInput', selected = 'Select a date...')
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
        category == 'Field data',
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
      'vaisala_std_curve_id',
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
    curveId <- row()$vaisala_std_curve_id
    
    if (!is.na(curveId) & curveId > 0) {
      stdCurve <- getRows(
        pool,
        'standard_curves',
        id == curveId
      )
      
      # Update select input if needed
      curveDate <- stdCurve %>% pull('date') %>% as_date()
      updateSelectInput(session, 'vaisalaStdCurveInput', selected = curveDate)
    } else {
      updateSelectInput(session, 'vaisalaStdCurveInput', selected = 'Select a date...')
    }
  })
  
  # Reactive that returns the std curves ids
  stdCurveIds <- reactive({
    vaisalaStdDate <- input$vaisalaStdCurveInput
    
    data.frame(
      vaisala_std_curve_id = ifelse(
        vaisalaStdDate == 'Select a date...',
        as.numeric(NA),
        getRows(
          pool,
          'standard_curves',
          parameter == 'Vaisala corr',
          date == vaisalaStdDate
        ) %>% pull('id')
      )
    )
  })
  
  
  
  
  
  
  
  ## Render raw data ####################################################################
  
  # Row filtering
  rawData <- reactive({
    row() %>% select(-c(
      id, station, starts_with('DATE'), starts_with('TIME'), ends_with('GMT'),
      ends_with('_at'), starts_with('Reach_depth'), Field_BP_altitude, ends_with('_corr'),
      vaisala_std_curve_id
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
      calculations$bpAltitude,
      select(
        stdCurveIds(),
        vaisala_std_curve_id
      )
    )
    
    calculations$co2corr <- data.frame(
      'Vaisala_CO2_min_corr' = calcCO2corr(co2 %>% select(-matches('_avg|_max')), pool),
      'Vaisala_CO2_avg_corr' = calcCO2corr(co2 %>% select(-matches('_min|_max')), pool),
      'Vaisala_CO2_max_corr' = calcCO2corr(co2 %>% select(-matches('_avg|_min')), pool)
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
          reachDepthUpdated(),
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
      noCheckCols = reactive(row() %>% select(matches('^Reach_depth_(sd|rep)|Vaisala_CO2_(avg|min|max)$|_altitude$')) %>% colnames()),
      # Return a list containing key-value pairs of columns to check with the regex to get the columns to check against
      checkCols = reactive(list())
    )
  )
}
