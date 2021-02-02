## This module contains the UI and server code for the pCO2 tool

## Create module UI function ######################################################

pCO2ToolUI <- function(id, ...) {
# Create the UI for the pCO2Tool module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'pco2-tool tools-layout',
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
        checkboxInput(ns('useTCst'), 'Use lab temp constant', value = FALSE),
        checkboxInput(ns('usePCst'), 'Use lab pressure constant', value = FALSE),
        actionButton(ns('calculate'), 'Calculate', class = 'custom-style custom-style--primary')
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('pco2Ch4')),
        toolTableUI(ns('avgSd'))
      )
    )
  )
}



## Create module server function ##################################################

pCO2Tool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the pCO2Tool module
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
      # Get pCO2 parameters
      getRows(
        pool,
        'grab_param_categories',
        category == 'pCO2',
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
      # Get field data used in calculation
      'WTW_Temp_degC_1', 'Field_BP', 'Field_BP_altitude',
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
    row() %>% select(starts_with('lab_co2_'), -c(lab_co2_lab_temp, lab_co2_lab_press, starts_with('lab_co2_ch4_dry')))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, replicates = TRUE, ...)
  
  
  
  
  
  
  ## Render lab constant table ####################################################################
  
  # Row filtering
  labCst <- reactive({
    row() %>% select(lab_co2_lab_temp, lab_co2_lab_press)
  })
  
  # Call table module and retrieve updates
  labCstUpdated <- callModule(toolTable, 'labCst', labCst, ...)
  
  
  
  
  
  
  ## Render pCO2 and CH4 calculation #####################################################
  
  # Calculated values
  pco2Ch4 <- reactive({
    if (useCalculated()) {
      calculations$pco2Ch4
    } else {
      row() %>% select(matches('^CO2_HS|^pCO2_HS|^CH4|^lab_co2_ch4_dry_'), -matches('_avg$|_sd$'))
    }
  })
  
  # Call table module and retrieve updates
  pco2Ch4Updated <- callModule(toolTable, 'pco2Ch4', pco2Ch4, readOnly = TRUE, replicates = TRUE)
  
  
  
  
  
  
  ## Render avg and sd calculation ##################################################
  
  # Calculated values
  avgSd <- reactive({
    if (useCalculated()) {
      calculations$avgSd
    } else {
      row() %>% select(matches('_avg$|_sd$'))
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
    # Calculate pCO2 and CH4
    # Set to NULL in case of new calculation
    calculations$pco2Ch4 <- NULL
    
    # Get Field data used in calculation
    fieldData <- row() %>% select(WTW_Temp_degC_1, Field_BP, Field_BP_altitude)
    
    # Get use constant input value
    if (input$useTCst) {
      labTemp <- 'cst'
    } else {
      labTemp <- 'db'
    }
    
    if (input$usePCst) {
      labPa <- 'cst'
    } else {
      labPa <- 'db'
    }
    
    # For each replicate
    for (rep in c('A', 'B')) {
      # Get replicate values
      co2_raw <- rawDataUpdated() %>% select(starts_with('lab_co2_co2ppm') & ends_with(rep),)
      ch4_h20 <- rawDataUpdated() %>% select(matches('^lab_co2_h2o|^lab_co2_ch4') & ends_with(rep))
      
      # Create column names
      colNames <- paste0(
        c('lab_co2_ch4_dry_',
          'CH4_calc_umol_L_',
          'CO2_HS_Um_',
          'pCO2_HS_uatm_',
          'pCO2_HS_P1_uatm_',
          'pCO2_HS_P2_uatm_'),
        rep
      )
      
      # Calculate values that are needed for subsequent calculation
      lab_co2_ch4_dry <- calcCH4dry(ch4_h20)
      
      CO2_HS_Um <- calcCO2(
        bind_cols(
          co2_raw,
          labCstUpdated()
        ),
        pool,
        labTemp,
        labPa
      )
      
      # Parameters for pCO2 calculation
      pC02parameters <- fieldData %>%
        mutate(
          !!colNames[3] := CO2_HS_Um
        )
      
      # Calculate new column
      newCols <- setNames(
        data.frame(
          lab_co2_ch4_dry = lab_co2_ch4_dry,
          CH4_calc_umol_L <- calcCH4(
            bind_cols(
              labCstUpdated(),
              fieldData
            ) %>% mutate(
              !!colNames[1] := lab_co2_ch4_dry
            ),
            pool,
            labTemp,
            labPa
          ),
          CO2_HS_Um = CO2_HS_Um,
          pCO2_HS_uatm = calcpCO2(pC02parameters, pool),
          pCO2_HS_P1_uatm = calcpCO2P1(pC02parameters, pool),
          pCO2_HS_P2_uatm = calcpCO2P2(pC02parameters, pool)
        ),
        colNames
      )
      
      # If calculations$pco2Ch4 is NULL, create it else update it
      if (is.null(calculations$pco2Ch4)) {
        calculations$pco2Ch4 <- newCols
      } else {
        calculations$pco2Ch4 <- bind_cols(
          calculations$pco2Ch4,
          newCols
        )
      }
    }
    
    # Calculate CO2 and CH4 avg and sd
    # Set to NULL in case of second calculation
    calculations$avgSd <- NULL
    
    for (param in c('CO2_HS_Um', 'pCO2_HS_uatm', 'pCO2_HS_P1_uatm', 'pCO2_HS_P2_uatm', 'd13C_CO2', 'CH4')) {
      # Select data
      if (param == 'd13C_CO2') {
        df <- rawDataUpdated() %>% select(starts_with('lab_co2_ico2'))
      } else {
        df <- calculations$pco2Ch4 %>% select(starts_with(param), -matches('_avg$|_sd$'))
      }
      
      # Calculate mean and sd
      newMean <- calcMean(df)
      newSd <- calcSd(df)
      if (param == 'CH4') {
        meanCol <-  'CH4_umol_L_avg'
        sdCol <-  'CH4_umol_L_sd'
      } else {
        meanCol <- paste0(param, '_avg')
        sdCol <- paste0(param, '_sd')
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
          pco2Ch4Updated(),
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
      observers = observersOutput,
      # Return a character vector containing the name of the columns not to check
      noCheckCols = reactive(row() %>% select(ends_with('_sd')) %>% colnames()),
      # Return a list containing key-value pairs of columns to check with the regex to get the columns to check against
      checkCols = reactive({
        cols2check <- list()
        # Add all standard comparisons
        cols <- row() %>% select(matches('_avg$|_temp$|_press$')) %>% colnames()
        cols2check <- c(
          cols2check,
          `names<-`(as.list(cols), cols)
        )
        # Add complex comparisons
        cols <- row() %>% select(matches('_A$|_B$')) %>% colnames()
        cols2check <- c(
          cols2check,
          `names<-`(as.list(sub('_[AB]$', '_(A|B)', cols)), cols)
        )
        # Return list
        cols2check
      })
    )
  )
}
