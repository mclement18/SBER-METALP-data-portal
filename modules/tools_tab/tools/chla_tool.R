## This module contains the UI and server code for the Chla tool

## Create module UI function ######################################################

chlaToolUI <- function(id, pool, ...) {
# Create the UI for the chlaTool module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Get std curves
  stdCurves <- getRows(
    pool,
    'standard_curves',
    parameter %in% c('chla acid', 'chla noacid'),
    columns = c('date', 'parameter')
  ) %>% arrange(desc(date))
  
  # Create layout
  div(
    class = 'chla-tool tools-layout',
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
        class = 'std-selection',
        selectInput(
          ns('chlaAcidInput'),
          'Chla acid standard curve',
          choices = c(
            'Select a date...',
            parseOptions(
              stdCurves %>% filter(parameter == 'chla acid') %>% arrange(desc(date)),
              'date'
            )
          )
        ),
        selectInput(
          ns('chlaNoAcidInput'),
          'Chla noacid standard curve',
          choices = c(
            'Select a date...',
            parseOptions(
              stdCurves %>% filter(parameter == 'chla noacid') %>% arrange(desc(date)),
              'date'
            )
          )
        ),
      ),
      div(
        class = 'calculated',
        toolTableUI(ns('chla')),
        toolTableUI(ns('avgSd'))
      )
    )
  )
}



## Create module server function ##################################################

chlaTool <- function(input, output, session, pool, site, datetime, ...) {
# Create the logic for the chlaTool module
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
        category == 'Chl a',
        columns = c('order', 'param_name')
      ) %>% pull('param_name'),
      'chla_acid_std_curve_id', 'chla_noacid_std_curve_id',
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
    # Get curves ids that are not NA
    curveIds <- c(row()$chla_acid_std_curve_id, row()$chla_noacid_std_curve_id)
    
    # Get the curves info
    stdCurves <- getRows(
      pool,
      'standard_curves',
      id %in% curveIds
    )
    
    # For each curve
    for (curve in c('chla_acid_std_curve_id', 'chla_noacid_std_curve_id')) {
      # Get the id
      curveId <- row() %>% pull(curve)
      # If is not NA and superior to 0
      if (!is.na(curveId) & curveId > 0) {
        # Define the curve parameter name and select input id
        if (curve == 'chla_acid_std_curve_id') {
          paramName <- 'chla acid'
          selectName <- 'chlaAcidInput'
        } else {
          paramName <- 'chla noacid'
          selectName <- 'chlaNoAcidInput'
        }
        # Get the curve date and update the select input
        curveDate <- stdCurves %>% filter(parameter == paramName) %>% pull('date') %>% as_date()
        updateSelectInput(session, selectName, selected = curveDate)
      } else {
        selectName <- ifelse(curve == 'chla_acid_std_curve_id', 'chlaAcidInput', 'chlaNoAcidInput')
        updateSelectInput(session, selectName, selected = 'Select a date...')
      }
    }
  })
  
  # Reactive that returns the std curves ids
  stdCurveIds <- reactive({
    chlaAcidDate <- input$chlaAcidInput
    chlaNoAcidDate <- input$chlaNoAcidInput
    
    data.frame(
      chla_acid_std_curve_id = ifelse(
        chlaAcidDate == 'Select a date...',
        as.numeric(NA),
        getRows(
          pool,
          'standard_curves',
          parameter == 'chla acid',
          date == chlaAcidDate
        ) %>% pull('id')
      ),
      chla_noacid_std_curve_id = ifelse(
        chlaNoAcidDate == 'Select a date...',
        as.numeric(NA),
        getRows(
          pool,
          'standard_curves',
          parameter == 'chla noacid',
          date == chlaNoAcidDate
        ) %>% pull('id')
      )
    )
  })
  
  
  
  
  
  
  ## Render raw data ####################################################################
  
  # Row filtering
  rawData <- reactive({
    row() %>% select(starts_with('lab_chla_'), -starts_with('lab_chla_vol_filtrated_rep_'))
  })
  
  # Call table module and retrieve updates
  rawDataUpdated <- callModule(toolTable, 'rawData', rawData, replicates = TRUE, ...)
  
  
  
  
  
  
  ## Render Chla calculation ####################################################################
  
  # Calculated values
  chla <- reactive({
    if (useCalculated()) {
      calculations$chla
    } else {
      row() %>% select(
        starts_with('chla_', ignore.case = FALSE) | starts_with('afdm_') | starts_with('lab_chla_vol_filtrated_rep_'),
        -ends_with('std_curve_id')
      )
    }
  })
  
  # Call table module and retrieve updates
  chlaUpdated <- callModule(toolTable, 'chla', chla, readOnly = TRUE,  replicates = TRUE)
  
  
  
  
  
  
  ## Render Chla avg and sd calculation ##################################################
  
  # Calculated values
  avgSd <- reactive({
    if (useCalculated()) {
      calculations$avgSd
    } else {
      row() %>% select(starts_with('Chla_', ignore.case = FALSE) | starts_with('benthic_AFDM_'))
    }
  })
  
  # Call table module and retrieve updates
  avgSdUpdated <- callModule(toolTable, 'avgSd', avgSd, readOnly = TRUE)
  
  
  
  
  
  
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
    # Calculate Chla
    # Set to NULL in case of new calculation
    calculations$chla <- NULL
    # For each replicate
    for (rep in c('A', 'B', 'C', 'D', 'E')) {
      # Get replicate values
      replicateValues <- rawDataUpdated() %>% select(ends_with(rep))
      
      # Create column names
      colNames <- paste0(
        c('lab_chla_vol_filtrated_rep_',
          'chla_acid_ugL_rep_',
          'chla_acid_ugm2_rep_',
          'chla_noacid_ugL_rep_',
          'chla_noacid_ugm2_rep_',
          'afdm_g_filter_rep_',
          'afdm_gm2_rep_'),
        rep
      )
      
      # Calculate values that are needed for subsequent calculation
      lab_chla_vol_filtrated_rep <- calcMinus(
        select(
          rawDataUpdated(),
          all_of(
            paste0(
              c('lab_chla_tot_vol_rep_','lab_chla_vol_after_rep_'),
              rep
            )
          )
        )
      )
      
      chla_acid_ugL_rep <- calcChlaAcid(
        bind_cols(
          select(
            rawDataUpdated(),
            all_of(
              paste0(
                c('lab_chla_fluor_1_rep_', 'lab_chla_fluor_2_rep_'),
                rep
              )
            )
          ),
          select(
            stdCurveIds(),
            chla_acid_std_curve_id
          )
        ),
        pool
      )
      chla_acid_ugL_rep <- ifelse(
        chla_acid_ugL_rep != 'KEEP OLD',
        chla_acid_ugL_rep,
        pull(row(), colNames[2])
      )
      
      chla_noacid_ugL_rep <- calcChlaNoAcid(
        bind_cols(
          select(
            rawDataUpdated(),
            all_of(
              paste0('lab_chla_fluor_1_rep_', rep)
            )
          ),
          select(
            stdCurveIds(),
            chla_noacid_std_curve_id
          )
        ),
        pool
      )
      chla_noacid_ugL_rep <- ifelse(
        chla_noacid_ugL_rep != 'KEEP OLD',
        chla_noacid_ugL_rep,
        pull(row(), colNames[4])
      )
      
      afdm_g_filter_rep <- calcMinus(
        select(
          rawDataUpdated(),
          all_of(
            paste0(
              c('lab_chla_wgt_1_rep_', 'lab_chla_wgt_2_rep_'),
              rep
            )
          )
        )
      )
      
      # Get columns used to convert to per m2
      perM2Cols <- rawDataUpdated() %>% select(
        all_of(
          paste0(
            c('lab_chla_sizeA_rep_',
              'lab_chla_sizeB_rep_',
              'lab_chla_sizeC_rep_',
              'lab_chla_tot_vol_rep_'),
            rep
          )
        )
      ) %>% mutate(
        !!colNames[1] := lab_chla_vol_filtrated_rep
      )
      
      # Calculate chla per m2
      chla_acid_ugm2_rep <- calcChlaPerM2(
        perM2Cols %>% mutate(
          !!colNames[2] := chla_acid_ugL_rep
        )
      )
      chla_acid_ugm2_rep <- ifelse(
        chla_acid_ugm2_rep != 'KEEP OLD',
        chla_acid_ugm2_rep,
        pull(row(), colNames[3])
      )
      
      chla_noacid_ugm2_rep = calcChlaPerM2(
        perM2Cols %>% mutate(
          !!colNames[4] := chla_noacid_ugL_rep
        )
      )
      chla_noacid_ugm2_rep <- ifelse(
        chla_noacid_ugm2_rep != 'KEEP OLD',
        chla_noacid_ugm2_rep,
        pull(row(), colNames[5])
      )
      
      # Calcualte afdm
      afdm_gm2_rep = calcBenthicAFDM(
        perM2Cols %>% mutate(
          !!colNames[6] := afdm_g_filter_rep
        )
      )
      afdm_gm2_rep <- ifelse(
        afdm_gm2_rep != 'KEEP OLD',
        afdm_gm2_rep,
        pull(row(), colNames[7])
      )
      
      # Create data.frame
      newCols <- setNames(
        data.frame(
          lab_chla_vol_filtrated_rep = lab_chla_vol_filtrated_rep,
          chla_acid_ugL_rep = chla_acid_ugL_rep,
          chla_acid_ugm2_rep = chla_acid_ugm2_rep,
          chla_noacid_ugL_rep = chla_noacid_ugL_rep,
          chla_noacid_ugm2_rep = chla_noacid_ugm2_rep,
          afdm_g_filter_rep = afdm_g_filter_rep,
          afdm_gm2_rep = afdm_gm2_rep
        ),
        colNames
      )
      
      # If calculations$chla is NULL, create it else update it
      if (is.null(calculations$chla)) {
        calculations$chla <- newCols
      } else {
        calculations$chla <- bind_cols(
          calculations$chla,
          newCols
        )
      }
    }
    
    # Calculate Chla and benthic AFDM avg and sd
    # Set to NULL in case of second calculation
    calculations$avgSd <- NULL
    
    for (param in c('_acid_ugm2', '_noacid_ugm2','_acid_ugL', '_noacid_ugL', 'afdm_gm2')) {
      # Select data
      df <- calculations$chla %>% select(matches(param))
      
      # Calculate mean and sd
      newMean <- calcMean(df)
      newSd <- calcSd(df)
      if (param == 'afdm_gm2') {
        meanCol <-  'benthic_AFDM_avg_gm2'
        sdCol <-  'benthic_AFDM_sd_gm2'
      } else {
        if (grepl('ugL', param)) {
          meanCol <-  paste0('Chla', param, '_avg')
          sdCol <-  paste0('Chla', param, '_sd')
        } else {
          paramSplitted <- unlist(str_split(param, '_'))[-1]
          meanCol <- paste('Chla', paramSplitted[1], 'avg', paramSplitted[2], sep = '_')
          sdCol <- paste('Chla', paramSplitted[1], 'sd', paramSplitted[2], sep = '_')
        }
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
          chlaUpdated(),
          avgSdUpdated(),
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
      noCheckCols = reactive(row() %>% select(matches('_(avg|sd)(_(u)?gm2)?$')) %>% colnames()),
      # Return a list containing key-value pairs of columns to check with the regex to get the columns to check against
      checkCols = reactive({
        # Add complex comparisons
        cols <- row() %>% select(matches('_[ABCDE]$', ignore.case = FALSE)) %>% colnames()
        cols2check <- `names<-`(as.list(sub('_[ABCDE]$', '_(A|B|C|D|E)', cols)), cols)
        # Return list
        cols2check
      })
    )
  )
}
