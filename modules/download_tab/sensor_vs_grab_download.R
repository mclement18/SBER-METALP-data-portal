## This module contains the UI and server code for the sensor vs grab Download

## Create module UI ###############################################################

sensorVSGrabDownloadUI <- function(id, pool) {
# Create the UI for the sensorVSGrabDownload module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a tagList containing the UI elements
  
  # Create namespace
  ns <- NS(id)
  
  # Create the tagList containing the UI elements
  tagList(
    # Public or global grab param selection
    radioButtons(
      inputId = ns('grabSelection'),
      label = 'Grab selection',
      choices = list(
        'Grab table columns' = 'global',
        'Public grab selection' = 'public'
      )
    ),
    # Global grab parameter selection
    selectizeInput(
      inputId =  ns('globalGrabParam'),
      # Create a label with an icon button
      label = 'Grab parameters',
      choices = parseOptionsWithSections(
        getRows(pool, 'grab_param_categories', columns = c('category', 'param_name')),
        valueColumn = 'param_name', sectionColumn = 'category', optionColumn = 'param_name'
      ),
      multiple = TRUE,
      options = list(
        'placeholder' = 'Select some parameters...',
        'plugins' = list('remove_button')
      )
    ),
    # Hidden by default
    hidden(
      # Public grab parameter selection
      selectizeInput(
        inputId =  ns('grabParam'),
        # Create a label with an icon button
        label = 'Grab parameters',
        choices = parseOptionsWithSections(
          getRows(pool, 'grab_params_plotting', columns = c('section_name', 'option_name', 'param_name')),
          'param_name'
        ),
        multiple = TRUE,
        options = list(
          'placeholder' = 'Select some parameters...',
          'plugins' = list('remove_button')
        )
      )
    ),
    # High frequency data specific inputs
    # Select HF parameters
    selectizeInput(
      inputId =  ns('hfParam'),
      # Create a label with an icon button
      label = 'Sensor parameters',
      choices = parseOptionsWithSections(
        getRows(pool, 'sensor_params_plotting', columns = c('order', 'section_name', 'option_name', 'param_name')) %>%
          arrange(order) %>% select(-order),
        'param_name'
      ),
      multiple = TRUE,
      options = list(
        'placeholder' = 'Select some parameters...',
        'plugins' = list('remove_button')
      )
    ),
    # Select for modeled data
    checkboxInput(ns('addModeledData'), 'Add modeled data', value = FALSE)
  )
}



## Create module server function ##################################################

sensorVSGrabDownload <- function(input, output, session, pool, user, hfDf, selectedSites, dateRange, clear) {
# Create the logic for the sensorVSGrabDownload module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - user: Reactive values, the current user
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - selectedSites: Reactive expression that returns a character vector of sites
#  - dateRange: Reactive expression that returns a character vector of dates (1: min, 2: max)
#  - clear: Reactive expression that returns the clear button value
# 
# Returns a list of outputs

  ## Multi selection inputs debouncing ############################################
  
  # Debounce HF parameters selection
  hfParamReactive <- reactive(input$hfParam)
  hfParamReactive_d <- debounce(hfParamReactive, 1000)
  
  # Debounce Grab parameters selection
  grabParamReactive <- reactive(input$grabParam)
  grabParamReactive_d <- debounce(grabParamReactive, 1000)
  globalGrabParamReactive <- reactive(input$globalGrabParam)
  globalGrabParamReactive_d <- debounce(globalGrabParamReactive, 1000)
  
  
  
  
  
  ## Parameters selection logic ###################################################
  
  # Create an observeEvent that react to the grabSelection radio buttons
  observeEvent(input$grabSelection, ignoreInit = TRUE, {
    # Toggle grab parameters selection visibility depending on the grab selection
    selection <- input$grabSelection
    toggleElement('globalGrabParam', condition = selection == 'global')
    toggleElement('grabParam', condition = selection == 'public')
  })
  
  
  
  
  # Create a reactive expression that return the selected grab parameters
  grabParams <- reactive({
    # Use correct logic
    if (input$grabSelection == 'global') {
      # Returns column names
      globalGrabParamReactive_d()
    } else if (input$grabSelection == 'public') {
      # Get parameters info
      parametersInfo <- getRows(
        pool, 'grab_params_plotting',
        param_name %in% local(grabParamReactive_d()),
        columns = c('data', 'sd', 'min_max')
      )
      
      # Get parameters
      raw_params <- na.exclude(c(parametersInfo$data, parametersInfo$sd, parametersInfo$min_max))
      
      # Create an empty vector
      params <- c()
      # For each parameter unlist them and concatenate to params vector
      for (param in raw_params) {
        params <- c(params, unlist(str_split(param, ',')))
      }
      
      # Return parameters
      params
    }
  })
  
  # Create a reactive expression that return the selected sensor parameters
  hfParams <- reactive({
    getRows(
      pool, 'sensor_params_plotting',
      param_name %in% local(hfParamReactive_d()),
      columns = c('order', 'data')
    ) %>% arrange(order) %>% pull(data)
  })
  
  
  
  
  
  ## Data filtering logic #########################################################
  
  # Create a reactive expression returning the selected data
  selectedData <- reactive({
    # Return an empty data.table when the dateRange is NULL
    if (is.null(dateRange())) return(data.table())
    
    # Get reactive expressions
    sites <- selectedSites()
    minDate <- dateRange()[1]
    maxDate <- dateRange()[2]
    grabParams <- grabParams()
    addModeledData <- input$addModeledData
    hfParams <- hfParams()
    
    # Get the grab data
    grabDf <- getRows(
      pool = pool,
      table = 'data',
      station %in% sites,
      DATE_reading >= minDate,
      DATE_reading <= maxDate,
      columns = c(
        'station', 'DATE_reading', 'TIME_reading_GMT',
        grabParams
      )
      # Parse the DATE and time
    ) %>% mutate(
      station = as.factor(station),
      Date = ymd_hms(paste(DATE_reading, TIME_reading_GMT), tz = 'GMT')
      # Rename for the download
    ) %>% rename(
      Site_ID = station
      # Reorder columns and filter columns
    ) %>% select(
      Date, Site_ID, all_of(grabParams)
      # Reorder rows
    ) %>% arrange(
      Date, Site_ID
      # Convert to a data-table for further processing and print output
    ) %>% as.data.table()
    
    # If the grab data is empty return it
    if (nrow(grabDf) == 0) return(grabDf)
    
    # Define data types to remove depending on the state of addModeledData
    # If nothing to remove, set to 'NULL' as string to avoid match error
    typesToRemove <- c('modeled')
    if (addModeledData) typesToRemove <- 'NULL'
    
    # Filter the sensor data using the selected sites and the date range
    hfData <- hfDf$`10min` %>%
      # Filter rows
      filter(
        date(Date) >= minDate,
        date(Date) <= maxDate,
        Site_ID %in% sites
      ) %>%
      # Select the date, Site_ID, all the parameter specific columns
      # And remove the singlePoint column and the modeled one if needed
      select(Date, Site_ID, starts_with(hfParams), -ends_with(c(typesToRemove, 'singlePoint')))
    
    # Iterate over sensor parameters
    for (parameter in hfParams) {
      # Create the new parameter and parameter_sd columns
      parameter_avg <- paste0(parameter, '_avg')
      parameter_sd <- paste0(parameter, '_sd')
      grabDf %<>%  mutate(
        !!parameter_avg := as.numeric(rep(NA, nrow(grabDf))),
        !!parameter_sd := as.numeric(rep(NA, nrow(grabDf)))
      )
      
      # Get the sensor parameter column
      parameterData <- hfData %>% select(Site_ID, Date, starts_with(parameter))
      
      # For each grab data point calculate the corresponding HF data
      for (i in 1:nrow(grabDf)) {
        # Define the HF data starting time, e.i. 2 hours after the grab sample
        startingTime <- grabDf$Date[i] + hours(2)
        # Define the HF data ending time, e.i. 4 hours after the starting time
        endingTime <- startingTime + hours(4)
        # Get site
        site <- as.character(grabDf$Site_ID[i])
        
        # Filter the HF data using the interval and site
        filteredHf <- parameterData %>% filter(Site_ID == site, Date >= startingTime, Date <= endingTime)
        
        # If no data proceed to next iteration
        if (nrow(filteredHf) == 0) next
        
        # If addModeledData is true combine the measured and modeled HF data in one vector
        # Else get the values (the modeled data are already filtered)
        if (input$addModeledData) {
          filteredHf %<>% select(starts_with(parameter))
          valuesHf <- rowSums(filteredHf, na.rm=TRUE) * NA ^ !rowSums(!is.na(filteredHf))
        } else {
          valuesHf <- filteredHf %>% select(starts_with(parameter)) %>% pull()
        }
        
        # Calculate the average and sd
        averageHf <- mean(valuesHf, na.rm = TRUE)
        sdHf <- sd(valuesHf, na.rm = TRUE)
        
        # Set the values in the df (data.table)
        grabDf[i, (parameter_avg) := averageHf]
        grabDf[i, (parameter_sd) :=  sdHf]
      }
    }
    
    # Clear Hf data from memory
    rm(hfData, filteredHf, parameterData, valuesHf, averageHf, sdHf)
    
    # Return the updated df
    grabDf
  })
  
  
  
  
  
  
  
  
  ## Clear form logic #############################################################
  
  # Create a reactive expression that clear inputs
  clearInputs <- reactive({
    # Call clear inputs to rerun the reactive expression
    clear()
    
    # HF specific clearing
    
    # Reset modeled data selection
    updateCheckboxInput(session, 'addModeledData', value = FALSE)
    
    # Clear parameters selection
    updateSelectizeInput(session, 'hfParam', selected = '')
    
    # Grab specific clearing
    
    # Clear parameters selection
    updateSelectizeInput(session, 'grabParam', selected = '')
    updateSelectizeInput(session, 'globalGrabParam', selected = '')
  })
  
  
  
  
  
  
  
  ## Call Download data module #########################################################
  
  # Look for user update
  
  dlButton <- reactiveVal(downloadDataUI(session$ns('download')))
  callModule(downloadData, 'download', selectedData)
  
  
  
  
  
  
  
  ## Output returning logic #######################################################
  
  # Returns a list of output to the download layout module
  return(
    list(
      selectedData = selectedData,
      parameters = reactive(c(grabParams(), hfParams())),
      disclaimer = reactiveVal({}),
      button = dlButton,
      clearInputs = clearInputs
    )
  )
}
