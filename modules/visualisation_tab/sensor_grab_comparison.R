## This module contains the code for the sensor vs grab samples data comparison visualisation

## Create the UI function of the module ###############################################

sensorGrabComparisonUI <- function(id, sites, parameters) {
# Create the UI for the sensorGrabComparison module
# Parameters:
#  - id: String, the module id
#  - sites: Named list, contains all sites info, cf data_preprocessing.R
#  - parameters: Named list, contains both grab samples and high frequency data parameters info, cf data_preprocessing.R
# 
# Returns a list containing:
#  - inputs: the inputs UI elements of the module
#  - plots: the plots UI elements of the module
  
  # Create namespace
  ns <- NS(id)
  
  # Parse id to get the module unit nb
  splittedId <- str_split(id, '-') %>% unlist()
  unitNb <- splittedId[length(splittedId)]
  
  # Create the UI list to be returned
  list(
    # Create the UI inputs
    'inputs' = div(
      # Set UI inputs id and class
      id = str_interp('sensor-vs-grab-plot-input-${id}'),
      class = 'time-serie-input',
      # Create select input for site selection
      selectInput(ns('site'), str_interp('Station ${unitNb}'), sites$sitesSelectOptions),
      # Create a select input for parameter selection
      selectInput(
        ns('paramHf'),
        # Create a label with an icon button
        tags$span(
          str_interp('Parameter ${unitNb}'),
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        parameters$hf$vsGrabSelectOptions
      ),
      # Create checkbox to show/hide modeled data
      checkboxInput(ns('showModeledData'), 'Show modeled data', value = TRUE),
      # Create radio buttons to select the data frequency to display
      checkboxGroupInputWithClass(
        radioButtons(
          ns('dataFreq'),
          'Data frequency',
          choices = list('10min (raw)' = '10min', '24H'),
          selected = '24H'
        ),
        class = 'checkbox-grid'        
      ),
      # Create radio buttons group to select grab parameter
      radioButtons(
        ns('paramGrab'), 
        'Grab sample parameter',
        choices = 'NULL'
      )
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = str_interp('sensor-vs-grab-plots-${id}'),
      class = 'time-serie-plot two-plots point-hover-widget-plot',
      # Create a plotOutput for the grab vs grab plot
      spinnerPlotOutput(
        ns('sensorGrabTimeserie'),
        # Make data points hoverable
        hover = hoverOpts(ns('sensorGrabTimeserie_hover')),
        # Make plot brushable in the x direction with a debouncing delay type
        # Reset it when the plot is refreshed
        brush = brushOpts(
          ns('sensorGrabTimeserie_brush'),
          direction = 'x',
          delayType = 'debounce',
          resetOnNew = TRUE
        ),
        # Make plot double clickable
        dblclick = dblclickOpts(ns('sensorGrabTimeserie_dblclick'))
      ),
      spinnerPlotOutput(
        ns('sensorVsGrab'),
        # Make data points hoverable
        hover = hoverOpts(ns('sensorVsGrab_hover'))
      )
    )
  )
}



## Create the server function of the module ###############################################

sensorGrabComparison <- function(input, output, session, df, dateRange, sites, parameters) {
# Create the logic for the sensorGrabComparison module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - df: Named list of Data.frame, the grab samples and high frequency data
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - sites: Named list, contains all sites info, cf data_preprocessing.R
#  - parameters: Named list, contains both grab samples and high frequency data parameters info, cf data_preprocessing.R
# 
# Returns a reactive expression containing the updated date range with the same format as the input
  

  ## Parameters update logic ###################################################
  
  # Create a reactive expression that returns the selected high frequency parameter info
  paramHf <- reactive({
    # Run it only if input$paramHf is available
    req(input$paramHf)
    parameters$hf$parameters %>% filter(param_name == input$paramHf)
  })
  
  # Create an observeEvent that react to the paramHf reactive expression
  observeEvent(paramHf(),{
    # Get grab parameters
    grabParams <- paramHf() %>% select(grab_param_name) %>% str_split(',') %>% unlist()
    
    # Update grab sample parameter radio buttons group input with current parameter info
    updateRadioButtons(session, 'paramGrab',
                       choices = grabParams,
                       selected = grabParams[1])
  })
  
  # Create a reactive expression that returns the selected grab sample parameter info
  paramGrab <- reactive({
    parameters$grab$parameters %>% filter(param_name == input$paramGrab)
  })
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expressions that return a subsets of the HF and grab data
  # Using the dateRange, sites and parameters reactive expressions
  
  # Create the high frequency data subset
  hfDf <- reactive({
    # Get high frequency data
    hfDf <- df$hf[[input$dataFreq]]

    # Define data types to keep depending on the state of showModeledData
    types <- c('measured')
    if (input$showModeledData) types <- c(types, 'modeled')
    
    # Filter the data using the selected sites, the data type and the date range
    # Then select the parameter and rename the column to 'value'
    hfDf <- hfDf %>% filter(
      Site_ID == input$site,
      data_type %in% types,
      date(date) >= dateRange()$min,
      date(date) <= dateRange()$max
    ) %>% select(date, Site_ID, data_type, 'value' = paramHf()$data)
    
    # If there is no data return NULL
    if (nrow(hfDf) == 0) return(NULL)
    
    # Return the formatted data
    hfDf
  })
  
  # Create the grab data subset
  grabDf <- reactive({
    # If no grab parameter is selected return NULL
    if (nrow(paramGrab()) == 0) return(NULL)
    
    # Get grab samples data
    grabDf <- df$grab
    
    # Filter the data using the selected sites and the date range
    # Then select the parameter and rename the column to 'value'
    grabDf <- grabDf %>% filter(
      Site_ID == input$site,
      DATE_reading >= dateRange()$min,
      DATE_reading <= dateRange()$max
    ) %>% select(DATETIME_GMT, Site_ID, 'value' = unlist(str_split(paramGrab()$data, ','))[1])
    
    # If there is no data return NULL
    if (nrow(grabDf) == 0) return(NULL)
    
    # Return the formatted data
    grabDf
  })
  
  # Create the grab vs HF data subset
  vsDf <- reactive({
    # If grabDf is NULL or hfData is empty return NULL
    if (is.null(grabDf())) return(NULL)
    
    # Get hf data
    # Refilter HF data here to always get the 10min data and not rerender when changing the frequency
    hfData <- df$hf$`10min`
    
    # Define data types to keep depending on the state of showModeledData
    types <- c('measured')
    if (input$showModeledData) types <- c(types, 'modeled')
    
    # Filter the data using the selected sites, the data type and the date range
    # Then select the parameter and rename the column to 'value'
    hfData <- hfData %>% filter(
      Site_ID == input$site,
      data_type %in% types,
      date(date) >= dateRange()$min,
      date(date) <= dateRange()$max
    ) %>% select(date, Site_ID, data_type, 'value' = isolate(paramHf()$data)) # Isolate paramHF to avoid multiple rerender
    
    # If hfData is empty return NULL
    if (nrow(hfData) == 0) return(NULL)
    
    # Get the non NA grab data, if empty return NULL
    vsDf <- grabDf() %>% filter(!is.na(value))
    if (nrow(vsDf) == 0) return(NULL)
    
    # Rename the value column  and add new empty columns
    vsDf <- rename(vsDf, 'grab_value' = value)
    vsDf['hf_value'] <- rep(NA, nrow(vsDf))
    # vsDf['hf_sd'] <- rep(NA, nrow(vsDf))
    
    # For each grab data point calculate the corresponding HF data
    for (i in 1:nrow(vsDf)) {
      # Define the HF data starting time, e.i. 2 hours after the grab sample
      startingTime <- vsDf[i, 'DATETIME_GMT'] + hours(2)
      # Define the HF data ending time, e.i. 4 hours after the starting time
      endingTime <- startingTime + hours(4)
      
      # Filter the HF data using the interval
      filteredHf <- hfData %>% filter(date >= startingTime, date <= endingTime)
      
      # If no data proceed to next iteration
      if (nrow(filteredHf) == 0) next
      
      # If showModeledData is true combine the measured and modeled HF data in one vector
      # Else get the value (the modeled data are already filtered in hfData)
      if (input$showModeledData) {
        filteredHf <- filteredHf %>% pivot_wider(names_from = data_type, values_from = value) %>% select(measured, modeled)
        valuesHf <- rowSums(filteredHf, na.rm=TRUE) * NA ^ !rowSums(!is.na(filteredHf))
      } else {
        valuesHf <- filteredHf %>% pull(value)
      }
      
      # Calculate the average and sd
      averageHf <- mean(valuesHf, na.rm = TRUE)
      # sdHf <- sd(valuesHf, na.rm = TRUE)
      
      # Set the values in the df
      vsDf[i, 'hf_value'] <- averageHf
      # vsDf[i, 'hf_sd'] <- sdHf
    }
    
    # Clear Hf data from memory
    rm(hfData, filteredHf, valuesHf, averageHf)

    # If there are no HF data return NULL
    if (all(is.na(vsDf$hf_value))) return(NULL)

    # Return the df
    vsDf
  })

  
  
  
  ## Plots output logic ###########################################################

  # Render the regular timeserie plot
  output$sensorGrabTimeserie <- renderPlot({
    # If no grab parameter is selected return NULL
    if (nrow(paramGrab()) == 0) return(NULL)
    
    # Isolate HF data to avoid multiple rerender
    hfData <- isolate(hfDf())
    # Call inputs to rerender the plot
    input$showModeledData
    input$dataFreq
    
    # If there are no data return NULL
    if (hfData %>% is.null()) return(NULL)
    
    # Get unitNb
    splittedId <- str_split(session$ns('0'), '-') %>% unlist()
    unitNb <- splittedId[length(splittedId) - 1]
    
    # Create a highFreqTimeSeriePlot
    p <- highFreqTimeSeriePlot(
      df = hfData,
      # Isolate paramHF to avoid multiple rerender
      parameter = isolate(paramHf()),
      plotTitle = str_interp('Sensor High Frequency Time Serie ${unitNb}'),
      sites = sites$sites
    )
    
    # If there are some grab sample data available
    # Add them to the graph
    if (!is.null(grabDf())) {
      p <- addGrabSamplePoints(
        p = p,
        df = grabDf(),
        minHf = min(hfData$value, na.rm = TRUE),
        maxHf = max(hfData$value, na.rm = TRUE)
      )
    }
    
    # Return the graph
    p
  })

  
  
  # Render on vs one plot
  output$sensorVsGrab <- renderPlot({
    # If there are no data return NULL
    if (vsDf() %>% is.null()) return(NULL)
    
    # Get unitNb
    splittedId <- str_split(session$ns('0'), '-') %>% unlist()
    unitNb <- splittedId[length(splittedId) - 1]
    
    # Get current site name and color
    currentSite <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_full)
    currentColor <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_color)
    
    # Create a onVsOne plot add the one to one line and return the plot
    onVsOnePlot(
      df = vsDf(),
      x = 'grab_value',
      y = 'hf_value',
      parameterX = paramGrab(),
      # Isolate paramHF to avoid multiple rerender
      parameterY  = isolate(paramHf()),
      plotTitle = str_interp('${currentSite} Sensor VS Grab ${unitNb}'),
      color = currentColor
    ) %>% addOneToOneLine(
      minData = min(vsDf()$grab_value, vsDf()$hf_value, na.rm = TRUE),
      maxData = max(vsDf()$grab_value, vsDf()$hf_value, na.rm = TRUE)
    )
  })
  
  
  
  
  ## Plot hovering logic ##########################################################

  # Activate the hover widget for the regular timeserie plot
  pointHoverWidgetServer(session, 'sensorGrabTimeserie', hfDf, reactive(input$sensorGrabTimeserie_hover),
                         x_label = 'Date', y_label = 'Parameter',
                         secondDf = grabDf, secondX = 'DATETIME_GMT', secondY = 'value')
  # Activate the hover widget for the vs plot
  pointHoverWidgetServer(session, 'sensorVsGrab', vsDf, reactive(input$sensorVsGrab_hover),
                         x_label = 'Grab Sample', y_label = 'Sensor')

  
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelper, {
    # Render the description UI in the modal
    output$description <- renderUI(tags$p(
      class = 'description',
      paramHf()$description
    ))
    
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Parameters description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  ## Update dateRange with plot brushing and double click logic ####################################

  # Create a reactive expression that contains the new dateRange to be used globally
  # With the same format as the input dateRange
  # Should be returned by the module
  # Converting number to date using the Linux epoch time as origin
  updateDateRange <- reactive(list(
    'min' = as.Date(as.POSIXct(input$sensorGrabTimeserie_brush$xmin, origin = "1970-01-01", tz = "GMT")),
    'max' = as.Date(as.POSIXct(input$sensorGrabTimeserie_brush$xmax, origin = "1970-01-01", tz = "GMT"))
  ))

  # Create a reactive value that update each time the plot is double clicked
  # Used as trigger to reset the date range in the outer module
  # Initialised to NULL to avoid a dateRange reset when a new unit is created
  resetDateRange <- reactiveVal(NULL)

  # Create an observe event that react on plot double click to reset the date range
  observeEvent(input$sensorGrabTimeserie_dblclick, {
    if (is.null(resetDateRange())) {
      resetDateRange(1)
    } else {
      resetDateRange(resetDateRange() + 1)
    }
  })

  # Return the new dateRange values and date range reset trigger in order to update the outer module dateRangeInput
  return(list(
    'update' = updateDateRange,
    'reset' = resetDateRange
  ))
}

