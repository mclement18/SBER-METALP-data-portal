## This module contains the code for the sensor vs grab samples data comparison visualisation

## Create the UI function of the module ###############################################

sensorGrabComparisonUI <- function(id, pool) {
# Create the UI for the sensorGrabComparison module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a list containing:
#  - inputs: the inputs UI elements of the module
#  - plots: the plots UI elements of the module
  
  # Create namespace
  ns <- NS(id)
  
  # Create the UI list to be returned
  list(
    # Create the UI inputs
    'inputs' = div(
      # Set UI inputs id and class
      id = paste0('sensor-vs-grab-plot-input-', id),
      class = 'time-serie-input',
      # Create select input for site selection
      selectInput(
        ns('site'),
        'Station',
        parseOptionsWithSections(
          getRows(pool, 'stations', columns = c('name', 'full_name', 'catchment')),
          valueColumn = 'name',
          sectionColumn = 'catchment',
          optionColumn = 'full_name'
        )
      ),
      # Create a select input for parameter selection
      selectInput(
        ns('paramHf'),
        # Create a label with an icon button
        tags$span(
          'Parameter',
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        parseOptionsWithSections(
          getRows(pool, 'sensor_params_plotting',
                  !is.na(grab_param_name),
                  columns = c('section_name', 'option_name', 'param_name')),
          'param_name'
        )
      ),
      # Create checkbox to show/hide modeled data
      checkboxInput(ns('showModeledData'), 'Show modeled data', value = TRUE),
      # Create radio buttons to select the data frequency to display
      checkboxGroupInputWithClass(
        radioButtons(
          inputId = ns('dataFreq'),
          label = tags$span(
            'Data frequency',
            # Create an icon button that trigger a modal to display the parameter description
            actionButton(ns('hfFreqHelper'), icon('question-circle'), class = 'icon-btn')
          ),
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
      id = paste0('sensor-vs-grab-plots-', id),
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

sensorGrabComparison <- function(input, output, session, df, dateRange, pool) {
# Create the logic for the sensorGrabComparison module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - df: Named List of Data.frame, the sensors high frequency data at different frequency
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - pool: The pool connection to the database
# 
# Returns a reactive expression containing the updated date range with the same format as the input
  
  ## Selected station logic #######################################################
  
  # Create a reactive expression that return the current site info
  currentSite <- reactive(getRows(
    pool,
    'stations',
    name == local(input$site),
    columns = c('name', 'full_name', 'color')
  ))
  
  

  ## Parameters update logic ###################################################
  
  # Create a reactive expression that returns the selected high frequency parameter info
  paramHf <- reactive({
    # Run it only if input$paramHf is available
    req(input$paramHf)
    getRows(
      pool, 'sensor_params_plotting',
      param_name == local(input$paramHf),
      columns = c('param_name', 'units', 'data', 'grab_param_name', 'description')
    )
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
  paramGrab <- reactive(getRows(
    pool, 'grab_params_plotting',
    param_name == local(input$paramGrab),
    columns = c('param_name', 'units', 'data')
  ))
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expressions that return a subsets of the HF and grab data
  # Using the dateRange, sites and parameters reactive expressions
  
  # Create the high frequency data subset
  hfDf <- reactive({
    # Get high frequency data
    hfDf <- df[[input$dataFreq]]

    # If the raw data is selected filter also for modeled data
    if (input$dataFreq == '10min') {
      # Define data types to remove depending on the state of showModeledData
      # If nothing to remove, set to 'NULL' as string to avoid match error
      typesToRemove <- c('modeled')
      if (input$showModeledData) typesToRemove <- 'NULL'
      
      # Filter the data using the selected sites and the date range
      hfDf %<>% filter(
        Site_ID == input$site,
        date(Date) >= dateRange()$min,
        date(Date) <= dateRange()$max
      ) %>%
        # Select the date, Site_ID, all the parameter specific columns and remove modeled column not used
        select(Date, Site_ID, starts_with(paramHf()$data), -ends_with(typesToRemove)) %>% 
        # Pivot longer the data to get a data_type and a value column
        pivot_longer(
          ends_with(c('measured', 'modeled')),
          names_to = 'data_type',
          names_pattern = '.*_(.*)',
          names_transform = list('data_type' = as.factor),
          values_to = 'value'
        ) %>% 
        # Rename with singlePoint column
        rename(singlePoint = ends_with('singlePoint'))
    } else {
      # Filter the data using the selected sites and the date range
      # Then select the parameter and rename the column to 'value'
      hfDf %<>% filter(
        Site_ID == input$site,
        date(Date) >= dateRange()$min,
        date(Date) <= dateRange()$max
      ) %>% select(Date, Site_ID, 'value' = paramHf()$data)
    }
    
    # If there is no data return NULL
    if (nrow(hfDf) == 0) NULL else hfDf
  })
  
  # Create the grab data subset
  grabDf <- reactive({
    # If no grab parameter is selected return NULL
    if (nrow(paramGrab()) == 0) return(NULL)
    
    # Get grab parameter
    # If multiple sub params keep only the first one
    paramGrab <- unlist(str_split(paramGrab()$data, ','))[1]
    
    # Filter the data using the selected sites and the date range
    # And select the columns
    grabDf <- getRows(
      pool = pool,
      table = 'data',
      station == local(input$site),
      DATE_reading >= local(dateRange()$min),
      DATE_reading <= local(dateRange()$max),
      columns = c(
        'station', 'DATE_reading', 'TIME_reading_GMT',
        paramGrab
      )
      # Parse the DATE and time
    ) %>% mutate(
      station = as.factor(station),
      DATETIME_GMT = ymd_hms(paste(DATE_reading, TIME_reading_GMT), tz = 'GMT'),
      DATE_reading = ymd(DATE_reading),
      DATETIME_month_day_time_GMT = `year<-`(DATETIME_GMT, 2020)
      # Rename for the plotting function
    ) %>% rename(
      Site_ID = station,
      value = all_of(paramGrab)
    )
    
    # If there is no data return NULL
    if (nrow(grabDf) == 0) NULL else grabDf
  })
  
  # Create the grab vs HF data subset
  vsDf <- reactive({
    # If grabDf is NULL or hfData is empty return NULL
    if (is.null(grabDf())) return(NULL)
    
    # Get hf data
    # Refilter HF data here to always get the 10min data and not rerender when changing the frequency
    hfData <- df$`10min`
    
    # Define data types to remove depending on the state of showModeledData
    # If nothing to remove, set to 'NULL' as string to avoid match error
    typesToRemove <- c('modeled')
    if (input$showModeledData) typesToRemove <- 'NULL'
    
    # Filter the data using the selected sites and the date range
    hfData %<>% filter(
      Site_ID == input$site,
      date(Date) >= dateRange()$min,
      date(Date) <= dateRange()$max
    ) %>%
      # Select the date, Site_ID, all the parameter specific columns
      # And remove the singlePoint column and the modeled one if needed
      # Isolate paramHF to avoid multiple rerender
      select(Date, Site_ID, starts_with(isolate(paramHf()$data)), -ends_with(c(typesToRemove, 'singlePoint'))) %>% 
      # Pivot longer the data to get a data_type and a value column
      pivot_longer(
        ends_with(c('measured', 'modeled')),
        names_to = 'data_type',
        names_pattern = '.*_(.*)',
        names_transform = list('data_type' = as.factor),
        values_to = 'value'
      )
    
    # If hfData is empty return NULL
    if (nrow(hfData) == 0) return(NULL)
    
    # Get the non NA grab data, if empty return NULL
    vsDf <- grabDf() %>% filter(!is.na(value))
    if (nrow(vsDf) == 0) return(NULL)
    
    # Rename the value column  and add new empty columns
    vsDf <- rename(vsDf, 'grab_value' = value)
    vsDf %<>%  mutate(hf_value = as.numeric(rep(NA, nrow(vsDf))))
    # vsDf['hf_sd'] <- rep(NA, nrow(vsDf))
    
    # For each grab data point calculate the corresponding HF data
    for (i in 1:nrow(vsDf)) {
      # Define the HF data starting time, e.i. 2 hours after the grab sample
      startingTime <- vsDf$DATETIME_GMT[i] + hours(2)
      # Define the HF data ending time, e.i. 4 hours after the starting time
      endingTime <- startingTime + hours(4)
      
      # Filter the HF data using the interval
      filteredHf <- hfData %>% filter(Date >= startingTime, Date <= endingTime)
      
      # If no data proceed to next iteration
      if (nrow(filteredHf) == 0) next
      
      # If showModeledData is true combine the measured and modeled HF data in one vector
      # Else get the value (the modeled data are already filtered in hfData)
      if (input$showModeledData) {
        filteredHf %<>% pivot_wider(names_from = data_type, values_from = value) %>% select(measured, modeled)
        valuesHf <- rowSums(filteredHf, na.rm=TRUE) * NA ^ !rowSums(!is.na(filteredHf))
      } else {
        valuesHf <- filteredHf %>% pull(value)
      }
      
      # Calculate the average and sd
      averageHf <- mean(valuesHf, na.rm = TRUE)
      # sdHf <- sd(valuesHf, na.rm = TRUE)
      
      # Set the values in the df (data.table)
      vsDf <- as.data.table(vsDf)
      vsDf[i, hf_value := averageHf]
      # vsDf[i, 'hf_sd'] <- sdHf
    }
    
    # Clear Hf data from memory
    rm(hfData, filteredHf, valuesHf, averageHf)

    # If there are no HF data return NULL else vsDF
    if (all(is.na(vsDf$hf_value))) NULL else vsDf
  })

  
  
  
  ## Plots output logic ###########################################################

  # Render the regular timeserie plot
  output$sensorGrabTimeserie <- renderPlot({
    # If no grab parameter is selected return NULL
    if (nrow(paramGrab()) == 0) return(NULL)
    
    # Isolate HF data to avoid multiple rerender
    hfData <- isolate(hfDf())
    # Call inputs to rerender the plot
    if (input$dataFreq == '10min') input$showModeledData
    
    # If there are no data return NULL
    if (hfData %>% is.null()) return(NULL)
    
    # Create a highFreqTimeSeriePlot
    p <- highFreqTimeSeriePlot(
      df = hfData,
      # Isolate paramHF to avoid multiple rerender
      parameter = isolate(paramHf()),
      plotTitle = 'Sensor High Frequency Time Serie',
      sites = currentSite(),
      modeledData = 'data_type' %in% colnames(hfData)
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
    
    # Get current site name and color
    site <- currentSite()
    currentSiteName <- site %>% pull(full_name)
    currentSiteColor <- site %>% pull(color)
    
    # Create a onVsOne plot add the one to one line and return the plot
    onVsOnePlot(
      df = vsDf(),
      x = 'grab_value',
      y = 'hf_value',
      parameterX = paramGrab(),
      # Isolate paramHF to avoid multiple rerender
      parameterY  = isolate(paramHf()),
      plotTitle = paste(currentSiteName, 'Sensor VS Grab'),
      color = currentSiteColor
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
      title = 'Parameter description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  ## Data Frequency helper logic ####################################################
  
  # Create an observeEvent that react to the data freq helper button
  observeEvent(input$hfFreqHelper, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = 'Sensor data frequency selection',
      htmlTemplate('./html_components/data_freq_help.html', icon = icon('exclamation-triangle')),
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

