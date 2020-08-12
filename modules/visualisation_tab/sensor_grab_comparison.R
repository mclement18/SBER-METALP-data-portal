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
      radioButtons(
        ns('paramGrab'), 
        'Grab sample parameter',
        choices = 'NULL'
      ),
      checkboxInput(ns('showModeledData'), 'Show modeled data', value = TRUE)
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = str_interp('sensor-vs-grab-plots-${id}'),
      class = 'time-serie-plot two-plots point-hover-widget-plot',
      # Create a plotOutput for the grab vs grab plot
      plotOutput(
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
      plotOutput(
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
#  - dfList: Named list of Data.frame, the grab samples and high frequency data
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - sites: Named list, contains all sites info, cf data_preprocessing.R
#  - parameters: Named list, contains both grab samples and high frequency data parameters info, cf data_preprocessing.R
# 
# Returns a reactive expression containing the updated date range with the same format as the input
  

  ## Parameters update logic ###################################################

  paramHf <- reactive({
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
  
  paramGrab <- reactive({
    parameters$grab$parameters %>% filter(param_name == input$paramGrab)
  })
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, sites and paramfilter reactive expressions
  hfDf <- reactive({
    # Get high frequency data
    hfDf <- df$hf
    
    # Define data types to keep depending on the state of showModeledData
    types <- c('measured')
    if (input$showModeledData) types <- c(types, 'modeled')
    
    # Filter the data using the selected sites, the data type and the date range
    # Then select the parameter and rename the column to 'value'
    hfDf <- hfDf %>% filter(
      Site_ID == input$site,
      data_type %in% types,
      date >= as.POSIXct(dateRange()$min, tz = 'GMT'),
      date <= as.POSIXct(dateRange()$max, tz = 'GMT')
    ) %>% select(date, Site_ID, data_type, 'value' = paramHf()$data)
    
    # If there is no data return NULL
    if (dim(hfDf)[1] == 0) return(NULL)
    
    # Return the formatted data
    hfDf
  })
  
  
  grabDf <- reactive({
    # If no grab parameter is selected return NULL
    if (nrow(paramGrab()) == 0) return(NULL)
    
    # Get grab samples data
    grabDf <- df$grab
    
    grabDf <- grabDf %>% filter(
      Site_ID == input$site,
      DATE_reading >= dateRange()$min,
      DATE_reading <= dateRange()$max
    ) %>% select(DATETIME_GMT, Site_ID, 'value' = paramGrab()$data)
    
    # If there is no data return NULL
    if (dim(grabDf)[1] == 0) return(NULL)
    
    # Return the formatted data
    grabDf
  })
  
  
  vsDf <- reactive({
    if (is.null(grabDf()) | is.null(hfDf())) return(NULL)
    
    vsDf <- grabDf()
    vsDf <- rename(vsDf, 'grab_value' = value)
    vsDf['hf_value'] <- rep(NA, nrow(vsDf))
    vsDf['hf_sd'] <- rep(NA, nrow(vsDf))
    
    for (i in 1:nrow(vsDf)) {
      startingTime <- vsDf[i, 'DATETIME_GMT'] + hours(2)
      endingTime <- startingTime + hours(4)
      filteredHf <- hfDf() %>% filter(date >= startingTime, date <= endingTime)
      if (input$showModeledData) {
        filteredHf <- filteredHf %>% pivot_wider(names_from = data_type, values_from = value) %>% select(measured, modeled)
        valuesHf <- rowSums(filteredHf, na.rm=TRUE) * NA ^ !rowSums(!is.na(filteredHf))
      } else {
        valuesHf <- filteredHf %>% pull(value)
      }
      averageHf <- mean(valuesHf, na.rm = TRUE)
      sdHf <- sd(valuesHf, na.rm = TRUE)
      
      vsDf[i, 'hf_value'] <- averageHf
      vsDf[i, 'hf_sd'] <- sdHf
    }

    vsDf
  })

  
  
  
  ## Plots output logic ###########################################################

  # Render the regular timeserie plot
  output$sensorGrabTimeserie <- renderPlot({
    grabDf()
    isolate({
      # If there are no data return NULL
      if (hfDf() %>% is.null() | grabDf() %>% is.null()) return(NULL)
      
      # Get unitNb
      splittedId <- str_split(session$ns('0'), '-') %>% unlist()
      unitNb <- splittedId[length(splittedId) - 1]
      
      # # Get current site name and color
      # currentSite <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_full)
      # currentColor <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_color)
      
      # Create and return a highFreqTimeSeriePlot
      p <- highFreqTimeSeriePlot(
        df = hfDf(),
        parameter = paramHf(),
        plotTitle = str_interp('Sensors High Frequency Time Serie ${unitNb}'),
        sites = sites$sites
      )
      p <- p + geom_point(data = grabDf(), mapping = aes(x = DATETIME_GMT, y = value), size = 3, color = 'black')
      p
    })
  })

  # Render the regular timeserie plot
  output$sensorVsGrab <- renderPlot({
    grabDf()
    isolate({
      # If there are no data return NULL
      if (vsDf() %>% is.null()) return(NULL)
      
      # Get unitNb
      splittedId <- str_split(session$ns('0'), '-') %>% unlist()
      unitNb <- splittedId[length(splittedId) - 1]
      
      # Get current site name and color
      currentSite <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_full)
      currentColor <- sites$sites %>% filter(sites_short == input$site) %>% pull(sites_color)
      
      # Create and return a onVsOne plot
      onVsOnePlot(
        df = vsDf(),
        x = 'grab_value',
        y = 'hf_value',
        parameterX = paramGrab(),
        parameterY  = paramHf(),
        plotTitle = str_interp('${currentSite} Sensor VS Grab ${unitNb}'),
        color = currentColor
      )
    })
  })
  
  
  ## Plot hovering logic ##########################################################

  # Activate the hover widget for the regular timeserie plot
  pointHoverWidgetServer(session, 'sensorGrabTimeserie', hfDf, reactive(input$sensorGrabTimeserie_hover))
  # Activate the hover widget for the vs plot
  pointHoverWidgetServer(session, 'sensorVsGrab', vsDf, reactive(input$sensorVsGrab_hover))

  
  
  
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

