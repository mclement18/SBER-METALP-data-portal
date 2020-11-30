## This module contains the code for the global grab samples data comparison visualisation

## Create the UI function of the module ###############################################

globalGrabSamplesComparisonUI <- function(id, pool) {
# Create the UI for the globalGrabSamplesComparison module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a list containing:
#  - inputs: the inputs UI elements of the module
#  - plots: the plots UI elements of the module
  
  # Create namespace
  ns <- NS(id)
  
  # Get parameter select options
  parameterOptions <- parseOptionsWithSections(
    getRows(pool, 'grab_param_categories', columns = c('category', 'param_name')),
    valueColumn = 'param_name', sectionColumn = 'category', optionColumn = 'param_name'
  )
  
  # Create the UI list to be returned
  list(
    # Create the UI inputs
    'inputs' = div(
      # Set UI inputs id and class
      id = paste0('global-grab-vs-grab-plot-input-', id),
      class = 'time-serie-input',
      # Create select input for catchment selection
      checkboxGroupInputWithClass(
        checkboxGroupInput(
          ns('sites'),
          'Station',
          choices = parseOptions(getRows(pool, 'stations', columns = 'name'), 'name')
        ),
        class = 'checkbox-grid'        
      ),
      # Inputs for X axis
      # Create a select input for parameter selection
      selectInput(
        ns('paramX'),
        # Create a label with an icon button
        tags$span(
          'Parameter X-axis',
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelperX'), icon('question-circle'), class = 'icon-btn')
        ),
        parameterOptions
      ),
      # Inputs for Y axis
      # Create a select input for parameter selection
      selectInput(
        ns('paramY'),
        # Create a label with an icon button
        tags$span(
          'Parameter Y-axis',
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelperY'), icon('question-circle'), class = 'icon-btn')
        ),
        parameterOptions
      )
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = paste0('global-grab-vs-grab-plots-', id),
      class = 'time-serie-plot point-hover-widget-plot',
      # Create a plotOutput for the grab vs grab plot
      spinnerPlotOutput(
        ns('grabVsGrab'),
        # Make data points hoverable
        hover = hoverOpts(ns('grabVsGrab_hover'))
      )
    )
  )
}



## Create the server function of the module ###############################################

globalGrabSamplesComparison <- function(input, output, session, dateRange, pool) {
# Create the logic for the globalGrabSamplesComparison module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # ## Selected station logic #######################################################
  # 
  # # Create a reactive expression that return the current site info
  # currentSite <- reactive(getRows(
  #   pool,
  #   'stations',
  #   name == local(input$site),
  #   columns = c('full_name', 'color')
  # ))
  
  
  
  
  ## Parameter logic ###################################################
  
  # Create a reactive expression that return the parameter X and Y infos
  currentParamX <- reactive(getRows(
    pool, 'grab_param_categories',
    param_name == local(input$paramX),
    columns = c('param_name', 'description')
  ))
  
  currentParamY <- reactive(getRows(
    pool, 'grab_param_categories',
    param_name == local(input$paramY),
    columns = c('param_name', 'description')
  ))
  
  
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, sites and currentParam reactive expressions
  data <- reactive({
    # Save the sub parameters to display
    paramColsX <- currentParamX()$param_name
    paramColsY <- currentParamY()$param_name
    
    # If there is no sub parameter return NULL
    if (is.null(paramColsX) | is.null(paramColsY)) return(NULL)
    
    
    # Filter the data using the selected sites and the date range
    # And select the columns
    df <- getRows(
      pool = pool,
      table = 'data',
      DATE_reading >= local(dateRange()$min),
      DATE_reading <= local(dateRange()$max),
      columns = c(
        'station', 'DATE_reading', 'TIME_reading_GMT',
        paramColsX, paramColsY
      )
      # Parse the DATE and time
    ) %>% mutate(
      station = as.factor(station),
      DATETIME_GMT = ymd_hms(paste(DATE_reading, TIME_reading_GMT), tz = 'GMT'),
      DATE_reading = ymd(DATE_reading),
      DATETIME_month_day_time_GMT = `year<-`(DATETIME_GMT, 2020)
      # Rename for the plotting function
    ) %>% rename(
      Site_ID = station
    )
    
    
    # If there is no data return NULL else the df
    if (dim(df)[1] == 0) NULL else df
  })
  
  
  
  ## Plots output logic ###########################################################
  
  # Render the regular timeserie plot
  output$grabVsGrab <- renderPlot({
    # If there are no data return NULL
    if (data() %>% is.null()) return(NULL)
    
    # Create and return a onVsOne plot
    onVsOnePlot(
      df = data(),
      x = currentParamX()$param_name,
      y = currentParamY()$param_name,
      parameterX = currentParamX(),
      parameterY  = currentParamY(),
      plotTitle = 'Global Grab VS Grab',
      color = '#bababa'
      # Highlight the selecte sites
    ) %>% highlightDataSubset(
      color = '#e24727',
      data = data(),
      x = currentParamX()$param_name,
      y = currentParamY()$param_name,
      Site_ID %in% input$sites
    )
  })
  
  
  
  
  ## Plot hovering logic ##########################################################
  
  # Activate the hover widget for the regular timeserie plot
  pointHoverWidgetServer(session, 'grabVsGrab', data, reactive(input$grabVsGrab_hover))
  
  
  
  
  ## Parameter description modal logic ############################################
  
  # X parameter
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelperX, {
    # Render the description UI in the modal
    output$description <- renderUI(tags$p(
      class = 'description',
      currentParamX() %>% pull(description)
    ))
    
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Parameter description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  # Y parameter
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelperY, {
    # Render the description UI in the modal
    output$description <- renderUI(tags$p(
      class = 'description',
      currentParamY() %>% pull(description)
    ))
    
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Parameter description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}

