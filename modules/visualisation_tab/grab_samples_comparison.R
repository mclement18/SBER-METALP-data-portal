## This module contains the code for the grab samples data comparison visualisation

## Create the UI function of the module ###############################################

grabSamplesComparisonUI <- function(id, pool) {
# Create the UI for the grabSamplesComparison module
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
    getRows(pool, 'grab_params_plotting', columns = c('order', 'section_name', 'option_name', 'param_name')) %>%
      arrange(order) %>% select(-order),
    'param_name'
  )
  
  # Create the UI list to be returned
  list(
    # Create the UI inputs
    'inputs' = div(
      # Set UI inputs id and class
      id = paste0('grab-vs-grab-plot-input-', id),
      class = 'time-serie-input',
      # Create select input for catchment selection
      selectInput(
        ns('site'),
        'Station',
        parseOptionsWithSections(
          getRows(pool, 'stations', columns = c('order', 'name', 'full_name', 'catchment')) %>%
            arrange(order) %>% select(-order),
          valueColumn = 'name',
          sectionColumn = 'catchment',
          optionColumn = 'full_name'
        )
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
      # Create an hidden radio buttons input group (with shinyjs) for sub parameter selection
      # Will be displayed only if there is a sub parameter selection available
      hidden(
        radioButtons(
          ns('paramfilterX'),
          'Subparameter X-axis',
          choices = 'NULL'
        )
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
      ),
      # Create an hidden radio buttons input group (with shinyjs) for sub parameter selection
      # Will be displayed only if there is a sub parameter selection available
      hidden(
        radioButtons(
          ns('paramfilterY'),
          'Subparameter Y-axis',
          choices = 'NULL'
        )
      )
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = paste0('grab-vs-grab-plots-', id),
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

grabSamplesComparison <- function(input, output, session, dateRange, pool) {
# Create the logic for the grabSamplesComparison module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  ## Selected station logic #######################################################
  
  # Create a reactive expression that return the current site info
  currentSite <- reactive(getRows(
    pool,
    'stations',
    name == local(input$site),
    columns = c('order', 'full_name', 'color')
  ) %>% arrange(order) %>% select(-order))
  
  
  
  
  ## Parameter logic ###################################################
  
  # Create a reactive expression that return the parameter X and Y infos
  currentParamX <- reactive(getRows(
    pool, 'grab_params_plotting',
    param_name == local(input$paramX),
    columns = c('order', 'param_name', 'units', 'data', 'sd', 'min_max', 'plot_func', 'description')
  ) %>% arrange(order) %>% select(-order))
  
  currentParamY <- reactive(getRows(
    pool, 'grab_params_plotting',
    param_name == local(input$paramY),
    columns = c('order', 'param_name', 'units', 'data', 'sd', 'min_max', 'plot_func', 'description')
  ) %>% arrange(order) %>% select(-order))
  
  
  
  
  ## Sub parameter update logic ###################################################
  
  # X parameter
  # Create an observeEvent that react to the param select input
  observeEvent(input$paramX,{
    # Get parameter info
    dataColumns <- currentParamX() %>% select(data) %>% str_split(',') %>% unlist()
    
    # Update sub parameter radio buttons group input with current parameter info
    updateRadioButtons(session, 'paramfilterX',
                             choices = dataColumns,
                             selected = dataColumns[1])
    
    # Toggle sub parameter radio buttons group input visibility
    # In function of the number of sub parameters
    # length(dataColumns) > 1 == TRUE -> show else hide
    toggleElement('paramfilterX', condition = length(dataColumns) > 1)
  })
  
  
  # Y parameter
  # Create an observeEvent that react to the param select input
  observeEvent(input$paramY,{
    # Get parameter info
    dataColumns <- currentParamY() %>% select(data) %>% str_split(',') %>% unlist()
    
    # Update sub parameter radio buttons group input with current parameter info
    updateRadioButtons(session, 'paramfilterY',
                       choices = dataColumns,
                       selected = dataColumns[1])
    
    # Toggle sub parameter radio buttons group input visibility
    # In function of the number of sub parameters
    # length(dataColumns) > 1 == TRUE -> show else hide
    toggleElement('paramfilterY', condition = length(dataColumns) > 1)
  })
  
  
  
  # Create a reactive expression returning the parameter info as a named list:
  #  - filter(X|Y): the sub parameters to display
  #  - param(X|Y): the parameter info
  paramfilter <- reactive({
    # Save the parameter and sub parameters into variables
    # Isolate the parameters to recompute only when sub parameters are updated
    inputParamX <- isolate(input$paramX)
    paramToDisplayX <- input$paramfilterX
    inputParamY <- isolate(input$paramY)
    paramToDisplayY <- input$paramfilterY
    
    # If either paramToDisplay(X|Y) or inputParam(X|Y) is NULL
    # Return a list of NULL values
    if (is.null(paramToDisplayX) | is.null(inputParamX) | is.null(paramToDisplayY) | is.null(inputParamY)) return(list(
      'filterX' = NULL,
      'paramX' = NULL,
      'filterY' = NULL,
      'paramY' = NULL
    ))
    
    # Return the sub parameters and parameter info
    return(list(
      'filterX' = paramToDisplayX,
      'paramX' = isolate(currentParamX()),
      'filterY' = paramToDisplayY,
      'paramY' = isolate(currentParamY())
    ))
  })
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, sites and paramfilter reactive expressions
  data <- reactive({
    # Save the sub parameters to display
    paramColsX <- paramfilter()$filterX
    paramColsY <- paramfilter()$filterY
    
    # If there is no sub parameter return NULL
    if (is.null(paramColsX) | is.null(paramColsY)) return(NULL)
    
    
    # Filter the data using the selected sites and the date range
    # And select the columns
    df <- getRows(
      pool = pool,
      table = 'data',
      station == local(input$site),
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

    # Isolate paramfilter to rerender only when data is ready
    paramfilter <- isolate(paramfilter())
    
    # Get current site name and color
    site <- currentSite()
    currentSiteName <- site %>% pull(full_name)
    currentSiteColor <- site %>% pull(color)

    # Create and return a onVsOne plot
    onVsOnePlot(
      df = data(),
      x = paramfilter$filterX,
      y = paramfilter$filterY,
      parameterX = paramfilter$paramX,
      parameterY  = paramfilter$paramY,
      plotTitle = paste(currentSiteName, 'Grab VS Grab'),
      color = currentSiteColor
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

