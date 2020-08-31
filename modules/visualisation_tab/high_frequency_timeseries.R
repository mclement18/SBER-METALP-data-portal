## This module contains the code for the high frequency data timeserie visualisation

## Create the UI function of the module ###############################################

highFreqTimeSeriesUI <- function(id, sites, parameters) {
# Create the UI for the highFreqTimeSeries module
# Parameters:
#  - id: String, the module id
#  - sites: Named list, contains all sites info, cf data_preprocessing.R
#  - parameters: Named list, contains high frequency data parameters info, cf data_preprocessing.R
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
      id = str_interp('hf-time-serie-plot-input-${id}'),
      class = 'time-serie-input',
      # Create select input for catchment selection
      checkboxGroupInputWithClass(
        checkboxGroupInput(
          ns('sites'),
          str_interp('Stations ${unitNb}'),
          choices = sites$sitesOptions,
          selected = sites$sitesOptions[[1]]
        ),
        class = 'checkbox-grid'        
      ),
      selectInput(
        ns('param'),
        # Create a label with an icon button
        tags$span(
          'Parameter',
          # Create an icon button that trigger a modal to display the parameter description
          actionButton(ns('paramHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        parameters$selectOptions
      ),
      # Create a checkbox to select or unselect modeled data, hidden by default
      hidden(
        checkboxInput(ns('showModeledData'), 'Show modeled data', value = TRUE)
      ),
      # Create radio buttons to select the data frequency to display
      checkboxGroupInputWithClass(
        radioButtons(
          ns('dataFreq'),
          'Data frequency',
          choices = list('10min (raw)' = '10min', '6H', '12H', '24H'),
          selected = '24H'
        ),
        class = 'checkbox-grid'        
      )
    ),
    # Create the UI plots
    'plots' = div(
      # Set UI plots id and class
      id = str_interp('hf-time-serie-plots-${id}'),
      class = 'time-serie-plot point-hover-widget-plot',
      # Create a plotOutput for the regular timeserie plot
      spinnerPlotOutput(
        ns('highfreq'),
        # Make data points hoverable
        hover = hoverOpts(ns('highfreq_hover')),
        # Make plot brushable in the x direction with a debouncing delay type
        # Reset it when the plot is refreshed
        brush = brushOpts(
          ns('highfreq_brush'),
          direction = 'x',
          delayType = 'debounce',
          resetOnNew = TRUE
        ),
        # Make plot double clickable
        dblclick = dblclickOpts(ns('highfreq_dblclick'))
      )
    )
  )
}



## Create the server function of the module ###############################################

highFreqTimeSeries <- function(input, output, session, df, dateRange, sites, parameters) {
# Create the logic for the highFreqTimeSeries module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - df: Named List of Data.frame, the sensors high frequency data at different frequency
#  - dateRange: Reactive expression that returns the date range to filter the data with.
#               Date range format must be a list containing:
#               + min: Date, the lower bound to filter the date
#               + max: Date, the upper bound to filter the data
#  - sites: Named list, contains all sites info, cf data_preprocessing.R
#  - parameters: Named list, contains high frequency data parameters info, cf data_preprocessing.R
# 
# Returns a reactive expression containing the updated date range with the same format as the input
  
  ## Stations update logic ########################################################
  
  # Create a reactive expression returning the selected sites
  selectedSites <- reactive({input$sites})
  
  # Create a debounced reactive expression returning the selected sites
  selectedSites_d <-  selectedSites %>% debounce(1000)
  
  
  
  ## Parameter logic ##############################################################
  
  # Create a reactive expression that returns the filtered parameters df
  param <- reactive({
    parameters$parameters %>% filter(param_name == input$param)
  })
  
  
  
  
  ## Modeled data selection logic #################################################
  
  # Create observeEvent that react to frequence update
  # Display showModeledData checkbox if the selected data frequence is 10min
  observeEvent(input$dataFreq, ignoreInit = TRUE, {
    toggleElement('showModeledData', condition = input$dataFreq == '10min')
  })
  
  
  
  
  ## Data manipulation logic ######################################################
  
  # Create a data reactive expression that return a subset of the data
  # Using the dateRange, selectedSites_d and param reactive expressions
  data <- reactive({
    # Select df
    df <- df[[input$dataFreq]]
    
    # If the raw data is selected filter also for modeled data
    if (input$dataFreq == '10min') {
      # Define data types to keep depending on the state of showModeledData
      types <- c('measured')
      if (input$showModeledData) types <- c(types, 'modeled')
      
      # Filter the data using the selected sites, the data type and the date range
      # Then select the parameter and rename the column to 'value'
      df %<>% filter(
        Site_ID %in% selectedSites_d(),
        data_type %in% types,
        date(date) >= dateRange()$min,
        date(date) <= dateRange()$max
      ) %>% select(date, Site_ID, data_type, 'value' = param()$data)
    } else {
      # Filter the data using the selected sites and the date range
      # Then select the parameter and rename the column to 'value'
      df %<>% filter(
        Site_ID %in% selectedSites_d(),
        date(date) >= dateRange()$min,
        date(date) <= dateRange()$max
      ) %>% select(date, Site_ID, 'value' = param()$data)
    }
    
    # If there is no data return NULL
    if (dim(df)[1] == 0) return(NULL)
    
    # Return the formatted data
    df
  })
  
  
  ## Plots output logic ###########################################################
  
  # Render the regular timeserie plot
  output$highfreq <- renderPlot({
    # If there are no data return NULL
    if (data() %>% is.null()) return(NULL)
    
    # Get unitNb
    splittedId <- str_split(session$ns('0'), '-') %>% unlist()
    unitNb <- splittedId[length(splittedId) - 1]
    
    # Create and return a highFreqTimeSeriePlot
    highFreqTimeSeriePlot(
      df = data(),
      parameter = param(),
      plotTitle = str_interp('Sensors High Frequency Time Serie ${unitNb}'),
      sites = sites$sites,
      modeledData = 'data_type' %in% colnames(data())
    )
  })
  
  
  
  
  ## Plot hovering logic ##########################################################
  
  # Activate the hover widget for the regular timeserie plot
  pointHoverWidgetServer(session, 'highfreq', data, reactive(input$highfreq_hover),
                         x_label = 'Date', y_label = 'Parameter')

  
  
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the parameter helper icon button
  observeEvent(input$paramHelper, {
    # Render the description UI in the modal
    output$description <- renderUI(tags$p(
      class = 'description',
      param()$description
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
    'min' = as.Date(as.POSIXct(input$highfreq_brush$xmin, origin = "1970-01-01", tz = "GMT")),
    'max' = as.Date(as.POSIXct(input$highfreq_brush$xmax, origin = "1970-01-01", tz = "GMT"))
  ))
  
  # Create a reactive value that update each time the plot is double clicked
  # Used as trigger to reset the date range in the outer module
  # Initialised to NULL to avoid a dateRange reset when a new unit is created
  resetDateRange <- reactiveVal(NULL)
  
  # Create an observe event that react on plot double click to reset the date range
  observeEvent(input$highfreq_dblclick, {
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

