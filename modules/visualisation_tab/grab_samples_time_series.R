# This module contains the UI and server code for the Grab samples time series visualisation

## Load parameters and sites information ##########################################

# Use fread() from data.table library because of the long description text that lead EOF error with read.csv
# Beware it produces a data.table and not a data.frame which similar but has some differences
# Convertible to a data.frame with as.data.frame()

parameters <- fread('./data/parameters_grab_samples.csv', header = TRUE, sep = ',')
sites <- fread('./data/sites.csv', header = TRUE, sep = ',')

## Create lists containing select input element options ########################################################

## Function to parse options for select input with section

## Takes in a data.frame or data.table with a 'section_name' and an 'option_name' column
## Containing the dropdown and option text, respectively
## And a third column containing the option value of the name of your choice that you need to pass as second parameter
## It returns a named list of named lists to be used as choices parameter for shiny selectInput()
parseOptionsWithSections <- function(paramOptions, valueColumn) {
  
  optionsList <- list()
  
  ## For each row in the data
  ## Add a list to optionsList if the corresponding section_name list is not already created
  ## Add an option to the corresponding section_name list
  for (i in c(1:dim(paramOptions)[1])) {
    currentRow <- as.data.frame(paramOptions[i,])
    
    if (optionsList[[currentRow$section_name]] %>% is.null()) {
      optionsList[[currentRow$section_name]] <- list()
    }
    
    optionsList[[currentRow$section_name]][[currentRow$option_name]] <- currentRow[[valueColumn]]
  }
  
  return(optionsList)
}

## Function that create a simple options list for select input
parseOptions <- function(optionsTable, optionsColumn) {
  return(
    optionsTable[[optionsColumn]] %>% unique()
  )
}


## Create the two optionsLists needed
paramOptions <- parseOptionsWithSections(parameters, 'param_name')

catchmentsOptions <- parseOptions(sites, 'catchments')

## Create module UI function ######################################################

grabSamplesTimeSeriesUI <- function(id, minDate, maxDate) {
  ns <- NS(id)

  timeSeriesPlottingUIList <- timeSeriesPlottingUI(
    ns('1'),
    catchmentsOptions = catchmentsOptions,
    paramOptions = paramOptions
  )
  
  div(
    div(
      class = 'main-inputs',
      dateRangeInput(ns('time'), 'Date range:',
                     start = minDate,
                     end = maxDate,
                     min = minDate,
                     max = maxDate,
                     format = 'dd/mm/yyyy',
                     separator = '-'),
      div(
        class = 'btn-group',
        actionButton(ns('toggleSidebar'), 'Hide inputs', class = 'custom-style'),
        actionButton(ns('addUnit'), 'Add Unit', class = 'custom-style'),
        disabled(
          actionButton(ns('removeUnit'), 'Remove Unit', class = 'custom-style')
        )
      )
    ),
    sidebarLayout(
      sidebarPanel(
        id = 'time-series-inputs',
        timeSeriesPlottingUIList$inputs,
        width = 3
      ),
      mainPanel(
        id = 'time-series-plots',
        timeSeriesPlottingUIList$plots,
        width = 9
      )
    )
  )
}

## Create module server function ##################################################

grabSamplesTimeSeries <- function(input, output, session, grabSampleDf) {
  dateRange <- reactiveValues()
  updateDateRange <- reactiveValues()
  
  observeEvent(input$time, {
    dateRange$min <- input$time[1]
    dateRange$max <- input$time[2]
  })
  
  catchmentsNb <- reactiveVal(1)
  
  updateDateRange <- callModule(timeSeriesPlotting, '1', grabSampleDf, dateRange)
  
  observeEvent(updateDateRange$update, {
    updateDateRangeInput(session, 'time', start = updateDateRange$min, end = updateDateRange$max)
  })
  
  observeEvent(input$addUnit, {
    if (catchmentsNb() == 1) enable('removeUnit')
    catchmentsNb(catchmentsNb() + 1)
    
    timeSeriesPlottingUIList <- timeSeriesPlottingUI(
      session$ns(catchmentsNb()),
      catchmentsOptions = catchmentsOptions,
      paramOptions = paramOptions
    )
    
    insertUI(
      '#time-series-inputs', where = 'beforeEnd',
      ui = timeSeriesPlottingUIList$inputs,
      immediate = TRUE
    )
    
    insertUI(
      '#time-series-plots', where = 'beforeEnd',
      ui = timeSeriesPlottingUIList$plots,
      immediate = TRUE
    )
    
    updateDateRange <- callModule(timeSeriesPlotting, catchmentsNb(), grabSampleDf, dateRange)
    
    observeEvent(updateDateRange$update, {
      updateDateRangeInput(session, 'time', start = updateDateRange$min, end = updateDateRange$max)
    })
  })
  
  observeEvent(input$removeUnit, {
    inputsId <- str_interp('#time-serie-plot-input-${session$ns(catchmentsNb())}')
    plotsId <- str_interp('#time-serie-plots-${session$ns(catchmentsNb())}')
    
    removeUI(
      plotsId,
      immediate = TRUE
    )
    
    removeUI(
      inputsId,
      immediate = TRUE
    )
    
    catchmentsNb(catchmentsNb() - 1)
    
    if (catchmentsNb() == 1) disable('removeUnit')
  })
  
  # Sidebar inputs toggle logic ###################################################
  # Create a boolean rective value that keep track of the sidebar vsibility state
  sidebarVisible <- reactiveVal(TRUE)
  
  # Call sideBarToggleServer that contains server logic to toggle sidebar visibility
  sideBarToggleServer(session, reactive(input$toggleSidebar), sidebarVisible,
                      'time-series-inputs', 'time-series-plots', 'toggleSidebar',
                      'Show inputs', 'Hide inputs')
}
