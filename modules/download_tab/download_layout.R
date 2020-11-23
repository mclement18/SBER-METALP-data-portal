## This module contains the UI and server code for the Download layout

## Create module UI ###############################################################

downloadLayoutUI <- function(id, pool, minDate, maxDate, innerModuleUI) {
# Create the UI for the downloadLayout module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - minDate, maxDate: Dates, the min and max dates of both the hfDf and grab data
#  - innerModuleUI: Function, the UI function for the inner module
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Get innerModule UI elements
  innerModuleUIElements <- innerModuleUI(ns('innerDl'), pool)
  
  # Create main download tab element
  div(
    class = 'download-main',
    # Create download inputs element
    div(
      class = 'download__inputs',
      # Create download global inputs element
      div(
        class = 'download__global-inputs',
        # Date selection
        div(
          class = 'download__date-range',
          # Create data range
          dateRangeInput(
            ns('time'), 'Date range:',
            start = minDate,
            end = maxDate,
            min = minDate,
            max = maxDate,
            format = 'dd/mm/yyyy',
            separator = '-'
          ),
          # Create a nutton to reset the date range
          actionButton(ns('resetDateRange'), 'Reset Date', class = 'custom-style')
        ),
        # Site selection
        selectizeInput(
          inputId =  ns('sites'),
          label = 'Stations',
          choices = parseOptionsWithSections(
            getRows(pool, 'stations', columns = c('name', 'full_name', 'catchment')),
            valueColumn = 'name',
            sectionColumn = 'catchment',
            optionColumn = 'full_name'
          ),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          ),
        ),
        # innerModule global inputs
        innerModuleUIElements$globalInputs
      ),
      # Create download data specific inputs
      div(
        class = 'download__specific-inputs',
        # innerModule specific inputs
        innerModuleUIElements$specificInputs
      )
    ),
    # Create the data preview table output
    div(
      class = 'download__data-preview',
      # Create a text output with a spinner
      withSpinner(verbatimTextOutput(ns('preview')), type = 4, color = "#e24727", size = .5)
    ),
    # Add disclaimer if present
    div(
      class = 'download__data-privacy',
      htmlOutput(ns('disclaimer'))
    ),
    # Create the download actions
    div(
      class = 'download__actions',
      # Add the download or request button
      htmlOutput(ns('button')),
      # Clear form button
      actionButton(ns('clear'), 'Clear', class = 'custom-style')
    )
  )
}



## Create module server function ##################################################

downloadLayout <- function(input, output, session, pool, user, hfDf, innerModule) {
# Create the logic for the downloadLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - user: Reactive values, the current user
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - innerModule: Function, the inner module server function
# 
# Returns NULL
  
  ## Multi selection inputs debouncing ############################################
  
  # Debouce sites select input
  sitesReactive <- reactive(input$sites)
  sitesReactive_d <- debounce(sitesReactive, 1000)
  
  
  
  
  ## Date resetting logic #########################################################
  
  # Get min and max dates
  grabMinMaxDates <- getMinMaxValues(pool, 'data', DATE_reading) %>%
    mutate(across(everything(), ymd))
  
  minDate <- min(grabMinMaxDates$min, date(hfDf$`10min`$Date), na.rm = TRUE)
  maxDate <- max(grabMinMaxDates$max, date(hfDf$`10min`$Date), na.rm = TRUE)
  
  # Create an observeEvent that allows to reset the date range when resetDateRange is clicked
  observeEvent(input$resetDateRange, ignoreInit = TRUE, {
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
  })
  
  
  
  
  
  ## Create rective expression for layout inputs ##################################
  
  dateRange <- reactive(input$time)
  
  clearButton <- reactive(input$clear)
  
  
  
  
  
  ## Call the submodule ###########################################################
  
  innerModuleOutput <- callModule(innerModule, 'innerDl',
                                  pool, user, hfDf,
                                  sitesReactive_d, dateRange, clearButton)
  
  

  
  ## Output rendering #####################################################
  
  # Preview output
  output$preview <- renderPrint({
    # Get inner module selected data
    selectedData <- innerModuleOutput$selectedData()
    
    # Get selected data column names
    columnsNames <- colnames(selectedData)
    
    # If date is present, display the min and max dates
    # Else display NAs
    if ('Date' %in% columnsNames) {
      dateSummary <- selectedData %>% summarise_if(is.POSIXct, list(
        'Min' = min,
        'Max' = max
      ), na.rm = TRUE) %>% pivot_longer(everything(), names_to = 'Stat', values_to = 'Date') %>% 
        as.data.table()
    } else {
      dateSummary <- data.table('Stats' = c('Min', 'Max'), 'Date' = c(NA, NA))
    }
    
    # If Site_ID is present, display the selected stations and there number of observation
    # Else display NAs
    if ('Site_ID' %in% columnsNames) {
      sitesSummary <- selectedData %>% pull(Site_ID) %>% fct_count() %>%
        rename(Station = 'f', N = 'n') %>% filter(N != 0) %>% as.data.table()
    } else {
      sitesSummary <- data.table('Station' = c(NA), 'N' = c(NA))
    }
    
    
    # If user as the right to see
    if (user$role != 'visitor') {
      # If there is at least one parameter selected
      # Summarise each parameter
      # Else display only Stat column
      if (length(parameters()) > 0) {
        parametersSummary <- selectedData %>% summarise_if(is.numeric, list(
          'Min' = ~ min(.x, na.rm = TRUE),
          'Mean' = ~ mean(.x, na.rm = TRUE),
          'Max' = ~ max(.x, na.rm = TRUE),
          'NAs' = ~ sum(is.na(.x))
        ))
        
        # If there is only one parameter and three or four columns set manually the summary columns names
        # Else get them programmatically
        if (length(parameters()) == 1 & length(columnsNames) %in% c(3, 4)) {
          parametersSummary %<>% pivot_longer(everything(), names_to = 'Stat', values_to = columnsNames[3])
        } else {
          parametersSummary %<>% pivot_longer(everything(), names_to = c('.value', 'Stat'), names_pattern = '(.*)_(.*)')
        }
        
        # Convert summary to data.table
        parametersSummary %<>% as.data.table()
      } else {
        parametersSummary <- data.table('Stat' = c('Min', 'Mean', 'Max', 'NAs'))
      }
    }
    
    
    # Create print layout
    cat('# Data summary:', '\n\n')
    cat('## Date info', '\n\n')
    print(dateSummary)
    cat('\n')
    cat('## Stations info', '\n\n')
    print(sitesSummary)
    cat('\n')
    cat('## Parameters info', '\n\n')
    # If user as the right to see it
    if (user$role != 'visitor') {
      print(parametersSummary, scientific = FALSE, drop0trailing = TRUE)
      cat('\n\n')
      cat('---------------------------------------------------------------------------')
      cat('\n\n')
      cat('# Selected data preview:', '\n\n')
      print(selectedData, topn = 5, nrows = 15)
    } else {
      print(columnsNames)
    }
  })
  
  # Disclaimer output
  output$disclaimer <- renderUI(innerModuleOutput$disclaimer())
  
  # Download button
  output$button <- renderUI(innerModuleOutput$button())
  
  
  
  
  
  
  ## Clear form logic #############################################################
  
  # Create an observeEvent that react to clear button
  observeEvent(input$clear, ignoreInit = TRUE, {
    # Add modal spinner to block user interaction
    show_modal_spinner(spin = 'cube-grid', color = '#e24727',
                       text = 'Clearing form...')
    
    # Reset date range
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
    
    # Clear sites selection
    updateSelectizeInput(session, 'sites', selected = '')
    
    # Clear inner module inputs
    innerModuleOutput$clearInputs()
    
    # Remove modal when finished
    remove_modal_spinner()
  })
}
