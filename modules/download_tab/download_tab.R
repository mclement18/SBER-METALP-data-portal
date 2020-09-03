## This module contains the UI and server code for the Download tab

## Source needed files ############################################################
source('./modules/download_tab/download_data.R')
source('./modules/download_tab/request_data.R')


## Create module UI ###############################################################

downloadTabUI <- function(id, minDate, maxDate, sites, grabSampleParameters, hfParameters) {
# Create the UI for the downloadTab module
# Parameters:
#  - id: String, the module id
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - grabSampleDf: Data.frame, the grab samples data
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - sites: Named list of sites info, cf data_preprocessing.R
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
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
          choices = sites$sitesSelectOptions,
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          ),
        ),
        # Data selection
        selectInput(
          ns('data'),
          'Data',
          choices = list(
            'Choose data...' = '',
            'Sensors data' = 'hfDf',
            'Grab samples data' = 'grabDf'
          )
        )
        # End download__global-inputs
      ),
      # Create download data specific inputs
      div(
        class = 'download__specific-inputs',
        # Hide by default high frequency data inputs
        hidden(
          # High frequency data specific inputs
          div(
            id = 'download-hf-inputs',
            checkboxGroupInputWithClass(
              radioButtons(
                ns('hfDataFreq'),
                'Data frequency',
                choices = list('10min (raw)' = '10min', '6H', '12H', '24H'),
                selected = '10min'
              ),
              class = 'checkbox-grid'        
            ),
            # Select for modeled data
            checkboxInput(ns('addModeledData'), 'Add modeled data', value = FALSE),
            # Select for single points info
            checkboxInput(ns('addSinglePointInfo'), 'Add single point info', value = FALSE),
            # Select HF parameters
            selectizeInput(
              inputId =  ns('hfParam'),
              # Create a label with an icon button
              label = tags$span(
                'Parameter',
                # Create an icon button that trigger a modal to display the parameter description
                actionButton(ns('hfParamHelper'), icon('question-circle'), class = 'icon-btn')
              ),
              choices = hfParameters$selectOptions,
              multiple = TRUE,
              options = list(
                'placeholder' = 'Select some parameters...',
                'plugins' = list('remove_button')
              )
            )
            # End download__specific-inputs
          )
          # End hidden object
        ),
        # Hide by default the grab specific inputs
        hidden(
          # Create the grab specific inputs
          div(
            id = 'download-grab-inputs',
            # Grab parameter selection
            selectizeInput(
              inputId =  ns('grabParam'),
              # Create a label with an icon button
              label = tags$span(
                'Parameter',
                # Create an icon button that trigger a modal to display the parameter description
                actionButton(ns('grabParamHelper'), icon('question-circle'), class = 'icon-btn')
              ),
              choices = grabSampleParameters$selectOptions,
              multiple = TRUE,
              options = list(
                'placeholder' = 'Select some parameters...',
                'plugins' = list('remove_button')
              )
            )
            # End grab-inputs
          )
          # End hidden object
        )
        # End download__specific-inputs
      )
      # End download__inputs
    ),
    # Create the data preview table output
    div(
      class = 'download__data-preview',
      # Create a text output with a spinner
      withSpinner(verbatimTextOutput(ns('preview')), type = 4, color = "#e24727", size = .5)
    ),
    # Create the download actions
    div(
      class = 'download__actions',
      if (F) {
        downloadDataUI(ns('download'))
      } else {
        requestDataUI(ns('request'))
      },
      # Clear form button
      actionButton(ns('clear'), 'Clear', class = 'custom-style')
    )
  )
}



## Create module server function ##################################################

downloadTab <- function(input, output, session, grabSampleDf, hfDf, minDate, maxDate, sites, grabSampleParameters, hfParameters) {
# Create the logic for the downloadTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - grabSampleDf: Data.frame, the data of the grab samples
#                 (to pass to the grabSamplesTimeSeries, grabSamplesComparison and sensorsVsGrabSamplesComparison modules)
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - sites: Named list of sites info, cf data_preprocessing.R
#  - grabSampleParameters: Named list of grab samples parameters info, cf data_preprocessing.R
#  - hfParameters: Named list of high frequency parameters info, cf data_preprocessing.R
# 
# Returns NULL
  
  ## Data specific inputs display logic ###########################################
  
  # Create an observeEvent that react to data selection changes
  # Show and hide correct specific inputs depending on the selected data
  observeEvent(input$data, {
    df <- input$data
    toggleElement(selector = '#download-hf-inputs', condition = df == 'hfDf')
    toggleElement(selector = '#download-grab-inputs', condition = df == 'grabDf')
  }, ignoreInit = TRUE)
  
  
  
  
  ## Modeled data and single points info selection logic #################################################
  
  # Create observeEvent that react to frequence update
  # Display addModeledData checkbox if the selected data frequence is 10min
  observeEvent(input$hfDataFreq, ignoreInit = TRUE, {
    toggleElement('addModeledData', condition = input$hfDataFreq == '10min')
    toggleElement('addSinglePointInfo', condition = input$hfDataFreq == '10min')
  })
  
  
  
  
  ## Multi selection inputs debouncing ############################################
  
  # Debouce sites select input
  sitesReactive <- reactive(input$sites)
  sitesReactive_d <- debounce(sitesReactive, 1000)
  
  # Debounce HF parameters selection
  hfParamReactive <- reactive(input$hfParam)
  hfParamReactive_d <- debounce(hfParamReactive, 1000)
  
  # Debounce Grab parameters selection
  grabParamReactive <- reactive(input$grabParam)
  grabParamReactive_d <- debounce(grabParamReactive, 1000)
  
  
  
  
  
  ## Parameters selection logic ###################################################
  
  # Create a rective expression that retrive the selected parameters
  parameters <- reactive({
    inputDf <- input$data
    if (inputDf == 'hfDf') {
      hfParameters$parameters %>% filter(param_name %in% hfParamReactive_d()) %>% pull(data)
    } else if (inputDf == 'grabDf') {
      grabSampleParameters$parameters %>% filter(param_name %in% grabParamReactive_d()) %>% pull(data)
    }
  })
  
  
  
  
  
  ## Data filtering logic #########################################################
  
  # Create a reactive expression returning the selected data
  selectedData <- reactive({
    # Select df and correct parameter input
    inputDf <- input$data
    
    if (inputDf == 'hfDf') {
      df <- hfDf[[input$hfDataFreq]]
    } else if (inputDf == 'grabDf') {
      df <- grabSampleDf
      df %<>% rename(date = DATETIME_GMT)
    } else {
      return(data.table())
    }
    
    
    # Filter rows
    df %<>% filter(
      date(date) >= input$time[1],
      date(date) <= input$time[2],
      Site_ID %in% sitesReactive_d()
    )
    
    # If the selected data is the raw HF data
    if (inputDf == 'hfDf' & input$hfDataFreq == '10min') {
      # If add modeled data is selected
      if (input$addModeledData) {
        # Create a new df with the date and sites columns
        newDf <- df %>% select(date, Site_ID)
        
        # For each parameter create a combined column
        for (parameter in parameters()) {
          # Select the parameter columns
          tmpDf <- df %>% select(starts_with(parameter), -ends_with('singlePoint'))
          # Create combined column name
          newcolName <- str_interp('${parameter}_combined')
          # Create the combined column
          tmpDf %<>% mutate(!!newcolName := rowSums(tmpDf, na.rm=TRUE) * NA ^ !rowSums(!is.na(tmpDf)))
          # Add parameter columns to the new df
          newDf <- bind_cols(newDf, tmpDf)
          
          if(input$addSinglePointInfo) {
            tmpSPCol <- df %>% select(starts_with(parameter) & ends_with('singlePoint'))
            newDf <- bind_cols(newDf, tmpSPCol)
          }
        }
        
        # Assigne the new df to the df
        df <- newDf

        # Remove the tmpDf and newDf
        rm(tmpDf, newDf, tmpSPCol)
      } else {
        removeSPCol <- 'singlePoint'
        if (input$addSinglePointInfo) removeSPCol <- 'NULL'
        # Keep only the measured value and rename the columns
        df %<>% select(date, Site_ID, starts_with(parameters()), -ends_with(c('modeled', removeSPCol)))
      }
    } else {
      # For anything else than HF 10min data
      # Select date, stations and all parameters
      df %<>% select(date, Site_ID, all_of(parameters()))
    }
    
    # Convert df to data.table for print output
    df <- as.data.table(df)
    
    # Return the filtered df
    return(df)
  })
  
  
  
  
  ## Preview table rendering logic ################################################
  
  # Render the preview table and summary
  output$preview <- renderPrint({
    # Get selected data column names
    columnsNames <- colnames(selectedData())
    
    # If date is present, display the min and max dates
    # Else display NAs
    if ('date' %in% columnsNames) {
      dateSummary <- selectedData() %>% summarise_if(is.POSIXct, list(
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
      sitesSummary <- selectedData() %>% pull(Site_ID) %>% fct_count() %>%
        rename(Station = 'f', N = 'n') %>% filter(N != 0) %>% as.data.table()
    } else {
      sitesSummary <- data.table('Station' = c(NA), 'N' = c(NA))
    }
    
    # If there is at least one parameter selected
    # Summarise each parameter
    # Else display only Stat column
    if (length(parameters()) > 0) {
      parametersSummary <- selectedData() %>% summarise_if(is.numeric, list(
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
    
    if (any(grepl('singlePoint', columnsNames))) {
      singlePointsSummary <- selectedData() %>% select(ends_with('singlePoint')) %>%
        summarise(across(everything(), ~ sum(as.numeric(as.character(.x))))) %>% as.data.table()
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
    print(parametersSummary, scientific = FALSE, drop0trailing = TRUE)
    if (any(grepl('singlePoint', columnsNames))) {
      cat('\n')
      cat('## Single points info', '\n\n')
      print(singlePointsSummary)
    }
    cat('\n\n')
    cat('---------------------------------------------------------------------------')
    cat('\n\n')
    cat('# Selected data preview:', '\n\n')
    print(selectedData(), topn = 5, nrows = 15)
  })
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the HF parameter helper icon button
  observeEvent(input$hfParamHelper | input$grabParamHelper, ignoreInit = TRUE, {
    # Select the correct parameters df
    if (input$data == 'hfDf') {
      parameters <- hfParameters$parameters
    } else if (input$data == 'grabDf') {
      parameters <- grabSampleParameters$parameters
    }
    
    # Render the descriptions UI in the modal
    output$description <- renderUI({
      descriptions <- tagList()
      
      # For each selected parameter
      for (i in c(1:nrow(parameters))) {
        descriptions <- tagList(
          descriptions,
          # Add the parameter name ad its description
          div(
            class = 'description-group',
            h5(parameters %>% slice(i) %>% pull(option_name)),
            p(
              class = 'description',
              parameters %>% slice(i) %>% pull(description)
            )
          )
        )
      }
      
      # Return the descriptions
      return(descriptions)
    })
      
    
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Parameter description',
      htmlOutput(session$ns('description')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  ## Date resetting logic #########################################################
  
  # Create an observeEvent that allows to reset the date range when resetDateRange is clicked
  observeEvent(input$resetDateRange, ignoreInit = TRUE, {
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
  })
  
  
  
  
  ## Clear form logic #############################################################
  
  # Create an observeEvent that react to clear button
  observeEvent(input$clear, ignoreInit = TRUE, {
    # Add modal spinner to block user interaction
    show_modal_spinner(spin = 'cube-grid', color = '#e24727',
                       text = 'Clearing form...')
    
    # Clear data selection
    updateSelectInput(session, 'data', selected = '')
    
    # Reset date range
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
    
    # Clear sites selection
    updateSelectizeInput(session, 'sites', selected = '')
    
    # HF specific clearing
    
    # Reset data frequency and modeled data selection
    updateRadioButtons(session, 'hfDataFreq', selected = '10min')
    updateCheckboxInput(session, 'addModeledData', value = FALSE)
    
    # Clear parameters selection
    updateSelectizeInput(session, 'hfParam', selected = '')
    
    # Grab specific clearing
    
    # Clear parameters selection
    updateSelectizeInput(session, 'grabParam', selected = '')
    
    # Remove modal when finished
    remove_modal_spinner()
  })
  
  
  
  ## Download and request data logic ###########################################################
  
  if (F) {
    callModule(downloadData, 'download', selectedData)
  } else {
    
    dataSelectionInput <- reactive({
      if (input$data == 'hfDf') {
        data <- 'sensor'
      } else {
        data <- 'grab samples'
      }
      
      list(
        'min' = input$time[1],
        'max' = input$time[2],
        'data' = data,
        'dataFreq' = input$hfDataFreq,
        'modeled' = input$addModeledData,
        'singelPoint' = input$addSinglePointInfo,
        'sites' = sitesReactive_d(),
        'parameters' = parameters()
      )
    })
    
    callModule(requestData, 'request', selectedData, dataSelectionInput)
  }
  
}
