## This module contains the UI and server code for the Main Download

## Create module UI ###############################################################

mainDownloadUI <- function(id, pool) {
# Create the UI for the mainDownload module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a list containing the UI elements
  
  # Create namespace
  ns <- NS(id)
  
  # Create the list containing the UI elements
  list(
    # Global inputs for layout
    globalInputs = tagList(
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
    ),
    # Specific inputs for layout
    specificInputs = tagList(
      # Hide by default high frequency data inputs
      hidden(
        # High frequency data specific inputs
        div(
          id = 'download-hf-inputs',
          checkboxGroupInputWithClass(
            radioButtons(
              inputId = ns('hfDataFreq'),
              label = tags$span(
                'Data frequency',
                # Create an icon button that trigger a modal to display the parameter description
                actionButton(ns('hfFreqHelper'), icon('question-circle'), class = 'icon-btn')
              ),
              choices = list('10min (raw)' = '10min', '6H', '12H', '24H'),
              selected = '10min'
            ),
            class = 'checkbox-grid'        
          ),
          # Select for modeled data
          checkboxInput(
            ns('addModeledData'),
            # Create a label with an icon button
            tags$span(
              'Add modeled data',
              # Create an icon button that trigger a modal to display the modeled data description
              actionButton(ns('modeledHelper'), icon('question-circle'), class = 'icon-btn')
            ),
            value = FALSE),
          # Select HF parameters
          selectizeInput(
            inputId =  ns('hfParam'),
            # Create a label with an icon button
            label = tags$span(
              'Parameter',
              # Create an icon button that trigger a modal to display the parameter description
              actionButton(ns('hfParamHelper'), icon('question-circle'), class = 'icon-btn')
            ),
            choices = parseOptionsWithSections(
              getRows(pool, 'sensor_params_plotting', columns = c('section_name', 'option_name', 'param_name')),
              'param_name'
            ),
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
          # End grab-inputs
        )
        # End hidden object
      )
      # End of tagList
    )
  )
}



## Create module server function ##################################################

mainDownload <- function(input, output, session, pool, user, hfDf, selectedSites, dateRange, clear) {
# Create the logic for the mainDownload module
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
  
  ## Reactive values to return UI elements ########################################
  
  # Initiate them with an empty expression to render an empty UI
  dlButton <- reactiveVal({})
  disclaimer <- reactiveVal({})
  
  
  
  
  ## Download and request data logic according to authorization ###################
  
  # Look for user update
  observeEvent(user$role, {
    # Get the correct button depending on the user role
    # Call the downloadData or requestData module
    if (user$role == 'visitor') {
      ui <- requestDataUI(session$ns('request'))
      dlButton(ui$button)
      disclaimer(ui$disclaimer)
      
      # Create a reactive expression returning a list of the data selection inputs
      dataSelectionInput <- reactive({
        # Define the selected data
        if (input$data == 'hfDf') {
          data <- 'sensor'
        } else {
          data <- 'grab samples'
        }
        
        # Create the returned list with all the inputs info
        list(
          'min' = dateRange()[1],
          'max' = dateRange()[2],
          'data' = data,
          'dataFreq' = input$hfDataFreq,
          'modeled' = input$addModeledData,
          'sites' = selectedSites(),
          'parameters' = parameters()
        )
      })
      
      # Call the module
      callModule(requestData, 'request', pool, selectedData, dataSelectionInput)
    } else {
      dlButton(downloadDataUI(session$ns('download')))
      disclaimer({})
      callModule(downloadData, 'download', selectedData)
    }
  })
  
  
  
  
  
  
  
  
  ## Data specific inputs display logic ###########################################
  
  # Create an observeEvent that react to data selection changes
  # Show and hide correct specific inputs depending on the selected data
  observeEvent(input$data, {
    df <- input$data
    toggleElement(selector = '#download-hf-inputs', condition = df == 'hfDf')
    toggleElement(selector = '#download-grab-inputs', condition = df == 'grabDf')
  })
  
  
  
  ## Modeled data selection logic #################################################
  
  # Create observeEvent that react to frequence update
  # Display addModeledData checkbox if the selected data frequence is 10min
  observeEvent(input$hfDataFreq, ignoreInit = TRUE, {
    toggleElement('addModeledData', condition = input$hfDataFreq == '10min')
  })
  
  
  
  
  ## Multi selection inputs debouncing ############################################
  
  # Debounce HF parameters selection
  hfParamReactive <- reactive(input$hfParam)
  hfParamReactive_d <- debounce(hfParamReactive, 1000)
  
  # Debounce Grab parameters selection
  grabParamReactive <- reactive(input$grabParam)
  grabParamReactive_d <- debounce(grabParamReactive, 1000)
  
  
  
  
  
  ## Parameters selection logic ###################################################
  
  # Create a reactive expression that retrieve the selected parameters
  parameters <- reactive({
    inputDf <- input$data
    if (inputDf == 'hfDf') {
      getRows(
        pool, 'sensor_params_plotting',
        param_name %in% local(hfParamReactive_d()),
        columns = 'data'
      ) %>% pull()
    } else if (inputDf == 'grabDf') {
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
  
  
  
  
  
  ## Data filtering logic #########################################################
  
  # Create a reactive expression returning the selected data
  selectedData <- reactive({
    # Get df input
    inputDf <- input$data
    
    # If th hf data is selected
    if (inputDf == 'hfDf') {
      df <- hfDf[[input$hfDataFreq]] %>%
        # Filter rows
        filter(
          date(Date) >= dateRange()[1],
          date(Date) <= dateRange()[2],
          Site_ID %in% selectedSites()
        )
      
      # If the selected data is the raw HF data
      if (input$hfDataFreq == '10min') {
        # If add modeled data is selected
        if (input$addModeledData) {
          # Create a new df with the date and sites columns
          newDf <- df %>% select(Date, Site_ID)
          
          # For each parameter create a combined column
          for (parameter in parameters()) {
            # Select the parameter columns
            tmpDf <- df %>% select(starts_with(parameter), -ends_with('singlePoint'))
            # Create combined column name
            newcolName <- paste0(parameter, '_combined')
            # Create the combined column
            tmpDf %<>% mutate(!!newcolName := rowSums(tmpDf, na.rm=TRUE) * NA ^ !rowSums(!is.na(tmpDf)))
            # Add parameter columns to the new df
            newDf <- bind_cols(newDf, tmpDf)
          }
          
          # Assigne the new df to the df
          df <- newDf
          
          # Remove the tmpDf and newDf
          rm(tmpDf, newDf, tmpSPCol)
        } else {
          # If modeled data is not selected
          # Keep only the measured value and rename the columns
          df %<>% select(Date, Site_ID, starts_with(parameters()), -ends_with(c('modeled', 'singlePoint')))
        }
      } else {
        # For all the other HF data
        # Select date, stations and all parameters
        df %<>% select(Date, Site_ID, all_of(parameters()))
      }
      
      # If the grab data is selected
    } else if (inputDf == 'grabDf') {
      # Get the data
      df <- getRows(
        pool = pool,
        table = 'data',
        station %in% local(selectedSites()),
        DATE_reading >= local(dateRange()[1]),
        DATE_reading <= local(dateRange()[2]),
        columns = c(
          'station', 'DATE_reading', 'TIME_reading_GMT',
          local(parameters())
        )
        # Parse the DATE and time
      ) %>% mutate(
        station = as.factor(station),
        Date = ymd_hms(paste(DATE_reading, TIME_reading_GMT), tz = 'GMT')
        # Rename for the download
      ) %>% rename(
        Site_ID = station
        # Reorder columns and filter columns
      ) %>% select(Date, Site_ID, all_of(parameters()))
    } else {
      # If no data is selected set df as an empty data.table
      df <- data.table()
    }
    
    # Convert df to data.table for print output
    as.data.table(df)
  })
  
  
  
  
  
  
  
  ## Parameter description modal logic ############################################
  
  # Create an observeEvent that react to the HF parameter helper icon button
  observeEvent(input$hfParamHelper | input$grabParamHelper, ignoreInit = TRUE, {
    # Select the correct parameters df
    if (input$data == 'hfDf') {
      parameters <- getRows(pool, 'sensor_params_plotting', columns = c('option_name', 'description'))
    } else if (input$data == 'grabDf') {
      parameters <- getRows(pool, 'grab_params_plotting', columns = c('option_name', 'description'))
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
  
  
  
  
  ## Data helpers logic ####################################################
  
  # Create an observeEvent that react to the data freq helper button
  observeEvent(input$hfFreqHelper, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = 'Sensor data frequency selection',
      htmlTemplate('./html_components/data_freq_help.html', icon = icon('exclamation-triangle')),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  # Create an observeEvent that react to modeled data helper button
  observeEvent(input$modeledHelper, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = 'Modeled data',
      htmlTemplate('./html_components/data_modeled_help.html'),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  
  
  
  
  
  ## Clear form logic #############################################################
  
  # Create a reactive expression that clear inputs
  clearInputs <- reactive({
    # Call clear inputs to rerun the reactive expression
    clear()
    
    # Clear data selection
    updateSelectInput(session, 'data', selected = '')
    
    # HF specific clearing
    
    # Reset data frequency and modeled data selection
    updateRadioButtons(session, 'hfDataFreq', selected = '10min')
    updateCheckboxInput(session, 'addModeledData', value = FALSE)
    
    # Clear parameters selection
    updateSelectizeInput(session, 'hfParam', selected = '')
    
    # Grab specific clearing
    
    # Clear parameters selection
    updateSelectizeInput(session, 'grabParam', selected = '')
  })
  
  
  
  
  
  
  ## Output returning logic #######################################################
  
  # Returns a list of output to the download layout module
  return(
    list(
      selectedData = selectedData,
      disclaimer = disclaimer,
      button = dlButton,
      clearInputs = clearInputs
    )
  )
}
