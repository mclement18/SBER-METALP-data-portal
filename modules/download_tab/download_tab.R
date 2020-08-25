## This module contains the UI and server code for the Download tab

## Source needed files ############################################################



## Create module UI ###############################################################

downloadTabUI <- function(id, minDate, maxDate, sites, grabSampleParameters, hfParameters) {
# Create the UI for the downloadTab module
# Parameters:
#  - id: String, the module id
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
        dateRangeInput(
          ns('time'), 'Date range:',
          start = minDate,
          end = maxDate,
          min = minDate,
          max = maxDate,
          format = 'dd/mm/yyyy',
          separator = '-'
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
      tableOutput(ns('preview'))
    ),
    # Create the download actions
    div(
      class = 'download__actions',
      # Download button
      actionButton(ns('download'), 'Download', class = 'custom-style custom-style--primary'),
      # Clear form button
      actionButton(ns('clear'), 'Clear', class = 'custom-style')
    )
  )
}



## Create module server function ##################################################

downloadTab <- function(input, output, session, grabSampleDf, hfDf, sites, grabSampleParameters, hfParameters) {
# Create the logic for the downloadTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - grabSampleDf: Data.frame, the data of the grab samples
#                 (to pass to the grabSamplesTimeSeries, grabSamplesComparison and sensorsVsGrabSamplesComparison modules)
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
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
}
