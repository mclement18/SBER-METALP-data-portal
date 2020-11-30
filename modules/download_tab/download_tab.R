## This module contains the UI and server code for the Download tab

## Source needed files ############################################################
source('./modules/download_tab/download_layout.R')
source('./modules/download_tab/main_download.R')
source('./modules/download_tab/sensor_vs_grab_download.R')
source('./modules/download_tab/download_data.R')
source('./modules/download_tab/request_data.R')



## Create module UI ###############################################################

downloadTabUI <- function(id, pool, hfDfMinMaxDates) {
# Create the UI for the downloadTab module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
#  - hfDfMinMaxDates: List, the min and max dates of the hfDf, format list(min = '', max = '')
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Get grab data min and max dates
  grabMinMaxDates <- getMinMaxValues(pool, 'data', DATE_reading) %>%
    mutate(across(everything(), ymd))
  
  # Get min and max dates
  minDate <- min(grabMinMaxDates$min, hfDfMinMaxDates$min)
  maxDate <- max(grabMinMaxDates$max, hfDfMinMaxDates$max)
  
  # Create tab panel for the different downloads
  tabsetPanel(
    id = ns('downloadTabs'),
    # Create the main download panel
    tabPanel(
      title = 'Main download',
      downloadLayoutUI(
        ns('mainDl'),
        pool = pool,
        minDate = minDate,
        maxDate = maxDate,
        innerModuleUI = mainDownloadUI
      ),
      value = ns('mainDl')
    )
  )
}



## Create module server function ##################################################

downloadTab <- function(input, output, session, pool, user, hfDf) {
# Create the logic for the downloadTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - user: Reactive values, the current user
#  - hfDf: Named List of Data.frame, the sensors high frequency data at different frequency
# 
# Returns NULL
  
  ## Call main download module ####################################################
  
  callModule(downloadLayout, 'mainDl', pool, user, hfDf, mainDownload)
  
  
  
  
  ## Check authorization ##########################################################
  
  observeEvent(user$role, ignoreInit = TRUE, {
    if (user$role %in% c('intern', 'sber', 'admin')) {
      ## Get min and max dates ####################################################

      # Get grab data min and max dates
      grabMinMaxDates <- getMinMaxValues(pool, 'data', DATE_reading) %>%
        mutate(across(everything(), ymd))

      # Get min and max dates
      minDate <- min(grabMinMaxDates$min, date(hfDf$`10min`$Date), na.rm = TRUE)
      maxDate <- max(grabMinMaxDates$max, date(hfDf$`10min`$Date), na.rm = TRUE)



      ## Create Sensor vs Grab download  ##########################################

      # Append new tab
      appendTab(
        'downloadTabs',
        tabPanel(
          title = 'Sensor vs Grab download',
          downloadLayoutUI(
            session$ns('sensorVSGrabDl'),
            pool = pool,
            minDate = minDate,
            maxDate = maxDate,
            innerModuleUI = sensorVSGrabDownloadUI
          ),
          value = session$ns('sensorVSGrabDl')
        )
      )

      # Call Sensor VS Grab download module
      callModule(downloadLayout, 'sensorVSGrabDl', pool, user, hfDf, sensorVSGrabDownload)
    }
  })
}
