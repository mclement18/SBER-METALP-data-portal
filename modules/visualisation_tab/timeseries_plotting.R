# library(shiny)
# library(shinyjs)
# library(ggplot2)
# library(scales)
# library(data.table)
# library(lubridate)
# library(magrittr)
# library(stringr)
# library(tidyr)
# library(dplyr)


## Create the UI function of the module ###############################################

timeSeriesPlottingUI <- function(id, catchmentsOptions, paramOptions) {
  ns <- NS(id)
  
  splittedId <- str_split(id, '-') %>% unlist()
  serieNb <- splittedId[length(splittedId)]
  
  list(
    'inputs' = div(
      id = str_interp('time-serie-plot-input-${id}'),
      class = 'time-serie-input',
      selectInput(ns('catchment'), str_interp('Catchment ${serieNb}'), catchmentsOptions),
      checkboxGroupInput(ns('sites'), 'Stations'),
      selectInput(
        ns('param'),
        tags$span(
          'Parameter',
          actionButton(ns('paramHelper'), icon('question-circle'), class = 'icon-btn')
        ),
        paramOptions
      ),
      hidden(
        checkboxGroupInput(ns('paramfilter'), label = 'Parameter filter')
      ),
      actionButton(ns('showstats'), 'Show Stats', class = 'custom-style')
    ),
    'plots' = div(
      id = str_interp('time-serie-plots-${id}'),
      class = 'time-serie-plot point-hover-widget-plot',
      plotOutput(ns('lowfreq'), hover = hoverOpts(ns('lowfreq_hover'))),
      plotOutput(ns('doy'),  hover = hoverOpts(ns('doy_hover')))
    )
  )
}



## Create the server function of the module ###############################################

timeSeriesPlotting <- function(input, output, session, df, dateRange) {
  
  observeEvent(input$catchment, {
    currentSites <- sites %>% filter(catchments == input$catchment)
    
    updateCheckboxGroupInput(session, 'sites',
                             selected = currentSites$sites_short,
                             choiceNames = currentSites$sites_full,
                             choiceValues = currentSites$sites_short)
  })
  
  observeEvent(input$param,{
    dataColumns <- parameters %>% filter(param_name == input$param) %>% select(data) %>% str_split(',') %>% unlist()
    
    updateCheckboxGroupInput(session, 'paramfilter',
                             choices = dataColumns,
                             selected = dataColumns[1])
    
    toggleElement('paramfilter', condition = length(dataColumns) > 1)
  })
  
  paramfilter <- reactive({
    inputParam <- input$param
    paramToDisplay <- input$paramfilter
    if (is.null(paramToDisplay) | is.null(inputParam)) return(list(
      'filter' = NULL,
      'param' = NULL
    ))
    param <- parameters %>% filter(param_name == input$param)
    return(list(
      'filter' = paramToDisplay,
      'param' = param
    ))
  })
  
  paramfilter_d <- paramfilter %>% debounce(1000)
  
  selectedSites <- reactive({input$sites})
  
  selectedSites_d <-  selectedSites %>% debounce(1000)
  
  ## Create a data reactive expression that return a subset of the data using the time and site inputs
  data <- reactive({
    if (is.null(paramfilter_d()$filter)) return(NULL)
    
    paramCols <- paramfilter_d()$filter
    
    sdCols <- paramfilter_d()$param$sd %>% str_split(',') %>% unlist()
    if (!is.null(sdCols)) {
      if (sdCols %>% is.na()) sdCols <- NULL
    }
      
    minMaxCols <- paramfilter_d()$param$min_max %>% str_split(',') %>% unlist()
    if (!is.null(minMaxCols)) {
      if (minMaxCols %>% is.na()) minMaxCols <- NULL
    }
    
    filteredDf <- df %>% filter(
      Site_ID %in% selectedSites_d(),
      DATE_reading >= dateRange()$min,
      DATE_reading <= dateRange()$max
    )
    
    if (dim(filteredDf)[1] == 0) return(NULL)
    
    longDf <- filteredDf %>% select(Site_ID, DATETIME_GMT, all_of(paramCols), all_of(sdCols), all_of(minMaxCols)) %>% 
      pivot_longer(cols = c(all_of(paramCols), all_of(minMaxCols)), names_to = 'parameters', values_to = 'values')
    
    rm(filteredDf)
    
    ## Create a new DATE column with the same arbitrary year for all the samples to plot all the results on one year
    longDf <- longDf %>% mutate(DATETIME_month_day_time_GMT = DATETIME_GMT)
    year(longDf$DATETIME_month_day_time_GMT) <- 2020
    
    longDf
  })
  
  ## Plot lowfreq plot
  output$lowfreq <- renderPlot({
    if (data() %>% is.null()) return(NULL)
    timeSeriePlot(
      df = data(),
      x = 'DATETIME_GMT',
      parameter = paramfilter_d()$param,
      siteName = str_interp('${unique(sites$catchments[sites$sites_short %in% selectedSites_d()])} catchment')
    )
  })
  
  output$doy <- renderPlot({
    if (data() %>% is.null()) return(NULL)
    DOYPlot(
      df = data(),
      x = 'DATETIME_month_day_time_GMT',
      parameter = paramfilter_d()$param,
      siteName = str_interp('${unique(sites$catchments[sites$sites_short %in% selectedSites_d()])} catchment')
    )
  })
  
  pointHoverWidgetServer(session, 'lowfreq', data, reactive(input$lowfreq_hover),
                         x_label = 'Date', y_label = 'parameters')
  
  pointHoverWidgetServer(session, 'doy', data, reactive(input$doy_hover),
                         x_label = 'Date', y_label = 'parameters',
                         override.mapping = list('x' = 'DATETIME_GMT'))
  
  createTable <- function(df) {
    
    ## Function that create a data frame of one column with some summary statistics
    ## of a given data
    ## Takes in a numerical vector and a column name as string
    ## Return a data frame containing the statistics values
    getStats <- function(columnData, columnName) {
      newColumn <- data.frame(row.names = c(
        'Time Points',
        'N',
        "NA's",
        'Median',
        'Mean',
        'SD',
        'Min.',
        'Max.'
      ))
      
      newColumn['Time Points', columnName] <- length(columnData)
      newColumn['N', columnName] <- length(columnData) - sum(is.na(columnData))
      newColumn["NA's", columnName] <- sum(is.na(columnData))
      newColumn['Median', columnName] <- median(columnData, na.rm = TRUE)
      newColumn['Mean', columnName] <- mean(columnData, na.rm = TRUE)
      newColumn['SD', columnName] <- sd(columnData, na.rm = TRUE)
      newColumn['Min.', columnName] <- if (min(columnData, na.rm = TRUE) == Inf) NA else min(columnData, na.rm = TRUE)
      newColumn['Max.', columnName] <- if (max(columnData, na.rm = TRUE) == -Inf) NA else max(columnData, na.rm = TRUE)
      
      return(newColumn)
    }
    
    # Create a vector with the parameter data column name references
    columns <- df$parameters %>% unique()
    
    ## Create empty data frame
    statsTable <- data.frame()
    
    ## For each values in columns
    for (dataColumn in columns) {
      ## If statsTable is an empty data frame
      ## Assign it the getStats output
      ## Else combine both data frame
      if (statsTable %>% dim() %>% sum() == 0) {
        statsTable <- df %>% filter(parameters == dataColumn) %>% pull(values) %>% getStats(dataColumn)
      } else {
        newCol <- df %>% filter(parameters == dataColumn) %>% pull(values) %>% getStats(dataColumn)
        statsTable <- cbind(statsTable, newCol)
      }
    }

    ## Return table
    return(statsTable)
  }
  
  observeEvent(input$showstats, {
  
    renderMultiTable <- function(id, tables) {
      tablesOutput <- tagList()
      for (site in selectedSites_d()) {
        perSiteData <- data() %>% filter(Site_ID == site)
        tableId <- str_interp('${id}-${site}')
        site_name <- sites %>% filter(sites_short == site) %>% pull(sites_full)
        tablesOutput <- tagList(
          tablesOutput,
          tags$div(
            class = 'stats-summary-table',
            h4(site_name),
            tableOutput(session$ns(tableId)
          )
        ))
        output[[tableId]] <- renderTable(createTable(perSiteData), rownames = TRUE)
      }
      
      return(renderUI(tablesOutput))
    }
    
    output$stats <- renderMultiTable('statstable', tables)
    
    showModal(modalDialog(
      title = str_interp('Stats ${unique(sites$catchments[sites$sites_short %in% selectedSites_d()])} catchment'),
      htmlOutput(session$ns('stats'), class = 'stats-summary'),
      easyClose = TRUE
    ))
  })
  
  ## Display Parameter's description
  observeEvent(input$paramHelper, {
    
    output$description <- renderUI(tags$p(
      class = 'description',
      parameters %>% filter(param_name == input$param) %>% select(description) %>% unlist()
    ))
    
    showModal(modalDialog(
      title = 'Parameters description',
      htmlOutput(session$ns('description')),
      easyClose = TRUE
    ))
  })
}

