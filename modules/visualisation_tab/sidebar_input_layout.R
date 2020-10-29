## This module contains the UI and server code for the show/hide sidebar input layout
## with a dynamic interface to add and remove units of an inner module for visualisation

## Create module UI function ######################################################

sidebarInputLayoutUI <- function(id, minDate, maxDate, innerModuleUI, ...) {
# Create the UI for the sidebarInputLayout module
# Parameters:
#  - id: String, the module id
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - innerModuleUI: Function, the inner module UI function
#  - ...: All other arguments needed by the inner module function
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)

  # Create the first unit UI elements of the innerModule
  innerModuleUIList <- innerModuleUI(ns('1'), ...)
  
  # Create the module layout
  div(
    # First div containing the global inputs
    div(
      class = 'main-inputs',
      # Date Range to select the global dateRange
      dateRangeInput(ns('time'), 'Date range:',
                     start = minDate,
                     end = maxDate,
                     min = minDate,
                     max = maxDate,
                     format = 'dd/mm/yyyy',
                     separator = '-'),
      # Create a nutton to reset the date range
      actionButton(ns('resetDateRange'), 'Reset Date', class = 'custom-style'),
      # div grouping main actions to the left
      div(
        class = 'main-actions',
        # Button group containing the global actions
        div(
          class = 'btn-group',
          # Button to toggle sidebar visibility
          actionButton(ns('toggleSidebar'), 'Hide inputs', class = 'custom-style'),
          # Button to add an additional unit
          actionButton(ns('addUnit'), 'Add Unit', class = 'custom-style'),
          # Button to remove an unit
          # Disable by default because at least one unit is displayed
          disabled(
            actionButton(ns('removeUnit'), 'Remove Unit', class = 'custom-style')
          )
        ),
        # Create an icon button that trigger a modal to display the global help
        actionButton(ns('help'), 'Help', class = 'custom-style custom-style--primary')
      )
    ),
    # Create the sidebarLayout
    sidebarLayout(
      # Create a sidebar with the innerModule first unit input UI elements inside
      sidebarPanel(
        id = ns('sidebar-inputs'),
        innerModuleUIList$inputs,
        width = 3
      ),
      # Create the main panel with the innerModule first unit plot UI elements inside
      mainPanel(
        id = ns('main-plots'),
        innerModuleUIList$plots,
        width = 9
      )
    )
  )
}



## Create module server function ##################################################

sidebarInputLayout <- function(input, output, session,
                               innerModule, innerModuleUI, innerModulePrefixIds, df,
                               plotDateRangeSelection = TRUE, minDate, maxDate, ...) {
# Create the logic for the sidebarInputLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - innerModule: Function, the inner module server function
#  - innerModuleUI: Function, the inner module UI function
#  - innerModulePrefixIds: Named list containing:
#                            + inputs: String, the prefix for the inner module inputs UI element
#                            + plots: String, the prefix for the inner module plots UI element
#  - df: Data.frame, the data to pass to the inner module
#  - plotDateRangeSelection: Boolean, indicates if the inner module plots are modifying the date range, default TRUE
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - ...: All other arguments needed by the inner module function
# 
# Returns NULL
    
  ## DateRange input logic ########################################################
  
  # Create dateRange reactive expression containing the min and max values of the dateRangeInput
  dateRange <- reactive(list('min' = input$time[1], 'max' = input$time[2]))
  
  # Create an observeEvent that allows to reset the date range when resetDateRange is clicked
  observeEvent(input$resetDateRange, {
    updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
  })
  
  
  # Create a reactive value that track if the plot are zoomed if the plot can be zoomed
  if (plotDateRangeSelection) zoomed <- reactiveVal(FALSE)
  
  
  
  ## First unit module calling ####################################################

  # Call the first unit of the innerModule and retrieve, if any, the named list containing:
  #  - update: Reactive expression containing the updated dateRange
  #  - reset: Reactive value, updated each time the plot is double clicked, used as dateRange reset trigger
  dateRangeActions <- callModule(innerModule, '1', df, dateRange, ...)
  
  # If the inner module plots are modifying the date range
  if (plotDateRangeSelection) {
    # Add an observeEvent that track the plot brushing dateRange input for the first innerModule unit
    observeEvent(dateRangeActions$update(), ignoreInit = TRUE, {
      # Run only if dates are non null
      req(length(dateRangeActions$update()$min) != 0, length(dateRangeActions$update()$min) != 0)
      # Store new dates
      newMin <- dateRangeActions$update()$min
      newMax <- dateRangeActions$update()$max
      
      # Ensure that dates are within range
      if (newMin < date(minDate)) newMin <- minDate
      if (newMax < date(minDate)) newMax <- minDate
      if (newMin > date(maxDate)) newMin <- maxDate
      if (newMax > date(maxDate)) newMax <- maxDate
      
      # Update the dateRangeInput accordingly
      updateDateRangeInput(session, 'time', start = newMin, end = newMax)
      # Set the zoomed value to TRUE
      zoomed(TRUE)
    })
  
    # Add an observeEvent that react to the first innerModule unit dateRange reset trigger
    observeEvent(dateRangeActions$reset(), {
      if (zoomed()) {
        # Reset the dateRange to initial dates only if plots are zoomed
        updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
        # Set the zoomed value to FALSE
        zoomed(FALSE)
      }
    })
  }
  
  
  
  ## Unit Nb tracking #############################################################
  
  # Create a reactive value that keep track of the number of displayed units
  # One is displayed by default
  unitsNb <- reactiveVal(1)
  
  
  
  ## Unit adding logic ############################################################
  
  # Add an observeEvent that will run upon click on the add unit button
  observeEvent(input$addUnit, {
    # If there is currently one unit enable remove unit button
    if (unitsNb() == 1) enable('removeUnit')
    # Increment the number of units
    unitsNb(unitsNb() + 1)
    
    # Create new unit UI elements
    innerModuleUIList <- innerModuleUI(session$ns(unitsNb()), ...)
    
    # Insert the new unit input UI elements in the sidebar
    insertUI(
      str_interp("#${session$ns('sidebar-inputs')}"), where = 'beforeEnd',
      ui = innerModuleUIList$inputs,
      immediate = TRUE
    )
    
    # Insert the new unit plot UI elements in the main panel
    insertUI(
      str_interp("#${session$ns('main-plots')}"), where = 'beforeEnd',
      ui = innerModuleUIList$plots,
      immediate = TRUE
    )
    
    # Call new unit module function and retrieve, if any, the named list containing:
    #  - update: Reactive expression containing the updated dateRange
    #  - reset: Reactive value, updated each time the plot is double clicked, used as dateRange reset trigger
    dateRangeActions <- callModule(innerModule, unitsNb(), df, dateRange, ...)
    
    # If the inner module plots are modifying the date range
    if (plotDateRangeSelection) {
      # Add an observeEvent that track the plot brushing dateRange input for the new module unit
      observeEvent(dateRangeActions$update(), ignoreInit = TRUE, {
        # Run only if dates are non null
        req(length(dateRangeActions$update()$min) != 0, length(dateRangeActions$update()$min) != 0)
        # Store new dates
        newMin <- dateRangeActions$update()$min
        newMax <- dateRangeActions$update()$max
        
        # Ensure that dates are within range
        if (newMin < date(minDate)) newMin <- minDate
        if (newMax < date(minDate)) newMax <- minDate
        if (newMin > date(maxDate)) newMin <- maxDate
        if (newMax > date(maxDate)) newMax <- maxDate
        
        # Update the dateRangeInput accordingly
        updateDateRangeInput(session, 'time', start = newMin, end = newMax)
        # Set the zoomed value to TRUE
        zoomed(TRUE)
      })
      
      # Add an observeEvent that react to the new module unit dateRange reset trigger
      observeEvent(dateRangeActions$reset(), {
        if (zoomed()) {
          # Reset the dateRange to initial dates only if plots are zoomed
          updateDateRangeInput(session, 'time', start = minDate, end = maxDate)
          # Set the zoomed value to FALSE
          zoomed(FALSE)
        }
      })
    }
  })
  
  
  
  ## Unit removing logic ##########################################################
  
  # Add an observeEvent that will run upon click on the remove unit button
  observeEvent(input$removeUnit, {
    # Define last unit inputs and plots ids
    inputsId <- str_interp('#${innerModulePrefixIds$inputs}-${session$ns(unitsNb())}')
    plotsId <- str_interp('#${innerModulePrefixIds$plots}-${session$ns(unitsNb())}')
    
    # Remove last unit plots
    removeUI(
      plotsId,
      immediate = TRUE
    )
    
    # Remove last unit inputs
    removeUI(
      inputsId,
      immediate = TRUE
    )
    
    # Decrement units nb
    unitsNb(unitsNb() - 1)
    
    # If only one unit left, disable remove unit button
    if (unitsNb() == 1) disable('removeUnit')
  })
  
  
  
  ## Sidebar inputs toggle logic ##################################################
  
  # Create a boolean reactive value that keep track of the sidebar visibility state
  sidebarVisible <- reactiveVal(TRUE)
    
  # Create the observeEvent that react to the sidebar toggling button
  observeEvent(input$toggleSidebar, {
    
    # Invert sidebarVisible value
    sidebarVisible(!sidebarVisible())
    
    # Create JSON message to send to client containing:
    #  - sidebarId: the sidebar id defined in the UI
    #  - mainPanelId: the main panel id defined in the UI
    #  - show: the new state of the sidebar
    messageJSON <- toJSON(list(
      'sidebarId' = session$ns('sidebar-inputs'),
      'mainPanelId' = session$ns('main-plots'),
      'show' = sidebarVisible()
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle sidebar visibility
    # Linked to some JavaScript defined in './assets/js/sidebar_actions.js'
    session$sendCustomMessage('sidebarToggle', messageJSON)
    
    # Determine correct toggling button label
    newBtnLabel <- 'Show inputs'
    if (sidebarVisible()) newBtnLabel <- 'Hide inputs'
    
    # Update toggling button label
    updateActionButton(session, 'toggleSidebar', label = newBtnLabel)
  })
  
  
  
  ## Help modal logic #############################################################
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    showModal(modalDialog(
      title = 'Global Help',
      htmlTemplate('./html_components/help_table.html'),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}
