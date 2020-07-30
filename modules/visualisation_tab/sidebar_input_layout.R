# This module contains the UI and server code for the show/hide sidebar input layout 
# with a dynamic interface to add and remove units of an inner module for visualisation

## Create module UI function ######################################################

sidebarInputLayoutUI <- function(id, minDate, maxDate, innerModuleUI) {
# Create the UI for the sidebarInputLayout module
# Parameters:
#  - id: String, the module id
#  - minDate: Date, the lower bound for the dateRangeInput
#  - maxDate: Date, the upper bound for the dateRangeInput
#  - innerModuleUI: Function, the inner module UI function
# 
# Returns a div containing layout
  
  # Create namespace
  ns <- NS(id)

  # Create the first unit UI elements of the innerModule
  innerModuleUIList <- innerModuleUI(ns('1'))
  
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
      )
    ),
    # Create the sidebarLayout
    sidebarLayout(
      # Create a sidebar with the innerModule first unit input UI elements inside
      sidebarPanel(
        id = 'sidebar-inputs',
        innerModuleUIList$inputs,
        width = 3
      ),
      # Create the main panel with the innerModule first unit plot UI elements inside
      mainPanel(
        id = 'main-plots',
        innerModuleUIList$plots,
        width = 9
      )
    )
  )
}



## Create module server function ##################################################

sidebarInputLayout <- function(input, output, session, innerModule, innerModuleUI, innerModulePrefixIds, df) {
# Create the logic for the sidebarInputLayout module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - innerModule: Function, the inner module server function
#  - innerModuleUI: Function, the inner module UI function
#  - innerModulePrefixIds: Named list containing:
#                            + inputs: String, the prefix for the inner module inputs UI element
#                            + plots: String, the prefix for the inner module plots UI element
#  - df: Data.frame, the data to pass to the inner module
# 
# Returns NULL
    
  ## DateRange input logic ########################################################
  
  # Create dateRange reactive expression containing the min and max values of the dateRangeInput
  dateRange <- reactive(list('min' = input$time[1], 'max' = input$time[2]))
  
  
  
  ## First unit module calling ####################################################

  # Call the first unit of the innerModule and retrieve the reactive expression containing the updated dateRange
  updateDateRange <- callModule(innerModule, '1', df, dateRange)
  
  # Add an observeEvent that track the plot brushing dateRange input for the first innerModule unit
  # And update the dateRangeInput accordingly
  observeEvent(updateDateRange(), {
    updateDateRangeInput(session, 'time', start = updateDateRange()$min, end = updateDateRange()$max)
  })
  
  
  
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
    innerModuleUIList <- innerModuleUI(session$ns(unitsNb()))
    
    # Insert the new unit input UI elements in the sidebar
    insertUI(
      '#sidebar-inputs', where = 'beforeEnd',
      ui = innerModuleUIList$inputs,
      immediate = TRUE
    )
    
    # Insert the new unit plot UI elements in the main panel
    insertUI(
      '#main-plots', where = 'beforeEnd',
      ui = innerModuleUIList$plots,
      immediate = TRUE
    )
    
    # Call the new unit module function and retrieve the reactive expression containing the updated dateRange
    updateDateRange <- callModule(innerModule, unitsNb(), df, dateRange)
    
    # Add an observeEvent that track the plot brushing dateRange input of the new module unit
    # And update the dateRangeInput accordingly
    observeEvent(updateDateRange(), {
      updateDateRangeInput(session, 'time', start = updateDateRange()$min, end = updateDateRange()$max)
    })
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
      'sidebarId' = 'sidebar-inputs',
      'mainPanelId' = 'main-plots',
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
}
