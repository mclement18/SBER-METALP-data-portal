## Functions that extend shiny default functions ##################################


## UI function modifiers ##########################################################

navbarPageWithWrapper <- function(navbarPageOutput, wrapperClass = 'content-wrapper', footer = NULL, beforeFooterClass = 'before-footer') {
# Create a shiny navbarPage with a content wrapper class for the navbar and content
# Parameters:
# - navbarPageOutput: Output of shiny navbarPage() function, mandatory
# - wrapperClass: a class name for your wrapper, default 'content-wrapper'
# - footer: an optional custom footer, default NULL
# - beforeFooterClass: a class name for the content before the optional footer, default 'before-footer'
#
# Returns an updated shiny navbarPage UI element
  
  # Add wrapperClass to navabar
  navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class <- paste(
    navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class, wrapperClass,
    sep = ' '
  )
  # Add wrapperClass and beforeFooterClass to the content
  navbarPageOutput[[3]][[2]]$attribs$class <- paste(
    navbarPageOutput[[3]][[2]]$attribs$class, wrapperClass, beforeFooterClass,
    sep = ' '
  )
  
  # If footer is defined, add it after the content
  # And wrap all the body in a div with a CSS class used to send the footer to the bottom of the page
  if(!is.null(footer)) {
    navbarPageOutput[[3]][[length(navbarPageOutput[[3]]) + 1]] <- footer
    navbarPageOutput <- div(
      class = 'footer-to-bottom-container',
      navbarPageOutput
    )
  }
  
  return(navbarPageOutput)
}




tabsetPanelWithNULL <- function(..., id = NULL, selected = NULL, type = "tabs", position = NULL) {
# Create a shiny tabsetPanel that accept NULL tabs
# Parameters:
# - ...: shiny tabPanels or NULL
# - id, selected, type, position: shiny tabsetPanel arguments
#
# Returns an shiny tabsetPanel
  
  # Get tabs and remove NULL values
  args <- list(...)
  args[sapply(args, is.null)] <- NULL
  
  # Add named parameters
  args[['id']] <- id
  args[['selected']] <- selected
  args[['type']] <- type
  args[['position']] <- position
  
  # Call tabsetPanel function
  do.call(tabsetPanel, args)
}




withLoginAction <- function(navbarPageOutput, loginUI) {
# Return a Shiny navbarPage with a login UI on the right of the nav bar
# Parameters:
# - navbarPageOutput: Output of shiny navbarPage() function, mandatory
# - loginUI: UI element to insert, ideally output from the loginUI module function, mandatory
#
# Returns an updated shiny navbarPage UI element
  
  # Add the loginUI to the navbar of the navbarPageOutput
  navbarPageOutput[[3]][[1]]$children[[1]]$children[[length(navbarPageOutput[[3]][[1]]$children[[1]]$children) + 1]] <- loginUI
  # Return the updated UI
  return(navbarPageOutput)
}




checkboxGroupInputWithClass <- function(checkboxGroupInput, class) {
# Create a shiny checkboxGroupInput with additional custom class names
# Can be used with radioButtons as well
# Parameters:
# - checkboxGroupInput: Output of shiny checkboxGroupInput() function
# - class: String, contains the class names to add, separated by space
#
# Returns an updated shiny checkboxGroupInput UI element
  
  checkboxGroupInput$attribs$class <- paste(checkboxGroupInput$attribs$class, class, sep = ' ')
  return(checkboxGroupInput)
}



spinnerPlotOutput <- function(outputId, ...) {
# Create a plotOutput that display a spinner when the plot is computing
# Parameters:
# - outputId: String, the id of the plotOutput
# - ...: all other arguments accepted by the plotOutput function
# 
# Returns a plotOutput with a spinner
  
  # Add spinner to the plotOutput using the fading circle and the primary-color
  addSpinner(plotOutput(outputId = outputId, ...), spin = "fading-circle", color = "#e24727")
}


modalButtonWithClass <- function(label, icon = NULL, class) {
# Create a shiny modalButton with additional custom class names
# Parameters:
# - label: String, the content of the button
# - icon: Shiny icon, optional
# - class: String, contains the class names to add, separated by space
#
# Returns an updated shiny modalButton UI element
  
  # Create modalButton
  button <- modalButton(label = label, icon = icon)
  # Add the additional class
  button$attribs$class <- paste(button$attribs$class, class, sep = ' ')
  # Return the updated button
  return(button)
}



fileInputWithClass <- function(inputId, label, class, multiple = FALSE, accept = NULL,
                               width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
# Create a shiny fileInput with additional custom class names for the button
# Parameters:
# - inputId, label,
#   multiple, accept,
#   width, buttonLabel,
#   placeholder:        Arguments from the Shiny file input
# - class: String, contains the class names to add, separated by space
#
# Returns an updated shiny fileInput UI element
  
  # Create input
  input <- fileInput(
    inputId = inputId,
    label = label,
    multiple = multiple,
    accept = accept,
    width = width,
    buttonLabel = buttonLabel,
    placeholder = placeholder
  )
  
  # Add class to button
  input$children[[2]]$children[[1]]$children[[1]]$attribs$class <- paste(
    input$children[[2]]$children[[1]]$children[[1]]$attribs$class,
    class
  )
  
  # Return the updated input
  input
}



withAttributes <- function(textInput, ...) {
# Takes a Shiny textInput or textAreaInput and add attribute to its input element
# Parameters:
#  - textInput: A Shiny textInput or textAreaInput
#  - ...: named arguments to set as attribute
# 
# Returns an updated Shiny input
  
  # Parse named arguments
  args <- list(...)
  args <- args[names(args) != '']
  
  # If list is not empty
  if (length(args) > 0) {
    for (i in c(1:length(args))) {
      attributeName <- names(args)[i]
      attributeValue <- args[[i]]
      textInput$children[[2]]$attribs[[attributeName]] <- attributeValue
    }
  }
  
  # Return textInput
  textInput
}



roleToIcon <- function(role) {
# Convert user role to an icon
# Parameters:
#  - role: String, the role to get the icon for
# 
# Returns a shiny icon output
  
  # Create the role mapping
  mapping <- list(
    download = 'cloud-download-alt',
    intern = 'user-graduate',
    sber = 'mountain',
    admin = 'hat-wizard'
  )
  
  # Create the icon
  icon(mapping[[role]])
}



## Reusable server logic ##########################################################


createStatsTablePerSite <- function(site, data, sites) {
# Create and render a stats table that contain stats summary of one site
# Parameters:
#  - site: String, the site short name to use
#  - data: Reactive expression returning a Data.frame of the data to summarise
#  - sites: Data.frame, contains the information of the sites
# 
# Returns a stats table

  # Get the site full name
  site_name <- sites %>% filter(name == site) %>% pull(full_name)
  
  # Return the stats summary table
  tags$div(
    class = 'stats-summary-table',
    h4(site_name),
    renderTable(data %>% filter(Site_ID == site) %>% createStatsTable(), rownames = TRUE, digits = 2, format.args=list(drop0trailing = TRUE))
  )
}





createSensorStatsTable <- function(catchment, data, sites) {
# Create and render a stats table that contain stats summary of sensors for one catchment
# Parameters:
#  - catchment: String, the catchment name to use
#  - data: Reactive expression returning a Data.frame of the data to summarise
#  - sites: Data.frame, contains the information of the sites
# 
# Returns a stats table or NULL
  
  # Get current sites
  currentSites <- sites %>% filter(catchment == !!catchment) %>% pull(name)
  
  # If there is at least one selected site corresponding to this catchment
  # Create and return the stats table for this site
  # Else return NULL
  if (any(currentSites %in% colnames(data))) {
    tags$div(
      class = 'stats-summary-table',
      h4(catchment),
      renderTable(data %>% select(Stats, any_of(currentSites)), digits = 1, format.args=list(drop0trailing = TRUE))
    )
  } else {
    NULL
  }
}






renderStatsTables <- function(elements, data, sites, tableFunction) {
# Create and render a multiple stats tables
# Parameters:
#  - elements: Character Vector or Reactive expression returning one, the elements to loop over
#  - data: Reactive expression returning a Data.frame of the data to summarise
#  - sites: Data.frame, contains the information of the sites
#  - tableFunction: Fucntion, the function to use to make the stats tables
# 
# Returns multiple stats table in a renderUI call
  
  # Render a UI containing all the stats tables
  renderUI({
    # Get data
    data <- data()
    # If the elements are in a reactive expression
    # Retrieve them
    if (is.reactive(elements)) elements <- elements()
    if (is.reactive(sites)) sites <- sites()
    # Create and return all tables
    lapply(elements, function(element){tableFunction(element, data, sites)})
  })
}




pointHoverWidgetServer <- function(session, plotId, df, input,
                                   x_label = NULL, y_label = NULL,
                                   override.mapping = NULL, threshold = 5,
                                   secondDf = NULL, secondX = NULL, secondY = NULL) {
# Create an observeEvent that react to either an click or hover input on a plot and send information
# to the client to create an info bubble for the closest point
# Parameters:
#  - session: Shiny session, the session where the function is called
#  - plotId: String, the id of the plot to track
#  - df: Reactive expression returning a Data.frame, the data used to draw the plot
#  - input: reactive expression, returning either a click or a hover input
#  - x_label, y_label: String, either a column name of the df where to get the label
#                      or a new label, if NULL (default) values found in the input()$mapping is used
#  - override.mapping: Named list, containing the new mapping to use for the widget. Default NULL
#                      (i.e. list('x' = 'new_x_mapping', 'y' = 'new_y_mapping'), list('x' = 'new_x_mapping'))
#  - threshold: Int, threshold in pixels to determined the nearest point, default 5.
#  - secondDf: Reactive expression returning a Data.frame, a secondary data frame used to draw additional points
#  - secondX, secondY: String, the column name of the x or y data column of the secondDf
# 
# 
# Returns an observeEvent
  
  # Create an observeEvent that react to input() changes even if it is NULL
  # Is returned by the function
  observeEvent(input(), {
    # Parse the potId to add the current namespace to it
    plotId <- session$ns(plotId)
    # Extract the mapping information from the input()
    mapping <- input()$mapping
    
    # If there is a mapping
    if (length(mapping) > 0) {
      altDf <- FALSE
      
      # Get the nearest point
      # If a second df is specified look for a point in this one
      if (!is.null(secondDf)) {
        point <- nearPoints(secondDf(), input(), maxpoints = 1, threshold = threshold,
                            xvar = secondX, yvar = secondY)
        # If a point is fount set altDf to TRUE
        if (nrow(point) == 1) altDf <- TRUE
      }
      
      # If altDf is FALSE look for a point in the primary df
      if (!altDf) point <- nearPoints(df(), input(), maxpoints = 1, threshold = threshold)
      
      # If there is a point process it and return
      if (nrow(point) == 1) {
        
        # Correct the mapping if overrided
        if (typeof(override.mapping) == 'list') {
          if (!is.null(override.mapping$x)) mapping$x <- override.mapping$x
          if (!is.null(override.mapping$y)) mapping$y <- override.mapping$y
        }
        
        # Correct the mapping for the secondary df
        if (altDf) {
          mapping$x <- secondX
          mapping$y <- secondY
        }
        
        # Extract relevant point information
        pointInfo <- point %>% select(Site_ID, mapping$x, mapping$y)
        
        # Predefine the x and y labels with the mapping info
        x_y_labels = list(
          'x' = mapping$x,
          'y' = mapping$y
        )
        
        # If a label is specified for x or y process it
        if (!is.null(x_label)) {
          # If the label is a name of a column
          if (x_label %in% colnames(point)) {
            # Use the value stored in that column
            x_y_labels$x <- point %>% pull(x_label)
          } else {
            # Otherwise use the label
            x_y_labels$x <- x_label
          }
        }
        
        # The same for y label
        if (!is.null(y_label)) {
          if (y_label %in% colnames(point)) {
            x_y_labels$y <- point %>% pull(y_label)
          } else {
            x_y_labels$y <- y_label
          }
        }
        
        # Create a JSON message to send to the client containing:
        #  - pointInfo: the point information (use unbox() to get an object rather than an array)
        #  - mapping: the mapping information
        #  - coords_img: the coordinates of the input location on the plot
        #  - x_y_labels: the x and y labels
        #  - plotId: the id of the concerned plot
        messageJSON <- toJSON(list(
          'pointInfo' = unbox(pointInfo),
          'mapping' = mapping,
          'coords_css' = input()$coords_css,
          'coords_img' = input()$coords_img,
          'range' = input()$range,
          'x_y_labels' = x_y_labels,
          'plotId' = plotId
        ), auto_unbox = TRUE)
        
        # Send the shiny custom message to create a widget
        # Linked to some JavaScript defined in './assets/js/point_hover_widget.js'
        session$sendCustomMessage('addHoverWidget', messageJSON)
        
        # Return to stop the expression execution
        return()
      } # End if dim(point)[1] == 1
    } # End if length(mapping) > 0
    
    
    
    # This part is executed only if length(mapping) ≤ 0 or no nearest point were found
    
    # Create a JSON message to send to the client containing:
    #  - plotId: the id of the concerned plot
    messageJSON <- toJSON(list(
      'plotId' = plotId
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message to remove widgets
    # Linked to some JavaScript defined in './assets/js/point_hover_widget.js'
    session$sendCustomMessage('removeHoverWidget', messageJSON)
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
}



createInputs <- function(df, pool, table, session = getDefaultReactiveDomain()) {
# Create inputs based on a df column names, types and values
# Parameters:
#  - df: Data.frame, the data used to create the inputs, should either be empty or have only one row
#  - pool: Pool connection, the connection to an SQL Database
#  - table: String, the database corresponding table
#  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
# 
# Returns a tagList of inputs
  
  # Create the inputs tagList
  inputs <- tagList()
  # Get the df column types
  columnTypes <- df %>% lapply(type_sum) %>% unlist()
  
  # For each column
  for (i in c(1:length(columnTypes))) {
    # Get the type and the column name
    type <- columnTypes[i]
    column <- names(type)
    
    # If the df is empty, set the value to NULL
    # Otherwise, set it to the column value unless it is an NA, then set it to NULL
    if (nrow(df) == 0) {
      value <- NULL
    } else {
      value <- df %>% pull(column)
      if (is.na(value)) value <- NULL
    }
    
    # If value is factor, set levels
    levels <- NULL
    if (type == 'fct') levels <- df %>% pull(column) %>% levels()
    
    
    # Create the input and add it to the list
    inputs <- tagList(
      inputs,
      createInput(
        type = type,
        label = column,
        value = value,
        pool = pool,
        table = table,
        levels = levels,
        session = session
      )
    )
  }
  
  # Return the inputs list
  return(inputs)
}




createInput <- function(type, label, value = NULL, table, pool, levels = NULL, session = getDefaultReactiveDomain()) {
# Create an input based on a df column name, type and value
# Parameters:
#  - type: String, the columns type used to choose the input
#  - label: String the column name used to create the input label and id
#  - value: 'type' dependent, the value of the column used to populate the input
#  - table: String, the database corresponding table
#  - pool: Pool connection, the connection to an SQL Database
#  - levels: Character vector, options to use for select input for in case it cannot be recovered from DB
#  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
# 
# Returns the adequate input
  
  # Choose the input in function of the column type
  if (type == 'dbl' | type == 'int') {
    numericInput(session$ns(label), label = label, value = value)
  } else if (type == 'chr') {
    if (label %in% c('description', 'text')) {
      textAreaInput(session$ns(label), label = label, value = value)
    } else {
      textInput(session$ns(label), label = label, value = value)
    }
  } else if (type == 'lgl') {
    checkboxInput(session$ns(label), label = label, value = value)
  } else if (type == 'fct') {
    # For a select input get the possible values from the SQL database
    choices <- getEnumValues(pool, table, label)
    # If NULL, get the factor levels
    if (is.null(choices)) {
      selectizeInput(session$ns(label), label = label, choices = levels, selected = value, options = list(create = TRUE))
    } else {
      selectInput(session$ns(label), label = label, choices = choices, selected = value)
    }
  } else if (type == 'dttm') {
    airDatepickerInput(
      inputId = session$ns(label),
      label = paste(label, '(GMT)'),
      value = value,
      timepicker = TRUE,
      timepickerOpts = timepickerOptions(timeFormat = 'hh:ii')
    )
  } else if (type == 'date') {
    airDatepickerInput(
      inputId = session$ns(label),
      label = label,
      value = value,
      clearButton = TRUE,
      todayButton = TRUE,
      autoClose = TRUE,
      update_on = 'close',
      addon = 'none'
    )
  }
}




confirmationModal <- function(text = '', session = getDefaultReactiveDomain(), noAction = FALSE) {
# Create and display a modal for action confirmation purpose
# Confirmation need to be done with an 'observeEvent' listening for an 'input$YES' button
# And modal need to be closed within this observer with 'removeModal()'
# Parameters:
#  - text: String, the optional additional text to display
#  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
#  - noAction: Logical, create or not a NO button linked to an 'input$NO'. If yes, you will need to handle
#              the modal removal manually with 'removeModal()'. Default: FALSE
# 
# Returns NULL
  
  # Create and display the modal
  showModal(modalDialog(
    size = 's',
    div(
      class = 'confirmation-modal',
      h1('Are you sure ?'),
      p(text),
      div(
        class = 'confirmation-buttons',
        actionButton(session$ns('YES'), 'YES', class = 'btn-success'),
        if (noAction) {
          actionButton(session$ns('NO'), 'NO', class = 'btn-danger')
        } else {
          modalButtonWithClass('NO', class = 'btn-danger')
        }
      )
    ),
    footer = NULL
  ), session = session)
}



clearReactiveValues <- function(rv, fromInput = FALSE, id = NULL) {
# Clear the values from a list of reactive values created by reactiveValues()
# Or from the global input given a module id
# Parameters:
#  - rv: ReactiveValues, the list of reactive values to clear or the input variable
#  -fromInput: Boolean, indicates whether the passed reactive values is the input or not, default: FALSE
#  - id: String, the id used to select the inputs to delete. To use with fromInput = TRUE.
#        If NULL, the entire inputs will be cleared. Default NULL.
# 
# Returns NULL
  
  # Get the rv names
  if (fromInput) {
    if (is.null(id)) {
      # If is input and no id, take all names
      rvNames <- .subset2(rv, "impl")$names()
    } else {
      # If input and id, take the matching names
      rvNames <- grep(id, .subset2(rv, "impl")$names(), value = TRUE)
    }
  } else {
    # If reactiveValues, take the names
    rvNames <- names(rv)
  }

  # Remove the selected values
  invisible(
    lapply(rvNames, function(i) {
      .subset2(rv, "impl")$.values$remove(i)
    })
  )
}



destroyObservers <- function(observers) {
# Destroy all observers stored in a list
# And do it recursively if any list value contains a list
# Parameters:
#  - observers: List, the list of reactive values containing observers to destroy
# 
# Returns NULL
  
  # Destroy observers
  invisible(
    lapply(names(observers), function(name) {
      # If it is an observer, destroy it
      if ('Observer' %in% class(observers[[name]])) {
        observers[[name]]$destroy()
        # If it is a reactive values, destroy the observers recursively
      } else if (is.list(observers[[name]])) {
        destroyObservers(observers[[name]])
      }
    })
  )
}

