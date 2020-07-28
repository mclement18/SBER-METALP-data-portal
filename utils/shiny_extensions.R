# Functions that extend shiny default functions ###################################

# UI function modifiers ####################################################################

navbarPageWithWrapper <- function(navbarPageOutput, wrapperClass = 'content-wrapper', footer = NULL, beforeFooterClass = 'before-footer') {
# Create a shiny navbarPage with a content wrapper class for the navbar and content
# Parameters:
# - navbarPageOutput: Output of shiny navbarPage() function, mendatory
# - wrapperClass: a class name for your wrapper, default 'content-wrapper'
# - footer: an optional custom footer, default NULL
# - beforeFooterClass: a class name for the content before the optional footer, default 'before-footer'
#
# Returns an updated shiny navbarPage UI element
  
  # Add wrapperClass to navabar
  navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class} ${wrapperClass}')
  # Add wrapperClass and beforeFooterClass to the content
  navbarPageOutput[[3]][[2]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[2]]$attribs$class} ${wrapperClass} ${beforeFooterClass}')
  
  # If footer is defined, add it after the content
  if(!is.null(footer)) {
    navbarPageOutput[[3]][[length(navbarPageOutput[[3]]) + 1]] <- footer
  }
  
  return(navbarPageOutput)
}

# Reusable server logic ###########################################################

sideBarToggleServer <- function(session, buttonInput, sidebarVisible,
                                sidebarId, mainPanelId, buttonInputId,
                                buttonLabelShow, buttonLabelHide) {
# Create an observeEvent to toggle the sideBar visibility of a sidebarPanel in sidebarLayout
# Parameters:
# - session: the shiny session where the function is called
# - buttonInput: a reactive expression returning the input value of the actionButton that toggle the sidebar
# - sideBarVisible: a boolean reactive value that will keeep track of the sideBar state
# - sidebarId: a string containing the id given to the sidebarPanel
# - mainPanelId: a string containing the id given to the mainPanel
# - buttonInputId: a string containing the id given to the mainPanel
# - buttonLabelShow: a string containing the button label for when the sidebar is hidden
# - buttonLabelHide: a string containing the button label for when the sidebar is visible
# 
# Returns an observeEvent
  
  # Create the observeEvent that react to the buttonInput change
  observeEvent(buttonInput(), {
    
    # Invert sidebarVisible value
    sidebarVisible(!sidebarVisible())
    
    # Create JSON message to send to client
    messageJSON <- toJSON(list(
      'sidebarId' = sidebarId,
      'mainPanelId' = mainPanelId,
      'show' = sidebarVisible()
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message
    session$sendCustomMessage('sidebarToggle', messageJSON)
    
    # Determine correct toggling button label
    newBtnLabel <- buttonLabelShow
    if (sidebarVisible()) newBtnLabel <- buttonLabelHide
    
    # Update toggling button label
    updateActionButton(session, buttonInputId, label = newBtnLabel)
  })
}


pointHoverWidgetServer <- function(session, plotId, df, input,
                                   x_label = NULL, y_label = NULL,
                                   override.mapping = NULL, threshold = 5) {
  observeEvent(input(), {
    plotId <- session$ns(plotId)
    mapping <- input()$mapping
    
    if (length(mapping) > 0) {
      point <- nearPoints(df(), input(), maxpoints = 1, threshold = threshold)
      
      if (dim(point)[1] == 1) {
        
        if (typeof(override.mapping) == 'list') {
          if (!is.null(override.mapping$x)) mapping$x <- override.mapping$x
          if (!is.null(override.mapping$y)) mapping$y <- override.mapping$y
        }
        
        pointInfo <- point %>% select(Site_ID, mapping$x, mapping$y)
        
        x_y_labels = list(
          'x' = mapping$x,
          'y' = mapping$y
        )
        
        if (!is.null(x_label)) {
          if (x_label %in% colnames(point)) {
            x_y_labels$x <- point %>% pull(x_label)
          } else {
            x_y_labels$x <- x_label
          }
        }
        
        if (!is.null(y_label)) {
          if (y_label %in% colnames(point)) {
            x_y_labels$y <- point %>% pull(y_label)
          } else {
            x_y_labels$y <- y_label
          }
        }
        
        messageJSON <- toJSON(list(
          'pointInfo' = unbox(pointInfo),
          'mapping' = mapping,
          'coords_img' = input()$coords_img,
          'x_y_labels' = x_y_labels,
          'plotId' = plotId
        ), auto_unbox = TRUE)
        
        session$sendCustomMessage('addHoverWidget', messageJSON)
        return()
      }
    }
    
    
    messageJSON <- toJSON(list(
      'plotId' = plotId
    ), auto_unbox = TRUE)
    
    session$sendCustomMessage('removeHoverWidget', messageJSON)
    
  }, ignoreNULL = FALSE)
}
