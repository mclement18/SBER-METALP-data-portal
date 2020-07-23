# Functions that extend shiny default functions ###################################

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


pointHoverWidgetServer <- function(session, plotId, df, input, threshold = 5) {
  observeEvent(input(), {
    plotId <- session$ns(plotId)
    mapping <- input()$mapping
    
    if (length(mapping) > 0) {
      point <- nearPoints(df(), input(), maxpoints = 1, threshold = threshold) %>% 
        select(mapping$x, mapping$y)
      
      if (dim(point)[1] == 1) {
        messageJSON <- toJSON(list(
          'pointInfo' = unbox(point),
          'hoverInfo' = input(),
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
