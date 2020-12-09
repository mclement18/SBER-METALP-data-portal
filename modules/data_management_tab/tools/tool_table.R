## This module contains the UI and server code for the tool table

## Create module UI function ######################################################

toolTableUI <- function(id) {
# Create the UI for the toolTable module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  rHandsontableOutput(ns('toolTable'))
}



## Create module server function ##################################################

toolTable <- function(input, output, session, df, replicates = FALSE, canUpdate = TRUE, readOnly = FALSE) {
# Create the logic for the toolTable module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - df: Reactive expression, returns a data.frame with the data to put in the table
#  - replicates: Boolean, does the df contains replicates
#  - canUpdate: Boolean, authorize data update, have no effect if readOnly = TRUE
#  - readOnly: Boolean, if table is read only
# 
# Returns a reactive expression containing the updated df unless readOnly = TRUE
  
  ## JavaScript callback functions for the Handsontable ###########################
  
  # JavaScript logic in './assets/js/custom_handsontable.js'
  
  # Callback that register table hooks
  onTableRender <- 'CustomHandsontable.toolTableOnRenderCallback'
  
  # Callback to render the numbers up to the 4th decimal if any
  toolTableRenderer <- 'CustomHandsontable.toolTableRenderer'
  
  
  
  
  
  
  ## Render table #################################################################
  
  output$toolTable <- renderRHandsontable({
    df <- df()
    
    if (replicates) {
      df %<>% pivot_longer(everything(), names_to = c('Parameter', '.value'), names_pattern = '(.*)_(.*)')
    } else {
      df %<>% pivot_longer(everything(), names_to = 'Parameter', values_to = 'Value')
    }
    
    # Create handsontable
    rhandsontable(
      df,
      readOnly = readOnly,
      rowHeaders = NULL,
      contextMenu = FALSE
    ) %>%
      # Set the custom renderer globally
      hot_cols(renderer = toolTableRenderer) %>%
      # Hide and mark as read only the id
      hot_col('Parameter', readOnly = TRUE, type = 'text') %>%
      # Add hooks callback to the table
      htmlwidgets::onRender(
        onTableRender,
        data = list(
          canUpdate = canUpdate,
          readOnly = readOnly
        )
      )
  })
  
  
  
  
  
  
  
  ## Return df ####################################################################
  
  return(
    reactive({
      req(input$toolTable)
      # Get table data
      updatedDf <- hot_to_r(input$toolTable)
      
      # Pivot wider the table depending on the replicates
      if (replicates) {
        updatedDf %<>% pivot_wider(names_from = Parameter, values_from = -Parameter,  names_glue = '{Parameter}_{.value}')
      } else {
        updatedDf %<>% pivot_wider(names_from = Parameter, values_from = Value)
      }
      
      # Send back updated df
      if (readOnly) {
        df()
      } else if (canUpdate) {
        updatedDf
      } else {
        # If cannot update already set values
        # Get initial df
        df <- df()
        
        # Add a reference column to both df
        df %<>% mutate(joining_col = 1)
        updatedDf %<>% mutate(joining_col = 1)
        
        # Return a df with only the authorized updates
        coalesce_join(df, updatedDf, by = 'joining_col') %>% select(-joining_col)
      }
    })
  )
}
