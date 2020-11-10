## This module contains the UI and server code for the grab sample plotting options management tab

## Create module UI ###############################################################

gbPlotOptionsUI <- function(id) {
# Create the UI for the gbPlotOptions module
# Parameters:
#  - id: String, the module id
# 
# Returns a tagList with the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return the layout
  tagList(
    instructionsPanelUI(
      ns('info'),
      htmlTemplate('./html_components/grab_params_info.html'),
      initStateHidden = TRUE
    ),
    editableDTUI(ns('gbPlotOptions'))
  )
}



## Create module server function ##################################################

gbPlotOptions <- function(input, output, session, pool) {
# Create the logic for the gbPlotOptions module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  # Call editableDT module
  callModule(editableDT, 'gbPlotOptions', pool = pool, tableName = 'grab_params_plotting', element = 'plotted grab parameter',
             tableLoading = expression(
               getRows(pool, 'grab_params_plotting') %>%
                 # Cast data types
                 mutate(
                   section_name = as.factor(section_name),
                   plot_func = as.factor(plot_func),
                   across(ends_with('_at'), ymd_hms)
                 )
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 section_name,
                 option_name,
                 param_name,
                 units,
                 data,
                 sd,
                 min_max,
                 plot_func,
                 description
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id,
                 section_name,
                 option_name,
                 param_name,
                 units,
                 data,
                 sd,
                 min_max,
                 plot_func,
                 description
               )
             ),
             creationExpr = expression(
               createGbPlotOption(
                 pool = pool,
                 section_name = input$section_name,
                 option_name = input$option_name,
                 param_name = input$param_name,
                 units = input$units,
                 data = input$data,
                 sd = input$sd,
                 min_max = input$min_max,
                 plot_func = input$plot_func,
                 description = input$description
               )
             ),
             updateExpr = expression(
               updateGbPlotOption(
                 pool = pool,
                 gbPlotOption = editedRow(),
                 section_name = input$section_name,
                 option_name = input$option_name,
                 param_name = input$param_name,
                 units = input$units,
                 data = input$data,
                 sd = input$sd,
                 min_max = input$min_max,
                 plot_func = input$plot_func,
                 description = input$description
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'grab_params_plotting',
                 ids = selectedRowIds
               )
             ),
             outputTableExpr = expression(
               loadedTable %>% mutate(
                 across(c(data, sd, min_max), ~gsub(',', ', ', .x))
               )
             ))
}
