## This module contains the UI and server code for the sensor plotting options management tab

## Create module UI ###############################################################

sensorPlotOptionsUI <- function(id) {
# Create the UI for the sensorPlotOptions module
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
      htmlTemplate('./html_components/sensor_params_info.html'),
      initStateHidden = TRUE
    ),
    editableDTUI(ns('sensorPlotOptions'))
  )
}



## Create module server function ##################################################

sensorPlotOptions <- function(input, output, session, pool) {
# Create the logic for the sensorPlotOptions module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  # Call editableDT module
  callModule(editableDT, 'sensorPlotOptions', pool = pool, tableName = 'sensor_params_plotting', element = 'plotted sensor parameter',
             tableLoading = expression(
               getRows(pool, 'sensor_params_plotting') %>%
                 # Cast data types
                 mutate(
                   section_name = as.factor(section_name),
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
                 grab_param_name,
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
                 grab_param_name,
                 description
               )
             ),
             creationExpr = expression(
               createSensorPlotOption(
                 pool = pool,
                 section_name = input$section_name,
                 option_name = input$option_name,
                 param_name = input$param_name,
                 units = input$units,
                 data = input$data,
                 grab_param_name = input$grab_param_name,
                 description = input$description
               )
             ),
             updateExpr = expression(
               updateSensorPlotOption(
                 pool = pool,
                 sensorPlotOption = editedRow(),
                 section_name = input$section_name,
                 option_name = input$option_name,
                 param_name = input$param_name,
                 units = input$units,
                 data = input$data,
                 grab_param_name = input$grab_param_name,
                 description = input$description
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'sensor_params_plotting',
                 ids = selectedRowIds
               )
             ),
             outputTableExpr = expression(
               loadedTable %>% mutate(
                 grab_param_name = gsub(',', ', ', grab_param_name)
               )
             ))
}
