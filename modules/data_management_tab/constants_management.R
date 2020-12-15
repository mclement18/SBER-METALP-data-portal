## This module contains the UI and server code for the constants management tab

## Create module UI ###############################################################

constantsManagementUI <- function(id) {
# Create the UI for the constantsManagement module
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
      htmlTemplate('./html_components/constants_info.html'),
      initStateHidden = TRUE
    ),
    editableDTUI(ns('constants'))
  )
}



## Create module server function ##################################################

constantsManagement <- function(input, output, session, pool) {
# Create the logic for the constantsManagement module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'constants', pool = pool, tableName = 'constants', element = 'constant',
             tableLoading = expression({
               getRows(pool, 'constants') %>%
                 # Cast data types
                 mutate(
                   across(ends_with('_at'), ymd_hms)
                 )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 name, unit, value, description
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id,name, unit, value, description
               )
             ),
             creationExpr = expression(
               createConstant(
                 pool = pool,
                 name = input$name,
                 unit = input$unit,
                 value = input$value,
                 description = input$description
               )
             ),
             updateExpr = expression(
               updateConstant(
                 pool = pool,
                 constant = editedRow(),
                 name = input$name,
                 unit = input$unit,
                 value = input$value,
                 description = input$description
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'constants',
                 ids = selectedRowIds
               )
             )
  )
}
