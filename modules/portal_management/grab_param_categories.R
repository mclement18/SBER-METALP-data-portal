## This module contains the UI and server code for the grab param categories management tab

## Create module UI ###############################################################

grabParamCategoriesUI <- function(id) {
  # Create the UI for the grabParamCategories module
  # Parameters:
  #  - id: String, the module id
  # 
  # Returns a tagList with the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return the layout
  tagList(
    htmlTemplate('./html_components/grab_param_categories_tab_info.html'),  
    editableDTUI(ns('gPCat'))
  )
}



## Create module server function ##################################################

grabParamCategories <- function(input, output, session, pool) {
  # Create the logic for the grabParamCategories module
  # Parameters:
  #  - input, output, session: Default needed parameters to create a module
  #  - pool: The pool connection to the database
  # 
  # Returns NULL
  
  # Call editableDT module
  callModule(editableDT, 'gPCat', pool = pool, tableName = 'grab_param_categories', element = 'grab param category',
             tableLoading = expression(
               getRows(pool, 'grab_param_categories') %>%
                 # Cast data types
                 mutate(
                   across(ends_with('_at'), ymd_hms)
                 )
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(category, param_name, description)
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id, category, param_name, description)
             ),
             creationExpr = expression(
               createGrabParamCat(
                 pool = pool,
                 category = input$category,
                 param_name = input$param_name,
                 description = input$description
               )
             ),
             updateExpr = expression(
               updateGrabParamCat(
                 pool = pool,
                 grabParamCat = editedRow(),
                 category = input$category,
                 param_name = input$param_name,
                 description = input$description
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'grab_param_categories',
                 ids = selectedRowIds
               )
             ))
}
