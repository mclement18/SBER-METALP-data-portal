## This module contains the UI and server code for the grab param categories management tab

## Create module UI ###############################################################

grabParamCategoriesUI <- function(id, pool) {
# Create the UI for the grabParamCategories module
# Parameters:
#  - id: String, the module id
#  - pool: The pool connection to the database
# 
# Returns a tagList with the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return the layout
  tagList(
    instructionsPanelUI(
      ns('info'),
      htmlTemplate('./html_components/grab_param_categories_tab_info.html'),
      initStateHidden = TRUE
    ),
    selectInput(
      ns('categoryFilter'),
      'Filter by category',
      choices = c(
        'All',
        parseOptions(
          getRows(pool, 'grab_param_categories', columns = 'category'),
          'category'
        )
      )
    ),
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
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'gPCat', pool = pool, tableName = 'grab_param_categories', element = 'grab param category',
             tableLoading = expression({
               # Get rows
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$categoryFilter from the current module
               if (categoryFilter() == 'All') {
                 table <- getRows(pool, 'grab_param_categories')
               } else {
                 # Use local to evaluate the categoryFilter reactive expression in the editableDT module and not in the DB context
                 table <- getRows(pool, 'grab_param_categories', category == local(categoryFilter()))
               }
               # Cast data types
               table %>% mutate(
                 across(ends_with('_at'), ymd_hms)
               )
             }),
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
             ),
             # Pass the categoryFilter input as an additional reactive expression which is used in the above expressions
             categoryFilter = reactive(input$categoryFilter)
             )
}
