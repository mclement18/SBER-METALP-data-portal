## This module contains the UI and server code for the parameter calculations

## Create module UI ###############################################################

parameterCalculationsUI <- function(id, pool) {
# Create the UI for the parameterCalculations module
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
      htmlTemplate('./html_components/parameter_calculations_info.html'),
      initStateHidden = TRUE
    ),
    h3('Filter by:'),
    div(
      class = 'data-filter',
      selectInput(
        ns('categoryFilter'),
        'Parameter category',
        choices = c(
          'All',
          parseOptions(
            getRows(pool, 'grab_param_categories', columns = 'category'),
            'category'
          )
        )
      )
    ),
    editableDTUI(ns('calculations'))
  )
}



## Create module server function ##################################################

parameterCalculations <- function(input, output, session, pool) {
# Create the logic for the parameterCalculations module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'calculations', pool = pool, tableName = 'parameter_calculations', element = 'calculation',
             tableLoading = expression({
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$categoryFilter from the current module
               if (categoryFilter() == 'All') {
                 calculations <- getRows(pool, 'parameter_calculations')
               } else {
                 calculations <- getRows(pool, 'parameter_calculations', param_category %in% local(categoryFilter()))
               }
               
               # Cast data types
               calculations %>% mutate(
                 across(ends_with('_at'), ymd_hms)
               )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 param_category, column_calculated, calcul_func, columns_used
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id, param_category, column_calculated, calcul_func, columns_used
               )
             ),
             creationExpr = expression(
               createCalculation(
                 pool = pool,
                 param_category = input$param_category,
                 column_calculated = input$column_calculated,
                 calcul_func = input$calcul_func,
                 columns_used = input$columns_used
               )
             ),
             updateExpr = expression(
               updateCalculation(
                 pool = pool,
                 calculation = editedRow(),
                 param_category = input$param_category,
                 column_calculated = input$column_calculated,
                 calcul_func = input$calcul_func,
                 columns_used = input$columns_used
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'parameter_calculations',
                 ids = selectedRowIds
               )
             ),
             # Pass the current module inputs as an additional reactive expression which are used in the above expressions
             categoryFilter = reactive(input$categoryFilter)
  )
}
