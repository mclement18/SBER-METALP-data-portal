## This module contains the UI and server code for the standard curves management tab

## Create module UI ###############################################################

standardCurvesUI <- function(id, pool) {
# Create the UI for the standardCurves module
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
      htmlTemplate('./html_components/standard_curves_info.html'),
      initStateHidden = TRUE
    ),
    h3('Filter by:'),
    div(
      class = 'data-filter',
      selectInput(
        ns('parameterFilter'),
        'Parameter',
        choices = c(
          'All',
          parseOptions(
            getRows(pool, 'standard_curves', columns = 'parameter'),
            'parameter'
          )
        )
      )
    ),
    editableDTUI(ns('standardCurves'))
  )
}



## Create module server function ##################################################

standardCurves <- function(input, output, session, pool) {
# Create the logic for the standardCurves module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'standardCurves', pool = pool, tableName = 'standard_curves', element = 'standard curve',
             tableLoading = expression({
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$parameterFilter from the current module
               if (parameterFilter() == 'All') {
                 table <- getRows(pool, 'standard_curves')
               } else {
                 table <- getRows(pool, 'standard_curves', parameter == local(parameterFilter()))
               }
               
               table %>%
                 # Cast data types
                 mutate(
                   date = as_date(date),
                   parameter = as.factor(parameter),
                   across(ends_with('_at'), ymd_hms)
                 )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 date, parameter, a, b
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id, date, parameter, a, b
               )
             ),
             creationExpr = expression(
               createStandardCurve(
                 pool = pool,
                 date = input$date,
                 parameter = input$parameter,
                 a = input$a,
                 b = input$b
               )
             ),
             updateExpr = expression(
               updateStandardCurve(
                 pool = pool,
                 standardCurve = editedRow(),
                 date = input$date,
                 parameter = input$parameter,
                 a = input$a,
                 b = input$b
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'standard_curves',
                 ids = selectedRowIds
               )
             ),
             # Pass the current module inputs as an additional reactive expression which are used in the above expressions
             parameterFilter = reactive(input$parameterFilter)
  )
}
