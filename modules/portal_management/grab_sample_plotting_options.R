## This module contains the UI and server code for the grab sample plotting options management tab

## Create module UI ###############################################################

gbPlotOptionsUI <- function(id, pool) {
# Create the UI for the gbPlotOptions module
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
      htmlTemplate('./html_components/grab_params_info.html'),
      initStateHidden = TRUE
    ),
    selectInput(
      ns('sectionFilter'),
      'Filter by section_name',
      choices = c(
        'All',
        getRows(pool, 'grab_params_plotting', columns = 'section_name') %>%
          pull() %>%
          unique()
      )
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
             tableLoading = expression({
               # Get rows
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$sectionFilter from the current module
               if (sectionFilter() == 'All') {
                 table <- getRows(pool, 'grab_params_plotting')
               } else {
                 # Use local to evaluate the sectionFilter reactive expression in the editableDT module and not in the DB context
                 table <- getRows(pool, 'grab_params_plotting', section_name == local(sectionFilter()))
               }
               # Cast data types
               table %>% mutate(
                 plot_func = as.factor(plot_func),
                 active = as.logical(active),
                 across(ends_with('_at'), ymd_hms)
               )
             }),
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
                 description,
                 active
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
                 description,
                 active
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
                 description = input$description,
                 active = input$active
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
                 description = input$description,
                 active = input$active
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
             ),
             # Pass the sectionFilter input as an additional reactive expression which is used in the above expressions
             sectionFilter = reactive(input$sectionFilter)
             )
}
