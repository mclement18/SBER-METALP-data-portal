## This module contains the UI and server code for the notes management tab

## Create module UI ###############################################################

notesUI <- function(id) {
# Create the UI for the notes module
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
      htmlTemplate('./html_components/notes_info.html'),
      initStateHidden = TRUE
    ),
    editableDTUI(ns('notes'))
  )
}



## Create module server function ##################################################

notes <- function(input, output, session, pool) {
# Create the logic for the notes module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'notes', pool = pool, tableName = 'notes', element = 'note',
             tableLoading = expression({
               getRows(pool, 'notes') %>%
                 # Cast data types
                 mutate(
                   station = factor(
                     station,
                     getRows(pool, 'stations', columns = c('order', 'name')) %>%
                       arrange(order) %>% pull(name)
                   ),
                   verified = as.logical(verified),
                   across(ends_with('_at'), ymd_hms)
                 )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 station, text, verified
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id, station, text, verified
               )
             ),
             creationExpr = expression(
               createNote(
                 pool = pool,
                 station = input$station,
                 text = input$text,
                 verified = input$verified
               )
             ),
             updateExpr = expression(
               updateNote(
                 pool = pool,
                 note = editedRow(),
                 station = input$station,
                 text = input$text,
                 verified = input$verified
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'notes',
                 ids = selectedRowIds
               )
             )
  )
}
