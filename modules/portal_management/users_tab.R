## This module contains the UI and server code for the users management tab

## Create module UI ###############################################################

usersTabUI <- function(id) {
# Create the UI for the usersTab module
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
      htmlTemplate(
        './html_components/user_tab_info.html',
        download = roleToIcon('download'),
        intern = roleToIcon('intern'),
        sber = roleToIcon('sber'),
        admin = roleToIcon('admin')
      ),
      initStateHidden = TRUE
    ),
    editableDTUI(ns('users'))
  )
}



## Create module server function ##################################################

usersTab <- function(input, output, session, pool) {
# Create the logic for the usersTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  # Call editableDT module
  callModule(editableDT, 'users', pool = pool, tableName = 'users', element = 'user',
             tableLoading = expression(
               getUsers(pool) %>%
                 # Cast data types
                 mutate(
                   role = as.factor(role),
                   across(ends_with('_at'), ymd_hms),
                   active = as.logical(active),
                   intern_confirmation = as.logical(intern_confirmation)
                 )
             ),
             templateInputsCreate = expression(
               inputsTemplate %>% select(name, role, active, intern_confirmation) %>% mutate(password = character(0))
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(id, name, role, active, intern_confirmation) %>% mutate(password = as.character(NA))
             ),
             creationExpr = expression(
               createUser(
                 pool = pool,
                 username = input$name,
                 password = input$password,
                 role = input$role,
                 active = input$active,
                 intern_confirmation = input$intern_confirmation
               )
             ),
             updateExpr = expression(
               updateUser(
                 pool = pool,
                 user = editedRow(),
                 username = input$name,
                 password = input$password,
                 role = input$role,
                 active = input$active,
                 intern_confirmation = input$intern_confirmation
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'users',
                 ids = selectedRowIds
               )
             ))
}
