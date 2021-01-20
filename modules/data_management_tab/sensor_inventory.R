## This module contains the UI and server code for the sensor inventory management tab

## Create module UI ###############################################################

sensorInventoryUI <- function(id, pool) {
# Create the UI for the sensorInventory module
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
      htmlTemplate('./html_components/sensor_inventory_info.html'),
      initStateHidden = TRUE
    ),
    h3('Filter by:'),
    div(
      class = 'data-filter',
      selectInput(
        ns('siteFilter'),
        'Station',
        choices = c(
          list(
            All = list(
              All = 'All'
            )
          ),
          parseOptionsWithSections(
            getRows(pool, 'stations', columns = c('order', 'name', 'full_name', 'catchment')) %>%
              arrange(order) %>% select(-order),
            valueColumn = 'name', sectionColumn = 'catchment', optionColumn = 'full_name'
          )
        )
      ),
      selectInput(
        ns('paramFilter'),
        'Parameter',
        choices = c(
          'All',
          parseOptions(
            getRows(pool, 'sensor_inventory', columns = 'param_name'),
            'param_name'
          )
        )
      )
    ),
    editableDTUI(ns('sensors'))
  )
}



## Create module server function ##################################################

sensorInventory <- function(input, output, session, pool) {
# Create the logic for the sensorInventory module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns NULL
  
  # Call instruction panel module
  callModule(instructionsPanel, 'info', initStateHidden = TRUE)
  
  
  # Call editableDT module
  callModule(editableDT, 'sensors', pool = pool, tableName = 'sensor_inventory', element = 'sensor',
             tableLoading = expression({
               # Use the reactive expression passed to the '...' as additional argument
               # To access the input$categoryFilter from the current module
               if (paramFilter() == 'All') {
                 params <- getRows(pool, 'sensor_inventory', columns = 'param_name') %>% pull()
               } else {
                 params <- paramFilter()
               }
               
               # Get rows
               if (siteFilter() == 'All') {
                 rows <- getRows(pool, 'sensor_inventory', param_name %in% params)
               } else {
                 rows <- getRows(pool, 'sensor_inventory', param_name %in% params, station %in% local(siteFilter()))
               }
               
               # Cast data types
               rows %>% mutate(
                 installation_date = ymd(installation_date),
                 in_field = as.logical(in_field),
                 across(ends_with('_at'), ymd_hms)
               )
             }),
             templateInputsCreate = expression(
               inputsTemplate %>% select(
                 station, param_name, param_full, model, serial_nb,
                 installation_date, in_field, calibration_a, calibration_b, description
               )
             ),
             templateInputsEdit = expression(
               selectedRow %>% select(
                 id, station, param_name, param_full, model, serial_nb,
                 installation_date, in_field, calibration_a, calibration_b, description
               )
             ),
             creationExpr = expression(
               createSensor(
                 pool = pool,
                 station = input$station,
                 param_name = input$param_name,
                 param_full = input$param_full,
                 model = input$model,
                 serial_nb = input$serial_nb,
                 installation_date = input$installation_date,
                 in_field = input$in_field,
                 calibration_a = input$calibration_a,
                 calibration_b = input$calibration_b,
                 description = input$description
               )
             ),
             updateExpr = expression(
               updateSensor(
                 pool = pool,
                 sensor = editedRow(),
                 station = input$station,
                 param_name = input$param_name,
                 param_full = input$param_full,
                 model = input$model,
                 serial_nb = input$serial_nb,
                 installation_date = input$installation_date,
                 in_field = input$in_field,
                 calibration_a = input$calibration_a,
                 calibration_b = input$calibration_b,
                 description = input$description
               )
             ),
             deleteExpr = expression(
               deleteRows(
                 pool = pool,
                 table = 'sensor_inventory',
                 ids = selectedRowIds
               )
             ),
             # Pass the current module inputs as an additional reactive expression which are used in the above expressions
             siteFilter = reactive(input$siteFilter),
             paramFilter = reactive(input$paramFilter)
  )
}
