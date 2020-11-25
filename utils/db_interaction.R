## This script contains all the database queries and connections

## Database connection ############################################################

connectToDB <- function() {
  # Create pool connection
  pool <- dbPool(
    drv = RMySQL::MySQL(),
    dbname = MY_DB_NAME,
    host = MY_DB_HOST,
    port = MY_DB_PORT,
    username = MY_DB_USER,
    password = MY_DB_PWD
  )
  
  # Set charset for connection
  dbGetQuery(pool, "SET NAMES 'utf8';")
  
  # Return pool
  return(pool)
}



## Parameters validation and parsing #######################################################

# Decorator function using tinsel package
validInputDecorator <- function(f) {
  function(input, oldValue = NULL, ...) {
    # Check for keyword to delete value
    if (!is.null(input)) {
      if (!is.na(input) & as.character(input) == 'NULL') return(SQL(input))
    }
    
    # Run validation function
    input <- f(input, ...)
    
    # Use oldValue if provided in case of NULL
    if (!is.null(oldValue) & input == SQL('NULL')) {
      oldValue
    } else {
      input
    }
  }
}

# Decorate function
validInputString <- validInputDecorator(
  function(input) {
    if (!is.character(input)) return(SQL('NULL'))
    
    if (input == '' | any(is.na(input)) | length(input) != 1) return(SQL('NULL'))
    
    return(input)
  }
)



# Decorate function
validInputDate <- validInputDecorator(
  function(input) {
    if (!is.Date(input)) input <- as_date(input)
    
    if (any(is.na(input)) | length(input) != 1) return(SQL('NULL'))
    
    return(as.character(input))
  }
)



# Decorate function
validInputNumber <- validInputDecorator(
  function(input, int = FALSE) {
    if (!is.numeric(input)) input <- as.numeric(input)
    
    if (any(is.na(input)) | length(input) != 1) return(SQL('NULL'))
    
    if (int & !is.integer(input)) return(as.integer(input))
    
    return(input)
  }
)



# Decorate function
validInputBool <- validInputDecorator(
  function(input) {
    if (!is.logical(input)) input <- as.numeric(input)
    
    if (any(is.na(input)) | length(input) != 1) return(SQL('NULL'))
    
    return(input)
  }
)



sqlInterpolateList <- function(conn, sql, vars=list(), list_vars=list()) {
  if (length(list_vars) > 0) {
    for (name in names(list_vars)) {
      sql <- sub(paste0("\\?", name), paste("?", name, "_list_var", 1:length(list_vars[[name]]), sep="", collapse=","), sql)
    }
    list_vars <- lapply(list_vars, function(sublist) {
      names(sublist) <- paste0("list_var", 1:length(sublist))
      sublist
    }) %>% unlist()
    # unlist gives names as "outer.inner" but DBI doesn't like names with periods
    names(list_vars) <- sub("\\.", "_", names(list_vars))
    vars <- c(vars, list_vars)
  }
  DBI::sqlInterpolate(conn, sql, .dots=vars)
}


sendQueryWithError <- function(pool, query) {
  # Send Query and catch errors
  result <- tryCatch(
    dbGetQuery(pool, query),
    error = function(e) return(e$message)
  )
  
  # Check if insertion succeed (i.e. empty df)
  # If not return the error message
  if (is.data.frame(result)) {
    return('')
  } else {
    return(result)
  }
}




## General queries ##############################################################

getEnumValues <- function(pool, table, column) {
  # Create query
  query <- sqlInterpolate(
    pool,
    "SELECT COLUMN_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ?table AND COLUMN_NAME = ?column AND DATA_TYPE = 'enum';",
    table = table, column = column
  )
  
  # Run query
  result <- dbGetQuery(pool, query)
  
  # If not empty, parse the info
  if (nrow(result) == 1) {
    result[1, 1] %>% str_extract_all("(?<=')[^,]+(?=')") %>% unlist()
  } else {
    NULL
  }
}




getMinMaxValues <- function(pool, table, column, ...) {
  # Enquote the column name
  column <- enquo(column)
  
  # Get the min and max values
  pool %>% tbl(table) %>%
    filter(...) %>%
    summarise(
      min = min(!!column, na.rm = TRUE),
      max = max(!!column, na.rm = TRUE)
    ) %>%
    collect()
}



countRows <- function(pool, table, ...) {
  # Count the rows
  countDf <- pool %>% tbl(table) %>% filter(...) %>% summarise(nb = n()) %>% collect()
  
  # Return only the count
  countDf$nb
}



getRows <- function(pool, table, ..., columns = NULL) {
  # Start query by selecting the table and filtering it
  query <- pool %>% tbl(table) %>% filter(...)
  
  # If some columns are provided, select only them
  if (!is.null(columns)) {
    query %<>% select(all_of(columns))
  }
  
  # Perform query
  query %>% collect()
}




deleteRows <- function(pool, table, ids) {
  # Perform deletion only if ids is not NULL, numeric, not empty and does not contains NA
  if (is.null(ids)) {
    error <- 'Error: IDs cannot be NULL.'
  } else if (!is.numeric(ids)) {
    error <- 'Error: IDs should be a numeric vector.'
  } else if (length(ids) == 0) {
    error <- 'Error: IDs cannot be empty.'
  } else if (any(is.na(ids))) {
    error <- 'Error: IDs cannot be NA.'
  } else {
    # If ids is of length 1, make a specific query,
    # Otherwise, make a IN query
    if (length(ids) == 1) {
      query <- sqlInterpolate(
        pool,
        "DELETE FROM ?table WHERE id = ?id;",
        table = dbQuoteIdentifier(pool, table), id = ids
      )
    } else {
      query <- sqlInterpolateList(
        pool,
        "DELETE FROM ?table WHERE id IN (?ids);",
        vars = list(table = dbQuoteIdentifier(pool, table)),
        list_vars = list(ids = ids)
      )
    }
    # Send Query and catch errors
    return(sendQueryWithError(pool, query))
  }
  
  # If error return the error message
  return(error)
}




## User queries ###################################################################

# Get user for login
loginUser <- function(pool, username) {
  pool %>% tbl('users') %>% filter(name == username, active == 1) %>% select(name, password, role) %>% head(1) %>% collect()
}



# Get all users
getUsers <- function(pool, columns = NULL) {
  # Initiate query with users table
  query <- pool %>% tbl('users')
  
  # Select columns if needed
  if (!is.null(columns)) {
    query %<>% select(all_of(columns), -password)
  } else {
    query %<>% select(-password)
  }
  
  # Perform query
  query %>% collect()
}



# Create a new user
createUser <- function(pool, username, password, role = 'sber', active = TRUE) {
  # Check for valid input string
  username <- validInputString(username)
  password <- validInputString(password)
  role <- validInputString(role)
  active <- validInputBool(active)
  
  # Hash password before saving
  if (password != SQL('NULL')) {
    hashedPassword <- sodium::password_store(password)
  } else {
    hashedPassword <- SQL('NULL')
  }
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO users (name, password, role, active) values(?username, ?hashedPassword, ?role, ?active);',
    username = username, hashedPassword = hashedPassword, role = role, active = active
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}



updateUser <- function(pool, user, username = '', password = '', role = '', active = TRUE) {
  # Check for valid input string
  username <- validInputString(username, user$name)
  password <- validInputString(password)
  role <- validInputString(role, user$role)
  active <- validInputBool(active, user$active)
  
  # UPdate password only if a new one is provided
  if (password == SQL('NULL')) {
    # Create query without password
    query <- sqlInterpolate(
      pool,
      "UPDATE users SET name = ?name, role = ?role, active = ?active WHERE id = ?id;",
      id = user$id, name = username, role = role, active = active
    )
  } else {
    # Hash the new password
    hashedPassword <- sodium::password_store(password)
    
    # Create query with password
    query <- sqlInterpolate(
      pool,
      "UPDATE users SET name = ?name, password = ?password, role = ?role, active = ?active WHERE id = ?id;",
      id = user$id, name = username, password = hashedPassword, role = role, active = active
    )
  }
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}






## Data queries ######################################################################

createData <- function(pool, station, DATE_reading, TIME_reading, Convert_to_GMT, TIME_reading_GMT) {
  # Check for valid input string
  station <- validInputString(station)
  DATE_reading <- validInputString(DATE_reading)
  TIME_reading <- validInputString(TIME_reading)
  Convert_to_GMT <- validInputString(Convert_to_GMT)
  TIME_reading_GMT <- validInputString(TIME_reading_GMT)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO data
    (station, DATE_reading, TIME_reading, Convert_to_GMT, TIME_reading_GMT)
    values(?station, ?DATE_reading, ?TIME_reading, ?Convert_to_GMT, ?TIME_reading_GMT);',
    station = station, DATE_reading = DATE_reading, TIME_reading = TIME_reading,
    Convert_to_GMT = Convert_to_GMT, TIME_reading_GMT = TIME_reading_GMT
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}



updateData <- function(pool, id, columns, values) {
  browser()
  # Check for NA in inputs
  if (any(is.na(id)) | any(is.na(columns)) | any(is.na(values))) return('Inputs cannot contain NA.')
  # Validate id
  if (!is.numeric(id)) return('Id must be an integer.')
  if (length(id) != 1) return('Id length must 1.')
  # Validate columns
  if (!is.character(columns)) return('Columns must be a character vector.')
  if ('id' %in% columns) return('Columns to update cannot contain id.')
  # Validate values
  if (length(columns) != length(values)) return('Must provide the same number of columns and values.')
  for (i in 1:length(values)) {
    # If the value id an empty string ignore changes
    if (values[[i]] == '') {
      values <- values[-i]
      columns <- columns[-i]
      # Set value to SQL NULL (i.e. delete) only when explicitly asked via 'NULL' string
    } else if (values[[i]] == 'NULL') {
      values[[i]] <- SQL('NULL')
    }
  }
  
  # Build base sql query
  sql <- 'UPDATE data SET ?values WHERE id = ?id;'
  
  # Build complete query to interpolate
  for (i in c(1:length(columns))) {
    sql <- sub("\\?values", paste("?column", 1:length(columns), " = ?value", 1:length(values), sep="", collapse=","), sql)
  }
  
  # Quote column names
  columns <- lapply(columns, function(column) {
    dbQuoteIdentifier(pool, column)
  })
  
  # Set variables names
  names(columns) <- paste0("column", 1:length(columns))
  names(values) <- paste0("value", 1:length(values))
  names(id) <- 'id'
  
  # Create variables list
  vars <- c(id, columns, values)
  
  # Interpolate SQL
  query <- sqlInterpolate(pool, sql, .dots=vars)
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}






## Stations queries ###################################################################

createStation <- function(pool, name, full_name, catchment, color, elevation = '') {
  # Check for valid input string
  name <- validInputString(name)
  full_name <- validInputString(full_name)
  catchment <- validInputString(catchment)
  color <- validInputString(color)
  elevation <- validInputNumber(elevation, int = TRUE)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO stations (name, full_name, catchment, color, elevation)
    values(?name, ?full_name, ?catchment, ?color, ?elevation);',
    name = name, full_name = full_name, catchment = catchment,
    color = color, elevation = elevation
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateStation <- function(pool, station, name = '', full_name = '', catchment = '', color = '', elevation = '') {
  # Check for valid input string
  name <- validInputString(name, station$name)
  full_name <- validInputString(full_name, station$full_name)
  catchment <- validInputString(catchment, station$catchment)
  color <- validInputString(color, station$color)
  elevation <- validInputNumber(elevation, station$elevation, int = TRUE)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    "UPDATE stations SET name = ?name, full_name = ?full_name,
    catchment = ?catchment, color = ?color, elevation = ?elevation
    WHERE id = ?id;",
    id = station$id, name = name, full_name = full_name,
    catchment = catchment, color = color, elevation = elevation
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}






## Grab sample plotting options queries ###################################################################

createGbPlotOption <- function(pool, section_name, option_name, param_name, units, data,
                               sd = '', min_max = '', plot_func, description = '') {
  # Check for valid input string
  section_name <- validInputString(section_name)
  option_name <- validInputString(option_name)
  param_name <- validInputString(param_name)
  units <- validInputString(units)
  data <- validInputString(data)
  sd <- validInputString(sd)
  min_max <- validInputString(min_max)
  plot_func <- validInputString(plot_func)
  description <- validInputString(description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO grab_params_plotting
    (section_name, option_name, param_name, units, data, sd, min_max, plot_func, description)
    values(?section_name, ?option_name, ?param_name, ?units, ?data, ?sd, ?min_max, ?plot_func, ?description);',
    section_name = section_name, option_name = option_name, param_name = param_name, units = units,
    data = data, sd = sd, min_max = min_max, plot_func = plot_func, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateGbPlotOption <- function(pool, gbPlotOption, section_name = '', option_name = '', param_name = '', units = '', data = '',
                               sd = '', min_max = '', plot_func = '', description = '') {
  # Check for valid input string
  section_name <- validInputString(section_name, gbPlotOption$section_name)
  option_name <- validInputString(option_name, gbPlotOption$option_name)
  param_name <- validInputString(param_name, gbPlotOption$param_name)
  units <- validInputString(units, gbPlotOption$units)
  data <- validInputString(data, gbPlotOption$data)
  sd <- validInputString(sd, gbPlotOption$sd)
  min_max <- validInputString(min_max, gbPlotOption$min_max)
  plot_func <- validInputString(plot_func, gbPlotOption$plot_func)
  description <- validInputString(description, gbPlotOption$description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'UPDATE grab_params_plotting SET
    section_name = ?section_name, option_name = ?option_name, param_name = ?param_name, units = ?units,
    data = ?data, sd = ?sd, min_max = ?min_max, plot_func = ?plot_func, description = ?description
    WHERE id = ?id;',
    id = gbPlotOption$id,
    section_name = section_name, option_name = option_name, param_name = param_name, units = units,
    data = data, sd = sd, min_max = min_max, plot_func = plot_func, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




## Sensor plotting options queries ###################################################################

createSensorPlotOption <- function(pool, section_name, option_name, param_name, units, data,
                                   grab_param_name = '', description = '') {
  # Check for valid input string
  section_name <- validInputString(section_name)
  option_name <- validInputString(option_name)
  param_name <- validInputString(param_name)
  units <- validInputString(units)
  data <- validInputString(data)
  grab_param_name <- validInputString(grab_param_name)
  description <- validInputString(description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO sensor_params_plotting
    (section_name, option_name, param_name, units, data, grab_param_name, description)
    values(?section_name, ?option_name, ?param_name, ?units, ?data, ?grab_param_name, ?description);',
    section_name = section_name, option_name = option_name, param_name = param_name, units = units,
    data = data, grab_param_name = grab_param_name, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateSensorPlotOption <- function(pool, sensorPlotOption, section_name = '', option_name = '', param_name = '', units = '', data = '',
                                   grab_param_name = '', description = '') {
  # Check for valid input string
  section_name <- validInputString(section_name, sensorPlotOption$section_name)
  option_name <- validInputString(option_name, sensorPlotOption$option_name)
  param_name <- validInputString(param_name, sensorPlotOption$param_name)
  units <- validInputString(units, sensorPlotOption$units)
  data <- validInputString(data, sensorPlotOption$data)
  grab_param_name <- validInputString(grab_param_name, sensorPlotOption$grab_param_name)
  description <- validInputString(description, sensorPlotOption$description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'UPDATE sensor_params_plotting SET
    section_name = ?section_name, option_name = ?option_name, param_name = ?param_name, units = ?units,
    data = ?data, grab_param_name = ?grab_param_name, description = ?description
    WHERE id = ?id;',
    id = sensorPlotOption$id,
    section_name = section_name, option_name = option_name, param_name = param_name, units = units,
    data = data, grab_param_name = grab_param_name, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




## Grab param categories queries ###################################################################

createGrabParamCat <- function(pool, category, param_name, description = '') {
  # Check for valid input string
  category <- validInputString(category)
  param_name <- validInputString(param_name)
  description <- validInputString(description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO grab_param_categories (category, param_name, description) values(?category, ?param_name, ?description);',
    category = category, param_name = param_name, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateGrabParamCat <- function(pool, grabParamCat, category = '', param_name = '', description = '') {
  # Check for valid input string
  category <- validInputString(category, grabParamCat$category)
  param_name <- validInputString(param_name, grabParamCat$param_name)
  description <- validInputString(description, grabParamCat$description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'UPDATE grab_param_categories SET
    category = ?category, param_name = ?param_name, description = ?description
    WHERE id = ?id;',
    id = grabParamCat$id,
    category = category, param_name = param_name, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}





## Data requests queries ###################################################################

createRequest <- function(pool, name, email, institution, data, reason) {
  # Check for valid input string
  name <- validInputString(name)
  email <- validInputString(email)
  institution <- validInputString(institution)
  data <- validInputString(data)
  reason <- validInputString(reason)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO data_requests (name, email, institution, data, reason)
    values(?name, ?email, ?institution, ?data, ?reason);',
    name = name, email = email, institution = institution, data = data, reason = reason
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}



updateRequest <- function(pool, request) {
  # Toggle read / unread
  query <- sqlInterpolate(
    pool,
    'UPDATE `data_requests` SET `read` = ?read WHERE id = ?id;',
    id = request$id, read = !request$read 
  )

  # Send Query and catch errors
  sendQueryWithError(pool, query)  
}




## Sensor inventory queries ###################################################################

createSensor <- function(pool, station = '', param_name, param_full, model, serial_nb = '',
                         installation_date = '', in_field = TRUE, calibration_a = '', calibration_b = '', description = '') {
  # Check for valid input string
  station <- validInputString(station)
  param_name <- validInputString(param_name)
  param_full <- validInputString(param_full)
  model <- validInputString(model)
  serial_nb <- validInputString(serial_nb)
  installation_date <- validInputDate(installation_date)
  in_field <- validInputBool(in_field)
  calibration_a <- validInputNumber(calibration_a)
  calibration_b <- validInputNumber(calibration_b)
  description <- validInputString(description)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO sensor_inventory
    (station, param_name, param_full, model, serial_nb,
    installation_date, in_field, calibration_a, calibration_b, description)
    values(?station, ?param_name, ?param_full, ?model, ?serial_nb,
    ?installation_date, ?in_field, ?calibration_a, ?calibration_b, ?description);',
    station = station, param_name = param_name, param_full = param_full,
    model = model, serial_nb = serial_nb, installation_date = installation_date, in_field = in_field,
    calibration_a = calibration_a, calibration_b = calibration_b, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}



updateSensor <- function(pool, sensor, station = '', param_name = '', param_full = '', model = '', serial_nb = '',
                         installation_date = '', in_field = TRUE, calibration_a = '', calibration_b = '', description = '') {
  # Check for valid input string
  station <- validInputString(station, sensor$station)
  param_name <- validInputString(param_name, sensor$param_name)
  param_full <- validInputString(param_full, sensor$param_full)
  model <- validInputString(model, sensor$model)
  serial_nb <- validInputString(serial_nb, sensor$serial_nb)
  installation_date <- validInputDate(installation_date, sensor$installation_date)
  in_field <- validInputBool(in_field, sensor$in_field)
  calibration_a <- validInputNumber(calibration_a, sensor$calibration_a)
  calibration_b <- validInputNumber(calibration_b, sensor$calibration_b)
  description <- validInputString(description, sensor$description)
  
  # Toggle read / unread
  query <- sqlInterpolate(
    pool,
    'UPDATE sensor_inventory
    SET station = ?station, param_name = ?param_name, param_full = ?param_full,
    model = ?model, serial_nb = ?serial_nb, installation_date = ?installation_date, in_field = ?in_field,
    calibration_a = ?calibration_a, calibration_b = ?calibration_b, description = ?description
    WHERE id = ?id;',
    id = sensor$id, station = station, param_name = param_name, param_full = param_full,
    model = model, serial_nb = serial_nb, installation_date = installation_date, in_field = in_field,
    calibration_a = calibration_a, calibration_b = calibration_b, description = description
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)  
}


