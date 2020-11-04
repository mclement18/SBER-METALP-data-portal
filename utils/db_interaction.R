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

validInputString <- function(input) {
  if (!is.character(input)){
    return(SQL('NULL'))
  }
  
  if (input == '' | is.na(input) | length(input) != 1) {
    return(SQL('NULL'))
  }
  
  return(input)
}



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
  username <- validInputString(username)
  password <- validInputString(password)
  role <- validInputString(role)
  
  # Use previous values if not defined
  if (username == SQL('NULL')) username <- user$name
  if (role == SQL('NULL')) role <- user$role
  if (!is.logical(active) | is.na(active)) active <- user$active
  
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
  for (value in values) {
    if (value == '') value <- SQL('NULL')
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

createStation <- function(pool, name, full_name, catchment, color) {
  # Check for valid input string
  name <- validInputString(name)
  full_name <- validInputString(full_name)
  catchment <- validInputString(catchment)
  color <- validInputString(color)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO stations (name, full_name, catchment, color) values(?name, ?full_name, ?catchment, ?color);',
    name = name, full_name = full_name, catchment = catchment, color = color
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateStation <- function(pool, station, name = '', full_name = '', catchment = '', color = '') {
  # Check for valid input string
  name <- validInputString(name)
  full_name <- validInputString(full_name)
  catchment <- validInputString(catchment)
  color <- validInputString(color)
  
  # Use previous values if not defined
  if (name == SQL('NULL')) name <- station$name
  if (full_name == SQL('NULL')) full_name <- station$full_name
  if (catchment == SQL('NULL')) catchment <- station$catchment
  if (color == SQL('NULL')) color <- station$color
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    "UPDATE stations SET name = ?name, full_name = ?full_name, full_name = ?full_name, color = ?color WHERE id = ?id;",
    id = station$id, name = name, full_name = full_name, catchment = catchment, color = color
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
  section_name <- validInputString(section_name)
  option_name <- validInputString(option_name)
  param_name <- validInputString(param_name)
  units <- validInputString(units)
  data <- validInputString(data)
  sd <- validInputString(sd)
  min_max <- validInputString(min_max)
  plot_func <- validInputString(plot_func)
  description <- validInputString(description)
  
  # Use previous values if not defined
  if (section_name == SQL('NULL')) section_name <- gbPlotOption$section_name
  if (option_name == SQL('NULL')) option_name <- gbPlotOption$option_name
  if (param_name == SQL('NULL')) param_name <- gbPlotOption$param_name
  if (units == SQL('NULL')) units <- gbPlotOption$units
  if (data == SQL('NULL')) data <- gbPlotOption$data
  if (sd == SQL('NULL')) sd <- gbPlotOption$sd
  if (min_max == SQL('NULL')) min_max <- gbPlotOption$min_max
  if (plot_func == SQL('NULL')) plot_func <- gbPlotOption$plot_func
  if (description == SQL('NULL')) description <- gbPlotOption$description
  
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
  section_name <- validInputString(section_name)
  option_name <- validInputString(option_name)
  param_name <- validInputString(param_name)
  units <- validInputString(units)
  data <- validInputString(data)
  grab_param_name <- validInputString(grab_param_name)
  description <- validInputString(description)
  
  # Use previous values if not defined
  if (section_name == SQL('NULL')) section_name <- sensorPlotOption$section_name
  if (option_name == SQL('NULL')) option_name <- sensorPlotOption$option_name
  if (param_name == SQL('NULL')) param_name <- sensorPlotOption$param_name
  if (units == SQL('NULL')) units <- sensorPlotOption$units
  if (data == SQL('NULL')) data <- sensorPlotOption$data
  if (grab_param_name == SQL('NULL')) grab_param_name <- sensorPlotOption$grab_param_name
  if (description == SQL('NULL')) description <- sensorPlotOption$description
  
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

createGrabParamCat <- function(pool, category, param_name) {
  # Check for valid input string
  category <- validInputString(category)
  param_name <- validInputString(param_name)
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'INSERT INTO grab_param_categories (category, param_name) values(?category, ?param_name);',
    category = category, param_name = param_name
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}




updateGrabParamCat <- function(pool, grabParamCat, category = '', param_name = '') {
  # Check for valid input string
  category <- validInputString(category)
  param_name <- validInputString(param_name)
  
  # Use previous values if not defined
  if (category == SQL('NULL')) category <- grabParamCat$category
  if (param_name == SQL('NULL')) param_name <- grabParamCat$param_name
  
  # Create SQL query
  query <- sqlInterpolate(
    pool,
    'UPDATE grab_param_categories SET
    category = ?category, param_name = ?param_name
    WHERE id = ?id;',
    id = grabParamCat$id,
    category = category, param_name = param_name
  )
  
  # Send Query and catch errors
  sendQueryWithError(pool, query)
}


