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
    result <- 'Error: IDs cannot be NULL.'
  } else if (!is.numeric(ids)) {
    result <- 'Error: IDs should be a numeric vector.'
  } else if (length(ids) == 0) {
    result <- 'Error: IDs cannot be empty.'
  } else if (any(is.na(ids))) {
    result <- 'Error: IDs cannot be NA.'
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
    result <- tryCatch(
      dbGetQuery(pool, query),
      error = function(e) return(e$message)
    )
  }
  
  
  # Check if insertion succeed (i.e. empty df)
  # If not return the error message
  if (is.data.frame(result)) {
    return('')
  } else {
    return(result)
  }
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
    "UPDATE users SET name = ?name, full_name = ?full_name, full_name = ?full_name, color = ?color WHERE id = ?id;",
    id = station$id, name = name, full_name = full_name, catchment = catchment, color = color
  )
  
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

