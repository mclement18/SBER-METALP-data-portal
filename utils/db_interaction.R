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



## Query parameters parsing #######################################################

validInputString <- function(input) {
  if (!is.character(input)){
    return(SQL('NULL'))
  }
  
  if (input == '' | length(input) != 1) {
    return(SQL('NULL'))
  }
  
  return(input)
}




## Technical queries ##############################################################

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
  username <- validInputString(username)
  password <- validInputString(password)
  role <- validInputString(role)
  
  if (username == SQL('NULL')) username <- user$name
  if (role == SQL('NULL')) role <- user$role
  if (!is.logical(active) | is.na(active)) active <- user$active
  
  if (password == SQL('NULL')) {
    query <- sqlInterpolate(
      pool,
      "UPDATE users SET name = ?name, role = ?role, active = ?active WHERE id = ?id;",
      id = user$id, name = username, role = role, active = active
    )
  } else {
    hashedPassword <- sodium::password_store(password)
    query <- sqlInterpolate(
      pool,
      "UPDATE users SET name = ?name, password = ?password, role = ?role, active = ?active WHERE id = ?id;",
      id = user$id, name = username, password = password, role = role, active = active
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


