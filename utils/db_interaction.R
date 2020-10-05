## This script contains all the database queries and connections

## Database connection ############################################################

connectToDB <- function() {
  dbPool(
    drv = RMySQL::MySQL(),
    dbname = MY_DB_NAME,
    host = MY_DB_HOST,
    username = MY_DB_USER,
    password = MY_DB_PWD
  )
}



## Query parameters parsing #######################################################

validInputString <- function(input) {
  if (input == '' | !is.character(input) | length(input) != 1) {
    return(SQL('NULL'))
  } else {
    return(input)
  }
}



## User queries ###################################################################

# Get user for login
loginUser <- function(pool, username) {
  pool %>% tbl('users') %>% filter(name == username, active == 1) %>% select(name, password, role) %>% head(1) %>% collect()
}


# Create a new user
createUser <- function(pool, username, password, role = 'sber') {
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
    'INSERT INTO users (name, password, role) values(?username, ?hashedPassword, ?role);',
    username = username, hashedPassword = hashedPassword, role = role
  )
  
  # Send Query and catch errors
  result <- tryCatch(
    dbGetQuery(pool, query),
    error = function(e) return(e$message)
  )
  
  # Check if insertion succeed (i.e. empty df)
  # If not return the error message
  if (is.data.frame(result)) {
    return(TRUE)
  } else {
    return(result)
  }
}
