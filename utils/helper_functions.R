## Regroup all helpers R functions

getStats <- function(columnData, columnName) {
# Function that create a data frame of one column with some summary statistics of a given data
# Parameters:
#  - columnData: Vector, data of a given column to get stats from
#  - columnName: String, the name of the data column
# 
# Returns a one column data frame containing the statistics values
  
  # Create an empty data.frame with adequate row names
  newColumn <- data.frame(row.names = c(
    'Time Points',
    'N',
    "NA's",
    'Median',
    'Mean',
    'SD',
    'Min.',
    'Max.'
  ))
  
  # Calculate all the stats
  newColumn['Time Points', columnName] <- length(columnData)
  newColumn['N', columnName] <- length(columnData) - sum(is.na(columnData))
  newColumn["NA's", columnName] <- sum(is.na(columnData))
  newColumn['Median', columnName] <- median(columnData, na.rm = TRUE)
  newColumn['Mean', columnName] <- mean(columnData, na.rm = TRUE)
  newColumn['SD', columnName] <- sd(columnData, na.rm = TRUE)
  newColumn['Min.', columnName] <- if (min(columnData, na.rm = TRUE) == Inf) NA else min(columnData, na.rm = TRUE)
  newColumn['Max.', columnName] <- if (max(columnData, na.rm = TRUE) == -Inf) NA else max(columnData, na.rm = TRUE)
  
  # Return the data.frame
  return(newColumn)
}



createStatsTable <- function(df) {
# Function that create a data frame of multiple column with some summary statistics of a given data
# Parameters:
#  - df: Data.frame, data to get stats from, must have the following columns:
#        + parameter: Factors, the parameter of the data point
#        + value: Numeric, the data values to summarise
# 
# Returns a data frame containing the statistics values for each parameter as a column
  
  # Create a vector with the parameter data column name references
  columns <- df$parameter %>% unique()
  
  # Create empty data frame
  statsTable <- data.frame()
  
  # For each value in columns
  # Filter the data by parameter and get stats from the values
  for (dataColumn in columns) {
    # If statsTable is an empty data frame
    if (statsTable %>% dim() %>% sum() == 0) {
      # Assign the output to the statsTable
      statsTable <- df %>% filter(parameter == dataColumn) %>% pull(value) %>% getStats(dataColumn)
    } else {
      # Else combine both data frame
      newCol <- df %>% filter(parameter == dataColumn) %>% pull(value) %>% getStats(dataColumn)
      statsTable <- cbind(statsTable, newCol)
    }
  }
  
  ## Return stats summary table
  return(statsTable)
}



parseOptionsWithSections <- function(optionsInfo, valueColumn, sectionColumn = 'section_name', optionColumn = 'option_name') {
# Function to parse options for select input with section
# Parameters:
#  - optionsInfo: Data.frame, containing the info to create the select input options. Columns format:
#                 + sectionColumn: Containing the name of the section.
#                 + optionColumn: Containing the name of the option.
#                 + valueColumn: A column with the same name as specified in valueColumn.
#                                Containing the value of the option.
#  - valueColumn: String, name of the column containing the options value
#  - sectionColumn: String, name of the column containing the section names, default 'section_name'
#  - optionColumn: String, name of the column containing the options names, default 'option_name'
# 
# Returns a named list of named lists to be used as choices parameter for shiny selectInput()
  
  # Create an empty list
  optionsList <- list()
  
  # For each row in the optionsInfo
  for (i in c(1:dim(optionsInfo)[1])) {
    # Extract current row, section, option and value
    currentRow <- optionsInfo[i,]
    currentSection <- currentRow %>% pull(sectionColumn)
    currentOption <- currentRow %>% pull(optionColumn)
    currentValue <- currentRow %>% pull(valueColumn)
    
    # Add a list to optionsList if the corresponding section name list is not already created
    if (optionsList[[currentSection]] %>% is.null()) {
      optionsList[[currentSection]] <- list()
    }
    
    # Add the option to the corresponding section name list
    optionsList[[currentSection]][[currentOption]] <- currentValue
  }
  
  return(optionsList)
}



parseOptions <- function(optionsInfo, optionsColumn) {
# Function that create a simple options list for select input
# Parameters:
#  - optionsInfo: Data.frame, containing the info to create the select input options. Columns format:
#                 + optionsColumn: A column with the same name as specified in optionsColumn
#                                Containing the name and value (value == name) of the option.
#  - optionsColumn: String, name of the column containing the options value (and name)
# 
# Returns a named list to be used as choices parameter for shiny selectInput()
  return(
    optionsInfo[[optionsColumn]] %>% unique()
  )
}




lm_eqn <- function(df, x, y){
# Function that compute the linear regression equation and r2 of a given dataset and variables
# Parameters:
#  - df: Data.frame, the data to perform the linear regression with
#  - x: String, the column containing the x values
#  - y: String, the column containing the y values
# 
# Returns a sgtring containg the formated linear regression equation and r2
  
  # If either x or y contains only NAs return an empty string
  if (all(is.na(pull(df, all_of(x)))) | all(is.na(pull(df, all_of(y))))) return('')
  
  # Compute the linear regression
  m <- lm(as.formula(paste0(y, ' ~ ', x)), df)
  # Create the equation and r2 expression
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  # Convert it to string and return it
  as.character(as.expression(eq))
}



js_parser <- function(inputDir = 'assets/js', outputDir = 'www', wd = getwd()) {
# Parse and combine all JavaScript files present referenced in the assets/js/manifest.json file
# Minify it and saves it in the www/ folder as metalpdataportal.js
# Parameters:
#  - inputDir: String, the path to the input directory, default: 'assets/js'
#  - outputDir: String, the path to the output directory, default: 'www'
#  - wd: String, the the full path to the working directory, default: getwd()
# 
# Returns NULL
  
  # Create full path using the wd
  inputDir <- file.path(wd, inputDir)
  outputDir <- file.path(wd, outputDir)
  
  # Load the manifest file
  manifest <- jsonlite::read_json(file.path(inputDir, 'manifest.json'), simplifyVector = TRUE)
  
  # Create an empty variable for the compiled JS
  compiled <- ''
  
  # For each file in in the manifest
  # Read the file and append the content to the compiled variable
  for (filename in manifest) {
    compiled <- paste0(compiled, readr::read_file(file.path(inputDir, filename)))  
  }
  
  # Create a temporary file with the compiled JS
  tmp <- tempfile()
  readr::write_file(compiled, tmp)
  
  # Create the full path to the output file
  outputFile = file.path(outputDir, 'metalpdataportal.js')
  
  # Get the correct terser command depending on the OS
  command <- 'terser'
  if (Sys.info()["sysname"] == 'Windows') command <- paste0(command, '.cmd')
  
  # Minify the compiled JS and saves it in the output file
  processx::run(command = command, args = c(tmp, '-c', '-o', outputFile))
  
  # Delete the temporary file
  file.remove(tmp)
}



isValidEmail <- function(x) {
# Check if string is valid email format
# Parameters:
#  - x: String, email to verify
# 
# Return a boolean value
  
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}



coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
# Perform a coalescing join
# Data from x will always be prioritized over the data from y
# Parameters:
#  - x, y: Data.frames, df to join
#  - by: Character vector, columns names to join by
#  - suffix: Character vector, suffix to add when joining, default: c('.x', '.y')
#  - join: Function, joining function, default: dplyr::full_join()
#  - ...: Other parameters to pass to the joining function
# 
# Return a data.frame
  
  # Perform the join
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output (e.i. names of the two df columns)
  cols <- union(names(x), names(y))
  
  # Get the names of the duplicated columns
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  # Index which suffix is used for each name
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  # Coalesce the duplicated columns
  suppressMessages(
    coalesced <- purrr::map_dfc(to_coalesce, ~coalesce(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
    ))
  )
  # Rename columns
  names(coalesced) <- to_coalesce
  
  # Merge the joined df and the coalesced columns
  bind_cols(joined, coalesced) %>%
    # Remove duplicated columns by only selecting the wanted columns
    select(all_of(cols))
}



getDistribution <- function(pool, site, date, columns) {
# Get the grab data of specified columns for a 5 months period around the provided date for all years
# And calculate the min, Q10, Q90 and max for each column
# Parameters:
#  - pool: Pool connection, the connection to an SQL Database
#  - site: String, the station name to get the distribution of
#  - date: Date or Datetime, the date used to define the 5 months period
#  - columns: Character vector, the column names to get from the DB
# 
# Returns a list containing a df with the distribution and a df with the quantiles
  
  # Define a vector containing the months to sample
  monthsToSample <- sapply(-2:2, function(i) month(date + months(i)))
  
  # Get all the distributions
  distribution <- getRows(
    pool,
    'data',
    station == site,
    month(DATE_reading) %in% monthsToSample,
    columns = columns
  )
  
  # Calculate quantiles
  quantiles <- distribution %>% summarise(
    across(
      where(is.numeric),
      list(
        min = ~min(.x, na.rm = TRUE),
        Q10 = ~quantile(.x, probs = .1, na.rm = TRUE),
        Q90 = ~quantile(.x, probs = .9, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      )
    )
  ) %>% pivot_longer(
    everything(),
    names_to = c('parameter', 'quantile'),
    names_pattern = '(.*)_(.*)'
  ) %>% pivot_wider(quantile, names_from = parameter)
  
  # Returns the distribution and quantiles as list
  return(list(
    df = distribution,
    quantiles = quantiles
  ))
}




checkDistribution <- function(quantiles, row) {
# Check each value from row against the correct quantile
# Parameters:
#  - quantiles: Data.frame, contains the quantiles information for all columns
#  - row: Data.frame, contains the row new values to compare to the quantiles
# 
# Returns a list of outliers
  
  # Create a list to track the outliers
  outliers <- list()
  
  # Get the row and quantiles column names
  rowColNames <- colnames(row)
  qColNames <- colnames(quantiles)
  
  # Check every column from row
  for (column in rowColNames) {
    # If the column is also in the quantiles
    if (column %in% qColNames) {
      # Get row value and quantiles
      value <- row %>% pull(column)
      # If value is not NA, check against quantiles
      if (!is.na(value)) {
        min <- quantiles %>% filter(quantile == 'min') %>% pull(column)
        Q10 <- quantiles %>% filter(quantile == 'Q10') %>% pull(column)
        Q90 <- quantiles %>% filter(quantile == 'Q90') %>% pull(column)
        max <- quantiles %>% filter(quantile == 'max') %>% pull(column)
        # Check if minus than min
        if (!is.na(min) & value < min) {
          outliers[[column]] <- 'min'
        } else if (!is.na(Q10) & value < Q10) {
          outliers[[column]] <- 'Q10'
        } else if (!is.na(Q90) & value > Q90) {
          outliers[[column]] <- 'Q90'
        } else if (!is.na(max) & value > max) {
          outliers[[column]] <- 'max'
        }
      }
    }
  }
  
  # Return the outliers list
  outliers
}



