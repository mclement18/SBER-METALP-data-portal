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


dailyAverage <- function(df, dateCol = 'date', valueCol = 'value', siteCol = 'Site_ID', dataTypeCol = 'data_type') {
# Function that compute the daily average of a parameter
# Parameters:
#  - df: Data.frame, the data to daily average
#  - dateCol: String, the df column name containing the date, default: 'date'
#  - valueCol: String, the df column name containing the values, default: 'value',
#  - siteCol: String, the df column name containing the site info, default: 'Site_ID',
#  - dataTypeCol: String, the df column name containing the data type, default: 'data_type'
# 
# Returns a data.frame with the daily average and standard deviation
  
  # Create a new date column and save the list of dates
  df$days <- df %>% pull(dateCol) %>% date()
  allDays <- df$days %>% unique()
  
  # Get the sites and data types
  sites <- df %>% pull(siteCol) %>% unique()
  types <- df %>% pull(dataTypeCol) %>% unique()
  
  # Create an new empty df
  newDf <- data.frame()
  
  # For each date
  for (currentDay in allDays) {
    # Subset the df with the current date
    dailyDf <- df %>% filter(days == currentDay)
    # For each site
    for (site in sites) {
      # Subset the df with the current site
      dailySiteDf <- dailyDf %>% filter(!!sym(siteCol) == site)
      # for each data type
      for (type in types) {
        # Subset the df with the current data type
        dailySiteTypeDf <- dailySiteDf %>% filter(!!sym(dataTypeCol) == type)
        
        # Calculate the daily mean and set it to 'NA' if 'NaN'
        dailyMean <- dailySiteTypeDf %>% pull(valueCol) %>% mean(na.rm = TRUE)
        if (is.nan(dailyMean)) dailyMean <- NA
        
        # Calculate the standard deviation
        dailySd <- dailySiteTypeDf %>% pull(valueCol) %>% sd(na.rm = TRUE)
        
        # Create the new row and set the names
        newRow <- data.frame(as_date(currentDay), site, type, dailyMean, dailySd)
        colnames(newRow) <- c(dateCol, siteCol, dataTypeCol, valueCol, 'sd')
        
        # Add the new row to the new df
        newDf <- bind_rows(newDf, newRow)
      }
    }
  }
  
  # Convert the sites and data type columns to factors
  newDf[siteCol] <- newDf %>% pull(siteCol) %>% as.factor()
  newDf[dataTypeCol] <- newDf %>% pull(dataTypeCol) %>% as.factor()
  
  return(newDf)
}

