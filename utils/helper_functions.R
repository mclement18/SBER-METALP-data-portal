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
#        + parameters: Factors, the parameter of the data point
#        + values: Numeric, the data values to summarise
# 
# Returns a data frame containing the statistics values for each parameter as a column
  
  # Create a vector with the parameter data column name references
  columns <- df$parameters %>% unique()
  
  # Create empty data frame
  statsTable <- data.frame()
  
  # For each values in columns
  # Filter the data by parameter and get stats from the values
  for (dataColumn in columns) {
    # If statsTable is an empty data frame
    if (statsTable %>% dim() %>% sum() == 0) {
      # Assign the output to the statsTable
      statsTable <- df %>% filter(parameters == dataColumn) %>% pull(values) %>% getStats(dataColumn)
    } else {
      # Else combine both data frame
      newCol <- df %>% filter(parameters == dataColumn) %>% pull(values) %>% getStats(dataColumn)
      statsTable <- cbind(statsTable, newCol)
    }
  }
  
  ## Return stats summary table
  return(statsTable)
}
