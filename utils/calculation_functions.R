## This script contains all the functions used to calculate specific parameters


## Parameter calculation functions ################################################

# All functions take a df with one row and all the columns needed for the calculation
# For the most specific function the right column names will be essential for the calculation
# And for the most general ones the order of the columns may some time be crucial
# Some function will need to get passed the connection to the db get further info
# Otherwise unused parameters will be ignored
# All the functions will return a single calculated numerical value

calcMinus <- function(df, ...) {
  # Check that the df has only 2 columns 1 row
  if (ncol(df) == 2 & nrow(df) == 1) {
    # Get the two first clumns of the df
    col1 <- df %>% pull(1)
    col2 <- df %>% pull(2)
    
    # Check that they are number non NA
    values <- c(col1, col2)
    if (length(values) == 2 & !any(is.na(values)) & is.numeric(values)) {
      # Return the difference
      return(col1 - col2)
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}



calcMean <- function(df, ...) {
  # Check that df has only one row
  if (nrow(df) == 1) {
    # Calculate and return mean
    avg <- df %>% pivot_longer(everything()) %>% pull(value) %>% mean(na.rm = TRUE)
    if (is.nan(avg)) avg <- as.numeric(NA)
    return(avg)
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}



calcSd <- function(df, ...) {
  # Check that df has only one row
  if (nrow(df) == 1) {
    # Calculate and return stdev
    stdev <- df %>% pivot_longer(everything()) %>% pull(value) %>% sd(na.rm = TRUE)
    if (is.nan(stdev)) stdev <- as.numeric(NA)
    return(stdev)
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}


calcEquals <- function(df, ...) {
  # Check that df has only one row and one column
  if (nrow(df) == 1 & ncol(df) == 2) {
    # Get the two first clumns of the df
    col1 <- df %>% pull(1)
    col2 <- df %>% pull(2)
    
    # If col1 is NA, set it to col2
    if (is.na(col1)) {
      # Return the difference
      return(col2)
    } else {
      return(col1)
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcAlt2BP <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & all(c('station', 'WTW_Temp_degC_1') %in% colnames(df))) {
    station <- df %>% pull('station')
    temp <- df %>% pull('WTW_Temp_degC_1')
    # Get elevation
    elev <- getRows(pool, 'stations', name == station, columns = 'elevation')
    
    # If there is an elevation and a temp, calculate the pressure
    if (!any(is.na(c(elev, temp)))) {
      return(
        bigleaf::pressure.from.elevation(elev, temp)
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcCO2corr <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & ncol(df) == 3 & all(c('WTW_Temp_degC_1', 'Field_BP', 'Field_BP_altitude') %in% colnames(df))) {
    rawCO2 <- df %>% pull(1)
    temp <- df %>% pull('WTW_Temp_degC_1')
    fieldPressure <- df %>% pull('Field_BP')
    altPressure <- df %>% pull('Field_BP_altitude')
   
    # If there is a temp
    if (!is.na(temp)) {
      # And that the fieldPressure is present and within the range
      if (!is.na(fieldPressure) & fieldPressure <= 1050 & fieldPressure >= 700) {
        # Correct the CO2 with the field temp
        return(
          rawCO2 * 1013 * (273 + temp) / (fieldPressure * 298)
        )
      } else if (!is.na(altPressure)) {
        # Else if the altPressure is present
        # Correct the CO2 with the pressure calculated from the altitude and temperature
        return(
          rawCO2 * 1013 * (273 + temp) / (altPressure * 298)
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcTSS <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & all(c('lab_tss_wgt_samp_filt_dried', 'lab_tss_wgt_filt_prefiltr', 'lab_tss_vol_filtered') %in% colnames(df))) {
    wgtDried <- df %>% pull('lab_tss_wgt_samp_filt_dried')
    wgtPrefilt <- df %>% pull('lab_tss_wgt_filt_prefiltr')
    volFiltered <- df %>% pull('lab_tss_vol_filtered')
    
    # Check values
    values <- c(wgtDried, wgtPrefilt, volFiltered)
    if (length(values) == 3 & !any(is.na(values)) & is.numeric(values)) {
      # Calculate the TSS value
      return(
        1000000 * (wgtDried - wgtPrefilt) / volFiltered
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}



calcAFDM <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & all(c('lab_tss_wgt_samp_filt_dried', 'lab_tss_wgt_samp_filt_ashed', 'lab_tss_vol_filtered') %in% colnames(df))) {
    wgtDried <- df %>% pull('lab_tss_wgt_samp_filt_dried')
    wgtAshed <- df %>% pull('lab_tss_wgt_samp_filt_ashed')
    volFiltered <- df %>% pull('lab_tss_vol_filtered')
    
    # Check values
    values <- c(wgtDried, wgtAshed, volFiltered)
    if (length(values) == 3 & !any(is.na(values)) & is.numeric(values)) {
      # Calculate the TSS value
      return(
        1000000 * (wgtDried - wgtAshed) / volFiltered
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




# calcDIC
# calcd13DIC
# calcChlaUgL
# calcChlaUgm2
# calcAFDMgm2
# calcCO2HSuM
# calcCO2HSuatm






## Run global calculation functions ################################################

runGlobalCalculations <- function(df, pool) {
# Run all the calculations found in the parameter_calculations table for each row of the df
# And send the update to the DB
# Parameters:
#  - df: Data.frame, should mirror the DB structure
#  - pool: The pool connection to the database
# 
# Returns a vector of character containing the update errors
  
  # Get the calculations info
  calcInfo <- getRows(
    pool, 'parameter_calculations',
    columns = c('column_calculated', 'calc_func', 'columns_used')
  )
  
  # Get df columns
  dfColumns <- colnames(df)
  
  # Track errors
  errors <- c()
  
  # For each row of the df
  for (i in 1:nrow(df)) {
    # Get row
    row <- df %>% slice(i)
    
    # Track updates
    updates <- list()
    
    # For each calculation
    for (j in 1:nrow(calcInfo)) {
      # Get calculation
      calculation <- calcInfo %>% slice(j)
      
      # Check that requiered columns are in the df
      targetCol <- calculation %>% pull('column_calculated')
      paramCols <- calculation %>% pull('columns_used')
      # If not go for the next calculation
      if (!all(c(targetCol, paramCols) %in% dfColumns)) next
      
      # Check that needed function exists
      func <- tryCatch(
        match.fun(pull(calculation, 'calc_func')),
        error = function(e) e
      )
      # If not go for the next calculation
      if (inherits(func, 'error')) next
      
      # Perform calculation and save update
      result <- func(
        df = select(row, all_of(paramCols)),
        pool = pool
      )
      
      if (is.na(result)) {
        updates[[targetCol]] <- 'NULL'
      } else {
        updates[[targetCol]] <- result
      }
    }
    
    # After all calculations are done for a row
    # Send the update to the DB
    error <- updateData(
      pool = pool,
      id = row$id,
      columns = names(updates),
      values = updates
    )
    
    # Parse error
    if (error != '') {
      errors <- c(
        errors,
        paste0(row$station, ' ', row$DATE_reading, ' could not update... \n', error)
      )
    }
  }
  
  # Return errors
  errors
}





