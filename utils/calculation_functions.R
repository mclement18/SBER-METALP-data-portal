## This script contains all the functions used to calculate specific parameters


## Parameter calculation functions ################################################

# All functions take a df with one row and all the columns needed for the calculation
# For the most specific function the right column names will be essential for the calculation
# And for the most general ones the order of the columns may some time be crucial
# Some function will need to get passed the connection to the db get further info
# Otherwise unused parameters will be ignored
# All the functions will return a single calculated numerical value
# Or the string 'KEEP OLD' if the value must not be updated

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
    avg <- df %>% tidyr::pivot_longer(everything()) %>% pull(value) %>% mean(na.rm = TRUE)
    if (is.na(avg)) avg <- 'KEEP OLD'
    return(avg)
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}



calcSd <- function(df, ...) {
  # Check that df has only one row
  if (nrow(df) == 1) {
    # Calculate and return stdev
    stdev <- df %>% tidyr::pivot_longer(everything()) %>% pull(value) %>% sd(na.rm = TRUE)
    if (is.na(stdev)) stdev <- 'KEEP OLD'
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
    elev <- getRows(pool, 'stations', name == station, columns = c('order', 'elevation')) %>% arrange(order) %>% pull(elevation)
    
    # If there is an elevation and a temp, calculate the pressure in hPa
    if (!any(is.na(c(elev, temp)))) {
      return(
        round(bigleaf::pressure.from.elevation(elev, temp) * 10)
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcCO2corr <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & ncol(df) == 4 & all(c('WTW_Temp_degC_1', 'Field_BP', 'Field_BP_altitude') %in% colnames(df))) {
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
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
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
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
}




calcSUVA <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & all(c('a254', 'DOC_avg_ppb') %in% colnames(df))) {
    a254 <- df %>% pull('a254')
    DOC_avg <- df %>% pull('DOC_avg_ppb')
    
    # Check for presence of DOC_avg and a254
    if (!any(is.na(c(a254, DOC_avg)))) {
      return(
        a254 * 1000 / DOC_avg
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcRatio <- function(df, ...) {
  # Check for the presence of the correct columns
  if (nrow(df) == 1 & ncol(df) == 2) {
    dividend <- df %>% pull(1)
    divisor <- df %>% pull(2)
    
    # Check for presence of both dividend and divisor
    if (!any(is.na(c(dividend, divisor))) & divisor != 0) {
      return(
        dividend / divisor
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcDIC <- function(df, pool, labTemp = 'default', labPressure = 'default', ...) {
  # labTemp values c('default', 'cst', 'db')
  
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_dic_air_temp',
          'lab_dic_acid_sample_wght',
          'lab_dic_acid_wght',
          'lab_dic_vol_overpressure',
          'lab_dic_SA_added',
          'lab_dic_co2_dry'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 6
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('h_co2_29815k', 'gas_const_r_mol', 'vial_volume', 'h3po4_added', 'lab_temp_avg_degC', 'lab_press_avg_atm')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Determine which constant to use, from data entry (db) or constant table (cst)
    # The default argument will prevail the 'db' and then fallback to the 'cst'
    # Get lab temp from data
    if (labTemp == 'db') {
      lab_temp <- df %>% pull('lab_dic_air_temp')
      # Get lab temp from constant
    } else if (labTemp == 'cst') {
      lab_temp <- constants %>%
        filter(name == 'lab_temp_avg_degC') %>%
        pull('value')
    } else if (labPressure == 'default') {
      # Get db temp
      lab_temp <- df %>% pull('lab_dic_air_temp')
      # If its value is NA, use constant
      if (is.na(lab_temp)) lab_temp <- constants %>%
          filter(name == 'lab_temp_avg_degC') %>%
          pull('value')
    }
    # Calculate temp in Kelvin
    lab_temp <- lab_temp + 273.15
    
    # values needed
    lab_dic_acid_sample_wght <- df %>% select(starts_with('lab_dic_acid_sample_wght')) %>% pull()
    lab_dic_acid_wght <- df %>% select(starts_with('lab_dic_acid_wght')) %>% pull()
    lab_dic_vol_overpressure <- df %>% select(starts_with('lab_dic_vol_overpressure')) %>% pull()
    lab_dic_SA_added <- df %>% select(starts_with('lab_dic_SA_added')) %>% pull()
    lab_dic_co2_dry <- df %>% select(starts_with('lab_dic_co2_dry')) %>% pull()
    
    # Constant needed
    h_co2_29815k <- constants %>% filter(name == 'h_co2_29815k') %>% pull('value')
    gas_const_r_mol <- constants %>% filter(name == 'gas_const_r_mol') %>% pull('value')
    vial_volume <- constants %>% filter(name == 'vial_volume') %>% pull('value')
    h3po4_added <- constants %>% filter(name == 'h3po4_added') %>% pull('value')
    
    # Calculate intermediate variables
    sampleV <- lab_dic_acid_sample_wght - lab_dic_acid_wght
    hsV <- vial_volume + lab_dic_vol_overpressure - (sampleV + h3po4_added)
    co2_acid <- lab_dic_co2_dry * (lab_dic_SA_added + hsV)
    gas_temp <- gas_const_r_mol * lab_temp
    exponent <- exp(2392.86 * (1/lab_temp - 1/298.15))
    
    dividend <- co2_acid * (h_co2_29815k * exponent * sampleV * gas_temp + 101.325 * hsV)
    divisor <- 10^3 * gas_temp * hsV * sampleV
    
    # Check for presence of both dividend and divisor
    if (!any(is.na(c(dividend, divisor))) & divisor != 0) {
      return(
        # Calculate DIC
        dividend / divisor
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}



calcd13DIC <- function(df, pool, labTemp = 'default', labPressure = 'default', ...) {
  # labTemp values c('default', 'cst', 'db')
  
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_dic_air_temp',
          'lab_dic_acid_sample_wght',
          'lab_dic_acid_wght',
          'lab_dic_vol_overpressure',
          'lab_dic_delta_13co2'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 5
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('h_co2_29815k', 'gas_const_r_mol', 'vial_volume', 'h3po4_added', 'lab_temp_avg_degC', 'lab_press_avg_atm')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Determine which constant to use, from data entry (db) or constant table (cst)
    # The default argument will prevail the 'db' and then fallback to the 'cst'
    # Get lab temp from data
    if (labTemp == 'db') {
      lab_temp <- df %>% pull('lab_dic_air_temp')
      # Get lab temp from constant
    } else if (labTemp == 'cst') {
      lab_temp <- constants %>%
        filter(name == 'lab_temp_avg_degC') %>%
        pull('value')
    } else if (labPressure == 'default') {
      # Get db temp
      lab_temp <- df %>% pull('lab_dic_air_temp')
      # If its value is NA, use constant
      if (is.na(lab_temp)) lab_temp <- constants %>%
          filter(name == 'lab_temp_avg_degC') %>%
          pull('value')
    }
    # Calculate temp in Kelvin
    lab_temp <- lab_temp + 273.15
    
    # values needed
    lab_dic_acid_sample_wght <- df %>% select(starts_with('lab_dic_acid_sample_wght')) %>% pull()
    lab_dic_acid_wght <- df %>% select(starts_with('lab_dic_acid_wght')) %>% pull()
    lab_dic_vol_overpressure <- df %>% select(starts_with('lab_dic_vol_overpressure')) %>% pull()
    lab_dic_delta_13co2 <- df %>% select(starts_with('lab_dic_delta_13co2')) %>% pull()
    
    # Constant needed
    h_co2_29815k <- constants %>% filter(name == 'h_co2_29815k') %>% pull('value')
    gas_const_r_mol <- constants %>% filter(name == 'gas_const_r_mol') %>% pull('value')
    vial_volume <- constants %>% filter(name == 'vial_volume') %>% pull('value')
    h3po4_added <- constants %>% filter(name == 'h3po4_added') %>% pull('value')
    
    # Calculate intermediate variables
    sampleV <- lab_dic_acid_sample_wght - lab_dic_acid_wght
    hsV <- vial_volume + lab_dic_vol_overpressure - (sampleV + h3po4_added)
    exponent <- exp(2392.86 * (1/lab_temp - 1/298.15))
    H_cst_expo_sampl_gas <- h_co2_29815k * exponent * sampleV * gas_const_r_mol
    
    dividend <- lab_dic_delta_13co2 * 101.325 * hsV + (lab_temp * (lab_dic_delta_13co2 + 0.19) - 373) * H_cst_expo_sampl_gas
    divisor <- 101.325 * hsV + H_cst_expo_sampl_gas * lab_temp
    
    # Check for presence of both dividend and divisor
    if (!any(is.na(c(dividend, divisor))) & divisor != 0) {
      return(
        # Calculate d13DIC
        dividend / divisor
      )
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




convertToUnitPerM2 <- function(s, d, vf, vt) {
  # Convert sample value (s) to a sample unit/m2
  # Using rock dimensions (d) format: c(length , width, depth)
  # Sample volume (vt) and volume filtrated (vf)
  
  # Calculate area
  area <- 2 * pi * mean(combn((d / 100)^1.6075, 2, prod))^(1/1.6075)
  
  # Convert to unit/m2
  s * vt / (vf * area)
}



calcBenthicAFDM <- function(df, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_chla_sizeA_rep',
          'lab_chla_sizeB_rep',
          'lab_chla_sizeC_rep',
          'lab_chla_tot_vol_rep',
          'lab_chla_vol_filtrated_rep',
          'afdm_g_filter_rep'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 6
  
  if (nrow(df) == 1 & allColumns) {
    # Get values
    lab_chla_sizeA_rep <- df %>% select(starts_with('lab_chla_sizeA_rep')) %>% pull()
    lab_chla_sizeB_rep <- df %>% select(starts_with('lab_chla_sizeB_rep')) %>% pull()
    lab_chla_sizeC_rep <- df %>% select(starts_with('lab_chla_sizeC_rep')) %>% pull()
    lab_chla_tot_vol_rep <- df %>% select(starts_with('lab_chla_tot_vol_rep')) %>% pull()
    lab_chla_vol_filtrated_rep <- df %>% select(starts_with('lab_chla_vol_filtrated_rep')) %>% pull()
    afdm_g_filter_rep <- df %>% select(starts_with('afdm_g_filter_rep')) %>% pull()
    
    # If no NAs, calculate AFDM per m2
    if (!any(is.na(c(lab_chla_sizeA_rep, lab_chla_sizeB_rep, lab_chla_sizeC_rep, lab_chla_tot_vol_rep, lab_chla_vol_filtrated_rep, afdm_g_filter_rep)))) {
      return(
        convertToUnitPerM2(
          afdm_g_filter_rep,
          c(lab_chla_sizeA_rep, lab_chla_sizeB_rep, lab_chla_sizeC_rep),
          lab_chla_vol_filtrated_rep,
          lab_chla_tot_vol_rep
        )
      )
    }
  }
  
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
}




calcChlaPerM2 <- function(df, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_chla_sizeA_rep',
          'lab_chla_sizeB_rep',
          'lab_chla_sizeC_rep',
          'lab_chla_tot_vol_rep',
          'lab_chla_vol_filtrated_rep',
          'chla_(no)?acid_ugL_rep'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 6
  
  if (nrow(df) == 1 & allColumns) {
    # Get values
    lab_chla_sizeA_rep <- df %>% select(starts_with('lab_chla_sizeA_rep')) %>% pull()
    lab_chla_sizeB_rep <- df %>% select(starts_with('lab_chla_sizeB_rep')) %>% pull()
    lab_chla_sizeC_rep <- df %>% select(starts_with('lab_chla_sizeC_rep')) %>% pull()
    lab_chla_tot_vol_rep <- df %>% select(starts_with('lab_chla_tot_vol_rep')) %>% pull()
    lab_chla_vol_filtrated_rep <- df %>% select(starts_with('lab_chla_vol_filtrated_rep')) %>% pull()
    chla_ugl <- df %>% select(matches('chla_(no)?acid_ugL_rep')) %>% pull()
    
    # If no NAs, calculate Chla per m2
    if (!any(is.na(c(lab_chla_sizeA_rep, lab_chla_sizeB_rep, lab_chla_sizeC_rep, lab_chla_tot_vol_rep, lab_chla_vol_filtrated_rep, chla_ugl)))) {
      return(
        convertToUnitPerM2(
          chla_ugl * 0.005,
          c(lab_chla_sizeA_rep, lab_chla_sizeB_rep, lab_chla_sizeC_rep),
          lab_chla_vol_filtrated_rep,
          lab_chla_tot_vol_rep
        )
      )
    }
  }
  
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
}





calcChlaAcid <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_chla_fluor_1_rep',
          'lab_chla_fluor_2_rep'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 2
  
  if (nrow(df) == 1 & allColumns) {
    # Get values
    lab_chla_fluor_1_rep <- df %>% select(starts_with('lab_chla_fluor_1_rep')) %>% pull()
    lab_chla_fluor_2_rep <- df %>% select(starts_with('lab_chla_fluor_2_rep')) %>% pull()
    
    # Define constants to get
    cst_to_get <- c('chla_acidified_slope', 'chla_acidified_intercept')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Constant needed
    chla_acidified_slope <- constants %>% filter(name == 'chla_acidified_slope') %>% pull('value')
    chla_acidified_intercept <- constants %>% filter(name == 'chla_acidified_intercept') %>% pull('value')
    
    # If no NAs, calculate Chla acidified
    if (!any(is.na(c(lab_chla_fluor_1_rep, lab_chla_fluor_2_rep, chla_acidified_slope, chla_acidified_intercept)))) {
      return(
        (lab_chla_fluor_1_rep - lab_chla_fluor_2_rep) * chla_acidified_slope + chla_acidified_intercept
      )
    }
  }
  
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
}




calcChlaNoAcid <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(grepl('lab_chla_fluor_1_rep', colnames(df))) == 1
  
  if (nrow(df) == 1 & allColumns) {
    # Get values
    lab_chla_fluor_1_rep <- df %>% select(starts_with('lab_chla_fluor_1_rep')) %>% pull()
    
    # Define constants to get
    cst_to_get <- c('chla_non_acidified_slope', 'chla_non_acidified_intercept')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Constant needed
    chla_non_acidified_slope <- constants %>% filter(name == 'chla_non_acidified_slope') %>% pull('value')
    chla_non_acidified_intercept <- constants %>% filter(name == 'chla_non_acidified_intercept') %>% pull('value')
    
    # If no NAs, calculate Chla non acidified
    if (!any(is.na(c(lab_chla_fluor_1_rep, chla_non_acidified_slope, chla_non_acidified_intercept)))) {
      return(
        lab_chla_fluor_1_rep * chla_non_acidified_slope + chla_non_acidified_intercept
      )
    }
  }
  
  
  # If nothing is returned, return 'KEEP OLD'
  'KEEP OLD'
}




calcCH4dry <- function(df, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_co2(air)?_h2o',
          'lab_co2(air)?_ch4'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 2
  
  if (nrow(df) == 1 & allColumns) {
    # Get values
    lab_co2_h2o <- df %>% select(matches('lab_co2(air)?_h2o')) %>% pull()
    lab_co2_ch4 <- df %>% select(matches('lab_co2(air)?_ch4')) %>% pull()
    
    # If no NAs, calculate Chla acidified
    if (!any(is.na(c(lab_co2_h2o, lab_co2_ch4)))) {
      return(
        (lab_co2_h2o * 1.2347 - 0.0016) * lab_co2_ch4 / 100 + lab_co2_ch4
      )
    }
  }
  
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcCH4 <- function(df, pool, labTemp = 'default', labPa = 'default', ...) {
  # labTemp and labPa values c('default', 'cst', 'db')
  labParams <- list(
    labTemp = labTemp,
    labPa = labPa
  )
  
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('WTW_Temp_degC_1',
          'Field_BP$',
          'Field_BP_altitude$',
          'lab_co2_lab_temp',
          'lab_co2_lab_press',
          'lab_co2_ch4_dry'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 6
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('lab_press_avg_atm', 'lab_temp_avg_degC', 'ch4_in_sa', 'gas_const_r_mol', 'h_ch4_29815k')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Determine which constant to use, from data entry (db) or constant table (cst)
    # The default argument will prevail the 'db' and then fallback to the 'cst'
    for (param in names(labParams)) {
      if (param == 'labTemp') {
        cstName <- 'lab_temp_avg_degC'
        dbName <- 'lab_co2_lab_temp'
      } else {
        cstName <- 'lab_press_avg_atm'
        dbName <- 'lab_co2_lab_press'
      }
      # Get lab temp from data
      if (labParams[[param]] == 'db') {
        labParams[[param]] <- df %>% pull(dbName)
        # Get lab temp from constant
      } else if (labParams[[param]] == 'cst') {
        labParams[[param]] <- constants %>%
          filter(name == cstName) %>%
          pull('value')
      } else if (labParams[[param]] == 'default') {
        # Get db temp
        labParams[[param]] <- df %>% pull(dbName)
        # If its value is NA, use constant
        if (is.na(labParams[[param]])) labParams[[param]] <- constants %>%
            filter(name == cstName) %>%
            pull('value')
      }
    }
    
    # Calculate temp in Kelvin
    labParams$labTemp <- labParams$labTemp + 273.15
    
    # values needed
    ch4_dry <- df %>% select(starts_with('lab_co2_ch4_dry')) %>% pull()
    water_temp_k <- 273.15 + df %>% pull('WTW_Temp_degC_1')
    fieldPressure <- df %>% pull('Field_BP')
    altPressure <- df %>% pull('Field_BP_altitude')
    # If the fieldPressure is present and within the range
    if (!is.na(fieldPressure) & fieldPressure <= 1050 & fieldPressure >= 700) {
      # Use filed pressure
     bp <- fieldPressure
    } else {
      # Else use altPressure
      bp <- altPressure
    }
    
    # Constant needed
    ch4_in_sa <- constants %>% filter(name == 'ch4_in_sa') %>% pull('value')
    gas_const_r_mol <- constants %>% filter(name == 'gas_const_r_mol') %>% pull('value')
    h_ch4_29815k <- constants %>% filter(name == 'h_ch4_29815k') %>% pull('value')
    
    if (!any(is.na(c(ch4_dry, water_temp_k, bp, ch4_in_sa, gas_const_r_mol, h_ch4_29815k, labParams$labTemp, labParams$labPa)))) {
      # Calculate intermediate variables
      h_ch4_t_eq <- h_ch4_29815k * exp(1750 * (1/labParams$labTemp - 1/298.15))
      A <- ch4_dry * labParams$labPa * 101.325 * water_temp_k - bp * (ch4_in_sa * labParams$labTemp * 10^3)
      B <- h_ch4_t_eq * gas_const_r_mol * 10 * water_temp_k + bp
      
      dividend <- A * B
      divisor <- labParams$labTemp * bp * gas_const_r_mol * water_temp_k
      
      # Check for presence of both dividend and divisor
      if (divisor != 0) {
        return(
          # Calculate CH4
          dividend / divisor
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcCO2 <- function(df, pool, labTemp = 'default', labPa = 'default', ...) {
  # labTemp and labPa values c('default', 'cst', 'db')
  labParams <- list(
    labTemp = labTemp,
    labPa = labPa
  )
  
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('lab_co2_lab_temp',
          'lab_co2_lab_press',
          'lab_co2_co2ppm'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 3
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('lab_press_avg_atm', 'lab_temp_avg_degC', 'vol_sa', 'vol_water', 'c_const', 'gas_const_r_atm')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # Determine which constant to use, from data entry (db) or constant table (cst)
    # The default argument will prevail the 'db' and then fallback to the 'cst'
    for (param in names(labParams)) {
      if (param == 'labTemp') {
        cstName <- 'lab_temp_avg_degC'
        dbName <- 'lab_co2_lab_temp'
      } else {
        cstName <- 'lab_press_avg_atm'
        dbName <- 'lab_co2_lab_press'
      }
      # Get lab temp from data
      if (labParams[[param]] == 'db') {
        labParams[[param]] <- df %>% pull(dbName) / 1013.25
        # Get lab temp from constant
      } else if (labParams[[param]] == 'cst') {
        labParams[[param]] <- constants %>%
          filter(name == cstName) %>%
          pull('value')
      } else if (labParams[[param]] == 'default') {
        # Get db temp
        labParams[[param]] <- df %>% pull(dbName) / 1013.25
        # If its value is NA, use constant
        if (is.na(labParams[[param]])) labParams[[param]] <- constants %>%
            filter(name == cstName) %>%
            pull('value')
      }
    }
    
    # Calculate temp in Kelvin
    labParams$labTemp <- labParams$labTemp + 273.15
    
    # values needed
    co2 <- df %>% select(starts_with('lab_co2_co2ppm')) %>% pull()
    
    # Constant needed
    vol_sa <- constants %>% filter(name == 'vol_sa') %>% pull('value')
    vol_water <- constants %>% filter(name == 'vol_water') %>% pull('value')
    c_const <- constants %>% filter(name == 'c_const') %>% pull('value')
    gas_const_r_atm <- constants %>% filter(name == 'gas_const_r_atm') %>% pull('value')
    
    if (!any(is.na(c(co2, vol_sa, vol_water, c_const, gas_const_r_atm, labParams$labTemp, labParams$labPa)))) {
      # Calculate intermediate variables
      exponent <- exp(c_const * (1/labParams$labTemp - 1/298.15))
      dividend <- co2 * labParams$labPa * (vol_sa + 0.034 * exponent * vol_water * gas_const_r_atm * labParams$labTemp)
      divisor <- gas_const_r_atm * vol_water * labParams$labTemp
      
      # Check for presence of both dividend and divisor
      if (divisor != 0) {
        return(
          # Calculate CH4
          dividend / divisor
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcpCO2 <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('WTW_Temp_degC_1',
          'CO2_HS_Um'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 2
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('c_const')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # values needed
    co2 <- df %>% select(starts_with('CO2_HS_Um')) %>% pull()
    water_temp_k <- 273.15 + df %>% pull('WTW_Temp_degC_1')
    
    # Constant needed
    c_const <- constants %>% filter(name == 'c_const') %>% pull('value')
    
    if (!any(is.na(c(co2, water_temp_k, c_const)))) {
      # Calculate intermediate variables
      dividend <- co2
      divisor <- 0.034 * exp(c_const * (1/water_temp_k - 1/298.15))
      
      # Check for presence of both dividend and divisor
      if (divisor != 0) {
        return(
          # Calculate CH4
          dividend / divisor
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}




calcpCO2P1 <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('WTW_Temp_degC_1',
          'Field_BP$',
          'Field_BP_altitude$',
          'CO2_HS_Um'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 4
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('c_const')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # values needed
    co2 <- df %>% select(starts_with('CO2_HS_Um')) %>% pull()
    water_temp_k <- 273.15 + df %>% pull('WTW_Temp_degC_1')
    fieldPressure <- df %>% pull('Field_BP')
    altPressure <- df %>% pull('Field_BP_altitude')
    # If the fieldPressure is present and within the range
    if (!is.na(fieldPressure) & fieldPressure <= 1050 & fieldPressure >= 700) {
      # Use filed pressure
      bp <- fieldPressure
    } else {
      # Else use altPressure
      bp <- altPressure
    }
    
    # Constant needed
    c_const <- constants %>% filter(name == 'c_const') %>% pull('value')
    
    if (!any(is.na(c(co2, water_temp_k, bp, c_const)))) {
      # Calculate intermediate variables
      dividend <- co2 * bp
      divisor <- 0.034 * exp(c_const * (1/water_temp_k - 1/298.15)) * 1013.25
      
      # Check for presence of both dividend and divisor
      if (divisor != 0) {
        return(
          # Calculate CH4
          dividend / divisor
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}





calcpCO2P2 <- function(df, pool, ...) {
  # Check for the presence of the correct columns
  allColumns <- sum(
    grepl(
      paste(
        c('WTW_Temp_degC_1',
          'Field_BP$',
          'Field_BP_altitude$',
          'CO2_HS_Um'),
        collapse = '|'
      ),
      colnames(df)
    )
  ) == 4
  
  if (nrow(df) == 1 & allColumns) {
    # Define constants to get
    cst_to_get <- c('c_const')
    
    # Get constants
    constants <- getRows(pool, 'constants', name %in% cst_to_get, columns = c('name', 'value'))
    
    # values needed
    co2 <- df %>% select(starts_with('CO2_HS_Um')) %>% pull()
    water_temp_k <- 273.15 + df %>% pull('WTW_Temp_degC_1')
    fieldPressure <- df %>% pull('Field_BP')
    altPressure <- df %>% pull('Field_BP_altitude')
    # If the fieldPressure is present and within the range
    if (!is.na(fieldPressure) & fieldPressure <= 1050 & fieldPressure >= 700) {
      # Use filed pressure
      bp <- fieldPressure
    } else {
      # Else use altPressure
      bp <- altPressure
    }
    
    # Constant needed
    c_const <- constants %>% filter(name == 'c_const') %>% pull('value')
    
    if (!any(is.na(c(co2, water_temp_k, bp, c_const)))) {
      # Calculate intermediate variables
      dividend <- co2 * 1013.25
      divisor <- 0.034 * exp(c_const * (1/water_temp_k - 1/298.15)) * bp
      
      # Check for presence of both dividend and divisor
      if (divisor != 0) {
        return(
          # Calculate CH4
          dividend / divisor
        )
      }
    }
  }
  
  # If nothing is returned, return NA
  as.numeric(NA)
}






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
    columns = c('order', 'column_calculated', 'calcul_func', 'columns_used')
  ) %>% arrange(order) %>% select(-order)
  
  # Get df columns
  dfColumns <- colnames(df)
  
  # Track errors and warnings
  errors <- list(
    errors = c(),
    warnings = list()
  )
  
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
      paramCols <- calculation %>% pull('columns_used') %>%
        str_split(',') %>% unlist()
      # If not go for the next calculation
      if (!all(c(targetCol, paramCols) %in% dfColumns)) {
        if (is.null(errors$warnings[[targetCol]])) {
          errors$warnings[[targetCol]] <- paste(
            'Warning: Some of the indicated columns could not by found in the data.frame.',
            paste('  Target column:', targetCol),
            paste('  Used columns: ', paste(paramCols, collapse = ', ')),
            sep = '\n'
          )
        }
        next
      }
      
      # Check that needed function exists
      funcName <- pull(calculation, 'calcul_func')
      func <- tryCatch(
        match.fun(funcName),
        error = function(e) e
      )
      # If not go for the next calculation
      if (inherits(func, 'error')) {
        if (is.null(errors$warnings[[funcName]])) {
          errors$warnings[[funcName]] <- paste(
            'Warning: The calculation function does not exists.',
            paste('  Function:', funcName),
            sep = '\n'
          )
        }
        next
      }
      
      # Perform calculation
      result <- func(
        df = select(row, all_of(paramCols)),
        pool = pool
      )
      
      # Save result to update the DB
      # And update row value for further calculations
      if (is.na(result)) {
        row[targetCol] <- as.numeric(NA)
        updates[[targetCol]] <- 'NULL'
        # Save update when it is not indicated to keep old value
      } else if (result != 'KEEP OLD') {
        row[targetCol] <- result
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
      errors$errors <- c(
        errors$errors,
        paste0(row$station, ' ', row$DATE_reading, ' could not update... \n', error)
      )
    }
  }
  
  # Return errors
  errors
}





