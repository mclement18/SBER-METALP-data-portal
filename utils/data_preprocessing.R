# Contains all the code for data load and preprocessing

## Load Grab Samples data #########################################################

loadGrabSampleDf <- function() {
  # Load data
  grabSampleDf <- read.csv('./data/Metalp_grab_20200717_ND.csv', header = TRUE, na.strings=c(""," ","NA", "<0.05"))
  
  # Convert Date to Date data type and create a DATETIME_GMT POSIXct column
  grabSampleDf$DATETIME_GMT <- paste(grabSampleDf$DATE_reading, grabSampleDf$TIME_reading_GMT) %>% dmy_hms(tz = 'GMT')
  grabSampleDf$DATE_reading <- dmy(grabSampleDf$DATE_reading)
  
  # Convert Site_ID to factor
  grabSampleDf$Site_ID <- grabSampleDf$Site_ID %>% as.factor()
  
  return(grabSampleDf)
}



## Load HF data ###################################################################

loadHighFreqDf <- function() {
  # Load each site data
  AND_df <- fread('./data/HF_data/AND.csv', header = TRUE, sep = ',')
  ANU_df <- fread('./data/HF_data/ANU.csv', header = TRUE, sep = ',')
  FED_df <- fread('./data/HF_data/FED.csv', header = TRUE, sep = ',')
  FEU_df <- fread('./data/HF_data/FEU.csv', header = TRUE, sep = ',')
  PEU_df <- fread('./data/HF_data/PEU.csv', header = TRUE, sep = ',')
  RIC_df <- fread('./data/HF_data/RIC.csv', header = TRUE, sep = ',')
  VAD_df <- fread('./data/HF_data/VAD.csv', header = TRUE, sep = ',')
  VAU_df <- fread('./data/HF_data/VAU.csv', header = TRUE, sep = ',')
  VEL_df <- fread('./data/HF_data/VEL.csv', header = TRUE, sep = ',')
  VID_df <- fread('./data/HF_data/VID.csv', header = TRUE, sep = ',')
  VIM_df <- fread('./data/HF_data/VIM.csv', header = TRUE, sep = ',')
  VIU_df <- fread('./data/HF_data/VIU.csv', header = TRUE, sep = ',')
  
  
  # Merge hf data into one df
  hfDf <- rbind(
    AND_df,
    ANU_df,
    FED_df,
    FEU_df,
    PEU_df,
    RIC_df,
    VAD_df,
    VAU_df,
    VEL_df,
    VID_df,
    VIM_df,
    VIU_df
  )

  # Convert date to POSIXct
  hfDf$date <- hfDf$date %>% ymd_hms(tz = 'GMT')
  
  # Convert data_type to factor
  hfDf$data_type <- hfDf$data_type %>% as.factor()
  
  # Convert Site_ID to factor
  hfDf$Site_ID <- hfDf$Site_ID %>% as.factor()
  
  return(hfDf)
}




## Load Sites data ################################################################

loadSites <- function() {
  # Load sites info
  sites <- fread('./data/sites.csv', header = TRUE, sep = ',')
  
  # Create the options for the catchments select input
  catchmentsOptions <- parseOptions(sites, 'catchments')
  
  # Create the options for the HF sites input
  sitesOptions <- parseOptions(sites, 'sites_short')
  
  # Create the options for the sites select input
  sitesSelectOptions <- parseOptionsWithSections(sites, 'sites_short', sectionColumn = 'catchments', optionColumn = 'sites_full')
  
  # Return a list containing the sites info and the catchment options for select input
  # And the sites options for the HF checkbox input group
  return(
    list(
      'sites' = sites,
      'catchmentsOptions' = catchmentsOptions,
      'sitesOptions' = sitesOptions,
      'sitesSelectOptions' = sitesSelectOptions
    )
  )
}



## Load grab samples parameters information #######################################

loadGrabSamplesParameters <- function() {
  # Load parameters info
  parameters <- fread('./data/parameters_grab_samples.csv', header = TRUE, sep = ',')
  
  # Create the options for the select input
  paramOptions <- parseOptionsWithSections(parameters, 'param_name')
  
  # Return a list containing the parameters info and options for a select input
  return(
    list(
      'parameters' = parameters,
      'selectOptions' = paramOptions
    )
  )
}




## Load HF paramters information ##################################################

loadHfParameters <- function() {
  # Load parameters info
  parameters <- fread('./data/parameters_hf.csv', header = TRUE, sep = ',')
  
  # Create the options for the select input
  paramOptions <- parseOptionsWithSections(parameters, 'param_name')
  
  # Create the options for the sensor vs grab select input
  vsGrabParamOptions <- parseOptionsWithSections(filter(parameters, !is.na(grab_param_name)), 'param_name')
  
  # Return a list containing the parameters info and options for a select input
  return(
    list(
      'parameters' = parameters,
      'selectOptions' = paramOptions,
      'vsGrabSelectOptions' = vsGrabParamOptions
    )
  )
}

