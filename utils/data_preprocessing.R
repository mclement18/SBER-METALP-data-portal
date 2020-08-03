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
  
  
  # Convert date to POSIXct
  AND_df$date <- AND_df$date %>% ymd_hms(tz = 'GMT')
  ANU_df$date <- ANU_df$date %>% ymd_hms(tz = 'GMT')
  FED_df$date <- FED_df$date %>% ymd_hms(tz = 'GMT')
  FEU_df$date <- FEU_df$date %>% ymd_hms(tz = 'GMT')
  PEU_df$date <- PEU_df$date %>% ymd_hms(tz = 'GMT')
  RIC_df$date <- RIC_df$date %>% ymd_hms(tz = 'GMT')
  VAD_df$date <- VAD_df$date %>% ymd_hms(tz = 'GMT')
  VAU_df$date <- VAU_df$date %>% ymd_hms(tz = 'GMT')
  VEL_df$date <- VEL_df$date %>% ymd_hms(tz = 'GMT')
  VID_df$date <- VID_df$date %>% ymd_hms(tz = 'GMT')
  VIM_df$date <- VIM_df$date %>% ymd_hms(tz = 'GMT')
  VIU_df$date <- VIU_df$date %>% ymd_hms(tz = 'GMT')
  
  # Convert parameter to factor
  AND_df$parameter <- AND_df$parameter %>% as.factor()
  ANU_df$parameter <- ANU_df$parameter %>% as.factor()
  FED_df$parameter <- FED_df$parameter %>% as.factor()
  FEU_df$parameter <- FEU_df$parameter %>% as.factor()
  PEU_df$parameter <- PEU_df$parameter %>% as.factor()
  RIC_df$parameter <- RIC_df$parameter %>% as.factor()
  VAD_df$parameter <- VAD_df$parameter %>% as.factor()
  VAU_df$parameter <- VAU_df$parameter %>% as.factor()
  VEL_df$parameter <- VEL_df$parameter %>% as.factor()
  VID_df$parameter <- VID_df$parameter %>% as.factor()
  VIM_df$parameter <- VIM_df$parameter %>% as.factor()
  VIU_df$parameter <- VIU_df$parameter %>% as.factor()
  
  # Merge hf data into list
  hfDfList <- list(
    'AND' = AND_df,
    'ANU' = ANU_df,
    'FED' = FED_df,
    'FEU' = FEU_df,
    'PEU' = PEU_df,
    'RIC' = RIC_df,
    'VAD' = VAD_df,
    'VAU' = VAU_df,
    'VEL' = VEL_df,
    'VID' = VID_df,
    'VIM' = VIM_df,
    'VIU' = VIU_df
  )
  
  return(hfDfList)
}




## Load Sites data ################################################################

loadSites <- function() {
  # Load sites info
  sites <- fread('./data/sites.csv', header = TRUE, sep = ',')
  
  # Create the options for the catchments select input
  catchmentsOptions <- parseOptions(sites, 'catchments')
  
  # Create the options for the HF sites input
  sitesOptions <- parseOptions(sites, 'sites_short')
  
  # Return a list containing the sites info and the catchment options for select input
  # And the sites options for the HF checkbox input group
  return(
    list(
      'sites' = sites,
      'catchmentsOptions' = catchmentsOptions,
      'sitesOptions' = sitesOptions
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
  
  # Return a list containing the parameters info and options for a select input
  return(
    list(
      'parameters' = parameters,
      'selectOptions' = paramOptions
    )
  )
}

