# Contains all the code for data load and preprocessing


## Load HF data ###################################################################

loadHighFreqDf <- function() {
  ## Loading 10min data ###########################################################
  
  hf_10min_df <- fread('./data/HF_data/10min_data.csv', header = TRUE, sep = ',')

  # Convert date to POSIXct
  hf_10min_df$Date <- hf_10min_df$Date %>% ymd_hms(tz = 'GMT')
  
  # Convert singlePoint columns to factor
  hf_10min_df %<>% mutate(across(ends_with('singlePoint'), as.factor))
  
  # Convert measured or modeled data columns to numeric
  hf_10min_df %<>% mutate(across(ends_with('measured') | ends_with('modeled'), as.numeric)) 
  
  # Convert Site_ID to factor
  hf_10min_df$Site_ID <- hf_10min_df$Site_ID %>% as.factor()
  
  
  
  ## Loading 6H data ##############################################################
  
  hf_6H_df <- fread('./data/HF_data/6H_data.csv', header = TRUE, sep = ',')
  
  # Convert date to POSIXct
  hf_6H_df$Date <- hf_6H_df$Date %>% ymd_hms(tz = 'GMT')
  
  # Convert Site_ID to factor
  hf_6H_df$Site_ID <- hf_6H_df$Site_ID %>% as.factor()
  
  
  ## Loading the 12H data #########################################################
  
  hf_12H_df <- fread('./data/HF_data/12H_data.csv', header = TRUE, sep = ',')
  
  # Convert date to POSIXct
  hf_12H_df$Date <- hf_12H_df$Date %>% ymd_hms(tz = 'GMT')
  
  # Convert Site_ID to factor
  hf_12H_df$Site_ID <- hf_12H_df$Site_ID %>% as.factor()
  
  
  
  ## Loading the 24H data #########################################################
  
  hf_24H_df <- fread('./data/HF_data/24H_data.csv', header = TRUE, sep = ',')
  
  # Convert date to POSIXct
  hf_24H_df$Date <- hf_24H_df$Date %>% ymd(tz = 'GMT')
  
  # Convert Site_ID to factor
  hf_24H_df$Site_ID <- hf_24H_df$Site_ID %>% as.factor()
  
  
  
  ## Ruturning data ###############################################################
  
  # Return a list containing all the df
  return(list(
    '10min' = hf_10min_df,
    '6H' = hf_6H_df,
    '12H' = hf_12H_df,
    '24H' = hf_24H_df
  ))
}



## Old functions to get data, parameters and sites infos from csv files ###################
# ## Load Grab Samples data #########################################################
# 
# loadGrabSampleDf <- function() {
#   # Load data
#   grabSampleDf <- fread('./data/Metalp_grab_20201009_ND.csv', header = TRUE, na.strings=c("", "NA", "<0.05"))
#   
#   # Convert Date to Date data type and create a DATETIME_GMT POSIXct column
#   grabSampleDf$DATETIME_GMT <- paste(grabSampleDf$DATE_reading, grabSampleDf$TIME_reading_GMT) %>% dmy_hm(tz = 'GMT')
#   grabSampleDf$DATE_reading <- dmy(grabSampleDf$DATE_reading)
#   
#   # Convert Site_ID to factor
#   grabSampleDf$Site_ID <- grabSampleDf$Site_ID %>% as.factor()
#   
#   return(grabSampleDf)
# }
# 
# 
# 
# ## Load Sites data ################################################################
# 
# loadSites <- function() {
#   # Load sites info
#   sites <- fread('./data/sites.csv', header = TRUE, sep = ',')
#   
#   # Create the options for the catchments select input
#   catchmentsOptions <- parseOptions(sites, 'catchments')
#   
#   # Create the options for the HF sites input
#   sitesOptions <- parseOptions(sites, 'sites_short')
#   
#   # Create the options for the sites select input
#   sitesSelectOptions <- parseOptionsWithSections(sites, 'sites_short', sectionColumn = 'catchments', optionColumn = 'sites_full')
#   
#   # Return a list containing the sites info and the catchment options for select input
#   # And the sites options for the HF checkbox input group
#   return(
#     list(
#       'sites' = sites,
#       'catchmentsOptions' = catchmentsOptions,
#       'sitesOptions' = sitesOptions,
#       'sitesSelectOptions' = sitesSelectOptions
#     )
#   )
# }
# 
# 
# 
# ## Load grab samples parameters information #######################################
# 
# loadGrabSamplesParameters <- function() {
#   # Load parameters info
#   parameters <- fread('./data/parameters_grab_samples.csv', header = TRUE, sep = ',')
#   
#   # Create the options for the select input
#   paramOptions <- parseOptionsWithSections(parameters, 'param_name')
#   
#   # Return a list containing the parameters info and options for a select input
#   return(
#     list(
#       'parameters' = parameters,
#       'selectOptions' = paramOptions
#     )
#   )
# }
# 
# 
# 
# 
# ## Load HF paramters information ##################################################
# 
# loadHfParameters <- function() {
#   # Load parameters info
#   parameters <- fread('./data/parameters_hf.csv', header = TRUE, sep = ',')
#   
#   # Create the options for the select input
#   paramOptions <- parseOptionsWithSections(parameters, 'param_name')
#   
#   # Create the options for the sensor vs grab select input
#   vsGrabParamOptions <- parseOptionsWithSections(filter(parameters, !is.na(grab_param_name)), 'param_name')
#   
#   # Return a list containing the parameters info and options for a select input
#   return(
#     list(
#       'parameters' = parameters,
#       'selectOptions' = paramOptions,
#       'vsGrabSelectOptions' = vsGrabParamOptions
#     )
#   )
# }

