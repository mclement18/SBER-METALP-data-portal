## Regroups all plotting functions used with ggplot2

## Plotting helpers ###############################################################

calculateYaxisLimits <- function(min, max, perc = 0.1) {
  axisMargin <- (max - min) * perc
  return(
    c(min - axisMargin, max + axisMargin)
  )
}

siteColors <- function(df, sitesTable) {
  namedColors <- c()
  for (site in df$Site_ID %>% unique()) {
    namedColors[site] <- sitesTable %>% filter(sites_short == site) %>% pull(sites_color)
  }
  
  return(namedColors)
}

## Plot types #####################################################################

basicPlot <- function(df, x, param, plotTitle, sites) {
# Function that create a simple scatter plot with a LOESS curve for one value
# Parameters:
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'parameter': factor
#       + x: POSIXct datetime (column name corresponding to the string passed through the argument 'x')
# - x: String corresponding to the column name of a POSIXct datetime value of the df to use as x coordinates
# - param: List or 1-row df containing the following values accessible with '$':
#          + param_name: String
#          + units: String
# - plotTitle: String containing the title of the plot
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Use !! to unquote the symbole returned by sym() -- trick to use string in ggplot2 aes()
  p <- ggplot(df, aes(!!sym(x), value, color = Site_ID, linetype = parameter, shape = parameter))+
    geom_point(size = 2, na.rm = TRUE)+
    # Use geom_line to plot LOESS curve in order to use linetype aes
    geom_line(stat="smooth", method = "loess", formula = y ~ x,
              size = 1.2,
              alpha = 0.5)+
    ggtitle(plotTitle)+
    ylab(str_interp('${param$param_name} [${param$units}]'))+
    xlab('Date')+
    # Set color of the data groups
    # And remove the line from the color legend images to keep the points only
    scale_color_manual(
      values = siteColors(df, sites),
      guide = guide_legend(override.aes = list(
        linetype = rep(0, length(unique(df$Site_ID)))
      ))
    )+
    # Change the linetype legend label to 'LOESS curve'
    scale_linetype(labels = 'LOESS curve')+
    # Set the y axis limits
    scale_y_continuous(limits = calculateYaxisLimits(min(df$value, na.rm = TRUE), max(df$value, na.rm = TRUE)))+
    # Set theme
    theme_bw()+
    # Remove legend title, move legend to the bottom of the plot and set text size
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11)
    )+
    # Remove the shape legend
    guides(shape = 'none')
  return(p)
}


sdPlot <- function(df, x, param, plotTitle, sites) {
# Function that create a simple scatter plot with a LOESS curve and error bars for each point for one value
# Parameters:
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'parameter': factor
#       + x: POSIXct datetime (column name corresponding to the string passed through the argument 'x')
#       + sd: numeric (column name corresponding to the string passed via 'param$sd')
# - x: String corresponding to the column name of a POSIXct datetime value of the df to use as x coordinates
# - param: List or 1-row df containing the following values accessible with '$':
#          + param_name: String
#          + units: String
#          + sd: String (column name for the corresponding parameter's sd)
# - plotTitle: String containing the title of the plot
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Create a symbole with the string contained in param$sd
  sd <- sym(param$sd)
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  # Add error bars to the plot using the sd
  p <- p + geom_errorbar(aes(ymin = value - !!sd, ymax = value + !!sd))
  return(p)
}


minMaxPlot <- function(df, x, param, plotTitle, sites) {
# Function that create a simple scatter plot with a LOESS curve for one value and a min and max value
# Parameters:
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'parameter': factor, 3 levels (the average, a min and a max)
#       + x: POSIXct datetime (column name corresponding to the string passed through the argument 'x')
# - x: String corresponding to the column name of a POSIXct datetime value of the df to use as x coordinates
# - param: List or 1-row df containing the following values accessible with '$':
#          + param_name: String
#          + units: String
# - plotTitle: String containing the title of the plot
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  p <- p + 
    # Set the linetype values of the parameters groups to make only the avg visible
    scale_linetype_manual(values = c(1, 0, 0))+
    # Manually set the shapes of the avg, min and max points
    scale_shape_manual(values = c(16, 1, 0))+
    # Display the shape legend
    guides(shape = 'legend')
  return(p)
}


multiPlot <- function(df, x, param, plotTitle, sites) {
# Function that create a simple scatter plot with a LOESS curve for one, two or three parameters
# Parameters:
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'parameter': factor
#       + x: POSIXct datetime (column name corresponding to the string passed through the argument 'x')
# - x: String corresponding to the column name of a POSIXct datetime value of the df to use as x coordinates
# - param: List or 1-row df containing the following values accessible with '$':
#          + param_name: String
#          + units: String
# - plotTitle: String containing the title of the plot
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Create a basicPlot
  p <- basicPlot(df, x, param, plotTitle, sites)
  p <- p +
    # Define the linetypes in case of multiple parameters
    scale_linetype_manual(values = c(1, 3, 2), name = 'parameter')+
    # Define the point shapes in case of multiple parameters
    scale_shape_manual(values = c(16, 15, 17), name = 'parameter')+
    # Display the shape legend
    guides(shape = 'legend')
  return(p)
}

timeSeriePlot <- function(df, x, parameter, siteName, sites) {
# Function that create a time series plot using the plotting function encoded in the parameter argument
# Parameters:
# - df: DataFrame in long format (for shape details, refer to subsequent plotting function used)
# - x: String corresponding to the column name of the df to use as x coordinates
# - parameter: List or 1-row df containing the following values accessible with '$':
#              + plot_func: String corresping to a plotting function name
#              + other required by the specific plotting function used
# - siteName: String containing the site name to use for the plot title generation
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Recover plotting function using string
  plottingFunc <- match.fun(parameter$plot_func)
  # Create plot title
  plotTitle <- str_interp('${siteName} Time Serie')
  # Create plot using the plotting function
  p <- plottingFunc(df, x, parameter, plotTitle, sites)
  # Set the x axis as datetime
  p <- p + scale_x_datetime(date_minor_breaks = '1 month')
  return(p)
}


DOYPlot <- function(df, x, parameter, siteName, sites) {
# Function that create a DOY time series plot using the plotting function encoded in the parameter argument
# Parameters:
# - df: DataFrame in long format with the folling requiered columns:
#       + x: POSIXct datetime for which each value has 2020 set as year
#            (column name corresponding to the string passed through the argument 'x')
#       + for shape details, refer to subsequent plotting function used
# - x: String corresponding to the column name of the df to use as x coordinates
# - parameter: List or 1-row df containing the following values accessible with '$':
#              + plot_func: String corresping to a plotting function name
#              + other required by the specific plotting function used
# - siteName: String containing the site name to use for the plot title generation
# - sites: Data.frame, contains the sites info (e.i. sites_color)
#
# Returns a ggplot2 plot
  
  # Recover plotting function using string
  plottingFunc <- match.fun(parameter$plot_func)
  # Create plot title
  plotTitle <- str_interp('${siteName} DOY Serie')
  # Create plot using the plotting function
  p <- plottingFunc(df, x, parameter, plotTitle, sites)
  # Set the x axis as datetime
  p <- p + scale_x_datetime(
    date_breaks = '1 month',
    date_labels = '%b',
    limits = c(ymd_hms('2020-01-01 00:00:00 GMT'), ymd_hms('2020-12-10 00:00:00 GMT'))
  )
  return(p)
}




highFreqTimeSeriePlot <- function(df, parameter, plotTitle, sites, modeledData = FALSE) {
# Function that create a time serie plot for the high frequency data
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'data_type': factor, either modeled or measured, if modeledData = TRUE
#       + 'date': POSIXct datetime
# - parameter: List or 1-row df containing the following values accessible with '$':
#          + param_name: String
#          + units: String
# - plotTitle: String containing the title of the plot
# - sites: Data.frame, contains the sites info (e.i. sites_color)
# - modeledData: Boolean, indicates if df contains modeled data column, default FALSE
#
# Returns a ggplot2 plot
  
  # Create plot base
  p <- ggplot(df, aes(date, value, color = Site_ID))
  
  # If df contains modeled data add some additional aes to geom_line
  if (modeledData) {
    p <- p + geom_line(mapping = aes(linetype = data_type, alpha = data_type), size = 1, na.rm = TRUE)
  } else {
    p <- p + geom_line(size = 1, na.rm = TRUE)
  }
  
  # Finished plot construction
  p <- p + ggtitle(plotTitle)+
    ylab(str_interp('${parameter$param_name} [${parameter$units}]'))+
    xlab('Date')+
    # Set the y axis limits
    scale_y_continuous(limits = calculateYaxisLimits(min(df$value, na.rm = TRUE), max(df$value, na.rm = TRUE)))+
    # Set color of the data groups
    scale_color_manual(values = siteColors(df, sites))+
    # Set alpha values
    scale_alpha_manual(values = c('measured' = 1, 'modeled' = 0.3))+
    # Set theme
    theme_bw()+
    # Remove legend title, move legend to the bottom of the plot and set text size
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11)
    )
  return(p)
}




addGrabSamplePoints <- function(p, df, minHf, maxHf) {
# Function that add points to an highFreqTimeSeriePlot
# - p: ggplot2 object, plot returned by the highFreqTimeSeriePlot function
# - df: DataFrame in long format containing the following columns:
#       + 'Site_ID': factor
#       + 'value': numeric
#       + 'DATETIME_GMT': POSIXct datetime
# - minHf: numeric, the minimal plotted value of the HF data
# - maxHf: numeric, the maximal plotted value of the HF data
#
# Returns a ggplot2 plot
  
  # Add the point to the graph
  p <- p + geom_point(data = df, mapping = aes(x = DATETIME_GMT, y = value), size = 3, color = 'black') +
    # Correct the y scale to display all the information
    scale_y_continuous(limits = calculateYaxisLimits(
      min(min(df$value, na.rm = TRUE), minHf),
      max(max(df$value, na.rm = TRUE), maxHf)
    ))
  
  return(p)
}




onVsOnePlot <- function(df, x, y, parameterX, parameterY, plotTitle, color) {
# Function that create a time serie plot for the high frequency data
# - df: DataFrame in long format containing the following columns:
#       + x: numeric
#       + y: numeric
# - x, y: String, df column names to use as x and y coordinates
# - parameterX,
#   parameterY: List or 1-row df containing the following values accessible with '$':
#               + param_name: String
#               + units: String
# - plotTitle: String containing the title of the plot
# - color: String, contains the site color
#
# Returns a ggplot2 plot
  
  p <- ggplot(df, aes(!!sym(x), !!sym(y)))+
    # Plot the lm confidence interval
    geom_smooth(method = lm, fill = color, alpha = .17, linetype = 0, na.rm = TRUE, fullrange = TRUE)+
    # PLot the lm line
    geom_line(stat = 'smooth', method = "lm", formula = y ~ x, size = 1.5, color = color, alpha = .5, fullrange = TRUE)+
    # Plot the data points
    geom_point(size = 2.5, color = color, na.rm = TRUE)+
    # Add the lm equation and r2 on top left
    annotate('text', -Inf, Inf, hjust = -.1, vjust = 1.5,
             label = lm_eqn(df, x, y), parse = TRUE, size = 5)+
    # Set plot title and axis names
    ggtitle(str_interp(plotTitle))+
    ylab(str_interp('${parameterY$param_name} [${parameterY$units}]'))+
    xlab(str_interp('${parameterX$param_name} [${parameterX$units}]'))+
    # Set theme
    theme_bw()+
    # Remove legend title, move legend to the bottom of the plot and set text size
    theme(
      plot.title = element_text(size = 16, face = 'bold'),
      legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 10),
      axis.title = element_text(size = 14), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 11)
    )
  return(p)
}





addOneToOneLine <- function(p, minData, maxData) {
# Function that add a one to one line to a oneVsOnePlot
# - p: ggplot2 object, plot returned by the oneVsOnePlot function
# - minData: numeric, the minimal plotted value of the data
# - maxData: numeric, the maximal plotted value of the data
#
# Returns a ggplot2 plot
  
  # Add the on to one line
  p <- p + geom_abline() +
    # Set the plot limits large enough to extend both the fitting curve and se to the edge of the plot
    xlim(calculateYaxisLimits(minData, maxData, perc = .5))+
    ylim(calculateYaxisLimits(minData, maxData, perc = .5))+
    # Set the visible area of the plot
    coord_cartesian(
      xlim = calculateYaxisLimits(minData, maxData),
      ylim = calculateYaxisLimits(minData, maxData)
    )
  
  # Return the updated plot
  return(p)
}


