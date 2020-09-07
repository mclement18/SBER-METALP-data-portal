# METALP Data Portal

This Shiny app is meant to provide an easy interface for the METALP researchers to interact with their data. It can also be used as a data sharing portal accessible via the Web if wanted.

## Dependecies

Main dependencies:
- R version 4.0.2
- Node.js (tested version: 14.9.0)
- Terser.js (tested version: 5.3.0)
- Java SE (tested version: 14.0.2)

R packages:
- mailR_0.4.1
- dplyr_1.0.0
- magrittr_1.5
- tidyr_1.1.0
- forcats_0.5.0
- lubridate_1.7.9      
- data.table_1.12.8
- Cairo_1.5-12.2
- ggplot2_3.3.2
- stringr_1.4.0
- readr_1.3.1
- jsonlite_1.6.1       
- sass_0.2.0
- shinycssloaders_1.0.0
- shinybusy_0.2.0
- shinyWidgets_0.5.3
- shinyjs_1.1
- shiny_1.4.0.2

## Installation

### R and Rstudio
You need to install R and we recommend to use Rstudio as well. You can get the latest version of R or the recommend version forthis app on the CRAN website https://cran.r-project.org/ and Rstutio from their website https://rstudio.com/products/rstudio/download/.

### Node and Terser
To be able to parse and minify the cusom JavaScript files you will need to install Node.js and Terser.js as well.

To install Node please refer yourselfe to the documentation https://nodejs.org/en/download/.

Terser.js will need to be install globally to be accessible by the app. Once Node is installed run the following command:
```sh
npm install terser -g
```

### Java SE

To use the `mailR` R package you will need to install a JDK. To download and install it please refer yourselfe to the Oracle documentation https://www.oracle.com/java/technologies/javase-downloads.html.

## App oraganisation

The main app is contained in the `App.R` script.
All app subfonctionalities are separated in modules within the `modules` folder organized by tabs.
All the data are in the `data` folder.
The `www` folder contains all publicly available ressources.
The assets folder contains the custom JavScript and SCSS sepereated in the `js` and `sass` folders. Both JavaScript and SCSS are compiled when the app is started and saved into the `www` folder.
The `html_components` contains all the custom HTML elements to use in the app.
All the custom script, helper functions and shiny extensions are kept in the `utils` folder.
