# METALP Data Portal

This Shiny app is meant to provide an easy interface for the METALP researchers to interact with their data. It can also be used as a data sharing portal accessible via the Web if wanted.

## Dependecies

Main dependencies:
- R version 4.0.2
- Node.js (tested version: 14.9.0)
- Terser.js (tested version: 5.3.0)

R packages:
- RMySQL_0.10.20
- bigleaf_0.7.1
- sodium_1.1
- purrr_0.3.4
- readr_1.4.0
- sass_0.3.1
- future_1.20.1
- promises_1.1.1
- rhandsontable_0.3.7
- DT_0.16
- dbplyr_1.4.4
- pool_0.1.4.3
- DBI_1.1.0
- dplyr_1.0.2
- magrittr_1.5
- tidyr_1.1.2
- forcats_0.5.0
- lubridate_1.7.9
- data.table_1.13.2
- Cairo_1.5-12.2
- ggplot2_3.3.2
- stringr_1.4.0
- jsonlite_1.7.1
- shinycssloaders_1.0.0
- shinybusy_0.2.2
- shinyWidgets_0.5.4
- shinyjs_2.0.0
- shiny_1.5.0

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

## App oraganisation

### App.R
The main app script is the `App.R` file. It contains the basic app structure and logic to create and operate the main tabs and the initialization tasks.

### Modules
The app is organized and subdivised by tabs. Each tab and sub-tab is contained in a isolated module present in the `modules` directory. The structure of the `modules` directory should mirror the app structure. Some other reusable shiny components that aren't tabs or sub-tabs, but require both an UI and a server function, are also containerized in modules and located in a relevant place of the directory structure of the `modules` directory.

### Utils
Some reusable functions are organized by functionality in different files located in the `utils` directory.

| File                    | Description |
|:----------------------- |:----------- |
| calculation_functions.R | Contains all the functions necessary for the calculation of the parameters from the database that can be calculated based on other entered values. |
| data_preprocessing.R    | Contains all the functions used for the preprocessing of the sensor data loadded during the app startup. |
| db_interaction.R        | Contains the functions used to communicate with the _SQL_ database. |
| helper_functions.R      | Contains various functions that does not fit in the other files. |
| plotting_functions.R    | Contains the functions used to create ggplots. |
| shiny_extensions.R      | Contains the functions that extend _Shiny_ functionalities. |

### Assets
The custom stylesheet in _SCSS_ format and _JavaScript_ are located in the `sass` and `js` directories, respectively. These directories are located in the `assets` directory.

During development, both _SCSS_ and _JavaScript_ files are compiled and minified in two files, `main.css` and `metalpdataportal.js`, which are saved in the `www` directory.

To deploy the app in **production**, the assets **must be compiled manually** by running the `assets_compilation.R` script file from the app folder. This will create new `main.css` and `metalpdataportal.js` files tagged with the current timestamp to avoid browser cache conflicts.

When assets are compiled, a `assets_name.R` file is created/updated. This file is containing environment variables indicating the names of the two assets files and is sourced during app startup.

#### _SCSS_
In the `sass` directory, the `main.scss` file is an index that is used to load in the correct order all the partial files organized in different thematic directories.

#### _JavaScript_
The `js` directory contains all the _JavaScript_ code organized in different files by functionnality. The `manifest.json` file is used to compile the files together in a predefined order.

### Other _R_ script files

#### secrets.R
A `secrets.R` file **is required** and should contains all sensible information, such as DB name or password. These information are saved in environment variables when the file is sourced during the app startup. More info at https://github.com/mclement18/METALP-Portal-server/tree/master/app_deployment#create-or-update-secretr-file

#### packages_installation.R
The `packages_installation.R` file contains instructions to install the _R_ packages with the correct version. To install them, just run the script file. Currently, only the `dbplyr` package need a specific version for the app to work.

### Other directories

#### HTML components
The `html_components` directory contains all the HTML template used in the app.

#### www
The `www` directory is the public directory of the _Shiny_ app in which all the publicly accessible ressources must be put, such as favicon, images or assets.

#### Data
The `data` directory should contain the sensor data that are loaded during the app startup. For more specific information about its structure and the files needed, see comments in the `utils/data_preprocessing.R` and `modules/portal_management/portal_actions.R` files.

#### DB backups
The `db_backups` directory **should be present** and will contains all the _SQL_ database backups made with the DB backup functionnality of the portal actions module. See `modules/portal_management/portal_actions.R` file for more information.

## App deployment
Detail information on how to deploy this app on an _Ubuntu_ server can be found here: https://github.com/mclement18/METALP-Portal-server/tree/master/app_deployment
