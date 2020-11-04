# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.7
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#    [1] rhandsontable_0.3.7   DT_0.16               dbplyr_1.4.4          pool_0.1.4.3         
#    [5] DBI_1.1.0             dplyr_1.0.2           magrittr_1.5          tidyr_1.1.2          
#    [9] forcats_0.5.0         lubridate_1.7.9       data.table_1.13.2     Cairo_1.5-12.2       
#   [13] ggplot2_3.3.2         stringr_1.4.0         jsonlite_1.7.1        shinycssloaders_1.0.0
#   [17] shinybusy_0.2.2       shinyWidgets_0.5.4    shinyjs_2.0.0         shiny_1.5.0          
# 
# loaded via a namespace (and not attached):
#    [1] Rcpp_1.0.4.6      lattice_0.20-41   ps_1.3.3          assertthat_0.2.1  digest_0.6.25    
#    [6] packrat_0.5.0     mime_0.9          R6_2.4.1          pillar_1.4.4      rlang_0.4.8      
#   [11] rstudioapi_0.11   blob_1.2.1        Matrix_1.2-18     labeling_0.3      splines_4.0.2    
#   [16] RMySQL_0.10.20    readr_1.4.0       htmlwidgets_1.5.1 munsell_0.5.0     compiler_4.0.2   
#   [21] httpuv_1.5.4      pkgconfig_2.0.3   mgcv_1.8-31       htmltools_0.5.0   tidyselect_1.1.0 
#   [26] tibble_3.0.1      crayon_1.3.4      withr_2.2.0       later_1.1.0.1     grid_4.0.2       
#   [31] nlme_3.1-148      xtable_1.8-4      gtable_0.3.0      lifecycle_0.2.0   scales_1.1.1     
#   [36] stringi_1.4.6     farver_2.0.3      fs_1.4.2          promises_1.1.1    ellipsis_0.3.1   
#   [41] generics_0.0.2    vctrs_0.3.4       tools_4.0.2       glue_1.4.1        purrr_0.3.4      
#   [46] hms_0.5.3         crosstalk_1.1.0.1 rsconnect_0.8.16  processx_3.4.2    fastmap_1.0.1    
#   [51] yaml_2.2.1        colorspace_1.4-1  sodium_1.1        sass_0.2.0  


## Load Libraries #################################################################
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(shinycssloaders)
library(jsonlite)
library(stringr)
library(ggplot2)
library(Cairo)
library(data.table)
library(lubridate)
library(forcats)
library(tidyr)
library(magrittr)
library(dplyr)
library(DBI)
library(pool)
library(dbplyr)
library(DT)
library(rhandsontable)



## Load Secret Global Variables ###################################################
source('./secrets.R')

## Load helper functions ##########################################################
source('./utils/helper_functions.R')



## Development environment specific ##########################################################

if (ENV == 'development') {
  # Compile CSS from Sass
  sass::sass(
    sass::sass_file('assets/sass/main.scss'), 
    output = 'www/main.css',
    options = sass::sass_options(output_style = 'compressed')
  )
  
  # Compile and minify JavaScript
  js_parser()
}



## Load data ######################################################################

# Load data loading functions
source('./utils/data_preprocessing.R')

grabSampleDf <- loadGrabSampleDf()
hfDf <- loadHighFreqDf()



## Connect to DB ##################################################################

# Load BD interactions functions
source('./utils/db_interaction.R')

# Create pool connection with the DB
pool <- connectToDB()



## Load Shiny extensions functions ################################################
source('./utils/shiny_extensions.R')



## Load tabs modules ##############################################################
source('./modules/login/login.R')
source('./modules/visualisation_tab/visualisation_tab.R')
source('./modules/download_tab/download_tab.R')
source('./modules/data_management_tab/data_management_tab.R')
source('./modules/users_tab/users_tab.R')
source('./modules/editableDT/editableDT.R')



## Create main UI #################################################################

ui <- tagList(
  # Load shinyjs
  useShinyjs(),
  # Load htmlwidgets.js at startup to avoid reference error problem in Firefox
  # when the datatables are loaded dynamically
  htmltools:: htmlDependency("htmlwidgets", packageVersion("htmlwidgets"),
                 src = system.file("www", package="htmlwidgets"),
                 script = "htmlwidgets.js"
  ),
  # Add stylesheet link and script tags to head
  tags$head(
    # Add link to main.css stylesheet
    tags$link(href = 'main.css', rel = 'stylesheet', type = 'text/css'),
    # Add link for js script
    tags$script(src = 'metalpdataportal.js')
  ),
  # Create the navbarPage using custom function to add a content-wrapper (defined in './utils/shiny_extensions.R')
  navbarPageWithWrapper(
    # Create Navabar page with login
    withLoginAction(
      # Pass in the output of shiny navbarPage()
      navbarPage(
        id = 'main-nav',
        # Load the custom logo for the navbar title
        htmlTemplate('./html_components/logo.html'),
        # Set a window browser window title
        windowTitle = 'METALP DATA PORTAL',
        # Create the home tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('home'),tags$span('Home', class = 'navbar-menu-name')),
          # Load the home page template with some icons
          htmlTemplate(
            'html_components/home.html',
            chartIcon = icon('chart-bar'),
            dataIcon = icon('database'),
            toolboxIcon = icon('toolbox'),
            downloadIcon = icon('download')
          ),
          value = 'home'
        ),
        # Create the visualisation tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('chart-bar'),tags$span('Data visualisation', class = 'navbar-menu-name')),
          # Load the visualisationTab module UI elements
          visualisationTabUI('visu', pool, grabSampleDf, hfDf),
          value = 'visu'
        ),
        # Create the download tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
          downloadTabUI(
            'dl',
            pool = pool,
            minDate = min(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
            maxDate = max(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE)
          ),
          value = 'dl'
        )
      ),
      # Add the login module UI
      loginUI('login')
    ),
    # Add footer to navbarPageWithWrapper
    footer = htmlTemplate('html_components/footer.html')
  )
)



## Create server function #########################################################

server <- function(input, output, session) {
  ## Load login module server logic ###############################################
  user <- callModule(login, 'login', pool)


  ## Load visualisationTab module server logic ####################################
  callModule(visualisationTab, 'visu',
             pool, user,
             grabSampleDf, hfDf)



  ## Load downloadTab module server logic #########################################
  callModule(downloadTab, 'dl',
             pool = pool,
             user,
             grabSampleDf, hfDf,
             minDate = min(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
             maxDate = max(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE))


  ## Check authorizations #########################################################

  # Do it when the user role changes
  observeEvent(user$role, {
    # if (user$role %in% c('intern', 'sber', 'admin')) {
    #   ## Generate toolsTab ##########################################################
    #
    #   # Create the toolbox tab
    #   appendTab(
    #     'main-nav',
    #     tabPanel(
    #       # Create a tab title with an icon
    #       tags$span(icon('toolbox'),tags$span('Toolbox', class = 'navbar-menu-name')),
    #       value = 'tools'
    #     )
    #   )
    #
    #   # Load tools tab server logic
    # }


    if (user$role %in% c('sber', 'admin')) {
      ## Generate dataManagementTab #################################################

      # Create the data management tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('database'),tags$span('Data management', class = 'navbar-menu-name')),
          dataManagementTabUI('data', pool),
          value = 'data'
        )
      )

      # Load data management server logic
      callModule(dataManagementTab, 'data', pool)
    }


    if (user$role == 'admin') {
      ## Generate usersTab ##########################################################

      # Create users tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('user'), tags$span('Users', class = 'navbar-menu-name')),
          usersTabUI('users'),
          value = 'users'
        )
      )

      # Load users tab server logic
      callModule(usersTab, 'users', pool)
    }
  })
}



## Launch App #####################################################################

shinyApp(ui, server, onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    poolClose(pool)
  })
})
