# R version 4.0.1 (2020-06-06)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.5
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
#   [1] dplyr_1.0.0       tidyr_1.1.0       lubridate_1.7.9   data.table_1.12.8 ggplot2_3.3.2     stringr_1.4.0     jsonlite_1.6.1    sass_0.2.0       
# [9] shinyjs_1.1       shiny_1.4.0.2    
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.4.6     compiler_4.0.1   pillar_1.4.4     later_1.1.0.1    tools_4.0.1      digest_0.6.25    packrat_0.5.0    lattice_0.20-41  nlme_3.1-148    
# [10] lifecycle_0.2.0  tibble_3.0.1     gtable_0.3.0     mgcv_1.8-31      pkgconfig_2.0.3  rlang_0.4.6      Matrix_1.2-18    rstudioapi_0.11  fastmap_1.0.1   
# [19] withr_2.2.0      fs_1.4.2         generics_0.0.2   vctrs_0.3.1      grid_4.0.1       tidyselect_1.1.0 glue_1.4.1       R6_2.4.1         farver_2.0.3    
# [28] purrr_0.3.4      magrittr_1.5     splines_4.0.1    scales_1.1.1     promises_1.1.1   htmltools_0.5.0  ellipsis_0.3.1   rsconnect_0.8.16 mime_0.9        
# [37] xtable_1.8-4     colorspace_1.4-1 httpuv_1.5.4     labeling_0.3     stringi_1.4.6    munsell_0.5.0    crayon_1.3.4  


## Load Libraries #################################################################
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(shinycssloaders)
library(sass)
# library(mailR)
library(jsonlite)
library(readr)
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



## Load Secret Global Variables ###################################################
source('./secrets.R')



## Compile CSS from Sass ##########################################################
sass(
  sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass_options(output_style = 'compressed')
)




## Load helper functions ##########################################################
source('./utils/helper_functions.R')



## Compile and minify JavaScript ##################################################
js_parser()



## Load data ######################################################################

# Load data loading functions
source('./utils/data_preprocessing.R')

grabSampleDf <- loadGrabSampleDf()
hfDf <- loadHighFreqDf()

sites <- loadSites()
grabSampleParameters <- loadGrabSamplesParameters()
hfParameters <- loadHfParameters()



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



## Create main UI #################################################################

ui <- tagList(
  # Load shinyjs
  useShinyjs(),
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
          visualisationTabUI('visu', grabSampleDf, hfDf, sites, grabSampleParameters, hfParameters),
          value = 'visu'
        ),
        # Create the download tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
          downloadTabUI(
            'dl',
            minDate = min(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
            maxDate = max(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
            sites = sites,
            grabSampleParameters = grabSampleParameters,
            hfParameters = hfParameters
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
             user,
             grabSampleDf, hfDf,
             sites, grabSampleParameters, hfParameters)
  
  
               
  ## Load downloadTab module server logic #########################################
  callModule(downloadTab, 'dl',
             user,
             grabSampleDf, hfDf,
             minDate = min(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
             maxDate = max(grabSampleDf$DATE_reading, date(hfDf$`10min`$Date), na.rm = TRUE),
             sites, grabSampleParameters, hfParameters)
    
  
  ## Check authorizations #########################################################
  
  # Do it when the user role changes
  observeEvent(user$role, {
    if (user$role %in% c('intern', 'sber', 'admin')) {
      ## Generate dataManagementTab #################################################
      
      # Create the data management tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('database'),tags$span('Data management', class = 'navbar-menu-name')),
          value = 'data'
        )
      )
      
      # Load data management server logic
      
      ## Generate toolsTab ##########################################################
      
      # Create the toolbox tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('toolbox'),tags$span('Toolbox', class = 'navbar-menu-name')),
          value = 'tools'
        )
      )
      
      # Load tools tab server logic
    }
    
    
    if (user$role == 'admin') {
      ## Generate usersTab ##########################################################
      
      # Create users tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('user'), tags$span('Users', class = 'navbar-menu-name')),
          value = 'users'
        )
      )
      
      # Load users tab server logic
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
