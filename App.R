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
#   [1] future_1.20.1         promises_1.1.1        rhandsontable_0.3.7   DT_0.16               dbplyr_1.4.4         
#   [6] pool_0.1.4.3          DBI_1.1.0             dplyr_1.0.2           magrittr_1.5          tidyr_1.1.2          
#  [11] forcats_0.5.0         lubridate_1.7.9       data.table_1.13.2     Cairo_1.5-12.2        ggplot2_3.3.2        
#  [16] stringr_1.4.0         jsonlite_1.7.1        shinycssloaders_1.0.0 shinybusy_0.2.2       shinyWidgets_0.5.4   
#  [21] shinyjs_2.0.0         shiny_1.5.0          
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-148      fs_1.4.2          usethis_1.6.1     devtools_2.3.1    rprojroot_1.3-2   tools_4.0.2      
#   [7] backports_1.1.8   R6_2.4.1          mgcv_1.8-31       colorspace_1.4-1  withr_2.2.0       tidyselect_1.1.0 
#  [13] sodium_1.1        prettyunits_1.1.1 processx_3.4.2    RMySQL_0.10.20    compiler_4.0.2    cli_2.0.2        
#  [19] desc_1.2.0        labeling_0.3      sass_0.2.0        scales_1.1.1      DEoptimR_1.0-8    robustbase_0.93-6
#  [25] readr_1.4.0       callr_3.4.3       digest_0.6.25     pkgconfig_2.0.3   htmltools_0.5.0   parallelly_1.21.0
#  [31] sessioninfo_1.1.1 bigleaf_0.7.1     fastmap_1.0.1     htmlwidgets_1.5.1 rlang_0.4.8       rstudioapi_0.11  
#  [37] farver_2.0.3      generics_0.0.2    crosstalk_1.1.0.1 Matrix_1.2-18     Rcpp_1.0.4.6      munsell_0.5.0    
#  [43] fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        pkgbuild_1.0.8    grid_4.0.2       
#  [49] blob_1.2.1        parallel_4.0.2    listenv_0.8.0     crayon_1.3.4      lattice_0.20-41   splines_4.0.2    
#  [55] hms_0.5.3         ps_1.3.3          pillar_1.4.4      codetools_0.2-16  pkgload_1.1.0     glue_1.4.1       
#  [61] packrat_0.5.0     remotes_2.2.0     vctrs_0.3.4       solartime_0.0.1   httpuv_1.5.4      testthat_2.3.2   
#  [67] gtable_0.3.0      purrr_0.3.4       assertthat_0.2.1  mime_0.9          xtable_1.8-4      later_1.1.0.1    
#  [73] rsconnect_0.8.16  tibble_3.0.1      memoise_1.1.0     globals_0.13.1    ellipsis_0.3.1   


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
library(promises)
library(future)



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
  
  # Plan future strategy
  # Use multisession because the development version will run mainly within Rstudio
  plan(multisession)
} else {
  # Plan future strategy
  # Use multicore on the production server
  plan(multicore)
}



## Load data ######################################################################

# Load data loading functions
source('./utils/data_preprocessing.R')

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
source('./modules/portal_management/portal_management.R')
source('./modules/data_requests_management/data_requests_management.R')
source('./modules/editableDT/editableDT.R')
source('./modules/instruction_panel/intruction_panel.R')



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
        # Create the about tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('campground'),tags$span('About', class = 'navbar-menu-name')),
          htmlTemplate(
            './html_components/about.html',
            dlTabLink = actionLink('aboutDlLink', 'Download tab'),
            extLinkIcon = icon('external-link-alt', class = 'ext-link')
          ),
          value = 'aboutTab'
        ),
        # Create the visualisation tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('chart-bar'),tags$span('Visualisation', class = 'navbar-menu-name')),
          # Load the visualisationTab module UI elements
          visualisationTabUI('visu', pool, hfDf),
          value = 'visuTab'
        ),
        # Create the download tab
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('download'),tags$span('Download', class = 'navbar-menu-name')),
          downloadTabUI(
            'dl',
            pool = pool,
            hfDfMinMaxDates = list(
              min = min(date(hfDf$`10min`$Date), na.rm = TRUE),
              max = max(date(hfDf$`10min`$Date), na.rm = TRUE)
            )
          ),
          value = 'dlTab'
        )
      ),
      # Add the login module UI
      loginUI('login')
    ),
    # Add footer to navbarPageWithWrapper
    footer = htmlTemplate(
      'html_components/footer.html',
      aboutLink = actionLink('aboutLink', 'About'),
      visuLink = actionLink('visuLink', 'Visualisation'),
      dlLink = actionLink('dlLink', 'Download'),
      creditsLink = actionLink('credits', 'Credits & Source code'),
      githubIcon = icon('github'),
      linkedinIcon = icon('linkedin'),
      twitterIcon = icon('twitter')
    )
  )
)



## Create server function #########################################################

server <- function(input, output, session) {
  
  ## Open specific tab when accessing the app #####################################
  isolate({
    if ('tab' %in% names(getQueryString())) {
      updateNavbarPage(session, 'main-nav', selected = getQueryString()$tab)
    }
  })
  
  
  ## Set maximum request size big enough for sensor data upload ###################
  # Set it to 100MB
  options(shiny.maxRequestSize=200*1024^2)
  
  
  
  
  ## Load login module server logic ###############################################
  user <- callModule(login, 'login', pool)


  ## Load visualisationTab module server logic ####################################
  callModule(visualisationTab, 'visu', pool, user, hfDf)



  ## Load downloadTab module server logic #########################################
  callModule(downloadTab, 'dl', pool, user, hfDf)
  
  
  
  ## Footer Navigation logic ######################################################
  
  observeEvent(input$aboutLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'aboutTab'))
  
  observeEvent(input$visuLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'visuTab'))
  
  observeEvent(input$dlLink | input$aboutDlLink, ignoreInit = TRUE, {
    req(input$dlLink != 0 | input$aboutDlLink != 0)
    updateNavbarPage(session, 'main-nav', selected = 'dlTab')
  })
  
  observeEvent(input$credits, ignoreInit = TRUE, {
    showModal(modalDialog(
      htmlTemplate(
        './html_components/credits.html',
        githubIcon = icon('github'),
        linkedinIcon = icon('linkedin'),
        twitterIcon = icon('twitter')
      ),
      footer = modalButtonWithClass('Close', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
  
  


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
          tags$span(icon('database'),tags$span('Data', class = 'navbar-menu-name')),
          dataManagementTabUI('data', pool, user$role),
          value = 'dataTab'
        )
      )

      # Load data management server logic
      callModule(dataManagementTab, 'data', pool, user$role)
      
      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('dataLink', 'Data')),
        immediate = TRUE
      )
      
      # Add nav update logic
      observeEvent(input$dataLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'dataTab'))
    }


    if (user$role == 'admin') {
      ## Generate portalManagement tab ##########################################################

      # Create users tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          tags$span(icon('empire'), tags$span('Portal', class = 'navbar-menu-name')),
          portalManagementUI('portal', pool),
          value = 'portalTab'
        )
      )

      # Load users tab server logic
      callModule(portalManagement, 'portal', pool)
      
      # Add link to footer
      insertUI(
        selector = '#footer-nav-insertion',
        where = 'beforeBegin',
        ui = tags$li(actionLink('portalLink', 'Portal')),
        immediate = TRUE
      )
      
      # Add nav update logic
      observeEvent(input$portalLink, ignoreInit = TRUE, updateNavbarPage(session, 'main-nav', selected = 'portalTab'))
    }
    
    
    
    
    ## Generate data request tab ##########################################################
    if (user$role %in% c('sber', 'admin')) {
      # Get the UI
      requestUI <- requestsManagementUI('requests', pool)
      
      # Insert the link into navbar
      insertUI('#login-ui', 'beforeBegin', requestUI$navbarUI, immediate = TRUE)
      
      # Add data requests tab
      appendTab(
        'main-nav',
        tabPanel(
          # Create a tab title with an icon
          'Data Requests',
          requestUI$tabContent,
          value = 'requestsTab'
        )
      )
      
      # Call request tab module
      callModule(requestsManagement, 'requests',
                 pool = pool,
                 navbarSession = session,
                 navbarId = 'main-nav',
                 requestTabId = 'requestsTab')
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
