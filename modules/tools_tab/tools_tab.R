## This module contains the UI and server code for the Tools tab

## Source needed files ############################################################

source('./modules/tools_tab/tool_layout.R')
source('./modules/tools_tab/entry_layout.R')
source('./modules/tools_tab/tool_table.R')
source('./modules/tools_tab/tools/field_data_tool.R')
source('./modules/tools_tab/tools/doc_tool.R')
source('./modules/tools_tab/tools/dom_tool.R')
source('./modules/tools_tab/tools/alkalinity_tool.R')
source('./modules/tools_tab/tools/co2_air_tool.R')
source('./modules/tools_tab/tools/pco2_tool.R')
source('./modules/tools_tab/tools/dic_tool.R')
source('./modules/tools_tab/tools/ions_tool.R')
source('./modules/tools_tab/tools/nutrients_tool.R')
source('./modules/tools_tab/tools/tss_afdm_tool.R')
source('./modules/tools_tab/tools/chla_tool.R')
source('./utils/calculation_functions.R')



## Create module UI ###############################################################

toolsTabUI <- function(id) {
# Create the UI for the toolsTab module
# Parameters:
#  - id: String, the module id
# 
# Returns a tabsetPanel containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create a tabsetPanel to create sub navigation
  tabsetPanel(
    id = ns('toolsTabs'),
    tabPanel(
      # Tab title
      'Field data',
      # Tab content
      toolsLayoutUI(
        ns('fieldDataTool'),
        'Field data',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_field_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('fieldDataTool')
    ),
    tabPanel(
      # Tab title
      'DOC',
      # Tab content
      toolsLayoutUI(
        ns('docTool'),
        'DOC',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_doc_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('docTool')
    ),
    tabPanel(
      # Tab title
      'DOM',
      # Tab content
      toolsLayoutUI(
        ns('domTool'),
        'DOM',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_dom_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('domTool')
    ),
    tabPanel(
      # Tab title
      'Alkalinity',
      # Tab content
      toolsLayoutUI(
        ns('alkalinityTool'),
        'Alkalinity',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_alkalinity_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('alkalinityTool')
    ),
    tabPanel(
      # Tab title
      'CO2 air',
      # Tab content
      toolsLayoutUI(
        ns('co2AirTool'),
        'CO2 air',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_co2air_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('co2AirTool')
    ),
    tabPanel(
      # Tab title
      'pCO2',
      # Tab content
      toolsLayoutUI(
        ns('pCO2Tool'),
        'pCO2',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_pco2_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('pCO2Tool')
    ),
    tabPanel(
      # Tab title
      'DIC',
      # Tab content
      toolsLayoutUI(
        ns('dicTool'),
        'DIC',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_dic_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('dicTool')
    ),
    tabPanel(
      # Tab title
      'Ions',
      # Tab content
      toolsLayoutUI(
        ns('ionsTool'),
        'Ions',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_ions_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('ionsTool')
    ),
    tabPanel(
      # Tab title
      'Nutrients',
      # Tab content
      toolsLayoutUI(
        ns('nutrientsTool'),
        'Nutrients',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_nut_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('nutrientsTool')
    ),
    tabPanel(
      # Tab title
      'TSS & AFDM',
      # Tab content
      toolsLayoutUI(
        ns('tssAfdmTool'),
        'TSS & AFDM',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_tss_afdm_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('tssAfdmTool')
    ),
    tabPanel(
      # Tab title
      'Chl a',
      # Tab content
      toolsLayoutUI(
        ns('chlaTool'),
        'Chl a',
        instructionsPanelUIArgs = list(
          instructionsUI = htmlTemplate('./html_components/tool_chla_info.html'),
          initStateHidden = TRUE
        )
      ),
      value = ns('chlaTool')
    )
  )
}



## Create module server function ##################################################

toolsTab <- function(input, output, session, pool, userRole) {
# Create the logic for the toolsTab module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
#  - userRole: String, the role of the currently logged in user
# 
# Returns NULL
  
  ## Call tools modules ###########################################################
  
  # Call the tools layout module with the field data tool
  callModule(toolsLayout, 'fieldDataTool', fieldDataTool, fieldDataToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = TRUE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the DOC tool
  callModule(toolsLayout, 'docTool', docTool, docToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the DOC tool
  callModule(toolsLayout, 'domTool', domTool, domToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the Alkalinity tool
  callModule(toolsLayout, 'alkalinityTool', alkalinityTool, alkalinityToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the CO2 air tool
  callModule(toolsLayout, 'co2AirTool', co2AirTool, co2AirToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the CO2 air tool
  callModule(toolsLayout, 'pCO2Tool', pCO2Tool, pCO2ToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the DIC tool
  callModule(toolsLayout, 'dicTool', dicTool, dicToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the Ions tool
  callModule(toolsLayout, 'ionsTool', ionsTool, ionsToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the nutrients tool
  callModule(toolsLayout, 'nutrientsTool', nutrientsTool, nutrientsToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the TSS & AFDM tool
  callModule(toolsLayout, 'tssAfdmTool', tssAfdmTool, tssAfdmToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
  
  # Call the tools layout module with the TSS & AFDM tool
  callModule(toolsLayout, 'chlaTool', chlaTool, chlaToolUI, pool,
             instructionPanel = TRUE, updateVerification = userRole == 'intern',
             createNew = FALSE, canUpdate = userRole %in% c('sber', 'admin'))
}

