## This module contains the UI and server code for the portal actions tab

## Create module UI ###############################################################

portalActionsUI <- function(id) {
# Create the UI for the portalActions module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create layout
  div(
    class = 'portal-actions',
    div(
      class = 'action',
      h2('Force restart app'),
      p('Create a new ', tags$code('restart.txt'), 'file in the app root directory.'),
      actionButton(ns('restart'), 'Restart', class = 'custom-style custom-style--primary')
    ),
    div(
      class = 'action',
      h2('Create SQL DB backup file'),
      p('Create a new DB backup in the ', tags$code('app_dir/db_backups'), ' directory.'),
      actionButton(ns('backup'), 'Backup', class = 'custom-style custom-style--primary')
    ),
    div(
      class = 'action',
      h2('Manage backup files'),
      p('Download or delete a DB backup file.'),
      selectInput(ns('backupFile'), 'Backup file', choices = c('Choose file', list.files('./db_backups', pattern = '.sql'))),
      div(
        class = 'btn-group',
        # Create and return a downloadButton disabled by default
        disabled(
          # Add "onclick = 'return false;'" additional attribute to disable the button which is in reality a hyper link
          downloadButton(ns('download'), class = 'custom-style custom-style--primary', onclick = 'return false;')
        ),
        actionButton(ns('delete'), 'Delete', class = 'custom-style custom-style--primary'),
      )
    )
  )
}



## Create module server function ##################################################

portalActions <- function(input, output, session) {
# Create the logic for the portalActions module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
# 
# Returns NULL
  
  ## Track action to confirm #####################################################
  
  actionToConfirm <- reactiveVal('')
  
  
  
  ## Force restart logic ##########################################################
  
  # Restart button event
  observeEvent(input$restart, ignoreInit = TRUE, {
    # Save action to confirm
    actionToConfirm('restart')
    
    # Show confirmation modal
    confirmationModal('Force restart the App?', noAction = TRUE)
  })
  
  # Restart function
  restartApp <- function() {
    tryCatch(
      {
        # Create a new restart.txt in the app root directory
        system2('touch', args = c('./restart.txt'))
        
        # Refresh the page
        session$reload()
      },
      error = function(e) showNotification(
        paste(
          'Could not restart the app... ',
          e$message,
          sep = '\n'
        ),
        type = 'error'
      )
    )
  }
  
  
  
  
  
  ## Backup logic ################################################################
  
  # Backup button event
  observeEvent(input$backup, ignoreInit = TRUE, {
    # Save action to confirm
    actionToConfirm('backup')
    
    # Show confirmation modal
    confirmationModal('Create new backup?', noAction = TRUE)
  })
  
  # Backup function
  backupDB <- function() {
    # You will need an '.my.cnf' file in the home directory of the app user
    # With the user, password, host and port infos, e.i.:
    # [client]
    # user=<user_name>
    # password=<password>
    # host=<host>
    # port=<port>
    
    tryCatch(
      {
        # Create file name
        backupFile <- paste0('./db_backups/', gsub('[ :-]', '', Sys.time()), '_', MY_DB_NAME, '_dump.sql')
        
        # Create command
        command <- 'mysqldump'
        if (Sys.info()["sysname"] == 'Darwin') command <- paste0('/usr/local/mysql/bin/', command)
        
        # Create The backup
        system2(command, args = c('--databases', MY_DB_NAME, '--add-drop-database', '-y'),
                stdout = backupFile)
        
        # Show success
        showNotification('DB backup successfully created!', type = 'message')
      },
      error = function(e) showNotification(
        paste(
          'Could not backup the DB... ',
          e$message,
          sep = '\n'
        ),
        type = 'error'
      )
    )
  }
  
  
  
  
  ## Delete logic ################################################################
  
  # Delete button event
  observeEvent(input$delete, ignoreInit = TRUE, {
    req(input$backupFile, input$backupFile != 'Choose file', input$backupFile != '')
    
    # Save action to confirm
    actionToConfirm('delete')
    
    # Show confirmation modal
    confirmationModal(paste('Delete this backup', input$backupFile, '?'), noAction = TRUE)
  })
  
  # Backup function
  deleteBackup <- function(backupFile) {
    # Get file path
    filePath <- list.files('./db_backups', pattern = backupFile, full.names = TRUE)
    
    # Save removed status
    removed <- FALSE
    
    # If a file is found try to delete
    if (length(filePath) == 1) removed <- file.remove(filePath)
        
    if (removed) {
      # Show success
      showNotification('Backup file successfully deleted!', type = 'message')
    } else {
      showNotification('Could not delete the backup... ', type = 'error')
    }
  }
  
  
  
  
  
  ## Download logic ################################################################
  
  # Create an observeEvent that react to backupFile to set downloadButton state
  observeEvent(input$backupFile, ignoreInit = TRUE, {
    # Get backup file
    backupFile <- input$backupFile
    
    # Create message to send to client in as a list containing:
    #  - id: the downloadButton id defined in the UI
    #  - disable: boolean, indicate is the button is disabled
    messageList <- list(
      'id' = session$ns('download'),
      'disable' = FALSE
    )
    
    
    if (backupFile != 'Choose file' & backupFile != '') {
      # Style the button
      enable('download')
      # Inform UI that button needs to be enabled
      messageList$disable <- FALSE
    } else {
      # Style the button
      disable('download')
      # Inform UI that button needs disabled
      messageList$disable <- TRUE
    }
    
    # Convert the list message to JSON
    messageJSON <- toJSON(messageList, auto_unbox = TRUE)
    
    # Send the shiny custom message to toggle downloadButton state
    # Linked to some JavaScript defined in './assets/js/download_button_state.js'
    session$sendCustomMessage('toggleDownloadButton', messageJSON)
  })
  
  # Create download handler
  output$download <- downloadHandler(
    filename = function() {
      input$backupFile
    },
    content = function(file) {
      # Get file path
      filePath <- list.files('./db_backups', pattern = input$backupFile, full.names = TRUE)
      
      # If a file is found
      if (length(filePath) == 1) {
        # Get content
        fileContent <- readr::read_file_raw(filePath)
        
        # Write file to download
        readr::write_file(fileContent, file)
      } else {
        # Otherwise return error file
        readr::write_file(paste0('Error: could not find file (', file, ')...'), file)
      }
    }
  )  
  
  
  
  
  
  ## Confirmation logic ##########################################################
  
  # YES button
  observeEvent(input$YES, ignoreInit = TRUE, {
    req(actionToConfirm() != '')
    if (actionToConfirm() == 'restart') {
      restartApp()
    } else if (actionToConfirm() == 'backup') {
      backupDB()
    } else if (actionToConfirm() == 'delete') {
      deleteBackup(input$backupFile)
    }
    
    if (actionToConfirm() %in% c('backup', 'delete')) {
      updateSelectInput(session, 'backupFile', choices = c('Choose file', list.files('./db_backups', pattern = '.sql')))
    }
    
    # Reset action to confirm
    actionToConfirm('')
    
    # Remove modal
    removeModal()
  })
  
  # NO button
  observeEvent(input$NO, ignoreInit = TRUE, {
    # Reset action to confirm
    actionToConfirm('')
    
    # Close modal
    removeModal()
  })
}

