## This module contains the UI and server code for the Login

## Create module UI ###############################################################

loginUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = 'navbar-login',
    actionLink(ns('showLoginForm'), 'Log In', class = 'custom-links'),
    hidden(
      htmlOutput(ns('userInfo'))
    )
  )
}



## Create module server function ##################################################

login <- function(input, output, session, pool) {
  ## Default User creation #########################################################
  
  # Create a some reactive values linked to the login status
  user <- reactiveValues(
    loggedin = FALSE,
    name = NULL,
    role = 'visitor',
    error = ''
  )
  
  
  
  
  ## Login modal form #############################################################
  
  # Create a modal dialog
  loginForm <- modalDialog(
    title = 'Log In', size = 's',
    div(
      class = 'login-form',
      # Add an text output to log the errors
      textOutput(session$ns('loginError')),
      # Username and password inputs
      textInput(session$ns('username'), 'Username'),
      passwordInput(session$ns('password'), 'Password'),
    ),
    # Action buttons
    footer = tagList(
      actionButton(session$ns('login'), 'Log In', class = 'custom-style custom-style--primary'),
      actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
    )
  )
  
  
  
  
  ## Login modal display ##########################################################
  
  # Create observeEvent that react to the showLoginForm button
  observeEvent(input$showLoginForm, ignoreInit = TRUE, {
    req(user$loggedin == FALSE)
    showModal(loginForm)
  })
  
  
  
  
  
  ## Login logic ##################################################################
  
  # Create an observeEvent that react to the login button
  observeEvent(input$login, ignoreInit = TRUE, {
    req(user$loggedin == FALSE)
    
    # Get user from db
    userResult <- loginUser(pool, input$username)
    
    # If a user was found, verify password
    if (nrow(userResult) == 1) {
      # Update user reactive values if correct password
      if (sodium::password_verify(userResult$password, input$password)) {
        user$loggedin <- TRUE
        user$name <- userResult$name
        user$role <- userResult$role
        
        # Remove login form
        removeModal()
        # Show login success notification
        showNotification('Logged in successfully!', type = 'message')
        
        # Stop here
        return()
      }
    }
    
    # Save errors
    user$error <- 'Incorrect username / password combination!'
  })
  
  
  
  
  ## Closing login modal logic ####################################################
  
  # Create an observeEvent that react to the cancel button
  observeEvent(input$cancel, ignoreInit = TRUE, {
    # Clear user loggin error
    user$error <- ''
    
    # Close modal
    removeModal()
  })
  
  
  
  
  ## Error display logic ##########################################################
  
  output$loginError <- renderText(shiny::validate(
    errorClass = 'form',
    need(FALSE, message = user$error)
  ))
  
  
  
  ## Log In Status logic ##########################################################
  
  # Create an observeEvent that react to the log in status update
  observeEvent(user$loggedin, ignoreInit = TRUE, {
    toggleElement('showLoginForm', condition = !user$loggedin)
    toggleElement('userInfo', condition = user$loggedin)
  
    if(user$loggedin) {
      output$userInfo <- renderUI(htmlTemplate(
        './html_components/user_status.html',
        username = user$name,
        role = user$role,
        logout = actionLink(session$ns('logout'), 'Log Out', class = 'custom-links')
      ))
    }
  })
  
  
  
  ## Log out logic ################################################################
  
  # Create an observeEvent that react to the logout button
  observeEvent(input$logout, ignoreInit = TRUE, {
    req(user$loggedin)
    session$reload()
  })
  
  
  
  
  ## Module return value ##########################################################
  
  # Return the the user status
  return(user)
}
