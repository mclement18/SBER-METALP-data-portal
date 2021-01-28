## This module contains the UI and server code for the Login

## Create module UI ###############################################################

loginUI <- function(id) {
# Create the UI for the login module
# Parameters:
#  - id: String, the module id
# 
# Returns a div containing the layout
  
  # Create namespace
  ns <- NS(id)
  
  # Create and return a div
  div(
    class = 'navbar-login',
    id = 'login-ui',
    # Create a login link
    actionLink(ns('showLoginForm'), 'Log In', class = 'custom-links'),
    # Create a HTML output that will contain the logged in user info and logout link
    # Hidden when logged out
    hidden(
      htmlOutput(ns('userInfo'))
    )
  )
}



## Create module server function ##################################################

login <- function(input, output, session, pool) {
# Create the logic for the login module
# Parameters:
#  - input, output, session: Default needed parameters to create a module
#  - pool: The pool connection to the database
# 
# Returns the reactive values containing the user info
  
  ## Default User creation #########################################################
  
  # Create a some reactive values linked to the login status
  user <- reactiveValues(
    loggedin = FALSE,
    name = NULL,
    role = 'visitor',
    error = ''
  )
  
  
  
  
  
  
  ## Login modal display ##########################################################
  
  # Create observeEvent that react to the showLoginForm button
  observeEvent(input$showLoginForm, ignoreInit = TRUE, {
    req(user$loggedin == FALSE)
    # Create a modal dialog
    showModal(
      modalDialog(
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
    )
  })
  
  
  
  
  
  ## Login logic ##################################################################
  
  # Track login attempts
  loginAttempts <- reactiveVal(0)
  
  # Create an observeEvent that react to the login button
  observeEvent(input$login, ignoreInit = TRUE, {
    req(user$loggedin == FALSE, loginAttempts() < 5)
    
    # Get user from db
    userResult <- loginUser(pool, input$username)
    
    # If a user was found, verify password
    if (nrow(userResult) == 1) {
      # Update user reactive values if correct password
      if (sodium::password_verify(userResult$password, input$password)) {
        user$loggedin <- TRUE
        user$name <- userResult$name
        user$role <- userResult$role
        user$error <- ''
        
        # Remove login form
        removeModal()
        # Show login success notification
        showNotification('Logged in successfully!', type = 'message')
        
        # Stop here
        return()
      }
    }
    
    # Increment attempts
    loginAttempts(loginAttempts() + 1)
    
    # Save errors
    if (loginAttempts() >= 5) {
      user$error <- 'Too many login attempts... Session is blocked!'
    } else {
      user$error <- 'Incorrect username / password combination!'
    }
  })
  
  
  
  
  ## Closing login modal logic ####################################################
  
  # Create an observeEvent that react to the cancel button
  observeEvent(input$cancel, ignoreInit = TRUE, {
    # Clear user log in error
    if (loginAttempts() < 5) user$error <- ''
    
    # Close modal
    removeModal()
  })
  
  
  
  
  ## Error display logic ##########################################################
  
  # Render the error with validate in a renderText
  output$loginError <- renderText(shiny::validate(
    errorClass = 'form',
    need(user$error == '', message = user$error)
  ))
  
  
  
  ## Log In Status logic ##########################################################
  
  # Create an observeEvent that react to the log in status update
  observeEvent(user$loggedin, ignoreInit = TRUE, {
    # Show and hide the correct UI element depending on the login status
    toggleElement('showLoginForm', condition = !user$loggedin)
    toggleElement('userInfo', condition = user$loggedin)
  
  })
  
  # Render the loggin status if user is logged in
  output$userInfo <- renderUI({
    if(user$loggedin) {
      htmlTemplate(
        './html_components/user_status.html',
        username = actionLink(session$ns('changePWD'), user$name, icon = roleToIcon(user$role), class = 'custom-links'),
        logout = actionLink(session$ns('logout'), 'Log Out', class = 'custom-links')
      )
    }
  })
  
  
  
  ## Log out logic ################################################################
  
  # Create an observeEvent that react to the logout button
  observeEvent(input$logout, ignoreInit = TRUE, {
    req(user$loggedin)
    # Refresh browser
    session$reload()
  })
  
  
  
  
  ## Change user password logic ###################################################
  
  # Create an observeEvent that react to the changePWD link
  observeEvent(input$changePWD, ignoreInit = TRUE, {
    req(user$loggedin)
    # Show update password modal
    showModal(
      modalDialog(
        title = 'Update your password', size = 's',
        div(
          class = 'login-form',
          # Add an text output to log the errors
          textOutput(session$ns('loginError')),
          # Password field
          passwordInput(session$ns('newPWD'), 'New password'),
          # Confirmation field
          passwordInput(session$ns('confirmPWD'), 'Confirm password'),
        ),
        footer = tagList(
          actionButton(session$ns('updatePWD'), 'Update', class = 'custom-style custom-style--primary'),
          actionButton(session$ns('cancel'), 'Cancel', class = 'custom-style')
        )
      )
    )
  })
  
  # Create an observeEvent that react to the updatePWD button
  observeEvent(input$updatePWD, ignoreInit = TRUE, {
    req(user$loggedin)
    # Get inputs
    newPWD <- input$newPWD
    confirmPWD <- input$confirmPWD
    # Check that fields are filled
    if (newPWD == '' | confirmPWD == '') {
      user$error <- 'Both field must filled!'
      # Check that both field are the same
    } else if (newPWD != confirmPWD) {
      user$error <- 'Passwords do not match!'
    } else if (newPWD == confirmPWD) {
      # Update password
      updateError <- updateUserPWD(pool, user$name, newPWD)
      # If success
      if (updateError == '') {
        # Show notif and remove modal and error
        showNotification('Password successfully updated!', type = 'message')
        user$error <- ''
        removeModal()
      } else {
        # Show error
        user$error <- updateError
      }
    }
  })
  
  
  
  
  ## Module return value ##########################################################
  
  # Return the the user status
  return(user)
}
