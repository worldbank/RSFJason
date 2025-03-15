module_accounts_server <- function(id,
                                   parent_session,
                                   pool,
                                   application_hashid,
                                   application_account_id,
                                   USER_ACCOUNT,
                                   cookie_name=NULL) {

  if (class(USER_ACCOUNT) != "reactivevalues") stop("USER_ACCOUNT prameter to module_accounts_server() must be a reactiveValues() object")

  getCookie <- function(cookie_name,
                        cookie_data) {
    
    if (!isTruthy(cookie_name)) return (NULL)
    
    cookie_name <- paste0(cookie_name,"=")
    cookie <- cookie_data

    if (!isTruthy(cookie)) return (NULL)
    cookie <- unlist(strsplit(cookie,";"))
    arl_cookie <- which(grepl(cookie_name,cookie))
    
    if (!isTruthy(arl_cookie)) return (NULL)
    arl_cookie <- cookie[arl_cookie]
    arl_cookie <- gsub(cookie_name,"",arl_cookie)
    cookie <- tryCatch({ fromJSON(arl_cookie)},error=function(err) { NULL })
    cookie
  }
  


  moduleServer(id,function(input,output,session) {
    USER_ACCOUNT$application_permissions <- NULL
    USER_ACCOUNT$application_session_id <- NULL
    USER_ACCOUNT$user_name <- NULL
    USER_ACCOUNT$user_account_id <- NULL
    USER_ACCOUNT$rememberme <- FALSE
    
    LOGIN_ATTEMPTS <- reactiveVal(0)
    LOGIN_CHANGE_FAILED_MSG <- reactiveVal(NULL)
    
    if (!is.null(cookie_name)) session$sendCustomMessage("cookies","now!")
  
    doLogin <- function(credentials) {
      
      if (!isTruthy(credentials) || 
          !isTruthy(credentials$session_id) ||
          !isTruthy(credentials$account_id)) return(NULL)

      removeUI(selector="header.main-header *",immediate = TRUE,multiple=TRUE)
      insertUI(selector="header.main-header",where="afterBegin",ui=tagList(ui_header_IN$children),immediate = TRUE)
      
      removeUI(selector="aside.main-sidebar *",immediate = TRUE,multiple=TRUE)
      insertUI(selector="aside.main-sidebar",where="afterBegin",ui=tagList(ui_sidebar_IN$children),immediate = TRUE)
      
      removeUI(selector="#dashboardBody *",immediate = TRUE,multiple=TRUE)
      insertUI(selector="#dashboardBody",where="afterBegin",ui=tagList(ui_body_IN$children),immediate = TRUE)
      
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      updateTabItems(parent_session, "sidebarMenu", "datasets")
      
      if (USER_ACCOUNT$rememberme==TRUE) {
        updateTextInput(session = session,
                        inputId="login_password",
                        value=credentials$session_id)
      }
      
      LOGIN_ATTEMPTS(0)
      
      USER_ACCOUNT$application_permissions <- credentials$application_permissions
      USER_ACCOUNT$application_session_id <- credentials$session_id
      USER_ACCOUNT$user_name <- credentials$users_name
      USER_ACCOUNT$user_account_id <- credentials$account_id
      USER_ACCOUNT$user_login <- credentials$user_login
    }
    
    show_error <- function(n) { 
      hideElement(id="login_failed1")
      hideElement(id="login_failed2")
      hideElement(id="login_failed3")
      hideElement(id="login_failed4")
      if(n>0) {
        showElement(id=paste0("login_failed",n))
      }
    }
    
    LOGGEDIN <- reactive({ isTruthy(USER_ACCOUNT$user_account_id) && isTruthy(USER_ACCOUNT$application_session_id) })
    
    
    observeEvent(LOGGEDIN(), {
      
      if (LOGGEDIN()==FALSE) return (NULL) #only want to remember login session info after they're logged in
      
      cookieValues <- list()
      
      rememberme <- USER_ACCOUNT$rememberme
      
      if (rememberme==TRUE) {
        cookieValues$session_id <- USER_ACCOUNT$application_session_id
        cookieValues$user_login <- USER_ACCOUNT$user_login
        cookieValues$rememberme <- rememberme
      } else {
        cookieValues$session_id <- NULL
        cookieValues$user_login <- NULL
        cookieValues$rememberme <- NULL
      }
      
      days30 <- 1000*60*60*24*30
      exptime <- Sys.time() + days30
      cookie <- list(name=cookie_name,
                     value=toJSON(cookieValues),
                     expires=format(exptime, format="%A, %B %d, %Y at %H:%M:%S"))
      cookie <- paste0(cookie$name,"=",cookie$value,"; expires=",cookie$expires,";")
      #print(paste0("Setting cookie: ",cookie))
      runjs(paste0("document.cookie = '",cookie,"';"))
      
    }, priority=-Inf,ignoreNULL = FALSE)
    
    #Other observers may also be observing the cookie_report
    #input$cookie_report is defined in UI with:
    #tags$head(tags$script('Shiny.addCustomMessageHandler("cookies", function(arg) {  Shiny.onInputChange("cookie_report", document.cookie); });')),
    observeEvent(input$cookie_report, {
      
      #print(paste0("COOKIE REPORT: ",input$cookie_report))
      cookie <- getCookie(cookie_name=cookie_name,
                          cookie_data=input$cookie_report)
      
      if (isTruthy(cookie$session_id)) {
        updateTextInput(session=session,
                        inputId = "login_password",
                        value=cookie$session_id)
      }
      
      if (isTruthy(cookie$user_login)) {
        updateTextInput(session=session,
                        inputId = "user_login",
                        value=cookie$user_login)
        
      }
      
      if (!is.null(cookie$rememberme) &&
          !is.na(as.logical(cookie$rememberme))) {
        updateTextInput(session=session,
                        inputId = "login_rememberme",
                        value=as.logical(cookie$rememberme))
        
      }
      
    })
    
    observeEvent(input$login_change_action, {
      if (!is.null(USER_ACCOUNT$application_session_id)) return (NULL)
      if (!isTruthy(input$login_change_action)) return (NULL)

      user_login <- trimws(input$user_login)
      new_password1 <- trimws(input$login_new_password1)
      new_password2 <- trimws(input$login_new_password2)
      old_password <- trimws(input$login_password)
      
      if (!isTruthy(new_password1) || !identical(new_password1,new_password2)) {
        LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
        LOGIN_CHANGE_FAILED_MSG(paste0("Change failed ",LOGIN_ATTEMPTS(),": passwords do not match"))
        showElement(id="login_change_failed")
      } else {
        
        error_msg <- ""
        account_id <- tryCatch({ account_id <- db_user_login_change_password(pool=pool,
                                                                             application_hashid=application_hashid,
                                                                             request_by_account_id=application_account_id,
                                                                             username=user_login,
                                                                             old_password=old_password,
                                                                             new_password=new_password1)
                                 unlist(account_id) #If successful, a character string
                               },
                               error = function(e) { 
                                 print(conditionMessage(e))
                                 if (grepl("HINT:\\s+LENGTH",conditionMessage(e))) error_msg <<- "Password must be at least 6 characters long."
                                 else if (grepl("HINT:\\s+DISALLOW",conditionMessage(e))) error_msg <<- "Password is not allowed. Contact your sys admin."
                                 NULL },
                               warning = function(w) { 
                                 print(conditionMessage(w))
                                 if (grepl("HINT:\\s+LENGTH",conditionMessage(w))) error_msg <<- "Password must be at least 6 characters long."
                                 else if (grepl("HINT:\\s+DISALLOW",conditionMessage(w))) error_msg <<- "Password is not allowed. Contact your sys admin."
                                 NULL })
        
        if (!isTruthy(account_id)) {
          LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
          LOGIN_CHANGE_FAILED_MSG(paste0("Change failed ",LOGIN_ATTEMPTS(),". ",error_msg))
          showElement(id="login_change_failed")
        } else {

          
          result <- db_user_login(pool=pool,
                                  application_hashid=application_hashid,
                                  username=user_login,
                                  password=new_password1)
          
          doLogin(credentials=result)
        }
      }
      
    })
    
    observeEvent(input$login_action, {
      

      if (isTruthy(USER_ACCOUNT$application_session_id)) return (NULL) #They're already logged in, but this action shouldn't be available. But just in case.
      
      user_login <- input$user_login
      password <- input$login_password
    
      result <- NULL

      result <- db_user_login(pool=pool,
                              application_hashid=application_hashid,
                              username=user_login,
                              password=password)

      if (!isTruthy(result)) {
        
        
        
        check_reset <- tryCatch({ db_user_login_reset_check(pool,
                                                            application_hashid=application_hashid,
                                                            username=user_login) },
                                error = function(e) { NULL },
                                warning = function(w) { NULL })
        #print("check_result")
        #print(check_reset)
        if (!isTruthy(check_reset) || empty(check_reset)) check_reset <- NULL
        
        if (!is.null(check_reset) && check_reset$resetable==TRUE) {
          
          #has_password == FALSE means new account logging in for the first time
          #has_resent_password == FALSE means reset has not been called.
          if (check_reset$has_password == FALSE || check_reset$has_reset_password == FALSE) {
            
            reset_code <- db_user_reset_password(pool=pool,
                                                 application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                                                 sysadmin_id=ACCOUNT_SYS_ADMIN$account_id,
                                                 username=user_login)
            
            if (!empty(reset_code) && reset_code$reset_success==TRUE) {
              email <- div(p("Dear ",paste0(ifelse(isTruthy(reset_code$users_name),reset_code$users_name,"User"),",")),
                           p("Your ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," account password has been reset"),
                           p("Please use the following temporary password to reset your account: ",reset_code$reset_password))
              
              showElement(id="login_change_reset",anim=TRUE,animType = "fade")
              hideElement(id="login_failed1")
              updateTextInput(session=session,inputId="login_password",label="Temporary Password",value="")  
              
              user_send_email(pool=pool,
                              to=reset_code$login_email,
                              subject="RSF JASON | password reset",
                              html=email) 
            }
          } else { 
            updateTextInput(session=session,inputId="login_password",label="Old Password",value="")  
          }
          
          hideElement(id="login_button",anim=TRUE,animType="fade")
          showElement(id="login_change_password",anim=TRUE,animType="fade")
          
          
        
        } else if (!is.null(check_reset) && check_reset$resetable==FALSE && check_reset$has_reset_password) {
          pwd <- ""
          if (!isTruthy(pwd)) pwd <- ""
          if (  (isTruthy(password) && check_reset$has_password==FALSE) | #assume they entered the temp pwd and retain it
                (isTruthy(password) && nchar(password)==10 && grepl("[^A-Z0-9]",password)==FALSE) ) { #Sys sets an uppercase MD5 hash for temp password, first 10 digits
            pwd <- password
          } 
          
          updateTextInput(session=session,
                          inputId="login_password",
                          label="Temporary Password",
                          value=pwd)
          
          hideElement(id="login_button",anim=TRUE,animType="fade")
          hideElement(id="login_rememberme_ui",anim=TRUE,animType="fade")
          
          showElement(id="login_change_password",anim=TRUE,animType="fade")
          #showElement(id="login_change_reset",anim=TRUE,animType = "fade")
          if (LOGIN_ATTEMPTS() > 1) {
            showElement(id="login_failed1")
          }
          LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
          
        } else {
          showElement(id="login_failed1")
          
          cookie <- getCookie(cookie_name=cookie_name,
                              cookie_data=input$cookie_report)
          
          if (identical(as.character(password),cookie$session_id)) {
            
            showNotification(type="message",
                             ui=h3("Login failed using 'remember me'.  Try logging in with your password."))
            
            updateTextInput(session=session,
                            inputId = "login_password",
                            value="")
          }
          LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
        }
      
      
      
      } else {
        doLogin(credentials=result)
      }
    })
    
    observeEvent(input$login_logout_action, { 
      
      result <- db_user_logout(pool=pool,
                               application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                               session_id=USER_ACCOUNT$application_session_id)

      if (result==TRUE) {
        
        #UI IN/OUT: This all need to be parameterized better in terms of selectorIDs and the UI replacement and any actions
        #maybe just define an inUI = function() { removeUI...insertUI...} and outUI = ... ? or maybe list(`header.main-header *` = ui ... )
        {
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      
          removeUI(selector="header.main-header *",immediate = TRUE,multiple=TRUE)
          insertUI(selector="header.main-header",where="afterBegin",ui=tagList(ui_header_OUT$children),immediate = TRUE)
          
          removeUI(selector="aside.main-sidebar *",immediate = TRUE,multiple=TRUE)
          insertUI(selector="aside.main-sidebar",where="afterBegin",ui=tagList(ui_sidebar_OUT$children),immediate = TRUE)
          
          removeUI(selector="#dashboardBody *",immediate = TRUE,multiple=TRUE)
          insertUI(selector="#dashboardBody",where="afterBegin",ui=tagList(ui_body_OUT$children),immediate = TRUE)
          
          shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
          updateTabItems(session, inputId="sidebarMenu", selected="datasets")
          
          #Jason system permissions
          USER_ACCOUNT$JASON <- list()
        }
       
        if (USER_ACCOUNT$rememberme %in% TRUE) {
          updateCheckboxInput(session=session,
                              inputId="login_rememberme",
                              value=TRUE)
          
          updateTextInput(session=session,
                          inputId="login_password",
                          value=USER_ACCOUNT$application_session_id)
        }
        
        if (isTruthy(USER_ACCOUNT$user_login)) {
          updateTextInput(session=session,
                          inputId="user_login",
                          value=USER_ACCOUNT$user_login)
        }
        
        USER_ACCOUNT$application_permissions <- NULL
        USER_ACCOUNT$application_session_id <- NULL
        USER_ACCOUNT$user_name <- NULL
        USER_ACCOUNT$user_account_id <- NULL
        
      }
    }, ignoreInit=TRUE)
    
    observeEvent(input$reset_password_button, {
 
      lookup <- db_user_check_email_exists(pool=pool, application_hashid=RSF_MANAGEMENT_APPLICATION_ID, email=input$user_login)
      is_can_login <- db_user_check_permission(pool=pool, application_hashid=RSF_MANAGEMENT_APPLICATION_ID, email=input$user_login, 'CAN_LOGIN')

      if (nrow(lookup) == 1 && 
          nrow(is_can_login) == 1) {
        
        reset_code <- db_user_reset_password(pool=pool,
                                             application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                                             sysadmin_id=ACCOUNT_SYS_ADMIN$account_id,
                                             username=input$user_login)

        if (!empty(reset_code) && 
            reset_code$reset_success==TRUE) {
          email <- div(p("Dear ",paste0(ifelse(!isTruthy(reset_code$users_name),"User",reset_code$users_name),",")),
                       p("Your ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," account password has been reset"),
                       p("Please use the following temporary password to reset your account: ",reset_code$reset_password))
          
          to <- input$user_login
          subject <- "RSF JASON | Password Reset"
#print("SENDING MAIL")          
          user_send_email(pool = pool,
                          to = to,
                          subject = subject,
                          html = email)
          
          updateTextInput(session=session,
                          inputId="user_login",
                          label="Username")
#print(email)          
          show_error(0)
          
          hideElement(id = "reset_password", anim = TRUE, animType = "fade")
          hideElement(id = "login_button",anim=TRUE,animType="fade")
          hideElement(id = "forgot_password",anim=TRUE,animType="fade")
          
          showElement(id = "login_change_reset",anim=TRUE,animType = "fade")
          showElement(id = "login_change_password",anim=TRUE,animType="fade")
          showElement(id = "login_password")
          updateTextInput(session=session,inputId="login_password",label="Temporary Password",value="")
        } else{
          show_error(3)
        }
      }
      else { 
        show_error(2)
      }
    })
     
    observeEvent(input$forgot_password, {
      if (LOGGEDIN()) return (NULL)
      updateTextInput(session=session,
                      inputId="user_login",
                      label="Enter your email address and Jason will send you a temporary password.")
      
      showElement(id="reset_password",anim=TRUE,animType = "fade")
      show_error(0)
      hideElement(id = "login_rememberme_ui",anim=TRUE,animType="fade")
      hideElement(id = "login_button",anim=TRUE,animType="fade")
      hideElement(id = "login_password")
    })
    
    observeEvent(input$login_rememberme, {
      if (as.logical(input$login_rememberme) %in% TRUE) {
        USER_ACCOUNT$rememberme <- TRUE
      } else {
        USER_ACCOUNT$rememberme <- FALSE
      }
    },ignoreNULL = FALSE, ignoreInit = FALSE)
    
    output$login_attempts <- renderText({ LOGIN_ATTEMPTS() })
    
    output$login_change_failed_message <- renderText({ LOGIN_CHANGE_FAILED_MSG() })
    
    output$login_who <- renderText({ format_name_abbreviation(USER_ACCOUNT$user_name) })
  
    
  }) #end moduleServer
}  
