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

    LOGIN_ATTEMPTS <- reactiveVal(0)
    LOGIN_CHANGE_FAILED_MSG <- reactiveVal(NULL)
    
    COOKIES <- reactiveValues()
  
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
      
      COOKIES$session_id <- credentials$session_id
      COOKIES$username <- input$login_username
      
      #In case user logs out and wants to log back in without needing to rely on cookie restore
      rememberme <- as.logical(input$login_rememberme)
      if (!isTruthy(rememberme)) rememberme <- FALSE
      if (rememberme) updateTextInput(session = session,
                                      inputId="login_password",
                                      value=credentials$session_id)
      
      USER_ACCOUNT$application_permissions <- credentials$application_permissions
      USER_ACCOUNT$application_session_id <- credentials$session_id
      USER_ACCOUNT$user_name <- credentials$users_name
      USER_ACCOUNT$user_account_id <- credentials$account_id
    }
    
    LOGGEDIN <- reactive({ isTruthy(USER_ACCOUNT$user_account_id) && isTruthy(USER_ACCOUNT$application_session_id) })
    
    
    observeEvent(LOGGEDIN(), {
      
      if (LOGGEDIN()==FALSE) return (NULL) #only want to remember login session info after they're logged in
      
      cookieValues <- list()
      rememberme <- as.logical(input$login_rememberme)
      if (!isTruthy(rememberme)) rememberme <- FALSE
      
      if (rememberme==TRUE) cookieValues$session_id <- COOKIES$session_id
      else cookieValues$session_id <- NULL
      
      if (rememberme==TRUE) cookieValues$rememberme <- TRUE
      else cookieValues$rememberme <- NULL
      
      if (isTruthy(input$login_username))  cookieValues$username <- input$login_username
      else cookieValues$login_username <- NULL
      
      days30 <- 1000*60*60*24*30
      exptime <- Sys.time() + days30
      cookie <- list(name=cookie_name,
                     value=toJSON(cookieValues),
                     expires=format(exptime, format="%A, %B %d, %Y at %H:%M:%S"))
      cookie <- paste0(cookie$name,"=",cookie$value,"; expires=",cookie$expires,";")
      runjs(paste0("document.cookie = '",cookie,"';"))
    }, priority=-Inf,ignoreNULL = FALSE)
    
    #Other observers may also be observing the cookie_report
    #input$cookie_report is defined in UI with:
    #tags$head(tags$script('Shiny.addCustomMessageHandler("cookies", function(arg) {  Shiny.onInputChange("cookie_report", document.cookie); });')),
    observeEvent(input$cookie_report, {
      
      cookie <- getCookie(cookie_name=cookie_name,
                          cookie_data=input$cookie_report)
      
      COOKIES$session_id <- cookie$session_id
      COOKIES$username   <- cookie$username
      COOKIES$rememberme <- cookie$rememberme
      
      if (isTruthy(cookie$session_id)) {
        updateTextInput(session=session,inputId = "password",value=cookie$session_id)
      }
    })
    
    observeEvent(input$login_change_action, {
      if (!is.null(USER_ACCOUNT$application_session_id)) return (NULL)
      if (!isTruthy(input$login_change_action)) return (NULL)
      
      username <- input$login_username
      new_password1 <- input$login_new_password1
      new_password2 <- input$login_new_password2
      old_password <- input$login_password
      
      if (isTruthy(new_password1) || !identical(new_password1,new_password2)) {
        LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
        LOGIN_CHANGE_FAILED_MSG(paste0("Change failed ",LOGIN_ATTEMPTS(),": passwords do not match"))
        showElement(id="login_change_failed")
      } else {
        error_msg <- ""
        account_id <- tryCatch({ account_id <- db_user_login_change_password(pool,
                                                               application_hashid=application_hashid,
                                                               request_by_account_id=application_account_id,
                                                               username=username,
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

          COOKIES$session_id <- result$session_id
          COOKIES$username <- username

          USER_ACCOUNT$application_permissions <- result$application_permissions
          USER_ACCOUNT$application_session_id <- result$session_id
          USER_ACCOUNT$user_name <- result$users_name
          
          doLogin(credentials=result)
        }
      }
      
    })
    
    observeEvent(input$login_action, {
      if (isTruthy(USER_ACCOUNT$application_session_id)) return (NULL) #They're already logged in, but this action shouldn't be available. But just in case.
      
      username <- input$login_username
      password <- input$login_password
    
      result <- NULL

      #testing!    
      if (!isTruthy(username)) username <- "sheitmann@ifc.org"
      if (!isTruthy(password)) password <- "tester"
      
      result <- db_user_login(pool=pool,
                              application_hashid=application_hashid,
                              username=username,
                              password=password)
      
      if (!isTruthy(result)) {
        check_reset <- tryCatch({ db_user_login_reset_check(pool,
                                                            application_hashid=application_hashid,
                                                            username=username) },
                                error = function(e) { NULL },
                                warning = function(w) { NULL })
        print("check_result")
        print(check_reset)
        if (!isTruthy(check_reset) || empty(check_reset)) check_reset <- NULL
        
        if (!is.null(check_reset) && check_reset$resetable==TRUE) {
          if (check_reset$has_password == FALSE || check_reset$has_reset_password == FALSE) {
            reset <- pool %>% db_user_reset_password(application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                                                       sysadmin_id=ACCOUNT_SYS_ADMIN$account_id,
                                                       username=username)
            
            if (isTruthy(reset) && reset$reset_success==TRUE) {
              email <- div(p("Dear ",paste(ifelse(isTruthy(reset$users_name),reset$users_name,"User"),",")),
                           p("Your RSF Systems account password has been reset"),
                           p("Please use the following temporary password to reset your account: ",reset$reset_password))
              
              showElement(id="login_change_reset",anim=TRUE,animType = "fade")
              hideElement(id="login_failed")
              updateTextInput(session=session,inputId="login_password",label="Temporary Reset Password",value="")  
              
              user_send_email(from="sheitmann@ifc.org",
                              to=reset$login_email,
                              subject="RSF System password reset",
                              html=email) 
            }
          } else { 
            updateTextInput(session=session,inputId="login_password",label="Old Password",value="")  
          }
          
          hideElement(id="login_button",anim=TRUE,animType="fade")
          showElement(id="login_change_password",anim=TRUE,animType="fade")
          
          
        } else if (!is.null(check_reset) && check_reset$resetable==FALSE && check_reset$has_reset_password) {
          updateTextInput(session=session,inputId="login_password",label="Temporary Reset Password",value="")
          hideElement(id="login_button",anim=TRUE,animType="fade")
          showElement(id="login_change_password",anim=TRUE,animType="fade")
          showElement(id="login_change_reset",anim=TRUE,animType = "fade")
          showElement(id="login_failed")
          LOGIN_ATTEMPTS(LOGIN_ATTEMPTS()+1)
          
        } else {
          showElement(id="login_failed")
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
        }
        
        USER_ACCOUNT$application_permissions <- NULL
        USER_ACCOUNT$application_session_id <- NULL
        USER_ACCOUNT$user_name <- NULL
        USER_ACCOUNT$user_account_id <- NULL

        rememberme <- as.logical(COOKIES$rememberme)
        if (!isTruthy(rememberme)) rememberme <- FALSE
    
        session_id <- COOKIES$session_id
        if (!isTruthy(session_id)) session_id <- NA
        
        username <- COOKIES$username
        if (!isTruthy(username)) username <- NA
        
        if (rememberme) {
          updateCheckboxInput(session=session,inputId="login_rememberme",value=rememberme)
          if (isTruthy(session_id)) updateTextInput(session=session,inputId="login_password",value=session_id)
        }
        if (isTruthy(username)) updateTextInput(session=session,inputId="login_username",value=username)
      }
    }, ignoreInit=TRUE)
    
    output$login_attempts <- renderText({ LOGIN_ATTEMPTS() })
    
    output$login_change_failed_message <- renderText({ LOGIN_CHANGE_FAILED_MSG() })
    
    output$login_who <- renderText({ format_name_abbreviation(USER_ACCOUNT$user_name) })
    
  }) #end moduleServer
}  
