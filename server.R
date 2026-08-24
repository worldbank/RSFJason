



server <- function(input, output, session) 
{
  ns <- NS("RSF")
  DBPOOL <- NULL
  serverENV <- environment()
  server_module_registry <- list()
  
  DBPOOL <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]]))
  if (!is.null(DBPOOL) && pool::dbIsValid(DBPOOL)) { print("DBPOOL (MAIN) Started")
  } else { print("DBPOOL (MAIN) FAILED TO START") }
  
  observeEvent(session, { 
    
    # print("Session information")
    # # session$url_hostname [1] "127.0.0.1"
    # # session$url_hostname [1] "datanalytics-int.worldbank.org"
    # #browser()
    # #print(reactiveValuesToList(session))
    # print("Session$user")
    # print(session$user)
    # print("Session$groups")
    # print(session$groups)
    # dbserver <- NULL
    # if (grepl("rsf-prod",session$clientData$url_pathname)==TRUE) { dbserver <- LOCATIONS[["Jason_PROD"]]
    # } else if (grepl("rsf-dev",session$clientData$url_pathname)==TRUE) { dbserver <- LOCATIONS[["Jason_DEV"]]
    # } else if (grepl("rsf-stage",session$clientData$url_pathname)==TRUE) { dbserver <- LOCATIONS[["Jason_STAGE"]]
    # } else {  }
    
    if (grepl("rsf-prod",session$clientData$url_pathname)) {
      
      if (!identical(LOCATION,"Jason_PROD")) {
        showNotification(type="error",
                         ui=h3(paste0("rsf-prod deployment does not match location setting: ",LOCATION)))
        
        stop(paste0("rsf-prod deployment does not match location setting: ",LOCATION))
      }
    } else if (grepl("rsf-dev",session$clientData$url_pathname)) {
      
      if (!identical(LOCATION,"Jason_DEV")) {
        showNotification(type="error",
                         ui=h3(paste0("rsf-dev deployment does not match location setting: ",LOCATION)))
        stop(paste0("rsf-dev deployment does not match location setting: ",LOCATION))
      }    
    } else if (grepl("rsf-stage",session$clientData$url_pathname)) {
      
      if (!identical(LOCATION,"Jason_STAGE")) {
        showNotification(type="error",
                         ui=h3(paste0("rsf-stage deployment does not match location setting: ",LOCATION)))
        
        stop(paste0("rsf-stage deployment does not match location setting: ",LOCATION))
      }
    } else if (session$clientData$url_pathname=="/") { #Local
      
      if (!identical(LOCATION,"Jason_DEV")) {
        showNotification(type="warning",
                         ui=h3("Warning: DATABASE LOCATION = '",LOCATION,"'"))
      }
    } else {
      showNotification(type="error",
                       ui=h3("Failed to identify database LOCATION (",LOCATION,") for application URL (",session$clientData$url_pathname,")"))
      
      stop(paste0("Failed to parse url_pathname '",session$clientData$url_pathname,"' and Global.R LOCATION=",LOCATION))
    }
    
    
    module_accounts_server(id="accounts_server",
                           parent_session = session,
                           APPLICATIONS=DBPOOL_APPLICATIONS,
                           application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                           application_account_id=ACCOUNT_SYS_ADMIN$account_id,
                           USER_ACCOUNT=USER_ACCOUNT,
                           cookie_name="ARL-applications")
    
  },once=T,priority = 1)
  
  #https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/
  #https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
  #remove_shiny_inputs() permanently removes the input object, seemingly would need to be re-created through insertUI maybe?
  #in any case, modals/dynamic UIs don't seem to re-register the input when the UI is re-drawn.
  # remove_shiny_inputs <- function(id, .input) {
  #   print(paste0("Remove inputs called for ",id))
  #   return (NULL)
  #   invisible(
  #     lapply(grep(id, names(.input), value = TRUE), function(i) {
  #       .subset2(.input, "impl")$.values$remove(i)
  #     })
  #   )
  # }
  
  registeredModules <- function() { names(server_module_registry) }
  destroyModule <- function(id) {
    module <- server_module_registry[[id]]
    if (!is.null(module)) {
      #print(paste0("Server destroyModule ",id))
      #current_module <<- module
      module$destroyModule()
      server_module_registry[[id]] <<- NULL
    }
  }
  registerModule <- function(id,.module) {
    if (!is.null(server_module_registry[id])) {
      destroyModule(id)
    }
    server_module_registry[[id]] <<- .module
  }
  
  Shiny.destroyList <- function(observers_list) {
    modules <- isolate({ observers_list() })
    if (all(is.na(modules)) || length(modules)==0) return(FALSE)
    for (i in 1:length(modules)) modules[[i]]$destroy()
    return (TRUE)
  }
  
  #https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
  Shiny.removeInputs <- function(moduleIds, .input=input) {
    
    if (!isTruthy(moduleIds)) return (NULL)
    for (mId in moduleIds) {
      invisible(
        lapply(grep(mId, names(.input), value = TRUE), function(i) {
          cat(paste0("Shiny.removeInputs: ",i,"\n"))
          .subset2(.input, "impl")$.values$remove(i)
        })
      )
    }
    #cat("\n")
  }
  
  STATUS_MESSAGE_PANEL <- reactiveValues(container_id="dataset_upload_log_container",panel_id="dataset_upload_log")
  USER_ACCOUNT <- reactiveValues()
  
  
  
  USER_ID <- eventReactive(USER_ACCOUNT$user_account_id, { USER_ACCOUNT$user_account_id },ignoreNULL = FALSE)
  USER_NAME <- eventReactive(USER_ACCOUNT$user_name, {  format_name_abbreviation(USER_ACCOUNT$user_name) },ignoreNULL=FALSE)
  LOGGEDIN <- reactive({ isTruthy(USER_ACCOUNT$user_account_id) && isTruthy(USER_ACCOUNT$application_session_id) })
  
 

  GLOBAL_CURRENCIES <- reactive({
    tryCatch({ get_fx_codes() },
             warning=function(w) { 
               showNotification(type="error",
                                duration=NULL,
                                ui=h3(conditionMessage(w)))
               NULL
             },
             error=function(e) { 
               showNotification(type="error",
                                duration=NULL,
                                ui=h3(conditionMessage(e)))
               NULL
             })
  })
  
  source("./R/app/server_programs.R",local=serverENV)
  
  source("./R/app/server_dashboard.R",local=serverENV)
  source("./R/app/server_dashboard_options.R",local=serverENV)
  source("./R/app/server_dashboard_reports.R",local=serverENV)
  source("./R/app/server_dashboard_edit.R",local=serverENV)
  source("./R/app/server_dashboard_exports.R",local=serverENV)
  source("./R/app/server_dashboard_exports_reports.R",local=serverENV)
  source("./R/app/server_datasets.R",local=serverENV)
  source("./R/app/server_datasets_review_flags.R",local=serverENV)
  
  source("./R/app/server_datasets_upload.R",local=serverENV)
  
  #When permissions are implemented, only load administrateive modules if an admin
  source("./R/app/server_setup_agreement.R",local=serverENV)
  source("./R/app/server_setup_program.R",local=serverENV)
  source("./R/app/server_setup_indicators.R",local=serverENV)
  source("./R/app/server_setup_checks.R",local=serverENV)
  source("./R/app/server_setup_templates.R",local=serverENV)
  source("./R/app/server_setup_create.R",local=serverENV)
  
  source("./R/app/server_admin_options.R",local=serverENV)
  source("./R/app/server_admin_options_module.R",local=serverENV)
  
  source("./R/app/server_admin_indicators.R",local=serverENV)
  source("./R/app/server_admin_indicator_formulas.R",local=serverENV)
  source("./R/app/server_admin_indicators_review.R",local=serverENV)
  
  source("./R/app/server_admin_checks.R",local=serverENV)
  source("./R/app/server_admin_checks_formulas.R",local=serverENV)
  source("./R/app/server_admin_checks_review.R",local=serverENV)
  
  source("./R/app/server_admin_users.R",local=serverENV)
  
  login_initialize <- observeEvent(USER_ID(),{ 
  
    if (!isTruthy(USER_ID())) return (NULL)
    #if (INITIALIZED()) return (NULL) #Don't want to re-re-load if user logs out and logs back in.

    t1 <- Sys.time()
    # source("./R/app/server_dashboard.R",local=serverENV)
    # source("./R/app/server_dashboard_options.R",local=serverENV)
    # source("./R/app/server_dashboard_reports.R",local=serverENV)
    # source("./R/app/server_dashboard_edit.R",local=serverENV)
    # source("./R/app/server_dashboard_exports.R",local=serverENV)
    # source("./R/app/server_dashboard_exports_reports.R",local=serverENV)
    # source("./R/app/server_datasets.R",local=serverENV)
    # source("./R/app/server_datasets_review_flags.R",local=serverENV)
    # 
    # source("./R/app/server_datasets_upload.R",local=serverENV)
    # 
    # #When permissions are implemented, only load administrateive modules if an admin
    # source("./R/app/server_setup.R",local=serverENV)
    # source("./R/app/server_setup_program.R",local=serverENV)
    # source("./R/app/server_setup_indicators.R",local=serverENV)
    # source("./R/app/server_setup_checks.R",local=serverENV)
    # source("./R/app/server_setup_templates.R",local=serverENV)
    # source("./R/app/server_setup_create.R",local=serverENV)
    # 
    # source("./R/app/server_admin_options.R",local=serverENV)
    # source("./R/app/server_admin_options_module.R",local=serverENV)
    # 
    # source("./R/app/server_admin_indicators.R",local=serverENV)
    # source("./R/app/server_admin_indicator_formulas.R",local=serverENV)
    # source("./R/app/server_admin_indicators_review.R",local=serverENV)
    # 
    # source("./R/app/server_admin_checks.R",local=serverENV)
    # source("./R/app/server_admin_checks_formulas.R",local=serverENV)
    # source("./R/app/server_admin_checks_review.R",local=serverENV)
    # 
    # source("./R/app/server_admin_users.R",local=serverENV)
    # 
    # source("./R/app/server_datasets_guidance_module.R",local=serverENV)
    login_initialize$destroy()
    
  })
  
  # output$dashboard_title <- renderText({ 
  #   user_id <- USER_ID()
  #   if (!isTruthy(user_id)) return ("RSF/ Jason")
  #   
  #   program <- SELECTED_PROGRAM()
  #   if (!isTruthy(program)) return ("RSF <Select>")
  #   else return (paste0("RSF: ",program$program_nickname))
  # })
  # 
  
  observeEvent(input$login_request_action, { 
    
    m <- modalDialog(id="server_admin_request__create_user_modal",
                     div(align="center",
                         div(style="background-color:white;padding:5px;height:275px;width:450px;",
                             align="left",
                             fluidRow(
                               column(12,
                                      textInput(inputId="server_admin_request__create_user_name",
                                                label="Your Name",
                                                placeholder="Enter Your First & Last Name"))),
                             fluidRow(
                               column(12,
                                      textInput(inputId="server_admin_request__create_user_email",
                                                label="Your Email Address",
                                                placeholder="Your email is your account login")))
                             
                         )),
                     
                     title=HTML("Request RSF Jason Account"),
                     easyClose = FALSE,
                     footer=div(style="display:inline-block;width:100%;",
                                div(style="display:inline-block;float:left;",
                                    modalButton("Cancel")),
                                div(style="display:inline-block;float:right;",
                                    actionButton(inputId="server_admin_request__create_action",
                                                 label="Submit",
                                                 class="btn-success"))),
                     size="s")
    showModal(m)
    
  })
  
  observeEvent(input$server_admin_request__create_action, {
    
    name <- trimws(input$server_admin_request__create_user_name)
    email <- trimws(input$server_admin_request__create_user_email)
    
    if (!isTruthy(name) || 
        !isTruthy(email)) {
      return(showNotification(type="error",
                              ui=h3("Name and email address must be completed")))
    }
    
    AD_user <- session$user
    
    if (!isTruthy(AD_user)) {
      return(showNotification(type="error",
                              ui=h3("Request failed: it appears you are not logged into a World Bank Group computer?")))
    }
    
    if (!grepl(paste0("^",AD_user),email)) {
      return(showNotification(type="error",
                              ui=h3("Request failed: You may only request an account for yourself and not on behalf of someone else.")))
    }

    if (!grepl("^[[:alnum:]_\\.]+@(ifc|worldbank|miga|worldbankgroup)\\.(org|onmicrosoft\\.com)$",email,ignore.case=T)) {
      return(showNotification(type="error",
                              ui=h3("Request failed: You may only request an account registered to a World Bank Group email address")))
    }
    
    #DBPOOL_APPLICATIONS <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[["ARL"]]))
    lookup_email <- db_user_check_email_exists(pool=DBPOOL_APPLICATIONS,
                                               RSF_MANAGEMENT_APPLICATION_ID,
                                               email=email)
    
    if (length(lookup_email)) {
      return(showNotification(type="error",
                              ui=h3("You already have an account registered under email addres '",lookup_email,"' Try clicking 'Forgot password' instead?")))
    }
    
    
    new_account_id <- tryCatch({
      new_account_id <- DBPOOL_APPLICATIONS %>% dbGetQuery("
      select * 
      from arlapplications.accounts_create(v_application_hashid => $1::text,
                                           v_request_by_account_id => $2::text,
                                           v_name => $3::text,
                                           v_login => $4::text)",
                                           params=list(RSF_MANAGEMENT_APPLICATION_ID,
                                                       ACCOUNT_SYS_ADMIN$account_id,
                                                       name,
                                                       email))
      
      new_account_id <- unlist(new_account_id)
      
      reset_code <- db_user_reset_password(pool=DBPOOL_APPLICATIONS,
                                           application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                                           sysadmin_id=ACCOUNT_SYS_ADMIN$account_id,
                                           username=email)
      
      email <- div(p(paste0("Dear ",tools::toTitleCase(name),",")),
                   p("Your ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," account has been created."),
                   p("You username is your email: ",email),
                   p("Please use the link above to login for the first time and complete setting up your account with this temporary password: ",reset_code$reset_password),
                   p("First Time Account Creation Steps"),
                   p("1: Click the ",tags$a(href="https://datanalytics-int.worldbank.org/rsf-prod/","RSF Jason")," link"),
                   p("2: Enter your email address '",email,"' in the username field."),
                   p("3: Enter your temporary password '",reset_code$reset_password,"' in the password field"),
                   p("4: Click the Login button -> You will be redirected to change your temporary password"),
                   p("5: Enter your temporary password '",reset_code$reset_password,"' in the tempoerary password field."),
                   p("6: Enter your own personalized (memoarable and secure) password in the New Password field."),
                   p("7: Again re-enter your own personalized password in the Verify Password field"),
                   p("8: Click the Login button -> You will now be logged into the Jason system"))
      
      user_send_email(arl_pool=DBPOOL_APPLICATIONS,
                      to=reset_code$login_email,
                      subject="RSF JASON | password reset",
                      html=email)
    },
    error = function(e) {
      showNotification(type="error",
                       ui=h3(conditionMessage(e)))
      NULL
    },
    warning = function(w) {
      showNotification(type="error",
                       ui=h3(conditionMessage(w)))
      NULL
    })
    
    SERVER_ADMIN_USERS_LIST.REFRESH(SERVER_ADMIN_USERS_LIST.REFRESH()+1)
    removeModal()
    
  })
  
  intercept_status_message <- function(...,
                                       class="none",
                                       clear.panel=FALSE) { 
    
    panel <- isolate({ STATUS_MESSAGE_PANEL$panel_id })
    container <- isolate({ STATUS_MESSAGE_PANEL$container_id })
    class <- tolower(class)
    if (!class %in% c("info","warning","error","none")) class <- "none"
    
    l <- list(...)
    txt <- do.call(c,l)
    txt <- paste0(txt,collapse="")
    txt <- gsub("[[:cntrl:]]+","<br>",txt)
    
    if (class=="error") txt <- paste0("<span style='color:red;font-weight:bold;'>",txt,"</span>")
    else if (class=="warning") txt <- paste0("<span style='color:orange;font-weight:bold;'>",txt,"</span>")
    else if (class=="info") txt <- paste0("<span style='color:skyblue;font-weight:bold;'>",txt,"</span>")
    
    
    if (clear.panel==TRUE) {
      removeUI(selector=paste0("#",panel),
               immediate=TRUE,)
      insertUI(selector=paste0("#",container),
               ui=div(id=panel),
               where="afterBegin",
               immediate=TRUE)
      #runjs(paste0("document.getElementById('",container,"').innerHTML = ''"))
    }
    
    where <- "beforeEnd"
    insertUI(paste0("#", panel), where = where,
             ui = HTML(paste0(txt)),immediate = TRUE)
    
    runjs(paste0("document.getElementById('",container,"').scrollTop = document.getElementById('",container,"').scrollHeight"))
    cat(txt,"\n")    
  }

  #onStop
  onSessionEnded(function() {
    print("Session onSessionEnded called.")
    
    if (!is.null(DBPOOL) && any(pool::dbIsValid(DBPOOL)==TRUE,DBPOOL$valid)) {
      print("Closing DBPOOL")
      poolClose(DBPOOL)
    }
  })
}


