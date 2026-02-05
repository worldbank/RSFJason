



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
  source("./R/app/server_setup.R",local=serverENV)
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
  
  output$dashboard_title <- renderText({ 
    user_id <- USER_ID()
    if (!isTruthy(user_id)) return ("RSF/ Jason")
    
    program <- SELECTED_PROGRAM()
    if (!isTruthy(program)) return ("RSF <Select>")
    else return (paste0("RSF: ",program$program_nickname))
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


