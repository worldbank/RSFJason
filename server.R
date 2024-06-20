



server <- function(input, output, session) 
{
  ns <- NS("RSF")
  DBPOOL <- NULL
  serverENV <- environment()
  server_module_registry <- list()
  
  # if (!is.null(DBPOOL) && any(pool::dbIsValid(DBPOOL)==TRUE,DBPOOL$valid)) {
  #   print("Closing DBPOOL")
  #   poolClose(DBPOOL)
  # }
  
  DBPOOL <- dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]]))
  if (!is.null(DBPOOL) && pool::dbIsValid(DBPOOL)) { print("DBPOOL (MAIN) Started")
  } else { print("DBPOOL (MAIN) FAILED TO START") }
  #pool <- DBPOOL
  #
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
  
  Shiny.destroyList <- function(observers_list)
  {
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
  
  #source("./R/rsf_calculations_global.R",local=serverENV)
  source("./R/app/server_programs.R",local=serverENV)

  module_accounts_server(id="accounts_server",
                         parent_session = session,
                         pool=DBPOOL_APPLICATIONS,
                         application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                         application_account_id=ACCOUNT_SYS_ADMIN$account_id,
                         USER_ACCOUNT=USER_ACCOUNT,
                         cookie_name="ARL-applications")
  
  USER_ID <- eventReactive(USER_ACCOUNT$user_account_id, { USER_ACCOUNT$user_account_id },ignoreNULL = FALSE)
  LOGGEDIN <- reactive({ isTruthy(USER_ACCOUNT$user_account_id) && isTruthy(USER_ACCOUNT$application_session_id) })
  
  login_initialize <- observeEvent(USER_ID(),{ 
  
    if (!isTruthy(USER_ID())) return (NULL)
    #if (INITIALIZED()) return (NULL) #Don't want to re-re-load if user logs out and logs back in.

    t1 <- Sys.time()
    source("./R/app/server_dashboard.R",local=serverENV)
    source("./R/app/server_dashboard_options.R",local=serverENV)
    source("./R/app/server_dashboard_reports.R",local=serverENV)
    
    source("./R/app/server_datasets.R",local=serverENV)
    source("./R/app/server_datasets_review_flags.R",local=serverENV)
    source("./R/app/server_datasets_indicatorflags_module.R",local=serverENV)
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
    
    source("./R/app/server_datasets_guidance_module.R",local=serverENV)
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

  onStop(function() {
    if (!is.null(DBPOOL) && any(pool::dbIsValid(DBPOOL)==TRUE,DBPOOL$valid)) {
      print("Closing DBPOOL")
      poolClose(DBPOOL)
    }
  })
}


