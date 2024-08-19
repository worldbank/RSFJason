

####################
###########OBSERVERS
####################

#Select new program: update dropdowns for appropriate program
observeEvent(SELECTED_PROGRAM(), {
  
  program <- SELECTED_PROGRAM()
  loaded_program_id <- LOAD_PROGRAM_ID()
  
  updateSelectizeInput(session=session,
                       inputId="setup_program_create_what",
                       selected="")
  
  if (!isTruthy(program)) {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_what",
                         choices=c("",Program="program"),
                         selected="")    
  } else {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_what",
                         choices=c("",
                                   Program="program",   #Only left-in for people to select to get an error message instead of wondering why it's not available
                                   Facility="facility",
                                   Client="client",
                                   Borrower="borrower",
                                   Loan="loan"),
                         selected="")
  }
  
  #Guide user to PROGRAM admin instead  
  if (isTruthy(loaded_program_id) && !empty(program) && loaded_program_id==program$rsf_program_id) {
    
    showNotification(type="message",
                     ui=h3("Setup the relevant indicators and checks for this program to monitor.  By default, program monitors all indicators and all checks.  Click on the indicator and/or check from the list below to toggle monitoring OFF for this Program."))
    
    updateTabItems(session=session,
                   inputId="sidebarMenu",
                   selected="admin")
    
    updateTabsetPanel(session=session,
                      inputId="tabset_setup_program",
                      selected="Setup Program Indicators")
    
  }
}, ignoreNULL = FALSE)

observeEvent(input$setup_program_create_action_button, {
  
  what <- input$setup_program_create_what
  rsf_program_id <- SELECTED_PROGRAM_ID()
  facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
  client_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_client)
  borrower_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_borrower)
  
  reporting_frequency <- "quarter"
  if (!isTruthy(what)) return (NULL)
  if (what=="program" && isTruthy(rsf_program_id)) {
    return(showNotification(type="error",
                            h3("Error: Selected RSF Program must be blank to create a new Program")))
  }
  if (what=="facility" && !isTruthy(rsf_program_id)) {
    return(showNotification(type="error",
                            h3("Error: RSF Program must be selected to create a new Facility")))
  }
  if (what=="client" && (!isTruthy(rsf_program_id) || !isTruthy(facility_rsf_pfcbl_id))) {
    return(showNotification(type="error",
                            h3("Error: Facility must be selected to create a new Client")))
  }
  if (what=="borrower" && (!isTruthy(rsf_program_id) || !isTruthy(facility_rsf_pfcbl_id) || !isTruthy(client_rsf_pfcbl_id))) {
    return(showNotification(type="error",
                            h3("Error: Client and Facility must be selected to create a new Borrower")))
  }
  if (what=="loan" && (!isTruthy(rsf_program_id) || !isTruthy(facility_rsf_pfcbl_id) || !isTruthy(client_rsf_pfcbl_id) || !isTruthy(borrower_rsf_pfcbl_id))) {
    return(showNotification(type="error",
                            h3("Error: Client, Facility and Borrower must be selected to create a new Loan")))
  }
  
  reporting_asof_date <- as.character(input$setup_program_create_setup_reporting_asof_date)
  if (what != "program") {
    if (!isTruthy(reporting_asof_date) || is.na(suppressWarnings(ymd(reporting_asof_date)))) {
      return(showNotification(type="error",
                              h3("Error: Creation reporting date must be selected")))
      
    }
    
    reporting_asof_date <- ymd(reporting_asof_date)
    reporting_frequency <- DBPOOL %>% dbGetQuery("select reporting_period from p_rsf.rsf_programs rp
                                                  where rp.rsf_program_id = $1::int",
                                                 rsf_program_id)
    reporting_frequency <- reporting_frequency$reporting_period
  } else {
    reporting_asof_date <- as.Date(as.numeric(NA))
    
  }
  
  reporting_asof_start <- floor_date(reporting_asof_date,reporting_frequency)
  
  setup_indicator_ids <- grep("^setup_program_create_setup_indicator_",names(reactiveValuesToList(input)),value=T)
  setup_indicators <- lapply(setup_indicator_ids,function(x) {
    val <- input[[x]]
    if (length(val) > 1) val <- paste0(val,collapse=" & ")
    if (is.null(val) || is.na(val) || length(val)==0 || nchar(val)==0) val <- as.character(NA) #not using isTruthy because it could submit FALSE
    else val <- as.character(val)
    
    data.table(setup_indicator=x,
               data_value=val)
  })
  setup_indicators <- rbindlist(setup_indicators)
  #si <<- as.data.frame(setup_indicators)
  #setup_indicators <- as.data.table(si)
  #what <<- what
  #rsf_program_id <<- rsf_program_id
  #facility_rsf_pfcbl_id <<- facility_rsf_pfcbl_id
  
  setup_indicators[,indicator_id:=as.numeric(gsub("^setup_program_create_setup_indicator_","",setup_indicator))]
  
  indicator_requirements <- DBPOOL %>% db_create_entity_indicator_requirements(entity_type=what)
  
  setDT(indicator_requirements)
  setup_indicators <- setup_indicators[indicator_requirements,
                                       on=.(indicator_id),
                                       nomatch=NULL][,.(indicator_id,
                                                        indicator_name,
                                                        indicator_sys_category,
                                                        data_category,
                                                        data_value,
                                                        default_value,
                                                        is_setup)]
  setup_indicators[,data_value:=trimws(data_value)]
  setup_indicators[!is.na(default_value) & is.na(data_value),
                   data_value:=as.character(default_value)]
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_local_currency_unit" & data_value %in% c("LCU","LCY")])) {
    return(showNotification(type="error",
                            h3("Error: Local Currency Unit cannot be a generic 'LCU'")))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_completion_date"][ymd(data_value) < reporting_asof_start])) {
    return(showNotification(type="error",
                            h3(paste0("Completion date cannot be before ",reporting_asof_start))))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="reporting_required_start_date"][ymd(data_value) < reporting_asof_start])) {
    return(showNotification(type="error",
                            h3(paste0("Reporting required start date cannot be before ",reporting_asof_start))))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) < reporting_asof_start])) {
    bad_names <- setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) < reporting_asof_start,
                                                                                  indicator_name]
    return(showNotification(type="error",
                            h3(paste0("Creation date ",(paste0(bad_names,collapse=" & "))," cannot be before start of reporting period ",
                                      as.character(reporting_asof_start)))))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_renewal_date"][ymd(data_value) < reporting_asof_start])) {
    return(showNotification(type="error",
                            h3(paste0("Renewal date cannot be before ",reporting_asof_start))))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) > reporting_asof_date])) {
    bad_names <- setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) > reporting_asof_date,
                                                                                  indicator_name]
    return(showNotification(type="error",
                            h3(paste0("Creation date ",(paste0(bad_names,collapse=" & "))," cannot be in the future from reporting date ",
                                      as.character(reporting_asof_date)))))
  }
  
  
  if (anyNA(setup_indicators[is_setup=="required",data_value])) {
    missing_values <- setup_indicators[is_setup=="required" & is.na(data_value),indicator_name]
    missing_values <- paste0(missing_values,collapse=", ")
    showNotification(type="error",
                     duration=NULL,
                     div(h3("The following fields are required: "),
                         p(missing_values)))
    return (NULL)
  }
  setup_indicators <- setup_indicators[is.na(data_value)==FALSE]
  nickname <- toupper(setup_indicators[indicator_sys_category=="nickname",data_value])
  if (!isTruthy(nickname)) nickname <- paste0(what,"_",trimws(toupper(setup_indicators[indicator_sys_category=="name",data_value])))
  if (!isTruthy(nickname)) nickname <- paste0("NEW_",what)
  
  export_filename <- paste0("create_RSF_",nickname,".xlsx")

  #browser()
  tryCatch({
    withProgress(value=.10,message="Saving datasets...", {
      excelwb <- NULL
      if (what=="program") {

        if (length(setdiff(setup_indicators$indicator_sys_category,
                           c("entity_creation_date",
                             "name",
                             "nickname",
                             "entity_local_currency_unit"))) > 0) {
          stop("Create program only recognizes fields: entity_creation_date, name, nickname, entity_local_currency_unit")
        }
        
        results <- DBPOOL %>% db_program_create(program_name=setup_indicators[indicator_sys_category=="name",data_value],
                                                program_nickname=setup_indicators[indicator_sys_category=="nickname",data_value],
                                                program_inception_date=ymd(setup_indicators[indicator_sys_category=="entity_creation_date",data_value]),
                                                program_lcu=setup_indicators[indicator_sys_category=="entity_local_currency_unit",data_value],
                                                reporting_user_id=USER_ID(),
                                                program_reporting_frequency="quarter",
                                                source_name="Program Created in JASON")
        LOAD_PROGRAM_ID(results$rsf_program_id)
          
      } else {
        
        parent_rsf_pfcbl_id <- NULL
        if (what=="facility") parent_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
        else if (what=="client") parent_rsf_pfcbl_id <- facility_rsf_pfcbl_id
        else if (what=="borrower") parent_rsf_pfcbl_id <- client_rsf_pfcbl_id
        else if (what=="loan") parent_rsf_pfcbl_id <- borrower_rsf_pfcbl_id
        
        excelwb <- DBPOOL %>% export_create_entity_to_excel(parent_rsf_pfcbl_id=parent_rsf_pfcbl_id,
                                                            reporting_asof_date=reporting_asof_date,
                                                            entity_data=setup_indicators,
                                                            exporting_user_id=USER_ID())
          
        saveWorkbook(excelwb,
                     file=export_filename,
                     overwrite=TRUE)
        
        incProgress(amount=0.40,message=paste0("Creating new ",what,"..."))
        
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Uploading data: ",dots))
        }
        
        results <- tryCatch({
          #Template gets its own database pool
          template_parse_process_and_upload(pool=dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]])),
                                            rsf_program_id = SELECTED_PROGRAM_ID(),
                                            reporting_user_id = USER_ID(),
                                            template_files=export_filename,
                                            source_note="None",
                                            delete_on_error=FALSE,
                                            status_message=progress_status_message)
        },
        error=function(e) {
          showNotification(type="error",
                           duration=NULL,
                           ui=h3(paste0("An error has occcurred. Failed to create new ",what,":  ",conditionMessage(e),
                                        "Please review the datasets panel and download the dataset, if available; then delete it.")))
          return(NULL)
          
        },
        warning=function(w) {
          showNotification(type="error",
                           duration=NULL,
                           ui=h3(paste0("An error has occcurred. Failed to create new ",what,":  ",conditionMessage(w),
                                        "Please review the datasets panel and download the dataset, if available; then delete it.")))
          return(NULL)
          
        })
        
        file.remove(export_filename)
        
        
          created_rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("select ids.rsf_pfcbl_id
                                                          from p_rsf.reporting_cohorts rc
                                                          inner join p_rsf.rsf_pfcbl_ids ids on ids.created_by_reporting_cohort_id = rc.reporting_cohort_id
                                                          where rc.reporting_cohort_id = $1::int
                                                             or rc.parent_reporting_cohort_id = $1::int",
                                                         params=list(unique(results$reporting_cohort_id)))
          created_rsf_pfcbl_ids <- unlist(created_rsf_pfcbl_ids)
          if (!isTruthy(created_rsf_pfcbl_ids)) created_rsf_pfcbl_ids <- NA
          
          LOAD_REPORTING_COHORT(results$reporting_cohort_id)
          if (what %in% c("facility","client")) LOAD_RSF_PFCBL_IDS(created_rsf_pfcbl_ids)
          
          if (!any(as.character(unique(results$reporting_asof_date)) %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) {
            new_dates <- setdiff(sort(as.character(unique(results$reporting_asof_date))),as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))
            LOAD_VALID_REPORTING_DATE(new_dates)
          }
          
          # DASH_LOAD_DASHBOARD(reporting_asof_date=unique(results$reporting_asof_date),
          #                     rsf_pfcbl_ids=created_rsf_pfcbl_ids,
          #                     indicator_ids=c(),
          #                     display_report_id=NA,
          #                     display_currency="LCU",
          #                     display_timeline=FALSE,
          #                     display_timeline_format="html",
          #                     display_flags=TRUE,
          #                     filter_flags=as.character(NA),
          #                     filter_text=paste0("SYSID:",paste0(created_rsf_pfcbl_ids,collapse=",")),
          #                     filter_indicator_types=c("USER"),
          #                     indicator_ids.sorted=FALSE)
          
          showNotification(type="message",
                           ui=h3(paste0("Edit additional information for this ",toupper(what)," using the dashboard OR download RSF SETUP FILE")))
          
        
        
        updateSelectizeInput(session=session,
                             inputId="setup_program_create_what",
                             selected="")
        
      }
    })
  },
  error=function(e) {
    showNotification(type="error",
                     duration=NULL,
                     ui=div(h3(paste0("An error has occurred: failed to create new ",what)),
                            p(conditionMessage(e))))
    
  },
  warning=function(w) {
    showNotification(type="error",
                     duration=NULL,
                     ui=div(h3(paste0("An error has occurred: failed to create new ",what)),
                            p(conditionMessage(w))))
    
    
  })
  
})

observeEvent(input$setup_program_create_what, {
  what <- input$setup_program_create_what
  program_id <- SELECTED_PROGRAM_ID()
  
  if (isTruthy(program_id) && any(what==c("client","borrower","loan"))) {
    
    facilities <- DBPOOL %>% dbGetQuery("select 
                                              nids.rsf_pfcbl_id,
                                              nids.rsf_full_name
                                            from p_rsf.view_current_entity_names_and_ids nids
                                            where rsf_program_id = $1::int
                                              and pfcbl_category = 'facility'
                                            order by nids.rsf_full_name",
                                        params=list(program_id))
    if (empty(facilities)) facilities <- setNames("","No facilities exist")
    else facilities <- c("",setNames(facilities$rsf_pfcbl_id,facilities$rsf_full_name))
    
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_facility",
                         choices=facilities,
                         selected="")
    showElement(id="setup_program_create_selected_facility",anim = TRUE,animType="fade")
  } else {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_facility",
                         choices=c(""),
                         selected="")
    
    hideElement(id="setup_program_create_selected_facility")
    hideElement(id="setup_program_create_selected_client")
    hideElement(id="setup_program_create_selected_borrower")
  }
}, ignoreNULL = FALSE)

observeEvent(input$setup_program_create_selected_facility, {
  facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
  what <- input$setup_program_create_what
  
  if (isTruthy(facility_rsf_pfcbl_id) && any(what==c("borrower","loan"))) {
    
    clients <- DBPOOL %>% dbGetQuery("select 
                                      	nids.rsf_pfcbl_id,
                                      	nids.rsf_full_name
                                      from p_rsf.rsf_pfcbl_id_family fam
                                      inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
                                      where fam.parent_rsf_pfcbl_id = $1::int 
                                      	and fam.child_pfcbl_category = 'client'
                                      order by nids.rsf_full_name",
                                     params=list(facility_rsf_pfcbl_id))
    
    if (empty(clients)) clients <- setNames("","No clients exist")
    else clients <- c("",setNames(clients$rsf_pfcbl_id,clients$rsf_full_name))
    
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_client",
                         choices=clients,
                         selected="")
    
    showElement(id="setup_program_create_selected_client",anim = TRUE,animType="fade")
    
  } else {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_client",
                         choices=c(""),
                         selected="")
    
    hideElement(id="setup_program_create_selected_client")
    hideElement(id="setup_program_create_selected_borrower")
    
  }
  
}, ignoreNULL = FALSE)

observeEvent(input$setup_program_create_selected_client, {
  client_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_client)
  what <- input$setup_program_create_what
  
  if (isTruthy(client_rsf_pfcbl_id) && any(what==c("loan"))) {
    
    borrowers <- DBPOOL %>% dbGetQuery("select 
                                      	nids.rsf_pfcbl_id,
                                      	nids.rsf_full_name
                                      from p_rsf.rsf_pfcbl_id_family fam
                                      inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = fam.child_rsf_pfcbl_id
                                      where fam.parent_rsf_pfcbl_id = $1::int 
                                      	and fam.child_pfcbl_category = 'borrower'
                                      order by nids.rsf_full_name",
                                      params=list(client_rsf_pfcbl_id))
    
    if (empty(borrowers)) borrowers <- setNames("","No borrowers exist")
    else borrowers <- c("",setNames(borrowers$rsf_pfcbl_id,borrowers$rsf_full_name))
    
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_borrower",
                         choices=borrowers,
                         selected="")
    showElement(id="setup_program_create_selected_borrower",anim = TRUE,animType="fade")
  } else {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_borrower",
                         choices=c(""),
                         selected="")
    
    hideElement(id="setup_program_create_selected_borrower")
  }
}, ignoreNULL = FALSE)


##################
###########OUTPUTS
##################

output$setup_program_create_what_text <- renderText({
  what <- input$setup_program_create_what
  what_text <- ""
  if (!isTruthy(what)) what_text <- ""
  else if (what=="program") {
    what_text <- "Create New Program"
  } else if (what=="facility") {
    what_text <- paste0("Create New Facility for RSF Program ",SELECTED_PROGRAM()$program_nickname)
  } else if (what=="client") {
    facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
    facility_name <- DBPOOL %>% dbGetQuery("select rsf_full_name
                                             from p_rsf.view_current_entity_names_and_ids nids
                                             where nids.rsf_program_id = $1::int and nids.rsf_pfcbl_id = $2::int",
                                           params=list(SELECTED_PROGRAM_ID(),
                                                       facility_rsf_pfcbl_id))
    what_text <- paste0("Create New Client for RSF Facility ",unlist(facility_name))
    
  } else if (what=="borrower") {
    client_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_client)
    client_name <- DBPOOL %>% dbGetQuery("select rsf_full_name
                                             from p_rsf.view_current_entity_names_and_ids nids
                                             where nids.rsf_program_id = $1::int and nids.rsf_pfcbl_id = $2::int",
                                         params=list(SELECTED_PROGRAM_ID(),
                                                     client_rsf_pfcbl_id))
    what_text <- paste0("Create New Borrower for IFC's Client ",unlist(client_name))
    
  } else if (what=="loan") {
    borrower_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_borrower)
    borrower_name <- DBPOOL %>% dbGetQuery("select rsf_full_name
                                             from p_rsf.view_current_entity_names_and_ids nids
                                             where nids.rsf_program_id = $1::int and nids.rsf_pfcbl_id = $2::int",
                                           params=list(SELECTED_PROGRAM_ID(),
                                                       borrower_rsf_pfcbl_id))
    what_text <- paste0("Create New Loan for IFC's Client's Borrower ",unlist(borrower_name))
    
  }
  return (what_text)
})

output$setup_program_create_ui <- renderUI({
  what <- input$setup_program_create_what
  if (!isTruthy(what) || !what %in% c("program","facility","client","borrower","loan")) {
    return (h3("Please select what type of new entity to create from the drop-down menu"))
  }
  
  if (what=="program" && isTruthy(as.numeric(SELECTED_PROGRAM_ID()))) {
    
    return (div(h3("A new program cannot be created while ",SELECTED_PROGRAM()$program_nickname," is actively selected."),
                p("Use back-space to remove any program selection from the main Program choice in upper-left Nav Bar")))
  } else if (what %in% c("facility","client","borrower","loan") && !isTruthy(as.numeric(SELECTED_PROGRAM_ID()))) {
    
    return (div(h3("Please select an RSF Program in the upper-left Nav Bar.")))
  }
  
  
  rsf_indicators <- RSF_INDICATORS()
  indicator_requirements <- DBPOOL %>% db_create_entity_indicator_requirements(entity_type=what)
  
  setDT(indicator_requirements)
  indicator_requirements <- split(indicator_requirements,by="indicator_id")
  
  #TODO Note: doesn't include any OPTIONS/drop down. A bit lazy for the UI, but at present no set-up indicators are choices and this UI is used so rarely that
  #Users can type in what is needed.
  
  ui_requirements <- lapply(indicator_requirements,
                            function(x) {
                              indicator <- rsf_indicators[indicator_id==x$indicator_id]
                              placeholder <- x$default_value
                              if (is.na(x$default_value) && x$data_type=="date") placeholder  <- "Use date format: YYYY-MM-DD"
                              
                              if (is.na(placeholder)) placeholder <- NULL
                              
                              label <- rbindlist(indicator$labels)[label_key=="EN" & is_primary==TRUE,label]
                              if (!isTruthy(label)) label <- indicator$indicator_name
                              label <- paste0(label," (",x$is_setup,")")
                              
                              inputId <- paste0("setup_program_create_setup_indicator_",x$indicator_id)
                              uiOb <- NULL
                              
                              if (isTruthy(as.numeric(indicator$options_group_id))) {
                                
                                multiple <- as.logical(indicator$indicator_options_group_allows_multiples)
                                if (is.na(multiple)) multiple <- FALSE
                                
                                options <- rbindlist(indicator$options_group)
                                options <- unique(options[label_key=="EN" & is_primary==TRUE,
                                                          .(options_group_key,label)])[order(label)]
                                choices <- c("",setNames(options$options_group_key,options$label))
                                uiOb <- selectizeInput(inputId=inputId,
                                                       label=label,
                                                       choices=choices,
                                                       selected="",
                                                       multiple=multiple,
                                                       options=list(placeholder=placeholder))
                                
                                
                              } else if (indicator$is_data_unit==TRUE & grepl("currency",indicator$indicator_name)) {
                                currencies <- GLOBAL_CURRENCIES()
                                currencies <- c("",currencies)
                                uiOb <- selectizeInput(inputId=inputId,
                                                       label=label,
                                                       choices=currencies,
                                                       selected="",
                                                       multiple=FALSE,
                                                       options=list(placeholder=placeholder))
                                
                              } else if (indicator$data_type=="date") {
                                uiOb <- dateInput(inputId=inputId,
                                                  label=label,
                                                  value="")
                                
                              } else {
                                uiOb <- textInput(inputId=paste0("setup_program_create_setup_indicator_",x$indicator_id),
                                                  label=label,
                                                  value="",
                                                  placeholder = placeholder)
                              }
                              
                              return(fluidRow(column(12,uiOb)))
                            })
  
  if (what != "program") {
    
    #Querying VIEW to ensure fully available valid reporting dates, eg, if user is creating entity whose start date is beyond the currentest program reporting date
    #They will be able to do so.  This should prompt a refresh of SELECTED_PROGRAM_VALID_REPORTING_DATES() so the new date becomes available in drop downs.
    program_dates <- DBPOOL %>% dbGetQuery("select rd.valid_reporting_date::text as valid_reporting_date 
                                            from p_rsf.rsf_program_generate_reporting_dates($1::int,now()::date) as rd
                                            order by rd.valid_reporting_date",
                                           params=list(SELECTED_PROGRAM_ID()))
    
    program_dates <- as.character(program_dates$valid_reporting_date)
    
    create_title <- fcase(what=="facility","Reporting Period: Facility IFC first commitment date (required)",
                          what=="client","Reporting Period: Client signing date (required)",
                          what=="borrower","Reporting Period: Borrower became RSF client (required)",
                          what=="loan","Reporting Period: Loan commitment date (required)",
                          default="Error")
    
    reporting_ui <- fluidRow(column(12,
                                    selectizeInput(inputId="setup_program_create_setup_reporting_asof_date",
                                                   label=create_title,
                                                   choices=program_dates,
                                                   selected=program_dates[length(program_dates)],
                                                   options=list(placeholder="Select reporting date"))))
    ui_requirements <- append(list(reporting_ui),ui_requirements)
  }
  ui_requirements <- tagList(ui_requirements)
  
  ui <- div(style="width:75%;",
            ui_requirements,
            fluidRow(column(12,align="right",
                            actionButton(inputId="setup_program_create_action_button",
                                         label=toTitleCase(paste0("Create ",what)),
                                         class="btn btn-success",
                                         icon=icon("gears"))
            )))
  return (ui)
})
