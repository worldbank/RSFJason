SERVER_DASHBOARD_REPORTS_LIST_REFRESH <- reactiveVal(0)
SERVER_DASHBOARD_REPORT_SELECTED <- reactiveVal(list())
SERVER_DASHBOARD_REPORTS_LIST <- eventReactive(c(LOGGEDIN(),
                                                 SELECTED_PROGRAM_ID(),
                                                 SERVER_DASHBOARD_REPORTS_LIST_REFRESH()), {

  program_id <- SELECTED_PROGRAM_ID()
  SERVER_DASHBOARD_REPORT_SELECTED(list())
  if (!LOGGEDIN()) return (NULL) 
  
  reports <- DBPOOL %>% dbGetQuery("
  select 
    re.report_id,
    re.created_by_user_id,
    coalesce(vai.users_name,'UNKNOWN') as users_name,
    re.is_public,
    re.report_title,
    re.report_notes,
    coalesce(re.for_client_sys_names,'[null]'::jsonb) as for_client_sys_names,
    coalesce(re.for_indicator_names,'[null]'::jsonb) as for_indicator_names,
    coalesce(re.for_asof_dates,'[null]'::jsonb) as for_asof_dates,
    coalesce(re.report_parameters,'[null]'::jsonb) as report_parameters
  from p_rsf.reports re 
  left join p_rsf.view_account_info vai on vai.account_id = re.created_by_user_id
  where exists(select * from p_rsf.rsf_data_current_names_and_ids nai
               where nai.rsf_pfcbl_id = $1::int
                 and nai.sys_name = re.for_program_sys_name)
  
  union all
  
  select 
    re.report_id,
    re.created_by_user_id,
    coalesce(vai.users_name,'UNKNOWN') as users_name,
    re.is_public,
    re.report_title,
    re.report_notes,
    coalesce(re.for_client_sys_names,'[null]'::jsonb) as for_client_sys_names,
    coalesce(re.for_indicator_names,'[null]'::jsonb) as for_indicator_names,
    coalesce(re.for_asof_dates,'[null]'::jsonb) as for_asof_dates,
    coalesce(re.report_parameters,'[null]'::jsonb) as report_parameters
  from p_rsf.reports re 
  
  left join p_rsf.view_account_info vai on vai.account_id = re.created_by_user_id
  where re.for_program_sys_name is NULL",
  params=list(program_id))

  json_cols <- names(reports)[sapply(reports,class)=="pq_jsonb"]
  for (col in json_cols) set(reports,i=NULL,j=col,value=lapply(reports[[col]],fromJSON))
  
  setDT(reports)
  reports[,
          owner:=format_name_abbreviation(users_name)]
  reports[created_by_user_id==USER_ID(),
          owner:=fcase(is_public==TRUE,"PUBLIC",
                       is_public==FALSE,"PRIVATE",
                       default="ERROR")]
  
  reports[,
          launch:=paste0("<div style='display:inline-block;'> ",
                         "<div onmousedown='event.stopPropagation();' style='display:inline-block;'> ",
                         "<i class='fa fa-eye icon-view pointer' title='View' onclick='Shiny.setInputValue(\"server_dashboard_reports__action_view\",",
                         report_id,
                         ",{priority:\"event\"})'></i>",
                         "</div></div>")]
  
  reports[,
          edit:=paste0("<div style='display:inline-block;'> ",
                         "<div onmousedown='event.stopPropagation();' style='display:inline-block;'> ",
                         "<i class='fa fa-edit icon-edit pointer' title='View' onclick='Shiny.setInputValue(\"server_dashboard_reports__action_edit\",",
                         report_id,
                         ",{priority:\"event\"})'></i>",
                         "</div></div>")]
  
  setorder(reports,
           report_title,
           report_id)
  
  
  return (reports)

  
},ignoreNULL=FALSE,ignoreInit = FALSE)

SERVER_DASHBOARD_REPORTS_LIST_FILTERED <- eventReactive(c(SERVER_DASHBOARD_REPORTS_LIST(),
                                                           input$server_dashboard_reports__owner,
                                                           input$server_dashboard_reports__search), {
                                                             
  reports <- SERVER_DASHBOARD_REPORTS_LIST()
  if (empty(reports)) return (NULL)
  
  if (isTruthy(input$server_dashboard_reports__owner)) {
    reports <- reports[users_name %in% input$server_dashboard_reports__owner]
  }
  
  if (isTruthy(input$server_dashboard_reports__search)) {
    keywords <- trimws(unlist(strsplit(input$server_dashboard_reports__search,"[^[:alnum:]]")))
    
    matches <- lapply(keywords,FUN=function(kw) { 
      rowSums(as.data.frame(lapply(reports[,.(users_name,report_title,report_notes,for_client_sys_names,for_indicator_names)],
                                   FUN=grepl,
                                   pattern=kw,
                                   ignore.case=TRUE))) > 0
    })
    
    matches <- which(Reduce(f=`&`,
                            x=matches))
    
    reports <- reports[matches]
  }
  
  return (reports)
})

observeEvent(input$server_dashboard_reports__action_view, {
  selected_report_id <- suppressWarnings(as.numeric(input$server_dashboard_reports__action_view))
  if (!isTruthy(selected_report_id)) {
    SERVER_DASHBOARD_REPORT_SELECTED(list())
    
  } else {
  
    report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id,
                                              .(report_id,
                                                report_title,
                                                for_client_sys_names,
                                                for_indicator_names,
                                                for_asof_dates,
                                                report_parameters)]
    SERVER_DASHBOARD_REPORT_SELECTED(as.list(report))
  }
},ignoreNULL=FALSE,priority = 100)

SERVER_DASHBOARD_DO_LOAD <- function(for_client_sys_names=NA,
                                     for_indicator_names=NA,
                                     for_asof_dates=NA,
                                     dashboard_parameters) {
  
  if (!LOGGEDIN()) return (NULL)
  #SERVER_DASHBOARD_AUTORUN(FALSE)
  #runjs(paste0('Shiny.setInputValue("server_dashboard__autorun",false,{priority: "event"})'))
  if (!is.null(dashboard_parameters) &&
      !identical(dashboard_parameters,list()) &&
      is.null(names(dashboard_parameters))) stop("Dashboard parameters submitted but does not appear to be a valid list?")
  
  #if (isTruthy(SERVER_DASHBOARD_REPORT_VIEW_ID())) SERVER_DASHBOARD_REPORT_VIEW_ID()
  for_asof_dates <- as.character(for_asof_dates) #could be coming in as relative ranks, as actual dates, or as character dates
  
  #Clear-out artifacts that may have been saved via a report within the parameters but that are also come via dedicated fields.
  dashboard_parameters$rsf_pfcbl_ids <- NULL
  dashboard_parameters$indicator_names <- NULL
  dashboard_parameters$asof_dates <- NULL
  
  #RESET RUN OPTIONS
  SERVER_DASHBOARD_RUN_OPTIONS_RESET()
  
  
  for (param in names(dashboard_parameters)) SERVER_DASHBOARD_RUN_OPTIONS[[param]] <- dashboard_parameters[[param]]
  
  if (isTruthy(for_indicator_names)) SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- for_indicator_names

  if (isTruthy(for_client_sys_names)) {
    selected_clients <- as.character(for_client_sys_names)
    client_rsf_pfcbl_ids <- NULL
    if (any(selected_clients=="ALL")) {
      client_rsf_pfcbl_ids <- SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id
      
    } else if (any(as.character(SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id) %in% as.character(for_client_sys_names),na.rm = T)) {
      
      for_client_sys_names <- for_client_sys_names[as.character(for_client_sys_names) %in% as.character(SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id)]
      client_rsf_pfcbl_ids <- suppressWarnings(as.numeric(for_client_sys_names))
               
    } else {
      client_rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
      select 
        sn.rsf_pfcbl_id
      from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
      where sn.sys_name = any(select unnest(string_to_array($1::text,','))::text)",
      params=list(paste0(selected_clients,collapse=",")))
      client_rsf_pfcbl_ids <- client_rsf_pfcbl_ids$rsf_pfcbl_id
    } 
    
    SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- client_rsf_pfcbl_ids
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__reporting_client",
                         selected=client_rsf_pfcbl_ids)
  }
  
  if (isTruthy(for_asof_dates)) {
    asof_dates <- NULL
    
    #means for_client_sys_names was not NA
    if (!empty(SERVER_DASHBOARD_VALID_ASOF_DATES())) {
      for_asof_ranks <- na.omit(suppressWarnings(as.numeric(for_asof_dates)))
      asof_dates <- c(for_asof_ranks,
                      SERVER_DASHBOARD_VALID_ASOF_DATES()[text_date %in% for_asof_dates,date_rank])  #requested as specific dates
      asof_dates <- as.character(asof_dates)
    }
    
    if (any(tolower(for_asof_dates) %in% c("all","inf"),na.rm=T)) asof_dates <- c("Inf")
    
    if (is.null(asof_dates) && length(for_asof_dates) >0 && !anyNA(suppressWarnings(as.numeric(for_asof_dates)))) {
      asof_dates <- for_asof_dates
    }
    
    if (!is.null(asof_dates)) {
      asof_dates <- sort(unique(asof_dates))
      
      SERVER_DASHBOARD_RUN_OPTIONS$asof_dates <- unique(asof_dates)
    }
  }
  
  if (!setequal(input$server_dashboard__view_currency,SERVER_DASHBOARD_RUN_OPTIONS$fx_currency)) {
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__view_currency",
                         selected=SERVER_DASHBOARD_RUN_OPTIONS$fx_currency)
  }
  
  if (!setequal(input$server_dashboard__format_pivot,SERVER_DASHBOARD_RUN_OPTIONS$format_pivot)) {
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__format_pivot",
                         selected=SERVER_DASHBOARD_RUN_OPTIONS$format_pivot)
  }
  
  if (!setequal(input$server_dashboard__flags_filter,SERVER_DASHBOARD_RUN_OPTIONS$flags_filter)) {
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__flags_filter",
                         selected=SERVER_DASHBOARD_RUN_OPTIONS$flags_filter)
  }
  
  removeModal() #If one exists...

  if (!"dashboard" %in% input$sidebarMenu)  {
    updateTabItems(session=session,
                   inputId="sidebarMenu",
                   selected="dashboard")
  }
  
  updateTabsetPanel(session=session,
                    inputId="tabset_dashboard",
                    selected="dashboard")
  
  #runjs(paste0('Shiny.setInputValue("server_dashboard__autorun",true,{priority: "event"})'))
  if (length(na.omit(SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids))==0) {
    showNotification(type="message",
                     ui=h3("This report does not save clients: Select relevant client(s) from the drop-down list"))
  } else if (length(na.omit(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates))==0) {
    showNotification(type="message",
                     ui=h3("This report does not save timeline: Select relevant date(s) from the drop-down list"))
  
  } else {
    SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
  }
  
  
}

observeEvent(SERVER_DASHBOARD_REPORTS_LIST(), {
  
  users <- c("",sort(unique(SERVER_DASHBOARD_REPORTS_LIST()$users_name)))
  
  updateSelectizeInput(session=session,
                       inputId="server_dashboard_reports__owner",
                       choices=users,
                       selected="")
  
  updateTextInput(session=session,
                  inputId="server_dashboard_reports__search",
                  value="")
  
})

observeEvent(input$action_server_dashboard_reports__save_as, {
  
  
  has_name_filter <- isTruthy(input$server_dashboard__name_filter)
  has_flag_filter <- isTruthy(input$server_dashboard__flags_filter)
  program <- SELECTED_PROGRAM()
  
  if (empty(SERVER_DASHBOARD_SELECTED_INDICATORS())) {
    return (showNotification(type="error",
                             ui=h3("One or more indicators must be selected to save a report.")))
  }
  
  all_clients_selected <- setequal(SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id,
                                   SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  
  m <- modalDialog(id="dash_reports_save_modal",
                   div(style="background-color:white;padding:5px;height:250px;",
                             
                     fluidRow(
                       column(10,
                       textInput(inputId="server_dashboard_reports__title",
                                 label="Report Title",
                                 value="",
                                 placeholder="Enter title for report")),
                       column(2,
                              div(style="padding-top:23px;",
                              switchInput(inputId="server_dashboard_reports__public",
                                          label= "<i class=\"fa fa-user-secret\"></i>",
                                          onLabel = "PUBLIC",
                                          onStatus= "success",
                                          offLabel = "PRIVATE",
                                          offStatus = "danger",
                                          inline=TRUE,
                                          size="normal",
                                          value=FALSE)))
                       
                     ),
                     fluidRow(
                       column(12,
                        textAreaInput(inputId="server_dashboard_reports__notes",
                          label=NULL,
                          rows=2,
                          value="",
                          placeholder="Comments on report..."))
                     ),
                     fluidRow(
                       column(3,
                        selectizeInput(inputId="server_dashboard_reports__save_program",
                                       label="Save Program",
                                       choices=c(setNames(TRUE,
                                                          program$program_nickname),
                                                 `NO`=FALSE),
                                       width="100%",
                                       multiple=FALSE,
                                       selected=FALSE,
                                       options=list(placeholder="NO"))),
                       column(3,
                        enabled(state=all_clients_selected,
                        selectizeInput(inputId="server_dashboard_reports__save_clients",
                                       label="Save Clients",
                                       choices=c(`YES`=TRUE,
                                                 `NO`=FALSE),
                                       width="100%",
                                       multiple=FALSE,
                                       selected=all_clients_selected,
                                       options=list(placeholder="NO")))),
                       column(3,
                              disabled(
                                selectizeInput(inputId="server_dashboard_reports__save_names",
                                               label="Save Name Filter",
                                               choices=c("",
                                                         `YES`=TRUE,
                                                         `NO`=FALSE),
                                               width="100%",
                                               multiple=FALSE,
                                               selected=fcase(has_name_filter==TRUE,"FALSE",default=""),
                                               options=list(placeholder=fcase(has_name_filter==TRUE,"NO",default="Name filter N/A"))))),
                       column(3,
                              enabled(state=has_flag_filter,
                                selectizeInput(inputId="server_dashboard_reports__save_flags",
                                               label="Save Flag Filter",
                                               choices=c("",
                                                         `YES`=TRUE,
                                                         `NO`=FALSE),
                                               width="100%",
                                               multiple=FALSE,
                                               selected=fcase(has_flag_filter==TRUE,"FALSE",default=""),
                                               options=list(placeholder=fcase(has_flag_filter==TRUE,"NO",default="Flag filter N/A")))))
                       
                    )
                     
                   ),
                   title=div(style="display:inline-block;","Save Current Dashboard as Report"),
                   footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                              modalButton("Cancel"),
                              actionButton(inputId="server_dashboard_reports__action_save",
                                           label="Save",
                                           class="btn-success")
                              ),
                   size="m")
  
  showModal(m)
})

observeEvent(input$server_dashboard_reports__save_program, {
  save_program <- isTruthy(as.logical(input$server_dashboard_reports__save_program))
  
  all_clients_selected <- setequal(SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id,
                                   SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  if (save_program || all_clients_selected) {
    enable(id="server_dashboard_reports__save_clients")
  } else {
    
    updateSelectInput(session=session,
                      inputId="server_dashboard_reports__save_clients",
                      selected=FALSE)
    
    disable(id="server_dashboard_reports__save_clients")
  }
})

observeEvent(input$server_dashboard_reports__save_clients, {
  save_clients <- isTruthy(as.logical(input$server_dashboard_reports__save_clients))
  has_name_filter <- isTruthy(as.logical(input$server_dashboard__name_filter))
  
  for_client_names <- as.character(NA)
  if ((save_clients & has_name_filter)) {
    enable(id="server_dashboard_reports__save_names")
  } else {
  
      updateSelectInput(session=session,
                        inputId="server_dashboard_reports__save_names",
                        selected=FALSE)
      
      disable(id="server_dashboard_reports__save_names")
  }
})

observeEvent(input$server_dashboard_reports__action_save, {
  
  if (!LOGGEDIN()) return (NULL)
  
  dashboard_settings <- reactiveValuesToList(SERVER_DASHBOARD_RUN_OPTIONS)
  
  save_program <- isTruthy(as.logical(input$server_dashboard_reports__save_program))
  all_clients_selected <- setequal(SERVER_DASHBOARD_CLIENTS_LIST()$rsf_pfcbl_id,
                                   SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  
  for_program_name <- as.character(NA)
  if (save_program) {
    for_program_name <- DBPOOL %>% dbGetQuery("
      select sn.sys_name
      from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
      where sn.rsf_pfcbl_id = $1::int",
      params=list(SELECTED_PROGRAM()$rsf_pfcbl_id))
    for_program_name <- for_program_name$sys_name
    if (!isTruthy(for_program_name)) for_program_name <- as.character(NA)
  }

  save_clients <- isTruthy(as.logical(input$server_dashboard_reports__save_clients))
  for_client_names <- as.character(NA)
  if (save_clients || all_clients_selected) {
    
    clients_list <- SERVER_DASHBOARD_CLIENTS_LIST()
    if (all_clients_selected) {
      for_client_names <- "ALL"
    } else {
      for_client_names <- DBPOOL %>% dbGetQuery("
        select sn.sys_name
        from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
        where sn.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
        params=list(paste0(dashboard_settings$rsf_pfcbl_ids,collapse=",")))
      for_client_names <- for_client_names$sys_name
      if (!isTruthy(for_client_names)) for_client_names <- as.character(NA)
    }
  }

  for_indicator_names <- as.character(NA)
  
  selected_indicator_ids <- input$server_dashboard__reporting_column_priority
  if (isTruthy(selected_indicator_ids)) {
    for_indicator_names <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% selected_indicator_ids]
    for_indicator_names <- for_indicator_names[match(selected_indicator_ids,for_indicator_names$indicator_id),indicator_name]
    
    if (any(grepl("^:report:",for_indicator_names))) {
      index <- min(grep("^:report:",for_indicator_names))
      for_indicator_names <- append(for_indicator_names,
                                    SERVER_DASHBOARD_SELECTED_INDICATORS()$indicator_name,
                                    after=index)
      for_indicator_names <- for_indicator_names[-(grep("^:report:",for_indicator_names))]
    }
  }
  
  for_asof_dates <- as.character(NA)
  valid_dates<- SERVER_DASHBOARD_VALID_ASOF_DATES()
  
  if (all(valid_dates$date_rank %in% SERVER_DASHBOARD_RUN_OPTIONS$asof_dates) ||
      any(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates=="Inf")) {
    for_asof_dates <- c("Inf")
  
  } else {
    valid_dates <- SERVER_DASHBOARD_VALID_ASOF_DATES()
    run_dates <- na.omit(suppressWarnings(as.numeric(SERVER_DASHBOARD_RUN_OPTIONS$asof_dates)))
    
    #If the most recent rank is selected, assume selection via recentest
    if (any(run_dates==1) &&
        any(run_dates==max(valid_dates$date_rank)) &&
        nrow(valid_dates[date_rank > 0]) > 3) {
      for_asof_dates <- c(run_dates,
                          (run_dates-1-max(valid_dates$date_rank)))
    } else if (any(run_dates==1)) {
        for_asof_dates <- run_dates
    } else if (any(run_dates==max(valid_dates$date_rank))) {
      for_asof_dates <- c((run_dates-1-max(valid_dates$date_rank)))
    } else {
      for_asof_dates <- run_dates
    }
    
  }

  filter_names <- suppressWarnings(as.numeric(input$server_dashboard__name_filter))
  filter_flags <- input$server_dashboard__flags_filter
  
  if (!isTruthy(input$server_dashboard_reports__save_flags)) filter_flags <- as.character(NA)
  if (!isTruthy(filter_flags)) filter_flags <- as.character(NA)

  if (!isTruthy(input$server_dashboard_reports__save_names)) filter_names <- as.character(NA)
  if (!isTruthy(filter_names)) {
    filter_names <- as.character(NA)
  } else if (isTruthy(input$server_dashboard_reports__save_names)) {
    rsf_data <- SERVER_DASHBOARD_CURRENT_QUERY()
    filter_names <- rsf_data[SYSID %in% filter_names,SYSNAME]
  } 
  
  dashboard_settings$filter_names <- filter_names
  dashboard_settings$filter_flags <- filter_flags
  
  report_title <- trimws(input$server_dashboard_reports__title)
  report_notes <- input$server_dashboard_reports__notes
  report_is_public <- isTruthy(input$server_dashboard_reports__public)
  
  user_id <- USER_ID()

  version <- ""
  if (grepl("V\\.\\d+$",report_title)) {
    version <- gsub("^.*(V\\.\\d+)$","\\1",report_title)
    report_title <- trimws(gsub(paste0(version,"$"),"",report_title))
  }

  versions <- DBPOOL %>% dbGetQuery("
    select count(*) as counts
    from p_rsf.reports re
    where re.report_title like ($1::text || '%')",
  params=list(report_title))
  
  if (!empty(versions)) {
    
    report_title <- paste0(report_title," V.",(as.numeric(versions$counts)+1))
  }
  
  #permissions not checked... 
  
  report_id <- withProgress(message="Saving report...", {
    DBPOOL %>% dbGetQuery("
    insert into p_rsf.reports(created_by_user_id,
                              is_public,
                              report_title,
                              report_notes,
                              for_program_sys_name,
                              for_client_sys_names,
                              for_indicator_names,
                              for_asof_dates,
                              report_parameters)
    select
      $1::text as created_by_user_id,
      $2::bool as is_public,
      $3::text as report_title,
      $4::text as report_notes,
      NULLIF($5::text,'NA') as for_program_sys_name,
      NULLIF($6::text,'[null]')::jsonb as for_client_sys_names,
      NULLIF($7::text,'[null]')::jsonb as for_indicator_names,
      NULLIF($8::text,'[null]')::jsonb as for_asof_dates,
      NULLIF($9::text,'[null]')::jsonb as report_parameters
    returning report_id",
    params=list(user_id,
                report_is_public,
                report_title,
                report_notes,
                for_program_name,
                toJSON(for_client_names),
                toJSON(for_indicator_names),
                toJSON(for_asof_dates),
                toJSON(dashboard_settings)))
  })
  
  if (!isTruthy(unlist(report_id))) {
    showNotification(type="error",
                     ui=h3("An error has occurred and the report failed to save"))
  } else {
    removeModal()
    SERVER_DASHBOARD_REPORTS_LIST_REFRESH(SERVER_DASHBOARD_REPORTS_LIST_REFRESH()+1)
  }
  
},ignoreInit = TRUE)

observeEvent(input$server_dashboard_reports__action_edits_save, {
  
  if (!LOGGEDIN()) return (NULL)
  
  selected_report_id <- as.numeric(input$server_dashboard_reports__action_edit)
  if (!isTruthy(selected_report_id) ||
      !selected_report_id %in% SERVER_DASHBOARD_REPORTS_LIST()$report_id) return (NULL)

  update_title <- trimws(input$server_dashboard_reports__edit_title)
  update_notes <- input$server_dashboard_reports__edit_notes
  update_is_public <- isTruthy(input$server_dashboard_reports__edit_public)

  version <- ""
  if (grepl("V\\.\\d+$",update_title)) {
    version <- gsub("^.*(V\\.\\d+)$","\\1",update_title)
    update_title <- trimws(gsub(paste0(version,"$"),"",update_title))
  }
  
  report_id <- withProgress(message="Saving report...", {
    
    versions <- DBPOOL %>% dbGetQuery("
      select re.report_id
      from p_rsf.reports re
      where re.report_title like ($1::text || '%')",
      params=list(update_title))
    
    if (!empty(versions) & !all(versions$report_id==selected_report_id)) {
      
      update_title <- paste0(update_title," V.",which(versions$report_id==selected_report_id))
      
    }
        
    DBPOOL %>% dbGetQuery("
    update p_rsf.reports re
    set is_public = $1::bool,
        report_title = $2::text,
        report_notes = $3::text
    where re.report_id = $4::int
    returning report_id",
    params=list(update_is_public,
                update_title,
                update_notes,
                selected_report_id))
  })
  
  if (!isTruthy(unlist(report_id))) {
    showNotification(type="error",
                     ui=h3("An error has occurred and the report failed to save"))
  } else {
    removeModal()
    SERVER_DASHBOARD_REPORTS_LIST_REFRESH(SERVER_DASHBOARD_REPORTS_LIST_REFRESH()+1)
  }
  
},ignoreInit = TRUE)

observeEvent(input$server_dashboard_reports__action_edit, {
  
  if (!LOGGEDIN()) return (NULL)
  
  selected_report_id <- as.numeric(input$server_dashboard_reports__action_edit)
  if (!isTruthy(selected_report_id) ||
      !selected_report_id %in% SERVER_DASHBOARD_REPORTS_LIST()$report_id) return (NULL)
  
  report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id]
  


  report_title <- report$report_title
  report_notes <- report$report_notes
  report_is_public <- report$is_public
  
  delete_UI <- NULL
  if (identical(report$created_by_user_id,USER_ID())) {
    delete_UI <- fluidRow(column(12,align="middle",
                                 actionButton(inputId="server_dashboard_reports__edit_delete_report",
                                              label="Permanently Delete Report",
                                              class="btn-danger",
                                              icon=icon("trash-can"))))
  }
  
  m <- modalDialog(id="dash_reports_save_modal",
                   div(style="background-color:white;padding:5px;height:250px;",

                       fluidRow(
                         column(10,
                                textInput(inputId="server_dashboard_reports__edit_title",
                                          label="Report Title",
                                          value=report_title,
                                          placeholder="Enter title for report")),
                         column(2,
                                div(style="padding-top:23px;",
                                    switchInput(inputId="server_dashboard_reports__edit_public",
                                                label= "<i class=\"fa fa-user-secret\"></i>",
                                                onLabel = "PUBLIC",
                                                onStatus= "success",
                                                offLabel = "PRIVATE",
                                                offStatus = "danger",
                                                inline=TRUE,
                                                size="normal",
                                                value=report_is_public)))
                         
                       ),
                       fluidRow(
                         column(12,
                                textAreaInput(inputId="server_dashboard_reports__edit_notes",
                                              label=NULL,
                                              rows=2,
                                              value=report_notes,
                                              placeholder="Comments on report..."))
                       ),
                       delete_UI,
                       
                   ),
                   title=div(style="display:inline-block;","Edit Report"),
                   footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                              modalButton("Cancel"),
                              actionButton(inputId="server_dashboard_reports__action_edits_save",
                                           label="Save & Close",
                                           class="btn-success")
                   ),
                   size="m")
  
  showModal(m)
  
},ignoreInit = TRUE)

observeEvent(input$server_dashboard_reports__edit_delete_report, {
  
  if (!LOGGEDIN()) return (NULL)
  
  selected_report_id <- as.numeric(input$server_dashboard_reports__action_edit)
  if (!isTruthy(selected_report_id) ||
      !selected_report_id %in% SERVER_DASHBOARD_REPORTS_LIST()$report_id) return (NULL)
  
  report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id]
  if (!identical(report$created_by_user_id,USER_ID())) {
    showNotification(type="error",
                     ui=h3("Reports can only be deleted by the users that created them"))
  } else {
    d <- DBPOOL %>% dbExecute("
      delete from p_rsf.reports re
      where re.report_id = $1::int
        and re.created_by_user_id = $2::text",
      params=list(selected_report_id,
                  USER_ID()))
    
    SERVER_DASHBOARD_REPORTS_LIST_REFRESH(SERVER_DASHBOARD_REPORTS_LIST_REFRESH()+1)
    removeModal()
  }
},ignoreInit = TRUE)

observeEvent(SERVER_DASHBOARD_REPORT_SELECTED(), {
  

  report <- SERVER_DASHBOARD_REPORT_SELECTED()
  dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()
  if (length(report)==0) return (NULL)
  
  for_indicator_names <- unlist(report$for_indicator_names)
  if (!empty(dashboard_indicators[indicator_id==0])) {
    for_indicator_names <- dashboard_indicators[indicator_id==0,indicator_name]
  }
  
  for_asof_dates <- NULL
  if (!anyNA(ymd(unlist(report$for_asof_dates),quiet=T))) { 
    for_asof_dates <- unique(unlist(report$for_asof_dates))
  } else if (!anyNA(suppressWarnings(as.numeric(unlist(report$for_asof_dates))))) {
    for_asof_dates <- unique(suppressWarnings(as.numeric(unlist(report$for_asof_dates))))
  } else { for_asof_dates <- 1 }
  
  #report_parameters <- report$report_parameters[[1]]
  
  # report_parameters$rsf_pfcbl_ids <- NULL
  # report_parameters$indicator_names <- NULL
  # report_parameters$asof_dates <- NULL
  
  SERVER_DASHBOARD_DO_LOAD(for_client_sys_names=unlist(report$for_client_sys_names),
                           for_indicator_names=for_indicator_names,
                           for_asof_dates=for_asof_dates,
                           dashboard_parameters=report$report_parameters[[1]])
  

},
priority = 10) #need this observer to fire before observeEvent(SERVER_DASHBOARD_INDICATORS()

# observeEvent(input$server_dashboard_reports__action_view, {
#   
#   report <- SERVER_DASHBOARD_REPORT_SELECTED()
#   dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()
#   if (empty(report)) return (NULL)
#   
#   for_indicator_names <- unlist(report$for_indicator_names)
#   if (!empty(dashboard_indicators[indicator_id==0])) {
#     for_indicator_names <- dashboard_indicators[indicator_id==0,indicator_name]
#   }
#   
#   for_asof_dates <- unique(suppressWarnings(as.numeric(unlist(report$for_asof_dates))))
#   if (!isTruthy(for_asof_dates)) for_asof_dates <- 1
#   
#   report_parameters <- report$report_parameters[[1]]
#   
#   report_parameters$rsf_pfcbl_ids <- NULL
#   report_parameters$indicator_names <- NULL
#   report_parameters$asof_dates <- NULL
#   
#   SERVER_DASHBOARD_DO_LOAD(for_client_sys_names=unlist(report$for_client_sys_names),
#                            for_indicator_names=for_indicator_names,
#                            for_asof_dates=unlist(report$for_asof_dates),
#                            dashboard_parameters=report_parameters)
# },
# priority = 10) #need this observer to fire before observeEvent(SERVER_DASHBOARD_INDICATORS()

observeEvent(input$tabset_dashboard, {
  
  if (input$tabset_dashboard %in% "dashboard") {
    
    updateSwitchInput(session=session,
                      inputId="server_dashboard__autorun",
                      value=TRUE)
  }
})

output$server_dashboard_reports__list <- DT::renderDataTable({
  
  req(SERVER_DASHBOARD_REPORTS_LIST())
  
  reports <- SERVER_DASHBOARD_REPORTS_LIST_FILTERED()
  reports <- reports[,
                     .(Launch=launch,
                       Title=report_title,
                       Owner=owner,
                       Edit=edit)]
  
  DT::datatable(reports,
                rownames = FALSE,
                fillContainer=TRUE,
                escape = FALSE, #Shouldn't be any HTML escapable text
                options=list(dom="tir",
                             scrollY="70vh",
                             paging=TRUE,
                             ordering=F,
                             orderable=F,
                             pageLength=100,
                            columnDefs = list(list(className = 'dt-left', targets = c(1,2)),  #Zero-based targets
                                              list(className = 'dt-center', targets = c(0,3)))))
              
})


