

SERVER_DASHBOARD_REPORTS_LIST <- eventReactive(SELECTED_PROGRAM_ID(), {

  program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(program_id)) return (NULL) 
  
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
  inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.sys_name = re.for_program_sys_name
                                                         and sn.pfcbl_category in ('global','program')
  left join p_rsf.view_account_info vai on vai.account_id = re.created_by_user_id
  where sn.rsf_program_id = $1::int
  
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

  
},ignoreNULL=FALSE)

SERVER_DASHBOARD_REPORT_SELECTED <- eventReactive(input$server_dashboard_reports__action_view, {
  selected_report_id <- as.numeric(input$server_dashboard_reports__action_view)
  if (!isTruthy(selected_report_id)) return (NULL)
  
  report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id]
  return (report)
},ignoreNULL=FALSE)

observeEvent(input$action_server_dashboard_reports__save_as, {
  
  
  has_name_filter <- isTruthy(input$server_dashboard__name_filter)
  has_flag_filter <- isTruthy(input$server_dashboard__flags_filter)
  program <- SELECTED_PROGRAM()
  
  if (empty(program)) {
    return (NULL)
  }
  
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
                        disabled(
                        selectizeInput(inputId="server_dashboard_reports__save_clients",
                                       label="Save Clients",
                                       choices=c(`YES`=TRUE,
                                                 `NO`=FALSE),
                                       width="100%",
                                       multiple=FALSE,
                                       selected=FALSE,
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
  
  if (save_program) {
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
  
  if (save_clients & has_name_filter) {
    enable(id="server_dashboard_reports__save_names")
  } else {
    
    updateSelectInput(session=session,
                      inputId="server_dashboard_reports__save_names",
                      selected=FALSE)
    
    disable(id="server_dashboard_reports__save_names")
  }
})

observeEvent(input$server_dashboard_reports__action_save, {
  
  if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
  
  dashboard_settings <- reactiveValuesToList(SERVER_DASHBOARD_RUN_OPTIONS)
  
  save_program <- isTruthy(as.logical(input$server_dashboard_reports__save_program))
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
  if (save_clients) {
    
    clients_list <- SELECTED_PROGRAM_CLIENTS_LIST()
    if (all(clients_list$rsf_pfcbl_id %in% dashboard_settings$rsf_pfcbl_ids)) {
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
    for_indicator_names <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% selected_indicator_ids,indicator_name]
  }
  
  for_asof_dates <- as.character(NA)
  valid_dates<- SERVER_DASHBOARD_VALID_ASOF_DATES()
  
  if (all(valid_dates[,is_future==FALSE,text_date] %in% SERVER_DASHBOARD_RUN_OPTIONS$asof_dates)) {
    for_asof_dates <- c("ALL")
  #If the "currentest" reporting date is selected in the dashboard, then save the report as a sequence of relative dates
  #so the report can auto update as latest date, previous, etc
  } else if (any(valid_dates[date_rank==1,text_date] %in% SERVER_DASHBOARD_RUN_OPTIONS$asof_dates)) {
    for_asof_dates <- valid_dates[text_date %in% SERVER_DASHBOARD_RUN_OPTIONS$asof_dates,
                              as.character(date_rank)]  
  }
  
  if (any("future" %in% SERVER_DASHBOARD_RUN_OPTIONS$asof_dates,na.rm=T)) {
    for_asof_dates <- c(for_asof_dates,"FUTURE")
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
  
  report_title <- input$server_dashboard_reports__title
  report_notes <- input$server_dashboard_reports__notes
  report_is_public <- isTruthy(input$server_dashboard_reports__public)
  
  user_id <- USER_ID()
  
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
  }
  
},ignoreInit = TRUE)

observeEvent(SERVER_DASHBOARD_REPORT_SELECTED(), {
  
  report <- SERVER_DASHBOARD_REPORT_SELECTED()
  dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()
  if (empty(report)) return (NULL)
  
  SERVER_DASHBOARD_RUN_AUTORUN(FALSE)
  
  parameters <- report$report_parameters[[1]]
  for (param in names(parameters)) SERVER_DASHBOARD_RUN_OPTIONS[[param]] <- parameters[[param]]
  
  report_indicator <- dashboard_indicators[indicator_id==0,indicator_name]
  if (isTruthy(report_indicator)) {
    SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- report_indicator
  } else if (isTruthy(report$for_indicator_names[[1]])) {
    SERVER_DASHBOARD_RUN_OPTIONS$indicator_names <- report$for_indicator_names
  }
  
  if (isTruthy(report$for_client_sys_names[[1]])) {
    selected_clients <- report$for_client_sys_names[[1]]
    client_rsf_pfcbl_ids <- NULL
    if (any(selected_clients=="ALL")) {
      client_rsf_pfcbl_ids <- SELECTED_PROGRAM_CLIENTS_LIST()$rsf_pfcbl_id
      
    } else {
      client_rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
      select 
        sn.rsf_pfcbl_id
      from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
      where sn.sys_name = any(select unnest(string_to_array($1::text,','))::text)",
      params=list(paste0(selected_clients,collapse=",")))
      client_rsf_pfcbl_ids <- client_rsf_pfcbl_ids$rsf_pfcbl_id
    } 
    
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__reporting_client",
                         selected=client_rsf_pfcbl_ids)
  }
  
  if (isTruthy(parameters$fx_currency)) {
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__view_currency",
                         selected=parameters[["fx_currency"]])
  }

  updateTabsetPanel(session=session,
                    inputId="tabset_dashboard",
                    selected="dashboard")
  
  # server_dashboard__reporting_asof_date
  # server_dashboard__flags_filter
  # server_dashboard__name_filter
  # 
  # server_dashboard__view_display
},
priority = 10) #need this observer to fire before observeEvent(SERVER_DASHBOARD_INDICATORS()

observeEvent(input$tabset_dashboard, {
  
  if (input$tabset_dashboard %in% "dashboard") {
    SERVER_DASHBOARD_RUN_AUTORUN(TRUE)
    updateSwitchInput(session=session,
                      inputId="server_dashboard__autorun",
                      value=TRUE)
  }
})
output$server_dashboard_reports__list <- DT::renderDataTable({
  
  req(SERVER_DASHBOARD_REPORTS_LIST())
  
  reports <- SERVER_DASHBOARD_REPORTS_LIST()
  reports <- reports[,
                     .(Edit=edit,
                       Title=report_title,
                       Owner=owner,
                       Launch=launch)]
  
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