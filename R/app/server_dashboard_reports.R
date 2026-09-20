SERVER_DASHBOARD_REPORTS_LIST_REFRESH <- reactiveVal(0)
SERVER_DASHBOARD_REPORT_SELECTED <- reactiveVal(list())

SERVER_DASHBOARD_REPORTS_LIST <- eventReactive(SERVER_DASHBOARD_REPORTS_LIST_REFRESH(), {

  if (!LOGGEDIN()) return (NULL) 
  
  reports <- DBPOOL %>% dbGetQuery("
    with id_filters as (
      select 
        report_id,
        array_to_string(array_agg(distinct nids.rsf_pfcbl_id),',') as report_pfcbl_id_filters
      from p_rsf.dashboard_reports dr
      CROSS JOIN LATERAL (select dr.for_program_sys_name
                          union all
                          select jsonb_array_elements_text(dr.for_facility_sys_names)) AS report(sys_name)
      -- includes timeseries data and name changes over time that may be historically referenced in reports
      inner join p_rsf.rsf_data_current_names_and_ids nids on nids.sys_name = report.sys_name                     
      where dr.for_program_sys_name is not null 
         or NULLIF(dr.for_facility_sys_names,'[\"ALL\"]'::jsonb) is not null
      group by report_id
    )
    select 
      re.report_id,
      re.created_by_user_id,
      re.created_timestamp,
      coalesce(vai.users_name,'UNKNOWN') as users_name,
      id_filters.report_pfcbl_id_filters,
      re.is_public,
      re.report_title,
      re.report_notes,
      coalesce(re.for_facility_sys_names,'[null]'::jsonb) as for_facility_sys_names,
      coalesce(re.for_indicator_names,'[null]'::jsonb) as for_indicator_names,
      coalesce(re.for_asof_dates,'[null]'::jsonb) as for_asof_dates,
      coalesce(re.report_parameters,'[null]'::jsonb) as report_parameters
    from p_rsf.dashboard_reports re 
    left join p_rsf.view_account_info vai on vai.account_id = re.created_by_user_id
    left join id_filters on id_filters.report_id = re.report_id")

  json_cols <- names(reports)[sapply(reports,class)=="pq_jsonb"]
  for (col in json_cols) set(reports,i=NULL,j=col,value=lapply(reports[[col]],fromJSON))
  
  reports[["report_pfcbl_id_filters"]] <- strsplit(reports[["report_pfcbl_id_filters"]],
                                                   split=",",
                                                   fixed=T)
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

  
},ignoreNULL=TRUE,ignoreInit = TRUE)

SERVER_DASHBOARD_REPORTS_LIST_FILTERED <- eventReactive(list(SERVER_DASHBOARD_REPORTS_LIST(),
                                                             SERVER_DASHBOARD_FACILITIES_LIST(),
                                                             input$server_dashboard_reports__owner,
                                                             input$server_dashboard_reports__search), {
                                                             
  reports <- SERVER_DASHBOARD_REPORTS_LIST()
  if (empty(reports)) return (NULL)
  filter_programs <- sapply(reports$report_pfcbl_id_filters,
                             function(ids,available_ids) {
                               ids <- na.omit(unlist(ids))
    !length(ids) | any(ids %in% available_ids,na.rm=T)
                               
  },available_ids=c(SELECTED_PROGRAM_ID(),SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id))
  
  reports <- reports[(filter_programs==TRUE)]
  
  if (isTruthy(input$server_dashboard_reports__owner)) {
    reports <- reports[users_name %in% input$server_dashboard_reports__owner]
  }
  
  if (isTruthy(input$server_dashboard_reports__search)) {
    keywords <- trimws(unlist(strsplit(input$server_dashboard_reports__search,"[^[:alnum:]]")))
    
    matches <- lapply(keywords,FUN=function(kw) { 
      rowSums(as.data.frame(lapply(reports[,.(users_name,report_title,report_notes,for_indicator_names)],
                                   FUN=grepl,
                                   pattern=kw,
                                   ignore.case=TRUE))) > 0
    })
    
    matches <- which(Reduce(f=`&`,
                            x=matches))
    
    reports <- reports[matches]
  }
  
  if (length(SERVER_DASHBOARD_REPORT_SELECTED())>0) {
    if (!any(SERVER_DASHBOARD_REPORT_SELECTED()$report_id %in% reports$report_id)) { 
      SERVER_DASHBOARD_REPORT_SELECTED(list())
    }
  }  
  return (reports)
})

SERVER_DASHBOARD_DO_LOAD <- function(for_facility_sys_names=NA,
                                     for_indicator_names=NA,
                                     for_asof_dates=NA,
                                     dashboard_parameters,
                                     do_run=TRUE) {
  
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

  if (isTruthy(for_facility_sys_names)) {

    #Could pass-in rsf_pfcbl_id as well as the SYSNAME
    for_facility_sys_names <- as.character(for_facility_sys_names)
    facility_rsf_pfcbl_ids <- NULL
    
    
    

  
    if (any(for_facility_sys_names=="ALL",na.rm=T)) {
      facility_rsf_pfcbl_ids <- SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id
      
    } else if (any(as.character(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id) %in% 
                   as.character(for_facility_sys_names),na.rm = T)) {
      
      for_facility_sys_names <- for_facility_sys_names[as.character(for_facility_sys_names) %in% as.character(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id)]
      facility_rsf_pfcbl_ids <- suppressWarnings(as.numeric(for_facility_sys_names))
               
    } else if (!empty(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST())) {
      
      # entities <- rbindlist(lapply(grep("^\\d+$",for_facility_sys_names,invert=T,value=T),
      # function(x) {
      #  
      #   y <- unlist(strsplit(for_facility_sys_names,">",fixed=T))
      #   data.table(program_pfcbl_name=trimws(y[1]),facility_pfcbl_name=trimws(y[2]),sys_name=x)
      # }))
      
      facility_rsf_pfcbl_ids <- unique(
        c(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()[
          sys_name %in% for_facility_sys_names,
          rsf_pfcbl_id],
          SERVER_DASHBOARD_FACILITIES_LIST()[
          sys_name %in% for_facility_sys_names,
          rsf_pfcbl_id]))
      
    } 
    
    SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids <- facility_rsf_pfcbl_ids
    updateSelectizeInput(session=session,
                         inputId="server_dashboard__reporting_facility",
                         selected=facility_rsf_pfcbl_ids)
  }
  
  if (isTruthy(for_asof_dates)) {
    asof_dates <- NULL
    
    #means for_facility_sys_names was not NA
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

  if (isTRUE(do_run)) {
    SERVER_DASHBOARD_DO_AUTORUN(TRUE) #DO_AUTORUN observer will call DASHBOARD_REFRESH
  } else {
    #Prompt refresh will clear previous results if auto-run is FALSE, but leave user to run RUN
    SERVER_DASHBOARD_DO_AUTORUN(FALSE)
    SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1) #If auto run is already false, setting above will not clear cashed reports
    
  }
  
}

observeEvent(input$server_dashboard_reports__action_view, {
  
  selected_report_id <- suppressWarnings(as.numeric(input$server_dashboard_reports__action_view))
  
  if (!isTruthy(selected_report_id)) {
    SERVER_DASHBOARD_REPORT_SELECTED(list())
    
  } else {
    
    report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id,
                                              .(report_id,
                                                report_title,
                                                report_pfcbl_id_filters,
                                                for_facility_sys_names,
                                                for_indicator_names,
                                                for_asof_dates,
                                                report_parameters)]
    
    SERVER_DASHBOARD_REPORT_SELECTED(as.list(report))
  }
},ignoreNULL=TRUE,ignoreInit=TRUE,priority = 100)

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
  
  all_facilities_selected <- setequal(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id,
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
                        enabled(state=all_facilities_selected,
                        selectizeInput(inputId="server_dashboard_reports__save_facilities",
                                       label="Save Facilities",
                                       choices=c(`YES`=TRUE,
                                                 `NO`=FALSE),
                                       width="100%",
                                       multiple=FALSE,
                                       selected=all_facilities_selected,
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
  
  all_facilities_selected <- setequal(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id,
                                   SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  if (save_program || all_facilities_selected) {
    enable(id="server_dashboard_reports__save_facilities")
  } else {
    
    updateSelectInput(session=session,
                      inputId="server_dashboard_reports__save_facilities",
                      selected=FALSE)
    
    disable(id="server_dashboard_reports__save_facilities")
  }
})

observeEvent(input$server_dashboard_reports__save_facilities, {
  save_facilities <- isTruthy(as.logical(input$server_dashboard_reports__save_facilities))
  has_name_filter <- isTruthy(as.logical(input$server_dashboard__name_filter))
  
  for_facility_names <- as.character(NA)
  if ((save_facilities & has_name_filter)) {
    enable(id="server_dashboard_reports__save_names")
  } else {
  
      updateSelectInput(session=session,
                        inputId="server_dashboard_reports__save_names",
                        selected=FALSE)
      
      disable(id="server_dashboard_reports__save_names")
  }
})

SERVER_DASHBOARD_REPORTS_function_CURRENT_PARAMETERS <- function() {
  
  dashboard_settings <- reactiveValuesToList(SERVER_DASHBOARD_RUN_OPTIONS)
  
  for_program_name <- SELECTED_PROGRAM()$sys_name
  if (!isTruthy(for_program_name)) for_program_name <- as.character(NA)
  
  all_facilities_selected <- setequal(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id,
                                      SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  for_facility_names <- as.character(NA)

  if (all_facilities_selected) {
    for_facility_names <- "ALL"
  } else {
    for_facility_names <- SERVER_DASHBOARD_FACILITIES_LIST()[
      rsf_pfcbl_id %in% SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids,sys_name]
    if (!isTruthy(for_facility_names)) for_facility_names <- as.character(NA)
  }

  for_indicator_names <- as.character(NA)
  for_report_id <- as.numeric(NA)
  for_title <- "RSF Jason Data Extract"
  selected_indicator_ids <- input$server_dashboard__reporting_column_priority
  if (isTruthy(selected_indicator_ids)) {
    for_indicator_names <- SERVER_DASHBOARD_INDICATORS()[indicator_id %in% selected_indicator_ids]
    for_indicator_names <- for_indicator_names[match(selected_indicator_ids,for_indicator_names$indicator_id),indicator_name]
    
    if (any(grepl("^:report:",for_indicator_names))) {
      
      if (any(paste0(":report:",SERVER_DASHBOARD_REPORT_SELECTED()$report_title) %in% for_indicator_names,na.rm=T)) {
        for_report_id <- SERVER_DASHBOARD_REPORT_SELECTED()$report_id
      }
      
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
  printed_dates <- ymd(valid_dates[as.numeric(for_asof_dates)]$date_value)
  
  for_title <- paste0(for_title," - ",
                      ifelse(length(na.omit(printed_dates))>1,
                             "TIMESERIES",
                             format_asof_date_label(printed_dates[[1]],F)))

  filter_names <- input$server_dashboard__name_filter
  filter_flags <- input$server_dashboard__flags_filter
  
  if (!isTruthy(input$server_dashboard_reports__save_flags)) filter_flags <- as.character(NA)
  if (!isTruthy(filter_flags)) filter_flags <- as.character(NA)
  
  if (!isTruthy(input$server_dashboard_reports__save_names)) filter_names <- as.character(NA)
  if (!isTruthy(filter_names)) {
    filter_names <- as.character(NA)
  } else if (isTruthy(input$server_dashboard_reports__save_names)) {
    rsf_data <- SERVER_DASHBOARD_CURRENT_QUERY()
    
    good_name <- sapply(filter_names,function(fn,sys_names) {
      any(grepl(fn,sys_names))
    },sys_names=unique(rsf_data$SYSNAME))
    
    filter_names <- filter_names[good_name]
    if (!length(filter_names)) filter_names <- as.character(NA)
  } 
  
  dashboard_settings$filter_names <- filter_names
  dashboard_settings$filter_flags <- filter_flags
  
  report_title <- trimws(input$server_dashboard_reports__title)
  report_notes <- input$server_dashboard_reports__notes

  return(list(user_id=USER_ID(),
              dashboard_settings=dashboard_settings,
              for_title=for_title,
              for_program_name=for_program_name,
              for_facility_names=for_facility_names,
              for_asof_dates=for_asof_dates,
              for_indicator_names=for_indicator_names,
              for_report_id=for_report_id))
  
}

observeEvent(input$server_dashboard_reports__action_save, {
  
  if (!LOGGEDIN()) return (NULL)
  parameters <- SERVER_DASHBOARD_REPORTS_function_CURRENT_PARAMETERS()
  
  save_program <- isTruthy(as.logical(input$server_dashboard_reports__save_program))
  save_facilities <- isTruthy(as.logical(input$server_dashboard_reports__save_facilities))
  
  all_facilities_selected <- setequal(SERVER_DASHBOARD_FACILITIES_LIST()$rsf_pfcbl_id,
                                      SERVER_DASHBOARD_RUN_OPTIONS$rsf_pfcbl_ids)
  
  for_program_name <- as.character(NA)
  if (!save_program) {
    parameters$for_program_name <- as.character(NA)
  }

  if (!save_facilities) {
    parameters$for_facility_names <- as.character(NA)
  }
  
  report_title <- trimws(input$server_dashboard_reports__title)
  report_notes <- input$server_dashboard_reports__notes
  report_is_public <- isTruthy(input$server_dashboard_reports__public)
  
  
  #permissions not checked... 
  
  report_id <- withProgress(message="Saving report...", {
    DBPOOL %>% dbGetQuery("
    insert into p_rsf.dashboard_reports(created_by_user_id,
                              is_public,
                              report_title,
                              report_notes,
                              for_program_sys_name,
                              for_facility_sys_names,
                              for_indicator_names,
                              for_asof_dates,
                              report_parameters)
    select
      $1::text as created_by_user_id,
      $2::bool as is_public,
      $3::text as report_title,
      $4::text as report_notes,
      NULLIF($5::text,'NA') as for_program_sys_name,
      NULLIF($6::text,'[null]')::jsonb as for_facility_sys_names,
      NULLIF($7::text,'[null]')::jsonb as for_indicator_names,
      NULLIF($8::text,'[null]')::jsonb as for_asof_dates,
      NULLIF($9::text,'[null]')::jsonb as report_parameters
    returning report_id",
    params=list(user_id,
                report_is_public,
                report_title,
                report_notes,
                parameters$for_program_name,
                toJSON(parameters$for_facility_names),
                toJSON(parameters$for_indicator_names),
                toJSON(parameters$for_asof_dates),
                toJSON(parameters$dashboard_settings)))
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
      from p_rsf.dashboard_reports re
      where re.report_title like ($1::text || '%')",
      params=list(update_title))
    
    if (!empty(versions) & !all(versions$report_id==selected_report_id)) {
      
      update_title <- paste0(update_title," V.",which(versions$report_id==selected_report_id))
      
    }
        
    DBPOOL %>% dbGetQuery("
    update p_rsf.dashboard_reports re
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
      delete from p_rsf.dashboard_reports re
      where re.report_id = $1::int
        and re.created_by_user_id = $2::text",
      params=list(selected_report_id,
                  USER_ID()))
    
    SERVER_DASHBOARD_REPORTS_LIST_REFRESH(SERVER_DASHBOARD_REPORTS_LIST_REFRESH()+1)
    removeModal()
  }
},ignoreInit = TRUE)


observeEvent(SELECTED_PROGRAM_ID(), {
  SERVER_DASHBOARD_REPORTS_LIST_REFRESH(SERVER_DASHBOARD_REPORTS_LIST_REFRESH()+1)
},ignoreNULL=TRUE)

observeEvent(SERVER_DASHBOARD_REPORT_SELECTED(), {
 
  report <- SERVER_DASHBOARD_REPORT_SELECTED()
  dashboard_indicators <- SERVER_DASHBOARD_INDICATORS()
  if (length(report)==0) return (NULL)

  auto_run <- isTRUE(report$do_run)
  for_indicator_names <- unlist(report$for_indicator_names)
  if (!empty(dashboard_indicators) && 
      !empty(dashboard_indicators[indicator_id==0])) {
    for_indicator_names <- dashboard_indicators[indicator_id==0,indicator_name]
  }
  
  for_asof_dates <- NULL
  if (!anyNA(ymd(unlist(report$for_asof_dates),quiet=T))) { 
    for_asof_dates <- unique(unlist(report$for_asof_dates))
  } else if (!anyNA(suppressWarnings(as.numeric(unlist(report$for_asof_dates))))) {
    for_asof_dates <- unique(suppressWarnings(as.numeric(unlist(report$for_asof_dates))))
  } else { for_asof_dates <- 1 }

  SERVER_DASHBOARD_DO_LOAD(for_facility_sys_names=unlist(report$for_facility_sys_names),
                           for_indicator_names=for_indicator_names,
                           for_asof_dates=for_asof_dates,
                           dashboard_parameters=report$report_parameters[[1]],
                           do_run=auto_run)
  

},
priority = 10) #need this observer to fire before observeEvent(SERVER_DASHBOARD_INDICATORS()


# observeEvent(input$tabset_dashboard, {
#   
#   if (input$tabset_dashboard %in% "dashboard") {
#     
#     updateSwitchInput(session=session,
#                       inputId="server_dashboard__autorun",
#                       value=TRUE)
#   }
# })

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


