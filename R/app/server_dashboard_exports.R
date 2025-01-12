
SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE <- reactiveVal(NA)
SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN <- reactiveVal(list())
SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH <- reactiveVal(0)
SERVER_DASHBOARD_EXPORTS_TEMPLATES <- eventReactive(c(SERVER_DASHBOARD_REPORTS_LIST(),
                                                      SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH()), {
  
  if (!LOGGEDIN()) return (NULL)
                                                        
  reports <- SERVER_DASHBOARD_REPORTS_LIST()
  report_ids <- paste0(reports$report_id,collapse=",")
  if (empty(reports)) report_ids <- NA
  
  templates <- DBPOOL %>% dbGetQuery("
    select 
      ext.export_template_id,
      ext.template_title
    from p_rsf.export_templates ext
    where (ext.is_public is true or ext.created_by_user_id = $1::text)
      and (
        exists(select * from p_rsf.export_template_reports etr
               where etr.report_id = any(select unnest(string_to_array(NULLIF($2::text,'NA'),','))::int))
        or
        not exists(select * from p_rsf.export_template_reports etr
                   where etr.export_template_id = ext.export_template_id
                     and etr.report_id is not null))",
    params=list(USER_ID(),
                report_ids))
  
  setDT(templates)
  setorder(templates,
           template_title)
  
  return (templates)
})

SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE <- eventReactive(input$server_dashboard_exports__selected_template, {
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE(NA)
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN(list())
  
  if (!isTruthy(selected_template_id)) return (NULL)
  
  export_template <- DBPOOL %>% dbGetQuery("
    select
      ext.export_template_id,
      ext.template_title,
      ext.template_file,
      ext.template_filename,
      ext.template_notes,
      ext.created_by_user_id,
      ext.is_public,
      vai.users_name
    from p_rsf.export_templates ext
    left join p_rsf.view_account_info vai on vai.account_id = ext.created_by_user_id
    where ext.export_template_id = $1::int",
    params=list(selected_template_id))
  setDT(export_template)
  export_template[,template_download_filename:=paste0(gsub("\\..*$","",basename(template_filename)),
                                                      "-",
                                                      toupper(format.Date(now(),"%b%d %Hh%M")),
                                                      ".",
                                                      file_ext(gsub("\\.gz$","",template_filename)))]
  
  return(export_template)
},ignoreNULL = FALSE)

SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE_REPORTS <- eventReactive(SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE(), {
  
  selected_template <- SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE()
  if (!isTruthy(selected_template)) return (NULL)
  
  export_reports <- DBPOOL %>% dbGetQuery("
    select
      exr.export_template_report_id,
      exr.export_template_id,
      exr.report_id,
      exr.sheet_name,
      exr.table_name,
      rep.report_title
    from p_rsf.export_template_reports exr
    left join p_rsf.reports rep on rep.report_id = exr.report_id
    where export_template_id = $1::int
    order by exr.export_template_report_id asc",
    params=list(selected_template$export_template_id))
  
  setDT(export_reports)
  return (export_reports)
}, ignoreNULL=FALSE)

SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES <- reactiveVal(list())

observeEvent(SERVER_DASHBOARD_EXPORTS_TEMPLATES(), {
  
  templates <- SERVER_DASHBOARD_EXPORTS_TEMPLATES()
  updateSelectizeInput(session=session,
                       inputId="server_dashboard_exports__selected_template",
                       choices=c("",
                                 setNames(templates$export_template_id,
                                        templates$template_title)),
                       selected=input$server_dashboard_exports__selected_template)
})

observeEvent(input$server_dashboard_exports__save_template, {
  SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH(SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH()+1)
  
})

observeEvent(input$server_dashboard_exports__run_template, {
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN(list())
  
  if (!isTruthy(selected_template_id)) return (NULL)
  
  report_ids <- DBPOOL %>% dbGetQuery("
    select etr.report_id
    from p_rsf.export_template_reports etr
    where etr.export_template_id = $1::int
      and etr.report_id is not NULL",
    params=list(selected_template_id))
  report_ids <- report_ids$report_id
  
  if (!isTruthy(report_ids)) {
    return (NULL)
  }
  
#browser()
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE(report_ids)
  
  #selected_report_id <- SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS()[1]
  report <-  SERVER_DASHBOARD_REPORTS_LIST()[report_id==report_ids[1],
                                             .(report_id,
                                               report_title,
                                               for_client_sys_names,
                                               for_indicator_names,
                                               for_asof_dates,
                                               report_parameters)]

  if (!empty(report)) {
    SERVER_DASHBOARD_REPORT_SELECTED(as.list(report))
  }

})

##############################################################################################
#Observing return of SERVER_DASHBOARD_DATA_DISPLAY() creates a defacto loop to iterate over
##############################################################################################
observeEvent(SERVER_DASHBOARD_DATA_DISPLAY(), {
  
  load_report <- SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE()
  #load_iteration <- SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_LOOPS
  
  #means first iteration has run already
  #browser
  if (isTruthy(load_report) && length(load_report) > 0) {

    export_data <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value",
                                                   display_data=SERVER_DASHBOARD_DATA_DISPLAY())

    completed_reports <- SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN()
    if (!isTruthy(completed_reports)) completed_reports <- list()
    completed_reports[[paste0("report_",load_report[1])]] <- (export_data)
    SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN(completed_reports)
  } else {
    return (NULL)
  }
  
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE(SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE()[-1])
  if (isTruthy(load_report) && 
      length(load_report) > 0 && 
      !isTruthy(SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE())) {
    
    if (length(SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN()) >= 1) {
      updateTabsetPanel(session=session,
                        inputId="tabset_dashboard",
                        selected="templates")

      hideElement(id="server_dashboard_exports__run_template_button",
                  anim=FALSE)
      
      showElement(id="server_dashboard_exports__download_template_button",
                  anim=TRUE,
                  animType="fade",
                  time=1.5)
      
    }
    
    return (NULL)
  }
  
  
  selected_report_id <- SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_ITERATE() #as.numeric(load_report[[1]])
  
  report <-  SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_report_id,
                                             .(report_id,
                                               report_title,
                                               for_client_sys_names,
                                               for_indicator_names,
                                               for_asof_dates,
                                               report_parameters)]
  if (!empty(report)) {
    
    SERVER_DASHBOARD_REPORT_SELECTED(as.list(report))
  }
  
  
  
  
},priority = -200)

observeEvent(input$server_dashboard_exports__create_export_template, {
  
  new_id <- DBPOOL %>% dbGetQuery("
    insert into p_rsf.export_templates(template_title,
                                          created_by_user_id,
                                          modified_by_user_id)
    values('Untitled export template',
           $1::text,
           $1::text)
    returning export_template_id",
    params=list(USER_ID()))
  
  SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH(SERVER_DASHBOARD_EXPORTS_TEMPLATES_REFRESH()+1)
  new_id <- new_id$export_template_id
  
  shinyjs::runjs(paste0("Shiny.setInputValue(\"server_dashboard_exports__selected_template\",",new_id,",{priority:\"event\"})"))
})

observeEvent(input$server_dashboard_exports__add_report, {
  
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  if (!isTruthy(selected_template_id)) return (NULL)
  #print(paste0("New formula click #",as.numeric(input$server_admin_indicators__add_formula)))
  
  new_export_report <- DBPOOL %>% dbGetQuery("
    insert into p_rsf.export_template_reports(export_template_id,
                                              sheet_name,
                                              table_name)
    values($1::int,'RSF_DATA','rsf_template_data')
    returning 
      export_template_report_id,
      export_template_id,
      sheet_name,
      table_name",
    params=list(selected_template_id))
  
  setDT(new_export_report)
  new_export_report[,module_id:=paste0("server_dashboard_exports__exportreport",export_template_report_id)]
  
  id <- new_export_report$module_id
  ui <- server_dashboard_exports__module_ui_export_report(id=ns(id),
                                                          report=as.list(new_export_report),
                                                          include_labels=length(SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES())==0,
                                                          is_owner=TRUE,
                                                          dashboard_reports_list=SERVER_DASHBOARD_REPORTS_LIST())
  
  
  server_dashboard_exports__module_session_export_report(id=ns(id),
                                                         module_id=id,
                                                         pool=DBPOOL,
                                                         SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES=SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES)
  
  insertUI(selector="#server_dashboard_exports__template_export_reports",
           where="beforeEnd",
           ui=ui)
  
}, ignoreInit=TRUE)

observeEvent(input$server_dashboard_exports__template_name, {
  
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  if (!isTruthy(input$server_dashboard_exports__template_name) || !isTruthy(selected_template_id)) return(NULL)
  
  DBPOOL %>% dbExecute("
    update p_rsf.export_templates ext
    set template_title = $2::text,
        modification_time = now()::timestamptz,
        modified_by_user_id = $3::text
    where ext.export_template_id = $1::int",
    params=list(selected_template_id,
                input$server_dashboard_exports__template_name,
                USER_ID()))
  
})
observeEvent(input$server_dashboard_exports__is_public, {
  
  selected_template_id <- as.numeric(input$server_dashboard_exports__is_public)
  if (is.na(as.logical(input$server_dashboard_exports__is_public)) || !isTruthy(selected_template_id)) return(NULL)
  
  DBPOOL %>% dbExecute("
    update p_rsf.export_templates ext
    set is_public = $2::bool,
        modification_time = now()::timestamptz,
        modified_by_user_id = $3::text
    where ext.export_template_id = $1::int",
    params=list(selected_template_id,
                as.logical(input$server_dashboard_exports__is_public),
                USER_ID()))
  
})
observeEvent(input$server_dashboard_exports__template_notes, {
  
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  if (!isTruthy(input$server_dashboard_exports__template_notes) || !isTruthy(selected_template_id)) return(NULL)
  
  DBPOOL %>% dbExecute("
    update p_rsf.export_templates ext
    set template_notes = $2::text,
        modification_time = now()::timestamptz,
        modified_by_user_id = $3::text
    where ext.export_template_id = $1::int",
    params=list(selected_template_id,
                input$server_dashboard_exports__template_notes,
                USER_ID()))
  
})
observeEvent(input$server_dashboard_exports__template_file, {
  upload <- input$server_dashboard_exports__template_file
  selected_template_id <- as.numeric(input$server_dashboard_exports__selected_template)
  if (!isTruthy(upload) || !isTruthy(selected_template_id)) return (NULL)
  
  #print("Uploading dataset")
  filename <- upload$name
  datapath <- upload$datapath
  copypath <- paste0(dirname(upload$datapath),"/", upload$name)
  if (all(file.copy(upload$datapath, copypath)))
  {
    print(paste0("File renamed FROM: ",datapath," TO: ",copypath))
    datapath <- copypath
  }
  
  if (!all(file_ext(filename) %in% c("xlsx"))) {
    showNotification(h1("Error: file must be Excel format .xlsx only (not .xls, .xlsxm or .xlsxb)"),
                     closeButton = TRUE,
                     duration=8,
                     type="error")
  } else {
    
    gzip(datapath,ext="gz",remove=F,overwrite=T)
    gzip_file <- paste0(datapath,".gz")
    
    file_con <- file(description=gzip_file,open="rb")
    file_info <- file.info(gzip_file)
    
    record_file_name <- basename(gzip_file)
    #record_file_name <- stringr::str_replace_all(record_file_name,"[[:digit:]]{1,3}\\-","")
    
    binfile <- readBin(file_con,what="raw",n=file_info$size,endian=.Platform$endian)
    close.connection(file_con)
    
    DBPOOL %>% dbExecute("
    update p_rsf.export_templates ext
    set template_file = $2::bytea,
        template_filename = $3::text,
        modification_time = now()::timestamptz,
        modified_by_user_id = $4::text
    where ext.export_template_id = $1::int",
    params=list(selected_template_id,
                paste0("\\x",paste0(binfile,collapse="")),
                record_file_name,
                USER_ID()))
    
    file.remove(gzip_file)
    return (TRUE)
    
  }
})

output$server_dashboard_exports__template_ui <- renderUI({
  
  selected_template_id <- req(as.numeric(input$server_dashboard_exports__selected_template))
  
  
  Shiny.destroyList(SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES)
  SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN(list())
  #Reset with a blank list
  SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES(list())
  
  export_template <- SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE()
  
  if (empty(export_template)) {
    return(showNotification(type="error",
                            ui=h3("Invalid template selection")))
  }
  
  export_reports <- SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE_REPORTS()
  is_owner <- export_template$created_by_user_id == USER_ID()
  ui_reports <- NULL
  
  if (!empty(export_reports)) {
    
    export_reports[,module_id:=paste0("server_dashboard_exports__exportreport",export_template_report_id)]
    
    for (i in 1:nrow(export_reports)) {
      
      report <- export_reports[i]
      id <- report$module_id
      ui <- server_dashboard_exports__module_ui_export_report(id=ns(id),
                                                              report=as.list(report),
                                                              include_labels=(i==1),
                                                              is_owner=is_owner,
                                                              dashboard_reports_list=SERVER_DASHBOARD_REPORTS_LIST())

      
      server_dashboard_exports__module_session_export_report(id=ns(id),
                                                             module_id=id,
                                                             pool=DBPOOL,
                                                             SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES=SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES)
      ui_reports[[length(ui_reports)+1]] <- ui
     
    }
  }
  
  ui <- div(style='width:1000px;background-color:white;padding:10px;margin: 0 auto;border:solid black 1px;',
            fluidRow(column(8,
                            enabled(state=is_owner,
                                    textInput(inputId="server_dashboard_exports__template_name",
                                              width="100%",
                                              label = "Export Template Name",
                                              value=export_template$template_title))),
                     column(2,
                            disabled(textInput(inputId="server_dashboard_exports__template_owner",
                                               width="100%",
                                               label="Created By",
                                               value=export_template$users_name))),
                     column(2,
                            div(style="padding-top:23px;",
                                enabled(state=is_owner,
                                        switchInput(inputId="server_dashboard_exports__is_public",
                                                    label= "<i class=\"fa fa-user-secret\"></i>",
                                                    onLabel = "PUBLIC",
                                                    onStatus= "success",
                                                    offLabel = "PRIVATE",
                                                    offStatus = "danger",
                                                    inline=TRUE,
                                                    size="normal",
                                                    value=export_template$is_public))))
            ),
            fluidRow(align="center",
                     column(12,align="left",
                            enabled(state=is_owner,
                                    fileInput(inputId="server_dashboard_exports__template_file",
                                              label="Excel template file",
                                              buttonLabel="Upload...",
                                              placeholder=ifelse(!is.na(export_template$template_filename),
                                                                 export_template$template_filename,
                                                                 "No template file"),
                                    )))),
            fluidRow(align="center",
                     column(12,align="left",
                            enabled(state=is_owner,
                                    textAreaInput(inputId="server_dashboard_exports__template_notes",
                                                  label="Template Notes",
                                                  width="100%",
                                                  height="50%",
                                                  value=ifelse(!is.na(export_template$template_notes),
                                                               export_template$template_notes,
                                                               ""),
                                                  placeholder="Enter template description or use instructions...")))),
            fluidRow(align="left",
                     column(12,
                            div(actionButton(inputId="server_dashboard_exports__add_report",
                                             label="Add Report",
                                             icon=icon("plus-square"),
                                             class="btn btn-primary")))),
            
            fluidRow(align="left",style="padding-bottom:5px;",
                     column(12,
                            div(id="server_dashboard_exports__template_export_reports",style="padding-top:10px;",
                                ui_reports))),
             
             fluidRow(style='padding-top:10px;',
                      div(style='border-top:solid gray 1px;margin-left:10px;margin-right:10px;',
                          column(6,align="left",style="padding-top:5px;",
                                 actionButton(inputId="server_dashboard_exports__delete_prompt",
                                              class="btn-danger",
                                              label="Delete",
                                              icon=icon("minus-circle"))
                          ),
                          column(5,align="right",style="padding-top:5px;",
                                 hidden(div(id="server_dashboard_exports__download_template_button",
                                            downloadButton(outputId="server_dashboard_exports__download_template",
                                                       label="Download",
                                                       icon=icon("download"),
                                                       class="btn-success"))),
                                 div(id="server_dashboard_exports__run_template_button",
                                     actionButton(inputId="server_dashboard_exports__run_template",
                                              class="btn-primary",
                                              label="Run",
                                              icon=icon("circle-play")))),
                          column(1,align="right",style="padding-top:5px;",
                                 div(id="server_dashboard_exports__save_template_button",
                                     actionButton(inputId="server_dashboard_exports__save_template",
                                                  class="btn-success",
                                                  label="Save",
                                                  icon=icon("save")))))
            )
  )
            
  return (ui)
})


SERVER_DASHBOARD_EXPORTS__DO_MAKE_TEMPLATE <- function(export_file,
                                                       export_template=SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE(),
                                                       export_reports=SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE_REPORTS(),
                                                       export_data=SERVER_DASHBOARD_EXPORTS_TEMPLATE_REPORTS_RUN()) {
  #browser() 
  
  
  # export_file <<- export_file
  # export_template <<- export_template
  # export_reports <<- export_reports
  # export_data <<- export_data
  
  template_path <- DBPOOL %>% db_export_template_download_file(export_template_id=export_template$export_template_id)
  TEMPLATE <- DBPOOL %>% db_export_get_template("PFCBL-DASHBOARD-TEMPLATE")
  reporting_user <- USER_NAME()
  
  if (!file.exists(template_path)) {
    showNotification(ui=h3("Failed to generate download file: ",eport_file))
  }
  #excelwb <- openxlsx::readWorkbook(export_file)
  excelwb <- openxlsx::loadWorkbook(template_path)
  program_name <- SELECTED_PROGRAM()$program_nickname
  if (!isTruthy(program_name)) program_name <- "GLOBAL"
  
  for (report_id_name in names(export_data)) {

    current_report_id <- as.numeric(gsub("^report_","",report_id_name))
    current_export_data <- export_data[[report_id_name]]
    current_export_report <- export_reports[report_id==current_report_id]
    
    if (empty(current_export_report)) {
      current_export_report <- data.table(sheet_name=paste0("RSF_DATA",current_report_id),
                                          table_name=paste0("RSF_TABLE",current_report_id))
    }
    reporting_asof_date <- max(current_export_data$REPORTING_asof_date,na.rm=T)
    
    sys_names <- data.table(SYSNAME=trimws(unique(unlist(strsplit(unique(export_data[[report_id_name]]$SYSNAME),
                                                                  split=">",
                                                                  fixed=T)))))
    sys_names[,pfcbl_category:=gsub(":.*$","",SYSNAME)]
    sys_names[,n:=length(unique(SYSNAME)),by=.(pfcbl_category)]
    sys_names <- sys_names[n==1,SYSNAME]
    if (length(sys_names)==0) sys_names <- "global:GLOBAL"
    
    if (any(toupper(excelwb$sheet_names)==toupper(current_export_report$sheet_name))) {
      remove_sheets <- which(toupper(excelwb$sheet_names) %in% toupper(current_export_report$sheet_name))
      sapply(remove_sheets,
             FUN=removeWorksheet,
             wb=excelwb)
    }
    
    
    
    excelwb <- rsf_reports_create_excel_sheet(excelwb=excelwb,
                                              sheet_name=current_export_report$sheet_name,
                                              sheet_data_table_name=current_export_report$table_name,
                                              sheet_data=current_export_data,
                                              
                                              program_name=program_name,
                                              
                                              template_key=TEMPLATE$template_key,
                                              #report_key=export_cohort$reporting_key,
                                              #data_integrity_key=export_cohort$data_integrity_key, 
                                              reporting_entity=sys_names,
                                              reporting_asof_date=reporting_asof_date,
                                              reporting_user=reporting_user,
                                              #reporting_time=as.character(now()),
                                              reporting_notes=paste0("JASON Export using Export Template: ",
                                                                     export_template$template_title))
  }
  saveWorkbook(wb=excelwb,
               file=export_file,
               overwrite=TRUE)
  
  return (export_file)
 
}

#Download the file that was uploaded
output$server_dashboard_exports__download_template <- downloadHandler(
  filename = function() {
    selected_template <- SERVER_DASHBOARD_EXPORTS__SELECTED_TEMPLATE()
    
    return(selected_template$template_download_filename)
  },
  content=function(file) {
    
    withProgress(message="Downloading file",value=0.5, {
      
      SERVER_DASHBOARD_EXPORTS__DO_MAKE_TEMPLATE(export_file=file)
      
      incProgress(amount=1.0,message="Completed")
    })
  }
)
