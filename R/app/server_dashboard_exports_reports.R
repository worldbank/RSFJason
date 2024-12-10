
server_dashboard_exports__module_ui_export_report <- function(id,
                                                              report,
                                                              dashboard_reports_list,
                                                              include_labels=TRUE,
                                                              is_owner) {
  ns <- NS(id)
  reports.choices <- setNames(dashboard_reports_list$report_id,
                              dashboard_reports_list$report_title)
  reports.choices <- sort(reports.choices)
  
  ui <- div(name="export_report_box",
            style="width:100%",
            id=ns("ui"),
            fluidRow(column(7,
                            enabled(state=is_owner,
                                    selectizeInput(inputId=ns("export_report"),
                                                   label=switch(include_labels,"Report Title",NULL),
                                                   choices=c("",reports.choices),
                                                   selected=ifelse(is.na(report$report_id),"",report$report_id),
                                                   multiple=FALSE,
                                                   width="100%"))),
                     column(2,
                            enabled(state=is_owner,
                                    textInput(inputId=ns("excel_sheet_name"),
                                              label=switch(include_labels,"Excel Sheet Name",NULL),
                                              value=report$sheet_name))),
                     column(2,
                            enabled(state=is_owner,
                                    textInput(inputId=ns("excel_table_name"),
                                              label=switch(include_labels,"Table Defined Name",NULL),
                                              value=report$table_name))),
                     column(1,
                            style=switch(include_labels,"padding-top:25px",""),
                            enabled(state=is_owner,
                                    actionButton(inputId=ns("delete_report"),
                                                 label=NULL,
                                                 icon=icon("trash"))))))
  return(ui)
}

server_dashboard_exports__module_session_export_report <- function(id,
                                                                   module_id,
                                                                   pool,
                                                                   SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES) {
  ns <- NS(id)
  modules <- isolate({ SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES() })
  moduleServer(id,function(input,output, session) { 
    
    this.id <- reactiveVal(as.numeric(gsub("^server_dashboard_exports__exportreport","",module_id)))
    
    o1 <- observeEvent(input$export_report, {
      report_id <- as.numeric(input$export_report)
      
      if (isTruthy(report_id)) {
        dbExecute(pool,"
          update p_rsf.export_template_reports etr
          set report_id = $2::int
          where etr.export_template_report_id = $1::int
            and etr.report_id is distinct from $2::int
            and exists(select * from p_rsf.reports rep
                       where rep.report_id = $2::int)",
          params=list(this.id(),
                      report_id))
      }
    },ignoreInit = TRUE)
    
    o2 <- observeEvent(input$excel_sheet_name, {
      
      sheet_name <- as.character(input$excel_sheet_name)
      
      if (isTruthy(sheet_name)) {
        dbExecute(pool,"
          update p_rsf.export_template_reports etr
          set sheet_name = $2::text
          where etr.export_template_report_id = $1::int
            and etr.sheet_name is distinct from $2::text",
          params=list(this.id(),
                      sheet_name))
      }
    },ignoreInit = TRUE)
    
    o3 <- observeEvent(input$excel_table_name, {
      table_name <- as.character(input$excel_table_name)
      
      if (isTruthy(table_name)) {
        dbExecute(pool,"
          update p_rsf.export_template_reports etr
          set table_name = $2::text
          where etr.export_template_report_id = $1::int
            and etr.table_name is distinct from $2::text",
          params=list(this.id(),
                      table_name))
      }
    },ignoreInit = TRUE)
    
    o4 <- observeEvent(input$delete_report, {
      
      deleted_id <- dbGetQuery(pool,"
        delete from p_rsf.export_template_reports etr
        where etr.export_template_report_id = $1::int
        returning export_template_report_id",
        params=list(this.id()))
      
      if (identical(as.numeric(unlist(deleted_id)),this.id())) {
        removeUI(selector=paste0("#",ns("ui")))        
      } else {
        showNotification(type="error",
                         h3("Failed to delete report"))
      }
    },ignoreInit = TRUE)
  
    modules[[length(modules)+1]] <- o1
    modules[[length(modules)+1]] <- o2
    modules[[length(modules)+1]] <- o3
    modules[[length(modules)+1]] <- o4
  
    SERVER_DASHBOARD_EXPORTS__TEMPLATE_REPORT_MODULES(modules)
  })
}