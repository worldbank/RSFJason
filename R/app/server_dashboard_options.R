#DISPLAY OPTIONS FUNCTIONS
SERVER_DASHBOARD_OPTIONS_SYSCOLS <- c(`REPORTING_asof_date`="reporting_asof_date", #must be kept and only filtered-out aesthetically.
                                                                                   #reporting_asof_date NOT in rsf_data, rather current_asof_date
                                      `SYSID`="SYSID",                             #must be kept and only filtered-out aesthetically.
                                                                                   #SYSID NOT in rsf_data
                                      `DATA_currentest_date`="data_asof_date",
                                      `SYSNAME`="SYSNAME",                        #must be kept and only filtered-out aesthetically (for upload matching)
                                                                                  #SYSNAME NOT in rsf_data  
                                      `REPORTING_status`="reporting_status",
                                      `REPORTING_expected`="reporting_expected",
                                      `REPORTING_qdate`="reporting_qdate",
                                      `RSFNAME`="rsf_full_name")

observeEvent(input$server_dashboard__view_options, {

  m <- modalDialog(id="dash_options_modal",
                   div(style="background-color:white;padding:5px;height:350px;",
                       #panel(heading="Advanced Dashboard Options",
                             fluidRow(column(12,align="center",style="font-weight:bold;color:black;padding-bottom:5px;margin-bottom:5px;","SYSTEM COLUMNS")),
                             fluidRow(column(12,
                                       selectizeInput(inputId="server_dashboard_options__syscols",
                                                      label=NULL,
                                                      choices=SERVER_DASHBOARD_OPTIONS_SYSCOLS,
                                                      width="100%",
                                                      multiple=TRUE,
                                                      selected=SERVER_DASHBOARD_RUN_OPTIONS$syscols,
                                                      options=list(placeholder="None")))
                             ),
                       #),
                       #panel(heading="Currency & FX Options",
                             fluidRow(column(12,align="center",style="font-weight:bold;color:gray;padding-bottom:5px;margin-bottom:5px;","FX PARAMETERS")),
                             fluidRow(column(3,
                                             selectizeInput(inputId="server_dashboard_options__fx_global",
                                                            label="FX Source",
                                                            choices=c(`Global (IFC)`="global",
                                                                      `Facility (If Available)`="facility"),
                                                            width="100%",
                                                            multiple=FALSE,
                                                            selected=fcase(SERVER_DASHBOARD_RUN_OPTIONS$fx_force_global==FALSE,"facility",
                                                                           default="global"),
                                                            options=list(placeholder="Global (IFC)"))),
                                      column(3,
                                             selectizeInput(inputId="server_dashboard_options__fx_date",
                                                            label="FX Rate Date",
                                                            choices=c(`Date of Request`="current",
                                                                      `Date of Data`="data"),
                                                            width="100%",
                                                            multiple=FALSE,
                                                            selected=fcase(SERVER_DASHBOARD_RUN_OPTIONS$fx_reported_date==TRUE,"data",
                                                                           default="current"),
                                                            options=list(placeholder="Date of Request"))),
                                      column(3,
                                             selectizeInput(inputId="server_dashboard_options__fx_units",
                                                            label="Merge Money & Units",
                                                            choices=c("YES","NO"),
                                                            width="100%",
                                                            multiple=FALSE,
                                                            selected=fcase(SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU==FALSE,"NO",
                                                                           default="YES"),
                                                            options=list(placeholder="YES"))),
                                      column(3,
                                             selectizeInput(inputId="server_dashboard_options__fx_audit",
                                                            label="Include FX Audit",
                                                            choices=c("YES","NO"),
                                                            width="100%",
                                                            multiple=FALSE,
                                                            selected=fcase(SERVER_DASHBOARD_RUN_OPTIONS$fx_audit==TRUE,"YES",
                                                                           default="NO"),
                                                            options=list(placeholder="NO")))
                              ),
                       #),
                       #panel(heading="Format Options",
                             fluidRow(column(12,align="center",style="font-weight:bold;color:gray;padding-bottom:5px;margin-bottom:5px;","DISPLAY PARAMETERS")),
                             fluidRow(column(3,
                                             selectizeInput(inputId="server_dashboard_options__format_unchanged",
                                                            label="Unchanged Data Format",
                                                            choices=c(`Gray`="gray",
                                                                      `Black`="black",
                                                                      `Empty`="empty",
                                                                      `Arrow`="arrow"),
                                                            multiple=FALSE,
                                                            selected=SERVER_DASHBOARD_RUN_OPTIONS$format_unchanged)),
                                      column(3,
                                             selectizeInput(inputId="server_dashboard_options__format_blanks",
                                                            label="Blanks Format",
                                                            choices=c(`As {BLANK}`="text",
                                                                      `As Empty`="empty"),
                                                            multiple=FALSE,
                                                            selected=SERVER_DASHBOARD_RUN_OPTIONS$format_blank)),
                                      column(3,
                                      selectizeInput(inputId="server_dashboard_options__display_flags",
                                                     label="Include Flags",
                                                     choices=c("",
                                                               `ACTIVE`="active",
                                                               `RESOLVED`="resolved"),
                                                     multiple=TRUE,
                                                     selected=SERVER_DASHBOARD_RUN_OPTIONS$display_flags,
                                                     options=list(placeholder="None"))),
                                      column(3,
                                             selectizeInput(inputId="server_dashboard_options__format_exceldates",
                                                            label="Export Excel Dates",
                                                            choices=c("YES","NO"),
                                                            multiple=FALSE,
                                                            selected=fcase(SERVER_DASHBOARD_RUN_OPTIONS$format_exceldates=="NO",FALSE,
                                                                           default=TRUE),
                                                            options=list(placeholder="None")))
                       #)
                       )
                   ),
                   title=div(style="display:inline-block;","Dashboard Options"),
                   footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                              modalButton("Cancel"),
                              actionButton(inputId="server_dashboard_options__close",
                                       label="Close & Run",
                                       class="btn-success")),
                   size="m")
  
  updateSwitchInput(session=session,
                    inputId="server_dashboard__autorun",
                    value=FALSE)
  showModal(m)
})

{    
  observeEvent(input$server_dashboard_options__close, {
    removeModal()
    updateSwitchInput(session=session,
                      inputId="server_dashboard__autorun",
                      value=TRUE)
  })
}  

observeEvent(input$server_dashboard_options__syscols, {

  if (!isTruthy(input$server_dashboard_options__syscols)) {
    SERVER_DASHBOARD_RUN_OPTIONS$syscols <- c()
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$syscols <- intersect(input$server_dashboard_options__syscols,
                                                      SERVER_DASHBOARD_OPTIONS_SYSCOLS)
  }
})


observeEvent(input$server_dashboard_options__fx_global, {
  
  if (input$server_dashboard_options__fx_global %in% c("facility")) {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_force_global <- FALSE
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_force_global <- TRUE
  }
})

observeEvent(input$server_dashboard_options__fx_date, {
  
  if (input$server_dashboard_options__fx_date %in% c("data")) {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_reported_date <- TRUE
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_reported_date <- FALSE
  }
})
observeEvent(input$server_dashboard_options__fx_audit, {
  
  if (input$server_dashboard_options__fx_audit %in% c("YES")) {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_audit <- TRUE
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_audit <- FALSE
  }
})

observeEvent(input$server_dashboard_options__fx_units, {
  
  if (input$server_dashboard_options__fx_units %in% c("NO")) {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU <- FALSE
  } else {
    SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU <- TRUE
  }
})


observeEvent(input$server_dashboard_options__format_unchanged, {
  
  if (!isTruthy(input$server_dashboard_options__format_unchanged)) SERVER_DASHBOARD_RUN_OPTIONS$format_unchanged <- "gray"
  else SERVER_DASHBOARD_RUN_OPTIONS$format_unchanged <- input$server_dashboard_options__format_unchanged
  
})

observeEvent(input$server_dashboard_options__format_blanks, {
  
  if (!isTruthy(input$server_dashboard_options__format_blanks)) SERVER_DASHBOARD_RUN_OPTIONS$format_blank <- "text"
  else SERVER_DASHBOARD_RUN_OPTIONS$format_blank <- input$server_dashboard_options__format_blanks
})

observeEvent(input$server_dashboard_options__format_exceldates, {
  
  if (!isTruthy(input$server_dashboard_options__format_exceldates)) SERVER_DASHBOARD_RUN_OPTIONS$format_exceldates <- FALSE
  else SERVER_DASHBOARD_RUN_OPTIONS$format_exceldates <- TRUE
})

observeEvent(input$server_dashboard_options__display_flags, {
  
  SERVER_DASHBOARD_RUN_OPTIONS$display_flags <- intersect(c("active","resolved"),tolower(input$server_dashboard_options__display_flags))
})
  # 
  # observeEvent(input$rsfdash_options_filter_status, {
  #   status <- toupper(input$rsfdash_options_filter_status)
  #   status <- status[status %in% c("ACTIVE","INACTIVE")]
  #   if (isTruthy(status)) DASH_DATA_OPTIONS$filter_status <- status
  #   else DASH_DATA_OPTIONS$filter_status <- as.character(NA)
  # }, ignoreNULL=FALSE)
  
  # observeEvent(input$rsfdash_options_timeseries_format, { 
  #   selected <- input$rsfdash_options_timeseries_format
  #   if (isTruthy(selected) && selected %in% c("html","current")) DASH_DATA_OPTIONS$rsfdash_options_timeseries_format <- selected
  #   else DASH_DATA_OPTIONS$rsfdash_options_timeseries_format <- "html"
  # }, ignoreNULL=FALSE)
  

  # observeEvent(input$rsfdash_options_blanks, { 
  #   selected <- input$display_blanks
  #   if (isTruthy(selected) && selected %in% c("text","empty")) DASH_DATA_OPTIONS$display_blanks <- selected
  #   else DASH_DATA_OPTIONS$display_blanks <- "text"
  # }, ignoreNULL=FALSE)
  
  
