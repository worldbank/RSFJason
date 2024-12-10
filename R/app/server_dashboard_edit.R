#EDITING FUNCTIONS
SERVER_DASHBOARD_EDIT_COLUMN_IS_EDITABLE <- function(col_name,
                                                     query_data=SERVER_DASHBOARD_CURRENT_QUERY(),
                                                     query_indicators=SERVER_DASHBOARD_SELECTED_INDICATORS()) {
  
  if (col_name %in% SERVER_DASHBOARD_SELECTED_INDICATORS()$indicator_name) return ("DATA")
  else if (grepl("^[[:digit:]]{4}Q[[:digit:]]{1}-[[:alpha:]]{3}$|^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$",col_name)) return ("DATE")
  else if (col_name %in% query_data$RSFNAME) return ("NAME")
  else return (as.character(NA))
}

SERVER_DASHBOARD_EDIT_STATUS <- reactiveVal(FALSE)
#DASH_DATA_EDITING_COUNT <- reactiveVal(0) #Counter of number of times user has edited data in this session -- used only for guidance message.
SERVER_DASHBOARD_EDIT_NEW_DATA <- reactiveVal(NULL)

SERVER_DASHBOARD_EDIT_CELL_INFO <- reactiveVal("")

SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE <- reactiveVal(as.character(NA))

observeEvent(input$server_dashboard_edit_timeline, {
  
  timeline <- as.logical(input$server_dashboard_edit_timeline)
  
  updateTextInput(session=session,
                  inputId="server_dashboard_edit_timeline_select_year",
                  value="")
  
  updateSelectizeInput(session=session,
                       inputId="server_dashboard_edit_timeline_select_quarter",
                       selected = "")
  SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE(as.character(NA))
  
  if (timeline %in% TRUE) {
    shinyjs::show(id="server_dashboard_edit_timeline_select_UI")
  } else {
    shinyjs::hide(id="server_dashboard_edit_timeline_select_UI")
  }
})

observeEvent(c(input$server_dashboard_edit_timeline_select_year,
               input$server_dashboard_edit_timeline_select_quarter), {

  selected_year <- input$server_dashboard_edit_timeline_select_year
  selected_quarter <- input$server_dashboard_edit_timeline_select_quarter
  
  timeline <- as.logical(input$server_dashboard_edit_timeline) 
  
  if (!timeline %in% TRUE) {
    SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE(as.character(NA))  
    return (NULL)
  }
  
  if (!isTruthy(selected_year)) selected_year <- year(today())
  
  timeline_date <- suppressWarnings(
                    lubridate::yq(
                      paste0(selected_year,selected_quarter)))
  
  if (anyNA(timeline_date)) {
    SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE(as.character(NA))  
  } else {
    SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE(lubridate::ceiling_date(timeline_date,unit="quarter")-1)
  }
})

observeEvent(input$action_server_dashboard__edit, {
  
  {
    if (SERVER_DASHBOARD_EDIT_STATUS()==TRUE) {
      return (showNotification(type="error",ui=h2("An editing session is currently active.  First Save and Exit")))
    } 
    
    
    if (!isTruthy(SERVER_DASHBOARD_RUN_AUTORUN())) {
      return (showNotification(type="error",ui=h2("Turn on RUN to edit current data")))
    }
    
    if (empty(SERVER_DASHBOARD_DATA_DISPLAY())) {
      return (showNotification(type="error",ui=h2("The dashboard has no data to edit!  Ensure relevant client, date and indicators are selected")))
    }
  }
  
  {
    #reset editing parameters
    SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE(as.character(NA))    
    SERVER_DASHBOARD_EDIT_NEW_DATA(NULL)
    
    default_filename <- SERVER_DASHBOARD_DOWNLOAD_FILENAME()

    fx_currency <- SERVER_DASHBOARD_RUN_OPTIONS$fx_currency
    if (!fx_currency %in% "LCU") {
      showNotification(type="warning",ui=h2("Resetting dashboard view to default Local Currency for data editing session"))
      updateSelectizeInput(session=session,
                           inputId="server_dashboard__view_currency",
                           selected="LCU")
    }
    
    #Else errors will inevitably creep in if currency and its unit are separated into different columns and edits require editing two columns
    if (!SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU %in% TRUE) {
      SERVER_DASHBOARD_RUN_OPTIONS$fx_concatenate_LCU <- TRUE
      SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
    }
    
    reporting_asof_date <- SERVER_DASHBOARD_RUN_ASOF_DATES()
    if (length(reporting_asof_date) != 1) {
      removeModal()
      return(showNotification(type="error",
                              ui=h2("An error has occured. One (and only one) reporting date may be seleted for editing.  Currently selected: ",
                                    paste0(reporting_asof_date,collapse=" & "))))
    }
    timeline_choices <- c(paste0("I want to modify existing data (edit what is currently displayed)"),  #FALSE
                          "I want to insert new data for a specific reporting date")                    #TRUE
    
    timeline_choices <- setNames(c(FALSE,TRUE),
                                 timeline_choices)
    
    timeline_selection <- fluidRow(id="server_dashboard_edit_timeline_select_UI",
                                   column(3,
                                          textInput(inputId="server_dashboard_edit_timeline_select_year",
                                                    label="Calendar Year",
                                                    value="",
                                                    placeholder=year(today()))),
                                   column(2,
                                          selectizeInput(inputId="server_dashboard_edit_timeline_select_quarter",
                                                         label="Quarter",
                                                         choices=c("Q1","Q2","Q3","Q4"),
                                                         selected="")))
    m <- modalDialog(id="server_dashboard_edit_modal",
                     div(style="background-color:white;padding:5px;overflow-y:auto;height:400px;",
                         panel(
                           fluidPage(
                             fluidRow(column(12,
                                             radioButtons(inputId="server_dashboard_edit_timeline",
                                                          label=NULL,
                                                          width="100%",
                                                          choices=timeline_choices,
                                                          selected=FALSE))),
                             hidden(timeline_selection),
                             
                             fluidRow(column(12,
                                             textInput(inputId="server_dashboard_edit_filename",
                                                       label="Save-as Excel Filename",
                                                       width="100%",
                                                       placeholder = paste0("Default Filename: ",default_filename," (Leave blank to use default)")))),
                             fluidRow(column(12,
                                             textAreaInput(inputId="server_dashboard_edit_note",
                                                           label="Notes:",
                                                           width="100%",
                                                           height="100px",
                                                           placeholder="Enter any reference notes about this editing session")
                             ))))),
                     title=div(style="display:inline-block;",
                               div("Create Editing Session: ")),
                     footer=div(style="display:inline-block;width:100%;",
                                div(style="display:inline-block;float:left;",
                                    actionButton(inputId="server_dashboard_edit_modal_cancel",
                                                 label="Cancel",
                                                 class="btn-primary btn-danger")),
                                div(style="display:inline-block;float:right;",
                                    actionButton(inputId="server_dashboard_edit_modal_begin",
                                                 label="Begin",
                                                 class="btn-primary btn-success"))),
                     size="m")
    
    showModal(m)
  }  
})

observeEvent(input$server_dashboard_edit_modal_begin, {
  
  
  if (SERVER_DASHBOARD_EDIT_STATUS() %in% TRUE) return (showNotification(type="error",
                                                                         ui=h2("You are already editing and cannot begin a new editing session.")))
  
  timeline <- as.logical(input$server_dashboard_edit_timeline) 
  
  reporting_date <- NULL
  if (timeline %in% TRUE) {
    reporting_date <- ymd(SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE())
    
    if (anyNA(reporting_date)) return (showNotification(type="error",
                                                        ui=h2("Please enter a valid timeline 4-digit Year and Quarter.")))
  } else {
    reporting_date <- SERVER_DASHBOARD_RUN_ASOF_DATES()
  }
  
  if (anyNA(reporting_date) ||
      length(reporting_date) != 1) return (showNotification(type="error",
                                           ui=h2("Please ensure dashboard data has selected one (and only one) reporting date")))
  

  if (empty(SERVER_DASHBOARD_CURRENT_QUERY())) return (showNotification(type="error",
                                                                        ui=h2("Dashboard data is empty and nothing is available to edit.")))

  editing_filename <- input$server_dashboard_edit_filename
  
  if (!isTruthy(editing_filename)) editing_filename <- SERVER_DASHBOARD_DOWNLOAD_FILENAME()
  
  if (nchar(basename(editing_filename)) < 5) {
    return (showNotification(type="error",ui=h2("Please enter a valid filename of at least 5 characters.  Punctuaction is disallowed, except for '-' '+' and '_'")))
  }
  
  shinyjs::hide(id="panel_actions_viewing")
  shinyjs::show(id="panel_actions_editing")
  
  showNotification(type="default",
                   duration=8,
                   ui=div(style="opacity:1.0;",
                          h3("Double-click the cell you want to edit."),
                          h3("Make as many edits as you like."),
                          h3("When done, remember to Save & Close (green button, upper-right).  Otherwise, edits will be discarded.")))
  removeModal()
  SERVER_DASHBOARD_EDIT_STATUS(TRUE)
  
})

observeEvent(input$server_dashboard_edit_modal_cancel, {
  shinyjs::show(id="panel_actions_viewing")
  shinyjs::hide(id="panel_actions_editing")
  
  removeModal()
  SERVER_DASHBOARD_EDIT_STATUS(FALSE)
  
})





observeEvent(SERVER_DASHBOARD_EDIT_STATUS(), {
  
  
  if (SERVER_DASHBOARD_EDIT_STATUS()==TRUE) {
    shinyjs::addClass(id="server_dashboard__panel_browser",
                      class="panel-border-editing")
    
    #DASH_DATA_CELL_INFO("<div><b>double-click</b> cell to edit its value.</div>")
  } else {
    shinyjs::removeClass(id="server_dashboard__panel_browser",
                         class="panel-border-editing")
  }
  
},
ignoreInit = TRUE)

observeEvent(input$server_dashboard__browser_cell_edit, {
  
  if (SERVER_DASHBOARD_EDIT_STATUS()==TRUE) {
    
    #only valid data columns should be editable in the first place.
    clicked_cell <- input$server_dashboard__browser_cell_edit
    
    if (!isTruthy(clicked_cell) || length(clicked_cell) == 0) return (NULL)
    
    clicked_data_id <- SERVER_DASHBOARD_RENDER_DISPLAY(view="id")[clicked_cell$row,clicked_cell$col+1,with=F]

    
    if (is.na(SERVER_DASHBOARD_EDIT_COLUMN_IS_EDITABLE(names(clicked_data_id)))) {
      return (showNotification(type="error",
                               ui=h3("This cell cannot be edited")))
    }
    #If the column clicked is a valid indicator name, then indicators are in the columns
    #else, indicator will be in a column named INDICATORS at the specified ROW: note that because of PIVOT settings, the indicator_name may NOT always be
    #within the clicked row of the INDICATORS column.  Therefore, matching column names will be more reliable.

    #If the column clicked is a valid date name (ymd or qdate), then reporting asof dates are in the columns--and PIVOT by date can cast either qdate which
    #is formatted as-of date, or currentest_date, which is the actual reporting date of the value.
    #else, reporting asof date will be in a column named REPORTING_asof_date or REPORTING_qdate at the specified ROW

    #If the column clicked is a valid CLIENT_LIST name, then reporting asof dates are in the columns
    #else, reporting asof date will be in a column named REPORTING_asof_date or REPORTING_qdate at the specified ROW
    
    #However, all of these heuristics are only necessary in the case that clicked_data_id is NA (meaning nothing ever reported for the cell)

    cell_data_id <- as.numeric(unlist(clicked_data_id))
    if (!is.na(cell_data_id)) {
      cell_info <- DBPOOL %>% dbGetQuery("
      select 
        rd.rsf_pfcbl_id,
        rd.indicator_id,
        rd.reporting_asof_date
      from p_rsf.rsf_data rd
      where data_id = $1::int",
      params=list(cell_data_id))
      
      if (empty(cell_info)) {
        return (showNotification(type="error",
                                 ui=h3(paste0("Failed to edit cell. Clicked data_id=",cell_data_id," could not be located in database"))))
      } 
      
      setDT(cell_info)
      
      cell_info[,data_value:=clicked_cell$value]
      
      if (!is.na(SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE())) {
        cell_info[,reporting_asof_date:=ymd(SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE())]
      }
      
      SERVER_DASHBOARD_EDIT_NEW_DATA(rbindlist(list(SERVER_DASHBOARD_EDIT_NEW_DATA(),
                                                     cell_info[,.(rsf_pfcbl_id,
                                                                  indicator_id,
                                                                  reporting_asof_date,
                                                                  data_value)])))
      
    
    } else {
      col_name <- names(clicked_data_id)
      clicked_indicator_id <- NULL
      clicked_sys_id <- NULL

      #Clicked on a data column (or a pivoted data column)
      if (col_name %in% SERVER_DASHBOARD_SELECTED_INDICATORS()$indicator_name) {
        clicked_indicator_id <- SERVER_DASHBOARD_SELECTED_INDICATORS()[indicator_name==col_name,indicator_id]
        clicked_sys_id <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value")[clicked_cell$row,SYSID]
      
      #Clicked on a pivoted date/qdate col  
      } else if (grepl("^[[:digit:]]{4}Q[[:digit:]]{1}-[[:alpha:]]{3}$|^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$",col_name)) {
        clicked_cell_info <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value")[clicked_cell$row,.(INDICATOR,SYSID)]
        clicked_indicator_id <- SERVER_DASHBOARD_SELECTED_INDICATORS()[indicator_name==clicked_cell_info$INDICATOR,indicator_id]
        clicked_sys_id <- clicked_cell_info$SYSID
      } else if (col_name %in% SERVER_DASHBOARD_CURRENT_QUERY()$RSFNAME) {
        clicked_cell_info <- SERVER_DASHBOARD_RENDER_DISPLAY(view="value")[clicked_cell$row,.(INDICATOR)]
        clicked_indicator_id <- SERVER_DASHBOARD_SELECTED_INDICATORS()[indicator_name==clicked_cell_info$INDICATOR,indicator_id]
        clicked_sys_id <- SERVER_DASHBOARD_CURRENT_QUERY()[RSFNAME==col_name,unique(SYSID)]
        
        if (length(clicked_sys_id) != 1) {
          return(showNotification(type="error",
                                  ui=h3("Error: Cell cannot be edited because PIVOT by NAME '",col_name,"' is not unique, but shared by ",length(clicked_sys_id)," entities")))
        }
      }
      
      if (is.null(clicked_indicator_id) || is.null(clicked_sys_id)) {
        return(showNotification(type="error",
                                ui=h3("Error: Cell cannot be edited because it cannot be found in the database")))
      }
      
      edited_rsf_pfcbl_id <- NULL
      edited_reporting_asof_date <- NULL
      edited_indicator_id <- clicked_indicator_id
      
      rsf_pfcbl <- DBPOOL %>% dbGetQuery("
        with mids as (
          select
            ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id, 
            count(*) over() as matches
          from p_rsf.view_rsf_pfcbl_id_family_tree ft
          where ft.from_rsf_pfcbl_id = $1::int
            and ft.to_pfcbl_category = (select ind.data_category from p_rsf.indicators ind
                                        where ind.indicator_id = $2::int)
        )
        select 
          ids.rsf_pfcbl_id,
          ids.created_in_reporting_asof_date::text
        from mids
        inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = mids.rsf_pfcbl_id
        where mids.matches = 1",
        params=list(clicked_sys_id,
                    edited_indicator_id))
      
      if (empty(rsf_pfcbl)) {
        return(showNotification(type="error",
                                ui=h3("Error: Cell cannot be edited because rsf_pfcbl_id cannot be uniquely found in the database")))
      }
      
      edited_rsf_pfcbl_id <- rsf_pfcbl$rsf_pfcbl_id
      
      if (!is.na(SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE())) {
        edited_reporting_asof_date <- ymd(SERVER_DASHBOARD_EDIT_REPORTING_TIMELINE())
      } else {
        edited_reporting_asof_date <- ymd(rsf_pfcbl$created_in_reporting_asof_date)
      }
      
      SERVER_DASHBOARD_EDIT_NEW_DATA(rbindlist(list(SERVER_DASHBOARD_EDIT_NEW_DATA(),
                                                     data.table(rsf_pfcbl_id=edited_rsf_pfcbl_id,
                                                                indicator_id=edited_indicator_id,
                                                                reporting_asof_date=edited_reporting_asof_date,
                                                                data_value=clicked_cell$value))))
    }
    
    
    #print(SERVER_DASHBOARD_EDIT_NEW_DATA())
  
  } else {
    return (showNotification(type="error",ui=h2("Edits can only be made during an editing session.  Click 'EDIT' to start session")))
  }
})

observeEvent(input$action_server_dashboard__edit_discard, {

  shinyjs::show(id="panel_actions_viewing")
  shinyjs::hide(id="panel_actions_editing")
  SERVER_DASHBOARD_EDIT_STATUS(FALSE)
  
  
})

observeEvent(input$action_server_dashboard__edit_save, {
  
  
  if (SERVER_DASHBOARD_EDIT_STATUS()==FALSE) return (NULL)
  
  
  editing_filename <- input$server_dashboard_edit_filename
 
  
  if (!isTruthy(editing_filename)) {
    editing_filename <- SERVER_DASHBOARD_DOWNLOAD_FILENAME()
  }  
  
  editing_filename <- basename(editing_filename)
  editing_filename <- gsub("\\.[[:alnum:]]+$","",editing_filename)
  editing_filename <- gsub("[^[:alnum:]_\\+\\-]"," ",editing_filename)
  editing_filename <- gsub("[[:space:]]+"," ",editing_filename)
  editing_filename <- trimws(editing_filename)
  
  if (!isTruthy(editing_filename) || nchar(editing_filename) < 5) {
    editing_filename <- SERVER_DASHBOARD_DOWNLOAD_FILENAME()
    if (!isTruthy(editing_filename)) editing_filename <- "RSF EDITS.xlsx"
  } else {
    
    if (!isTruthy(file_ext(editing_filename)) || file_ext(editing_filename) != "xlsx") {
      editing_filename <- paste0(editing_filename,".xlsx")
    }
  }
  
  #program_indicators <- DASH_INDICATORS()
  #dd_dt <- DASH_DATA_DISPLAY()
  #editing <<- as.data.frame(SERVER_DASHBOARD_EDIT_NEW_DATA())
  #pri <<- as.data.frame(program_indicators)
  #dd_df <<- as.data.frame(dd_dt)
  #warning("<<- observeEvent(input$rsfdash_browser_cell_edit")
  #edit_dt <- as.data.table(editing)
  #program_indicators <- as.data.table(pri)
  #dd_dt <- as.data.table(dd_df)
  
  edit_dt <- SERVER_DASHBOARD_EDIT_NEW_DATA()

  if (!isTruthy(edit_dt) || empty(edit_dt)) {
    shinyjs::show(id="panel_actions_viewing")
    shinyjs::hide(id="panel_actions_editing")
    SERVER_DASHBOARD_EDIT_STATUS(FALSE)
    SERVER_DASHBOARD_EDIT_NEW_DATA(NULL)
    
    return(showNotification(type="message",ui=h2("No changes made.  Nothing to save.")))
  }
  
  edit_dt[SERVER_DASHBOARD_SELECTED_INDICATORS(),
          indicator_name:=i.indicator_name,
          on=.(indicator_id)]
  
  sys_names <- DBPOOL %>% dbGetQuery("
    select
      sn.rsf_pfcbl_id,
      sn.sys_name
    from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
    where sn.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(paste0(unique(edit_dt$rsf_pfcbl_id),collapse=",")))
  
  setDT(sys_names)
  
  edit_dt[sys_names,
          SYSNAME:=i.sys_name,
          on=.(rsf_pfcbl_id)]
  
  if (anyNA(edit_dt$rsf_pfcbl_id) || 
      anyNA(edit_dt$SYSNAME)) {
    return(showNotification(type="error",ui=h2("Error: failed to match system IDs")))
  }
  
  edit_dt[,n:=.N:1,
          by=.(rsf_pfcbl_id,
               indicator_id,
               reporting_asof_date)]
  
  if (any(edit_dt$n > 1)) {
    edit_dt_dups <- edit_dt[n>1]
    edit_dt <- edit_dt[n==1]
    
    dups_ui <- "<table>"
    for (i in 1:nrow(edit_dt_dups)) {
      dups_ui <- paste0("<tr> ",
                        "<td>",edit_dt_dups[i]$SYSNAME,"</td>",
                        "<td>",edit_dt_dups[i]$indicator_name,"</td>",
                        "<td>",edit_dt_dups[i]$reporting_asof_date,"</td>",
                        "<td>",edit_dt_dups[i]$data_value,"</td></tr>")
    }
    dups_ui <- paste(dups_ui,"</table>")
    dups_ui <- div(h3("Duplicate values found and discarded. If multiple values are intentional they must be done in separate editing sessions"),
                   div(HTML(dups_ui)))
    
    showNotification(type="warning",
                     ui=dups_ui,
                     duration=NULL)
  }

  #Just want to gray-out the background  
  showModal(ui=HTML('<div id="shiny-modal" class="modal fade" tabindex="-1" data-backdrop="static" data-keyboard="false">
                      <div class="modal-dialog">
                      </div>
                      <script>$(\'#shiny-modal\').modal().focus();</script>
                    </div>'))
  
  results <- withProgress(value=.10,message="Saving datasets...", {
    progress_status_message <- function(class,...) {
      dots <- list(...)
      dots <- paste0(unlist(dots),collapse=" ")
      incProgress(amount=0,
                  message=paste0("Uploading edits: ",dots))
    }
    
    incProgress(amount=0.10,message="Validating edits...")

    
    #changing names in case anyone downloads it, so more clear what they're looking at.  Reason SYSNAMEs were added, too
    edit_dt <- edit_dt[,.(SYSNAME,
                          SYSID=rsf_pfcbl_id,
                          INDID=indicator_id,
                          reporting_asof_date,
                          indicator_name,
                          data_value)]
    
    
    incProgress(amount=0.10,message="Converting to excel...")    
    
    #USER_ID <- function() { CALCULATIONS_ENVIRONMENT$SYSTEM_CALCULATOR_ACCOUNT }
    #SELECTED_PROGRAM_ID <- function() { 66 }
    #progress_status_message <- testing_status_message
    excelwb <- DBPOOL %>% export_dashboard_edits_to_excel(edits_dt=edit_dt,
                                                          exporting_user_id=USER_ID(),
                                                          report_note="None")
    
    saveWorkbook(excelwb,
                 file=editing_filename,
                 overwrite=TRUE)
    
    incProgress(amount=0.10,message="Uploading edits...")

    results <- tryCatch({
      template_parse_process_and_upload(pool=dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]])),
                                        rsf_program_id = SELECTED_PROGRAM_ID(),
                                        reporting_user_id = USER_ID(),
                                        template_files=editing_filename,
                                        source_note="None",
                                        delete_on_error=FALSE,
                                        status_message=progress_status_message)
    },
    warning=function(w) {
      showNotification(type="error",
                       duration=NULL,
                       ui=h3(paste0("An error has occcurred. Failed to save edits: ",conditionMessage(w),
                                    "Please review the datasets panel and download the dataset, if available; then delete it.")))
      return (NULL)
    },
    error=function(e) {
      showNotification(type="error",
                       duration=NULL,
                       ui=h3(paste0("An error has occcurred. Failed to save edits: ",conditionMessage(e),
                                    "Please review the datasets panel and download the dataset, if available; then delete it.")))
      return (NULL)
    })
    
    file.remove(editing_filename)
    return(results)
  })
  
  if (!is.null(results)) {
    shinyjs::show(id="panel_actions_viewing")
    shinyjs::hide(id="panel_actions_editing")
    SERVER_DASHBOARD_EDIT_STATUS(FALSE)
    SERVER_DASHBOARD_EDIT_NEW_DATA(NULL)
    SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1)
    LOAD_REPORTING_COHORT(results[nrow(results),reporting_cohort_id])
    
    if (!any(as.character(unique(results$reporting_asof_date)) %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) {
      new_dates <- setdiff(sort(as.character(unique(results$reporting_asof_date))),as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))
      LOAD_VALID_REPORTING_DATE(new_dates)
    }
  }
  
  removeModal()
  
})
