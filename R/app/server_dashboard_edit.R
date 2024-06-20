#EDITING FUNCTIONS
{
  observeEvent(input$dash_edit_modal_begin, {
    
    rsf_family <- DASH_RSF_FAMILY()
    reporting_date <- as.character(input$rsfdash_reporting_asof_date)
    
    if (!isTruthy(rsf_family) || !isTruthy(reporting_date)) {
      removeModal()
      return (showNotification(type="error",ui=h2("An error has occured: ensure one or more clients and a valid date are selected.")))
    }
    
    if (!isTruthy(DASH_DATA_EDITING_TIMESERIES_CHOICE())) {
      return (showNotification(type="error",
                               ui=h2("Please select how you want edit timeseries data.")))
    }
    
    editing_filename <- input$rsfdash_editing_filename
    
    if (isTruthy(editing_filename) && nchar(basename(editing_filename)) < 5) {
      return (showNotification(type="error",ui=h2("Please enter a valid filename of at least 5 characters.  Punctuaction is disallowed, except for '-' '+' and '_'")))
    }
    #else its DASH_DATA_EDITING_DEFAULT_FILENAME()
    
    shinyjs::hide(id="panel_actions_viewing")
    shinyjs::show(id="panel_actions_editing")
    
    edit_count <- as.numeric(DASH_DATA_EDITING_COUNT())
    
    if (edit_count == 0) showNotification(type="default",
                                          duration=8,
                                          ui=div(style="opacity:1.0;",
                                                 h3("Double-click the cell you want to edit."),
                                                 h3("Make as many edits as you like."),
                                                 h3("When done, remember to Save & Close (green button, upper-right).")))
    removeModal()
    DASH_DATA_EDITING_COUNT(DASH_DATA_EDITING_COUNT()+1)
    DASH_DATA_EDITING(TRUE)
  })
  
  observeEvent(input$dash_edit_modal_cancel, {
    shinyjs::show(id="panel_actions_viewing")
    shinyjs::hide(id="panel_actions_editing")
    
    removeModal()
    DASH_DATA_EDITING(FALSE)
    
  })
  
  observeEvent(input$rsfdash_editing_timeseries_choice, {
    timeseries <- as.numeric(input$rsfdash_editing_timeseries_choice)
    
    if (DASH_DATA_EDITING()==TRUE) {
      return (showNotification(type="error",ui=h2("An editing session is currently active.  First Save and Exit")))
    } else if (is.na(timeseries)) {
      DASH_DATA_EDITING_TIMESERIES_CHOICE(as.numeric(NA))
    } else if (!timeseries %in% c(1,2,3)) {
      return (showNotification(type="error",ui=h2("Invlaid choice")))
    } else {
      DASH_DATA_EDITING_TIMESERIES_CHOICE(timeseries)
      if (timeseries==3) {
        DASH_DATA_OPTIONS$display_timeline <- TRUE
        updateSelectizeInput(session=session,
                             inputId="rsfdash_reporting_history",
                             selected=1) #Changes
      }
    }
  }, ignoreInit = T)
  
  observeEvent(input$action_dfsdash_editmode_start, {
    
    if (DASH_DATA_EDITING()==TRUE) {
      return (showNotification(type="error",ui=h2("An editing session is currently active.  First Save and Exit")))
    } else {
      
      default_filename <- DASH_DATA_EDITING_DEFAULT_FILENAME()
      
      if (!isTruthy(default_filename)) {
        removeModal()
        return(showNotification(type="error",ui=h2("An error has occured.  Ensure reporting date and clients are selected")))
      }
      
      display_currency <- DASH_DATA_OPTIONS$display_currency
      if (isTruthy(display_currency) && display_currency != "LCU") {
        showNotification(type="info",ui=h2("Resetting dashboard view to default Local Currency for data editing session"))
        updateSelectizeInput(session=session,
                             inputId="rsfdash_view_currency",
                             selected="LCU")
      }
      
      reporting_asof_date <- input$rsfdash_reporting_asof_date
      
      if (!isTruthy(reporting_asof_date)) {
        removeModal()
        return(showNotification(type="error",ui=h2("An error has occured. Reporting asof Date must be selected from the drop down before editing.")))
      }
      
      quarter_start_date <- floor_date(ymd(reporting_asof_date),"quarter")
      timeseries_display <- as.numeric(input$rsfdash_reporting_history)
      
      timeseries_choices <- NULL
      timeseries_choices_selected <- ""
      DASH_DATA_EDITING_TIMESERIES_CHOICE(NA)
      
      if (timeseries_display==0) {
        timeseries_choices <- c(paste0("I want to update existing data in the same time period that it is reported"),
                                paste0("I want to report new data that 'happened' between ",
                                       as.character(quarter_start_date),
                                       " and ",as.character(reporting_asof_date)),
                                "I don't know--or I want to edit existing data and also enter new data")
        timeseries_choices <- setNames(1:3,timeseries_choices)
      } else {
        timeseries_choices <- c("I am editing timeseries data"=3)
        timeseries_choices_selected <- 3
        DASH_DATA_EDITING_TIMESERIES_CHOICE(3)
      }
      
      m <- modalDialog(id="dash_edit_modal",
                       div(style="background-color:white;padding:5px;overflow-y:auto;height:400px;",
                           panel(
                             fluidPage(
                               fluidRow(column(12,
                                               radioButtons(inputId="rsfdash_editing_timeseries_choice",
                                                            label="How do you want to report timeseries data?",
                                                            width="100%",
                                                            choices=timeseries_choices,
                                                            selected=timeseries_choices_selected))),
                               fluidRow(column(12,
                                               textInput(inputId="rsfdash_editing_filename",
                                                         label="Save-as Excel Filename",
                                                         width="100%",
                                                         placeholder = paste0("Default Filename: ",default_filename," (Leave blank to use default)")))),
                               #textOutput(outputId="dataset_upload_program_name",inline=TRUE)))),
                               fluidRow(column(12,
                                               textAreaInput(inputId="dataset_upload_source_note",
                                                             label="Notes:",
                                                             width="100%",
                                                             height="100px",
                                                             placeholder="Enter any reference notes about this editing session")
                               ))))),
                       title=div(style="display:inline-block;",
                                 div("Start Editing Session: ",
                                     textOutput(outputId="dataset_upload_program_name",inline = T))),
                       footer=div(style="display:inline-block;width:100%;",
                                  div(style="display:inline-block;float:left;",
                                      actionButton(inputId="dash_edit_modal_cancel",
                                                   label="Cancel",
                                                   class="btn-primary btn-danger")),
                                  div(style="display:inline-block;float:right;",
                                      actionButton(inputId="dash_edit_modal_begin",
                                                   label="Begin",
                                                   class="btn-primary btn-success"))),
                       size="m")
      
      DASH_DATA_EDITING_NEW_DATA(NULL)
      
      showModal(m)
    }  
  })
  
  observeEvent(input$rsfdash_browser_cell_clicked, {
    
    clicked_cell <- input$rsfdash_browser_cell_clicked
    
    if (!isTruthy(clicked_cell) || length(clicked_cell) == 0) return (NULL)
    
    rsf_data <- DASH_DATA()
    if (!isTruthy(rsf_data)) return (NULL)
    
    rsf_family <- DASH_RSF_FAMILY()
    
    clicked_data <- DASH_DATA_GET_CLICK(clicked_cell=clicked_cell,
                                        display_cell="html")
    
    if (!isTruthy(clicked_data)) {
      if (DASH_DATA_EDITING()==TRUE) DASH_DATA_CELL_INFO("Edits disallowed for this cell")
      else DASH_DATA_CELL_INFO("No info available for this cell")
      
      return (NULL)
    }
    
    #browser()
    
    data_point <- rsf_data[data_id==clicked_data$data_id &
                             reporting_asof_date==clicked_data$reporting_asof_date,
                           .(reporting_asof_date,
                             reporting_intraperiod_rank,
                             rsf_pfcbl_id,
                             data_type,
                             indicator_name,
                             data_asof_date,
                             data_id,
                             data_value,
                             flagstext)]
    
    data_point <- unique(data_point) #because when history_reporting is true, then duplicate for history and non-history entries.
    
    #Means this entity never reported on this indicator before, so it's NULL
    #And therefore no info to collect about it
    info <- ""
    if (empty(data_point) || !isTruthy(data_point$data_id)) {
      info <- paste0(clicked_data$indicator_name," has never been reported")
      
    } else {
      id_point <- rsf_family[data_point[,.(rsf_pfcbl_id,reporting_asof_date)],
                             on=.(rsf_pfcbl_id,
                                  reporting_asof_date),
                             nomatch=NULL]
      
      flagstext <- data_point$flagstext
      
      if (isTruthy(flagstext)) flagstext <- paste0(flagstext,collapse="<br>")
      else flagstext <- ""
      
      info <- paste0(id_point$rsf_full_name)
      
      if (id_point$status != "ACTIVE") {
        info <- paste0(info," ",
                       "<b>",id_point$status,
                       ifelse(id_point$status=="INACTIVE",""," REPORTING"),"</b> ",
                       ifelse(data_point$reporting_asof_date != data_point$data_asof_date,
                              paste0("<u>data historically reported</u> in ",as.character(data_point$data_asof_date)),
                              paste0("in ",as.character(data_point$data_asof_date))))
        
      } else if (data_point$reporting_asof_date != data_point$data_asof_date) {
        info <- paste0(info," ",
                       "<b>HISTORICALLY REPORTED</b> in ",
                       as.character(data_point$data_asof_date))
      }
      
      info <- paste0(info,": ",
                     data_point$indicator_name," = ",
                     data_point$data_value)
      
      if (nchar(flagstext) > 0) {
        info <- paste0(info,"<br>",
                       flagstext)
      }
      
      
      #browser()
      
    }
    DASH_DATA_CELL_INFO(info)
  }, ignoreInit = TRUE)
  
  observeEvent(DASH_DATA_EDITING(), {
    
    if (DASH_DATA_EDITING()==TRUE) {
      shinyjs::addClass(id="rsfdash_panel_browser",
                        class="panel-border-editing")
      DASH_DATA_CELL_INFO("<div><b>double-click</b> cell to edit its value.</div>")
    } else {
      shinyjs::removeClass(id="rsfdash_panel_browser",
                           class="panel-border-editing")
      DASH_DATA_CELL_INFO("")
      DASH_DATA_EDITING_TIMESERIES_CHOICE(NA)
    }
  },
  ignoreInit = TRUE)
  
  observeEvent(input$rsfdash_browser_cell_edit, {
    
    if (DASH_DATA_EDITING()==TRUE) {
      
      timeseries_choice <- DASH_DATA_EDITING_TIMESERIES_CHOICE()
      if (!isTruthy(timeseries_choice)) timeseries_choice <- 1 #Shouldn't happen.  But default to 1: edit time in which it was submitted.
      
      #1: reporting_asof_date is the time that the cell was edited
      #2: reporting_asof_date is the currently selected drop-down filter date
      #3: reporting_asof_date is the reporting_asof_date of the edited cell (3 should only be available for timeseries data that will have multiple dates)
      #browser()
      clicked_cell <- input$rsfdash_browser_cell_edit
      clicked_data <- DASH_DATA_GET_CLICK(clicked_cel=clicked_cell,
                                          display_cell="edit")
      
      if (is.na(clicked_data$data_asof_date)) clicked_data[,data_asof_date:=reporting_asof_date]
      
      if (timeseries_choice==1) clicked_data[,reporting_asof_date:=data_asof_date]
      else if (timeseries_choice==2) clicked_data[,reporting_asof_date:=reporting_asof_date] #Available in current timeseries view
      else if (timeseries_choice==3) clicked_data[,reporting_asof_date:=reporting_asof_date] #Available in changes timeseries view
      
      current_data <- DASH_DATA_EDITING_NEW_DATA()
      
      
      #Below: if user re-edits the same cell twice, then discard the previous change and keep only the current change
      if (!empty(current_data) 
          && 
          !empty(current_data[reporting_SYSID==clicked_data$reporting_SYSID &
                              indicator_name==clicked_data$indicator_name &
                              reporting_asof_date==clicked_data$reporting_asof_date &
                              reporting_intraperiod_rank==clicked_data$reporting_intraperiod_rank])) {
        
        current_data <- current_data[!(reporting_SYSID==clicked_data$reporting_SYSID &
                                         indicator_name==clicked_data$indicator_name &
                                         reporting_asof_date==clicked_data$reporting_asof_date &
                                         reporting_intraperiod_rank==clicked_data$reporting_intraperiod_rank)]
        
      }
      
      DASH_DATA_EDITING_NEW_DATA(rbindlist(list(current_data,
                                                clicked_data)))
      
      print(DASH_DATA_EDITING_NEW_DATA())
    } else {
      return (showNotification(type="error",ui=h2("Edits can only be made during an editing session.  Click 'EDIT' to start session")))
    }
  })
  
  observeEvent(input$action_dfsdash_editmode_discard, {
    
    #TODO: Confirmation dialoguq 
    # if (isTruthy(DASH_DATA_EDITING_NEW_DATA())) {
    #   
    # }
    
    shinyjs::show(id="panel_actions_viewing")
    shinyjs::hide(id="panel_actions_editing")
    DASH_DATA_EDITING(FALSE)
    DASH_DATA_EDITING_NEW_DATA(NULL)
    
  })
  
  observeEvent(input$action_dfsdash_editmode_save, {
    
    
    if (DASH_DATA_EDITING()==FALSE) return (NULL)
    
    editing_filename <- input$rsfdash_editing_filename
    #browser()
    if (!isTruthy(editing_filename)) {
      editing_filename <- DASH_DATA_EDITING_DEFAULT_FILENAME()
    }  
    
    editing_filename <- basename(editing_filename)
    editing_filename <- gsub("[^[:alnum:]_\\+\\-]"," ",editing_filename)
    editing_filename <- gsub("[[:space:]]+"," ",editing_filename)
    editing_filename <- trimws(editing_filename)
    
    if (!isTruthy(editing_filename) || nchar(editing_filename) < 5) {
      editing_filename <- DASH_DATA_EDITING_DEFAULT_FILENAME()
      if (!isTruthy(editing_filename)) editing_filename <- "RSF EDITS"
    } else {
      
      if (!isTruthy(file_ext(editing_filename)) || file_ext(editing_filename) != "xlsx") {
        editing_filename <- paste0(editing_filename,".xlsx")
      }
      
    }
    
    program_indicators <- DASH_INDICATORS()
    #dd_dt <- DASH_DATA_DISPLAY()
    #editing <<- as.data.frame(DASH_DATA_EDITING_NEW_DATA())
    #pri <<- as.data.frame(program_indicators)
    #dd_df <<- as.data.frame(dd_dt)
    #warning("<<- observeEvent(input$rsfdash_browser_cell_edit")
    #edit_dt <- as.data.table(editing)
    #program_indicators <- as.data.table(pri)
    #dd_dt <- as.data.table(dd_df)
    
    edit_dt <- DASH_DATA_EDITING_NEW_DATA()
    print("TODO: observeEvent(input$action_dfsdash_editmode_save: if any intraperiod data is exported, include its rank AND ALSO other intraperiod and current data!")
    
    if (!isTruthy(edit_dt)) return (NULL)
    
    if (!isTruthy(edit_dt)) {
      shinyjs::show(id="panel_actions_viewing")
      shinyjs::hide(id="panel_actions_editing")
      DASH_DATA_EDITING(FALSE)
      DASH_DATA_EDITING_NEW_DATA(NULL)
      
      return(showNotification(type="message",ui=h2("No changes made.  Nothing to save.")))
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
      
      edit_dt[program_indicators,
              `:=`(pfcbl_category=i.data_category,
                   indicator_id=i.indicator_id),  #adding indicator_id as going to be saved for posterity; and indicator_name could be modified over time, causing future upload to fail
              on=.(indicator_name)]
      
      setorder(edit_dt,
               pfcbl_category,
               reporting_SYSID,
               reporting_asof_date,
               reporting_intraperiod_rank,
               indicator_name)
      
      # edit_dt[,`:=`(reporting_rowid=NULL,
      #          reporting_group=NULL,
      #          reporting_history=NULL,
      #          reporting_group_sort=NULL)]
      
      if (all(edit_dt$reporting_intraperiod_rank)==0) {
        edit_dt[,reporting_intraperiod_rank:=NULL]
      }
      
      setnames(edit_dt,
               old="cell_value",
               new="data_value")
      
      pfcbl_ids <- DBPOOL %>% db_rsf_get_pfcbl_from_sys_ids(sys_ids=unique(edit_dt[,.(reporting_SYSID,pfcbl_category)]),
                                                            include.sysnames = T)
      
      incProgress(amount=0.10,message="Validating edits...")
      
      edit_dt[pfcbl_ids,
              `:=`(rsf_pfcbl_id=i.rsf_pfcbl_id,
                   SYSNAME=i.sys_name),
              on=.(reporting_SYSID,
                   pfcbl_category)]
      
      if (anyNA(edit_dt$rsf_pfcbl_id)) {
        bad_edits <- edit_dt[is.na(rsf_pfcbl_id)]
        showNotification(type="error",
                         ui=div(h2("Failed to create new: ",paste0(bad_edits[,unique(pfcbl_category)],collapse=" AND ALSO ")),
                                p("To create new entities, download the dataset, add new lines in Excel with an empty SYSID column"),
                                p("The following edits will be DISCARDED:"),
                                p(paste0(bad_edits[,paste0(indicator_name,"=",data_value)],collapse="<BR>"))),
                         duration=NULL)
      }
      edit_dt <- edit_dt[is.na(rsf_pfcbl_id)==FALSE]
      
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
                                          status_message=progress_status_message)
      },
      warning=function(w) {
        showNotification(type="error",
                         ui=h3("Failed to save: ",conditionMessage(w)))
        return (NULL)
      },
      error=function(e) {
        showNotification(type="error",
                         ui=h3("Failed to save: ",conditionMessage(e)))
        return (NULL)
      })
      
      file.remove(editing_filename)
      return(results)
    })
    
    if (!is.null(results)) {
      shinyjs::show(id="panel_actions_viewing")
      shinyjs::hide(id="panel_actions_editing")
      DASH_DATA_EDITING(FALSE)
      DASH_DATA_EDITING_NEW_DATA(NULL)
      DASH_DATA_REFRESH(DASH_DATA_REFRESH()+1)
      LOAD_REPORTING_COHORT(results[nrow(results),reporting_cohort_id])
      
      if (!any(as.character(unique(results$reporting_asof_date)) %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) {
        new_dates <- setdiff(sort(as.character(unique(results$reporting_asof_date))),as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))
        LOAD_VALID_REPORTING_DATE(new_dates)
      }
    }
    
    removeModal()
    
  })
}