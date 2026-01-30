DATASET_UPLOAD_FILE <- reactiveVal(NA)
#DATASET_UPLOAD_FILE_RETURN_RESULTS <- reactiveVal(NA)

COHORT_NEW_ID <- reactiveVal(NA)
#COHORT_TEMPLATES <- reactiveVal(NA)


UPLOADED_COHORT <- reactive({ 
  new_cohort_id <- as.numeric(COHORT_NEW_ID())
  if (!isTruthy(new_cohort_id) || new_cohort_id ==-1) return (NULL)
  if (!isTruthy(COHORTS_LIST())) return (NULL)
  
  uploaded_cohort <- COHORTS_LIST()[reporting_cohort_id==new_cohort_id] 
  return (uploaded_cohort)
})

observeEvent(input$action_dataset_upload_close, { 
  
  shinyjs::reset(id="dataset_upload_file")
  shinyjs::reset(id="dataset_upload_source_note")
  COHORT_NEW_ID(NA)
  #COHORT_TEMPLATES(NA)
  DATASET_UPLOAD_FILE(NA)
  STATUS_MESSAGE_PANEL$container_id <- "dataset_upload_log_container"
  STATUS_MESSAGE_PANEL$panel_id <- "dataset_upload_log"
  removeModal()
})

observeEvent(input$action_template_upload_new, { 
  
  shinyjs::reset(id="dataset_upload_file")
  shinyjs::reset(id="dataset_upload_source_note")
  COHORT_NEW_ID(NA)
  #COHORT_TEMPLATES(NA)
  DATASET_UPLOAD_FILE(NA)
  STATUS_MESSAGE_PANEL$container_id <- "dataset_upload_log_container"
  STATUS_MESSAGE_PANEL$panel_id <- "dataset_upload_log"
  
  m <- modalDialog(id="dataset_upload",
                   div(id="dataset_upload_panel1",
                       div(style="background-color:white;padding:5px;overflow-y:auto;height:400px;",
                           panel(id="dataset_upload_panel1",
                                 fluidPage(
                                   fluidRow(column(12,
                                                   p("Upload to RSF Program: ",textOutput(outputId="dataset_upload_program_name",inline=TRUE)))),
                                   fluidRow(column(12,
                                                   fileInput(inputId="dataset_upload_file",
                                                             label="Upload File",
                                                             width="100%",
                                                             multiple=TRUE))),
                                   fluidRow(column(12,
                                                    hidden(selectizeInput(inputId="dataset_upload_rsf_facility_id",
                                                                   label="Select Facility",
                                                                   choices="",
                                                                   selected="",
                                                                   options=list(placeholder="Select facility for RSA"))
                                                    ))),
                                   fluidRow(column(12,
                                                   textAreaInput(inputId="dataset_upload_source_note",
                                                                 label="Notes:",
                                                                 width="100%",
                                                                 height="75px",
                                                                 placeholder="Enter any reference notes about this template")
                                   )))))),
                   hidden(div(id="dataset_upload_panel2",
                       div(style="background-color:white;padding:2px;height:400px;",
                            div(id="dataset_upload_log_container",
                                style="width:100%;height:100%;background-color:black;color:limegreen;overflow-y:auto;padding:3px;",
                                div(id="dataset_upload_log")
                            )))),
                   
                   title=textOutput(outputId="dataset_upload_title",inline=TRUE),
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  actionButton(inputId="action_dataset_upload_close",
                                               label="Cancel",
                                               class="btn-primary btn-danger")),
                              div(style="display:inline-block;float:right;",
                                  div(id="dataset_upload_nav1",
                                      disabled(actionButton(inputId="modal_dataset_upload_next",
                                               label=paste0("Next ",as.character(intToUtf8(0x25BC))),
                                               class="btn-primary")))),
                                  hidden(div(id="dataset_upload_nav2",
                                    div(style="display:inline-block;",
                                              actionButton(inputId="modal_dataset_upload_dashboard",
                                                           class="btn btn-primary btn-success",
                                                           style="display:inline-block;",
                                                           disabled="disabled",
                                                           label="View Dashboard")),
                                              actionButton(inputId="modal_dataset_upload_dataset",
                                                           class="btn btn-primary btn-success",
                                                           style="display:inline-block;",
                                                           disabled="disabled",
                                                           label="View Dataset")))),
                   size="m")
  
  showModal(m)
  
},ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$modal_dataset_upload_dashboard, {
  
  new_cohort_id <- COHORT_NEW_ID()
  
  if (isTruthy(new_cohort_id) && new_cohort_id >-1) {

    SERVER_DATASETS_COHORT_DASHBOARD(selected_cohort_id=new_cohort_id)
  }
  
  removeModal()
})
observeEvent(input$modal_dataset_upload_dataset, {
  new_cohort_id <- COHORT_NEW_ID()
  
  if (isTruthy(new_cohort_id) && new_cohort_id >-1) {
    shinyjs::runjs(paste0("Shiny.setInputValue(\"action_cohort_view\",",new_cohort_id,",{priority:\"event\"})"))
    
    # updateTabsetPanel(session=session,inputId="datasetsTabset",selected="review")
    # updateSelectizeInput(inputId="datasets_review_select",
    #                      session=session,
    #                      selected=new_cohort_id)
  }
  removeModal()
})

observeEvent(input$modal_dataset_upload_next, {

  filename <- DATASET_UPLOAD_FILE() #! May be multiple files uploaded!
  #new_cohort_id <- COHORT_NEW_ID()
  
  if (!isTruthy(filename)) {
    showNotification(type="error",
                     h2("An error has occurred.  Please re-try upload"))
    removeModal()
    return (NULL)
  }
  #return_method <- as.character(input$dataset_upload_return_method)
  #if (!isTruthy(return_method)) return_method <- "email"
  

  #print(paste0("UPLOAD PANEL 2: Parsing ",filename))
  disable(id="action_dataset_upload_close")
  disable(id="modal_dataset_upload_next")
  
  hideElement(id="dataset_upload_nav1",
              anim=TRUE,
              animType = "fade")
  hideElement(id="dataset_upload_panel1",
              anim=TRUE,
              animType = "slide")
  showElement(id="dataset_upload_panel2",
              anim=TRUE,
              animType = "slide")
  
  COHORT_NEW_ID(NA)
  #COHORT_TEMPLATES(NA)
  #DATASET_UPLOAD_FILE_RETURN_RESULTS(NA)

  #NA allows for create program scripts to run
  
  parse_rsf_pfcbl_id <- NULL
  if (isTruthy(as.numeric(input$dataset_upload_rsf_facility_id))) {
    parse_rsf_pfcbl_id <- as.numeric(input$dataset_upload_rsf_facility_id)
  }
  
  source_name <- input$dataset_upload_source_name
  source_note <- input$dataset_upload_source_note
  
  if (!isTruthy(source_name)) source_name = "Name: Unspecified"
  if (!isTruthy(source_note)) source_note <- NA
  
  results <- tryCatch({  

    #Template gets its own database pool
    template_parse_process_and_upload(pool=dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]])),
                                      reporting_user_id=USER_ID(),
                                      template_files=filename,
                                      parse_rsf_pfcbl_id=parse_rsf_pfcbl_id,
                                      source_note=source_note,
                                      delete_after_upload=TRUE,
                                      status_message=status_message)
  },
  warning=function(war) {
    print(conditionMessage(war));
    status_message(class="error",conditionMessage(war),"\n")
    showNotification("Error: Unable to load template.  File may not be a properly formatted template.  Please check upload log for details.")
    NULL
  },
  error=function(err) {
    print(conditionMessage(err));
    status_message(class="error",conditionMessage(err),"\n")
    showNotification("Error: Unable to load template.  File may not be a properly formatted template.  Please check upload log for details.")
    NULL
  })
  
  enable(id="action_dataset_upload_close")
  
  updateActionButton(session=session,
                     inputId="action_dataset_upload_close",
                     label="Close")
  
  DATASET_UPLOAD_FILE(NA) #Makes it not Truthy and will re-call this reactive, skipping this block and going into "else if" below
  
  if (empty(results)) {
    COHORT_NEW_ID(NA) 
    REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1) #failed cohorts will leave an artifact, generally, which should be viewable.
    #COHORT_TEMPLATES(NA)
  } else {
    #reporting_cohort_ids <- sapply(up_template,function(x) x[['reporting_cohort']][['reporting_cohort_id']])
    
    last_import_id <- results[nrow(results),import_id]
    import_ids <- DBPOOL %>% dbGetQuery("
      select 
      ids.rsf_pfcbl_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_client_id
      from p_rsf.reporting_imports ri
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ri.import_rsf_pfcbl_id
      where ri.import_id = $1::int",
      params=list(last_import_id))
    
    #print(paste0("SETTING NEW/LAST cohort_id=",last_cohort_id))
    #If we uploaded a dataset while the selected program is empty, then it means we've created a new program. So let's load it now.
    if (empty(SELECTED_PROGRAM()) || !identical(as.numeric(SELECTED_PROGRAM_ID()),as.numeric(import_ids$rsf_program_id))) {
      
      if (!as.numeric(import_ids$rsf_program_id) %in% USER_PROGRAMS()$rsf_pfcbl_id) {
        LOAD_PROGRAM_ID(import_ids$rsf_program_id)
      }
      
      updateSelectizeInput(session=session,
                           inputId="select_rsf_program_id",
                           selected=as.numeric(import_ids$rsf_program_id))
    }
    
    COHORT_NEW_ID(last_import_id)
    LOAD_IMPORT(last_import_id)
    
    if (!any(as.character(unique(results$reporting_asof_date)) %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) {
      new_dates <- setdiff(sort(as.character(unique(results$reporting_asof_date))),as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))
      LOAD_VALID_REPORTING_DATE(new_dates)
    }
    
    showElement(id="dataset_upload_nav2",
                anim=TRUE,
                animType = "fade")
    enable(id="modal_dataset_upload_dataset")
    enable(id="modal_dataset_upload_dashboard")

    
    if (is.na(as.numeric(input$dataset_review_filter_facility)) || 
        !any(as.numeric(unlist(import_ids)) %in% as.numeric(input$dataset_review_filter_facility))) {
      
    if (any(import_ids$rsf_facility_id %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,na.rm=T)) {
        setDT(import_ids)
        updateSelectInput(session=session,
                          inputId="dataset_review_filter_facility",
                          selected = import_ids[rsf_facility_id %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,rsf_pfcbl_id][[1]])
      } 
    }
  }
  
},priority = 100)


output$dataset_upload_program_name <- renderText({ 
  if (!isTruthy(SELECTED_PROGRAM())) return ("No program selected")
  else return (SELECTED_PROGRAM()$program_name)
})

output$dataset_upload_title <- renderText({
  if (!isTruthy(SELECTED_PROGRAM())) return ("Upload Dataset: No RSF Program Selected")
  else return(paste0("Upload Dataset: ",SELECTED_PROGRAM()$program_name))
})

observeEvent(input$dataset_upload_file, {
  
  upload <- input$dataset_upload_file
  if (!isTruthy(upload)) return (NULL)
  
  #print("Uploading dataset")
  filename <- upload$name
  datapath <- upload$datapath
  copypath <- paste0(dirname(upload$datapath),"/", upload$name)
  if (all(file.copy(upload$datapath, copypath)))
  {
    print(paste0("File renamed FROM: ",datapath," TO: ",copypath))
    datapath <- copypath
  }
  
  
  if (!all(file_ext(filename) %in% c("xlsx","csv","zip","pdf"))) {
    return (showNotification(h1("Error: file must be Excel format .xlsx only (not .xls, .xlsxm or .xlsxb); or .csv with client name and date in filename; or a zip of these"),
                     closeButton = TRUE,
                     duration=8,
                     type="error"))
  }

  DATASET_UPLOAD_FILE(datapath)
  
  if (any(file_ext(filename) %in% "pdf")) {
    showElement(id="dataset_upload_rsf_facility_id",
                anim=TRUE,
                time=1.0)
    
    facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
    if (!isTruthy(facilities)) {
      return(showNotification(type="error",
                       ui=h3("RSAs are the only PDF documents that may be uploaded: selecting the facility is required")))
      
    } else {
      
      updateSelectInput(session=session,
                        inputId="dataset_upload_rsf_facility_id",
                        choices = c("",setNames(facilities$rsf_facility_id,
                                                facilities$facility_name)))
    }
  } else {  
    enable(id="modal_dataset_upload_next")
  }
}, ignoreInit=TRUE)

observeEvent(input$dataset_upload_rsf_facility_id, {
  rsf_facility_id <- as.numeric(input$dataset_upload_rsf_facility_id)
  if (isTruthy(rsf_facility_id)) {
    if (rsf_facility_id && rsf_facility_id %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_facility_id) {
      enable(id="modal_dataset_upload_next")
    } else {
      disable(id="modal_dataset_upload_next")
    }
  } else {
    disable(id="modal_dataset_upload_next")
  }
})