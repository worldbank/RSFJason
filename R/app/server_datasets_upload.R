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
                                                   textAreaInput(inputId="dataset_upload_source_note",
                                                                 label="Notes:",
                                                                 width="100%",
                                                                 height="100px",
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

    uploaded_cohort <- DBPOOL %>% dbGetQuery("select 
                                              rc.reporting_cohort_id,
                                              rc.reporting_asof_date,
                                              rc.from_reporting_template_id, 
                                              rt.default_report_id,
                                              rc.reporting_rsf_pfcbl_id
                                              from p_rsf.reporting_cohorts rc
                                              left join p_rsf.reporting_templates rt on rt.template_id = rc.from_reporting_template_id
                                              where rc.reporting_cohort_id = $1::int",
                                             params=list(new_cohort_id))
    
    uploaded_clients <- DBPOOL %>% dbGetQuery("select rsf_pfcbl_id
                                               from p_rsf.get_rsf_pfcbl_id_family_tree(string_to_array($1::text,',')::int[]) ft
                                               where ft.pfcbl_category = 'client'",
                                              params=list(uploaded_cohort$reporting_rsf_pfcbl_id))
    
    uploaded_indicators <- DBPOOL %>% dbGetQuery("select distinct
                                                   rd.indicator_id
                                                 from p_rsf.reporting_cohorts rc
                                                 inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
                                                 inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
                                                 where rc.reporting_cohort_id = $1::int
                                                   and ind.is_system = false",
                                                 params=list(new_cohort_id))
    
      
    DASH_LOAD_DASHBOARD(reporting_asof_date=uploaded_cohort$reporting_asof_date,
                        rsf_pfcbl_ids=unique(uploaded_clients$rsf_pfcbl_id),
                        indicator_ids=unique(uploaded_indicators$indicator_id),
                        display_report_id=uploaded_cohort$default_report_id,
                        display_currency="LCU",
                        display_timeline=FALSE)
  }
  
  removeModal()
})
observeEvent(input$modal_dataset_upload_dataset, {
  new_cohort_id <- COHORT_NEW_ID()
  
  if (isTruthy(new_cohort_id) && new_cohort_id >-1) {
    updateTabsetPanel(session=session,inputId="datasetsTabset",selected="review")
    updateSelectizeInput(inputId="datasets_review_select",
                         session=session,
                         selected=new_cohort_id)
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
  rsf_program_id <- NA
  if (isTruthy(SELECTED_PROGRAM_ID())) rsf_program_id <- SELECTED_PROGRAM_ID()

  source_name <- input$dataset_upload_source_name
  source_note <- input$dataset_upload_source_note
  
  if (!isTruthy(source_name)) source_name = "Name: Unspecified"
  if (!isTruthy(source_note)) source_note <- NA
  
  results <- tryCatch({  

    #Template gets its own database pool
    template_parse_process_and_upload(pool=dbStart(credentials_file=paste0(getwd(),LOCATIONS[[LOCATION]])),
                                      rsf_program_id=rsf_program_id,
                                      reporting_user_id=USER_ID(),
                                      template_files=filename,
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
  
  print("Upload done!")
  
  enable(id="action_dataset_upload_close")
  
  updateActionButton(session=session,
                     inputId="action_dataset_upload_close",
                     label="Close")
  
  DATASET_UPLOAD_FILE(NA) #Makes it not Truthy and will re-call this reactive, skipping this block and going into "else if" below
  
  if (empty(results)) {
    COHORT_NEW_ID(NA) 
    #COHORT_TEMPLATES(NA)
  } else {
    #reporting_cohort_ids <- sapply(up_template,function(x) x[['reporting_cohort']][['reporting_cohort_id']])
    
    last_cohort_id <- results[nrow(results),reporting_cohort_id]
    #print(paste0("SETTING NEW/LAST cohort_id=",last_cohort_id))

    
    #If we uploaded a dataset while the selected program is empty, then it means we've created a new program. So let's load it now.
    if (empty(SELECTED_PROGRAM())) {
      new_program_id <- results[!is.na(rsf_program_id),unique(rsf_program_id)]
      
      LOAD_PROGRAM_ID(new_program_id)
      
    }
    
    COHORT_NEW_ID(last_cohort_id)
    LOAD_REPORTING_COHORT(last_cohort_id)
    
    if (!any(as.character(unique(results$reporting_asof_date)) %in% as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))) {
      new_dates <- setdiff(sort(as.character(unique(results$reporting_asof_date))),as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES()))
      LOAD_VALID_REPORTING_DATE(new_dates)
    }
    
    showElement(id="dataset_upload_nav2",
                anim=TRUE,
                animType = "fade")
    enable(id="modal_dataset_upload_dataset")
    enable(id="modal_dataset_upload_dashboard")
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



observeEvent(c(DATASET_UPLOAD_FILE(),
               SELECTED_PROGRAM(),
               COHORT_NEW_ID()), { 
                 

},
ignoreNULL = FALSE,ignoreInit = TRUE,priority=1)

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
  
  if (!all(file_ext(filename) %in% c("xlsx","csv","zip"))) {
    showNotification(h1("Error: file must be Excel format .xlsx only (not .xls, .xlsxm or .xlsxb); or .csv with client name and date in filename; or a zip of these"),
                     closeButton = TRUE,
                     duration=8,
                     type="error")
  } else {
    DATASET_UPLOAD_FILE(datapath)
    enable(id="modal_dataset_upload_next")
  }
}, ignoreInit=TRUE)

