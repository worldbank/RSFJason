SERVER_ADMIN_CHECKS_REVIEW.RESULTS <- reactiveVal(data.table())
SERVER_ADMIN_CHECKS_REVIEW__CHECK_FORMULA_ID <- reactiveVal(as.numeric(NA))
SERVER_ADMIN_CHECKS_REVIEW.VIEW <- reactive({
  
  results <- SERVER_ADMIN_CHECKS_REVIEW.RESULTS()
  if (empty(results)) return (NULL)
  
  fac_filter <- as.numeric(input$server_admin_checks_review__filter_facilities)
  fl_filter <- as.logical(input$server_admin_checks_review__filter_flags)
  
  if (length(fac_filter)==0) fac_filter <- as.logical(NA)
  if (length(cl_filter)==0) cl_filter <- as.numeric(NA)
  
  if (isTruthy(fac_filter)) {
    
    rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
      select distinct ft.to_family_rsf_pfcbl_id
      from p_rsf.view_rsf_pfcbl_id_family_tree ft 
      where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    ",params=list(paste0(fac_filter,collapse=",")))
    
    rsf_pfcbl_ids <- as.numeric(unlist(rsf_pfcbl_ids))
    
    results <- results[SYSID %in% rsf_pfcbl_ids]
    
  }
  
  if (!is.na(fl_filter) && fl_filter==TRUE) {
    results <- results[flag_status==TRUE]
  } else if (!is.na(fl_filter) && fl_filter==FALSE) {
    results <- results[flag_status==FALSE]
  }
  
  if (empty(results)) return (data.frame(Error="This selection has no data to display"))
  else return (results)
})

observeEvent(input$server_admin_checks_review__test_facilities_list, { 
  test_rsf_pfcbl_ids <- as.numeric(input$server_admin_checks_review__test_facilities_list)
  
  dates <- NULL
  if (isTruthy(test_rsf_pfcbl_ids)) {
    dates <- DBPOOL %>% dbGetQuery("
      select distinct
        rpr.reporting_asof_date::text
      from p_rsf.rsf_pfcbl_ids ids
      inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
      where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
        and rpr.reporting_asof_date <= now()::date",
      params=list(paste0(test_rsf_pfcbl_ids,collapse=",")))
    
    dates <- c("",sort(dates$reporting_asof_date))
  } else {
    dates <- c("",sort(as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES())))
  }
  
  #Indicator review reporting dates filters review for calculated indicators, for relevant years where selected client to be reviewed has reported.
  updateSelectizeInput(session=session,
                       inputId="server_admin_checks_review__test_reporting_date",
                       choices=dates,
                       selected = dates[[length(dates)]])
})



show_modal_server_admin_checks_review <- function(rsf_pfcbl_id,
                                                  review_asof_date,
                                                  review_check_formula_id) {
  
  
  review_check_formula_id <- as.numeric(review_check_formula_id)
  if (!isTruthy(review_check_formula_id)) {
    return(showNotification(type="error",
                            ui=h3("Error: no check formula ID is defined for review panel")))
  } else if (length(review_check_formula_id) != 1) {
    return(showNotification(type="error",
                            ui=h3("Error: only one check formula ID allowed for review panel")))
  }
  
  STATUS_MESSAGE_PANEL$container_id <- "check_test_log_container"
  STATUS_MESSAGE_PANEL$panel_id <- "check_test_log"
  SERVER_ADMIN_CHECKS_REVIEW.RESULTS(data.table())
  SERVER_ADMIN_CHECKS_REVIEW__CHECK_FORMULA_ID(review_check_formula_id)
  
  facility_list <- c("",setNames(SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,
                               SELECTED_PROGRAM_FACILITIES_LIST()$facility_name))
  
  if (!isTruthy(rsf_pfcbl_id)) rsf_pfcbl_id <- ""
  if (!isTruthy(review_asof_date)) review_asof_date <- ""
  
  review_asof_date <- as.character(review_asof_date)
  
  program_dates <- SELECTED_PROGRAM_VALID_REPORTING_DATES()
  program_dates <- sort(program_dates[program_dates <= as.character(today())])
  
  if (!isTruthy(review_asof_date)) {
    review_asof_date <- program_dates[length(program_dates)]
  }
  
  modal_check_test_start <- modalDialog(title="Check Test Calculation",
                                        footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                                                   modalButton("Cancel"),
                                                   enabled(state=FALSE,
                                                       actionButton(inputId="server_admin_checks_review__test_next",
                                                                    label="Next",
                                                                    class="btn-primary"))),
                                        div(style="background-color:white;padding:2px;height:400px;",
                                            
                                            fluidRow(style="padding-top:5px;",
                                                     column(6,
                                                            selectizeInput(inputId="server_admin_checks_review__test_facility_list",
                                                                           label="Client",
                                                                           choices=facility_list,
                                                                           selected=rsf_pfcbl_id,
                                                                           multiple=TRUE,
                                                                           options=list(placeholder="All projects selected..."))),
                                                     column(3,
                                                            selectizeInput(inputId="server_admin_checks_review__test_reporting_date",
                                                                           label="Reporting Date",
                                                                           choices=program_dates,
                                                                           selected=review_asof_date,
                                                                           options=list(placeholder="Select date.."))),
                                                     column(3,style='padding-left:10px;margin-top:25px;',
                                                            actionButton(inputId="server_admin_checks_review__test_start",
                                                                         label="Start Review",
                                                                         icon=icon("flask"),
                                                                         class="btn-success"))),
                                            div(id="check_test_log_container",
                                                style="width:100%;height:300px;background-color:black;color:limegreen;overflow-y:auto;padding:3px;",
                                                div(id="check_test_log"))
                                        ))
  showModal(modal_check_test_start)
}

observeEvent(input$server_admin_checks_review__test, {
  
  if (!isTruthy(SELECTED_PROGRAM_ID())) {
    return(showNotification(type="error",
                            h3("An RSF Program must be selected before testing a check formula, as it will be run on data for that program")))
  }
  test_check_formula_id <- as.numeric(input$server_admin_checks_review__test)
  if (!isTruthy(test_check_formula_id)) return (NULL)
  
  test_check_formula <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[check_formula_id==test_check_formula_id]
  if (empty(test_check_formula)) return (NULL)

  if (any(test_check_formula$edited==TRUE)) {
    return(showNotification(type="error",
                            h3("Formula has unsaved changes.  Save check formula before running test.")))
  }
  
  show_modal_server_admin_checks_review(rsf_pfcbl_id=NA,
                                        review_asof_date=NA,
                                        review_check_formula_id=test_check_formula_id)
  
},ignoreInit = TRUE)

observeEvent(input$server_admin_checks_review__test_start, {
  
  rsf_program_id <- SELECTED_PROGRAM_ID()
  test_rsf_pfcbl_ids <- as.numeric(input$server_admin_checks_review__test_facilities_list)
  check_test_reporting_date <- input$server_admin_checks_review__test_reporting_date
  test_check_formula_id <- as.numeric(SERVER_ADMIN_CHECKS_REVIEW__CHECK_FORMULA_ID())
  
  if (!isTruthy(test_check_formula_id)) return (NULL)
  
  #test_check_formula <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()[check_formula_id==test_check_formula_id]
  #if (empty(test_check_formula)) return (NULL)
  if (!isTruthy(rsf_program_id)) return (NULL)
  
  if (!isTruthy(check_test_reporting_date)) return (showNotification(type="error",h3("Reporting date must be selected")))
  else check_test_reporting_date <- as.Date(check_test_reporting_date)
  
  if (!isTruthy(test_rsf_pfcbl_ids)) test_rsf_pfcbl_ids <- NULL
  
  disable(id="server_admin_checks_review__test_start")
  disable(id="server_admin_checks_review__test_facilities_list")
  disable(id="server_admin_checks_review__test_reporting_date")
  
  status_message(class="info","Starting test.  This may take a few moments...\n")
  
  result <- tryCatch({
    DBPOOL %>% rsf_checks_do_test(rsf_program_id=rsf_program_id,
                                  pfcbl_ids.familytree=test_rsf_pfcbl_ids,
                                  reporting_current_date=check_test_reporting_date,
                                  check_formula_id=test_check_formula_id)
  },
  error = function(err) {
    status_message(class="error",
                   conditionMessage(err),"\n")
    NA
  },
  warning = function(war) {
    status_message(class="error",
                   conditionMessage(war),"\n")
    NA
  })
  
  if (all(is.na(result))) {
    status_message(class="info","CHECK RAISED ZERO FLAGS.  NOTHING TO REVIEW.")  
    SERVER_ADMIN_CHECKS_REVIEW.RESULTS(data.table())
    enable("server_admin_checks_review__test_next")
  } else if (!all(is.na(result))) {
    
    
    status_message(class="info","SUCCESS")  
    SERVER_ADMIN_CHECKS_REVIEW.RESULTS(result)
    enable("server_admin_checks_review__test_next")
  } else {
    status_message(class="info","Test failed due to an problem in the formula.  Review messages for a '^' that attempts to identify the location that is causing the problem.\n")
    updateActionButton(session=session,inputId="server_admin_checks_review__test_next",label="Close")
    enable(id="server_admin_checks_review__test_next")
    SERVER_ADMIN_CHECKS_REVIEW.RESULTS(data.table())
    SERVER_ADMIN_CHECKS_REVIEW__CHECK_FORMULA_ID(as.numeric(NA))
  }
  
  status_message(class="none","Test completed.  Please click NEXT to review results.\n")
  
},ignoreInit=TRUE)

observeEvent(input$server_admin_checks_review__test_next, {
  
  results <- SERVER_ADMIN_CHECKS_REVIEW.VIEW()
  if (!empty(results)) {
    
    check_test_reporting_date <- input$server_admin_checks_review__test_reporting_date
    check_test_reporting_date <- as.Date(check_test_reporting_date)
    
    test_check_formula_id <- as.numeric(input$server_admin_checks_review__test)
    selected_facility_review <- as.numeric(input$server_admin_checks_review__test_facility_list)
    
    check_formula <- DBPOOL %>% dbGetQuery("
      select 
        icf.formula,
        coalesce(icf.check_formula_title,'Formula') as check_formula_title
      from p_rsf.indicator_check_formulas icf
      where icf.check_formula_id = $1::int",
    params=list(test_check_formula_id))
    
    
    #check_formula <- check_formula$formula
    
    facility_choices <- c("",setNames(SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,
                                    SELECTED_PROGRAM_FACILITIES_LIST()$facility_name))
    
    if (!all(is.na(selected_facility_review))) {
      review_facilities <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_pfcbl_id %in% selected_facility_review,
                                                           .(rsf_pfcbl_id,
                                                             facility_name)]
      facility_choices <- c("",setNames(review_facilities$rsf_pfcbl_id,
                                        review_facilities$facility_name))
    }
    
    
    m <- modalDialog(title="Check Review Results",size="l",
                     div(style="background-color:white;padding:2px;height:700px;",
                         fluidRow(column(8,selectizeInput(inputId="server_admin_checks_review__filter_facilities",
                                                          label="Filter Projects",
                                                          width="100%",
                                                          choices=facility_choices,
                                                          selected="",
                                                          multiple=TRUE,
                                                          options=list(placeholder="Filter by project, or leave blank for all"))),
                                  column(2,selectizeInput(inputId="server_admin_checks_review__filter_flags",
                                                          label="Filter Flagged",
                                                          choices=c(`No Filter`="",
                                                                    `Flagged`="TRUE",
                                                                    `Unflagged`="FALSE"),
                                                          selected="")),
                                  column(2,align="right",
                                         downloadButton(outputId="server_admin_checks_review__download",label="Download",icon=icon("file-excel"),class="btn-success"))),
                         fluidRow(column(12,paste0(check_formula$check_formula_title,": ",check_formula$formula))),
                         fluidRow(column(12,DT::dataTableOutput(outputId="server_admin_checks_review__table",height="625px")))))
    
    showModal(m)
  } else { removeModal() }
  
},ignoreInit = TRUE)

output$server_admin_checks_review__table <- DT::renderDataTable({
  

    check_results <- SERVER_ADMIN_CHECKS_REVIEW.VIEW()
    
    if (empty(check_results)) return (data.frame(Error="This selection has no data to display"))
    

  DT::datatable(check_results,
                rownames = FALSE,
                fillContainer=TRUE,
                escape = FALSE,
                options=list(
                  dom="tip",
                  bSort=F,
                  scrollY="auto",
                  scrollX="auto",
                  scrollCollapse=TRUE,
                  ordering=F,
                  paging=TRUE,
                  pageLength=15,
                  columnDefs = list(list(className = 'dt-left', targets = c(0,1,2)),  #Zero-based targets
                                    list(className = 'dt-center', targets = c(3)))
                )) %>%
    formatStyle(columns=c(0),width="50px") %>%
    formatStyle(columns=c(1:ncol(check_results)),whiteSpace="nowrap",maxWidth="200px;",overflow="hidden",textOverflow="ellipsis")
  
})

output$server_admin_checks_review__download <- downloadHandler(
  filename = function() { 
    nickname <- isolate({ SELECTED_PROGRAM()$program_nickname })
    check_id <- isolate({ SERVER_ADMIN_CHECKS.SELECTED_CHECK()$indicator_check_id })
    check_test_reporting_date <- as.character(input$check_test_reporting_date)
    
    paste0(nickname,"-CHK",check_id,"-",check_test_reporting_date,".xlsx")
  },
  content = function(file) {
    dt <- SERVER_ADMIN_CHECKS_REVIEW.VIEW()
    openxlsx::write.xlsx(x=dt,file=file)
  },
  contentType="application/xlsx"
)
