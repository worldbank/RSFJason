
INDICATOR_REVIEW_INDICATOR_ID <- reactiveVal(NA) #holds indicator ID and the formula is determined as per what the selected rsf_pfcbl_ids are subscribed to
INDICATOR_REVIEW_RESULTS <- reactiveVal(data.table())
#SELECTED_REVIEW_CLIENT_ID <- reactiveVal(NA)

INDICATOR_REVIEW_RESULTS_VIEW <- reactive({
  
  results <- INDICATOR_REVIEW_RESULTS()
  if (empty(results)) return (NULL)
  
  f_filter <- as.numeric(input$indicator_review_results_filter_facilities)
  ch_filter <- as.logical(input$indicator_review_results_filter_changed)
  
  if (isTruthy(f_filter)) {
  
    rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
      select distinct ft.to_family_rsf_pfcbl_id
      from p_rsf.view_rsf_pfcbl_id_family_tree ft 
      where ft.from_rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
    ",params=list(paste0(f_filter,collapse=",")))
    
    rsf_pfcbl_ids <- as.numeric(unlist(rsf_pfcbl_ids))
    
    results <- results[SYSID %in% rsf_pfcbl_ids]
    
  }
  
  if (!is.na(ch_filter) && ch_filter==TRUE) {
    results <- results[data_changed==TRUE]
  } else if (!is.na(ch_filter) && ch_filter==FALSE) {
    results <- results[data_changed==FALSE]
  }
  
  if (empty(results)) return (data.frame(Error="This selection has no data to display"))
  else return (results)
})




show_modal_indicator_review <- function(rsf_pfcbl_id,
                                        review_asof_date,
                                        review_indicator_id) {

  review_indicator_id <- as.numeric(review_indicator_id)
  if (!isTruthy(review_indicator_id)) {
    return(showNotification(type="error",
                            ui=h3("Error: no indicator ID is defined for review panel")))
  } else if (length(review_indicator_id) != 1) {
    return(showNotification(type="error",
                            ui=h3("Error: only one indicator ID allowed for review panel")))
  }
  
  STATUS_MESSAGE_PANEL$container_id <- "indicator_review_log_container"
  STATUS_MESSAGE_PANEL$panel_id <- "indicator_review_log"
  INDICATOR_REVIEW_RESULTS(data.table())
  INDICATOR_REVIEW_INDICATOR_ID(review_indicator_id)
  
  facility_list <- c("",setNames(SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,
                                 SELECTED_PROGRAM_FACILITIES_LIST()$facility_name))
  
  
  if (!isTruthy(rsf_pfcbl_id)) rsf_pfcbl_id <- ""
  if (!isTruthy(review_asof_date)) review_asof_date <- ""
  
  review_asof_date <- as.character(review_asof_date)
  
  #client_selected <- SELECTED_REVIEW_CLIENT_ID()
  
  modal_indicator_review_start <- modalDialog(title="Indicator Formula Review Calculation",
                                              footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                                                         modalButton("Cancel"),
                                                         enabled(state=FALSE,
                                                             actionButton(inputId="action_indicator_review_next",
                                                                          label="Next",
                                                                          class="btn-primary"))),
                                              div(style="background-color:white;padding:2px;height:400px;",
                                                  fluidRow(style="padding-top:5px;",
                                                           column(5,
                                                                  selectizeInput(inputId="indicator_review_facility_list",
                                                                                 label="Review for Project",
                                                                                 choices=facility_list,
                                                                                 selected=rsf_pfcbl_id,
                                                                                 multiple=TRUE,
                                                                                 options=list(placeholder="All projects selected..."))),
                                                           column(2,
                                                                  selectizeInput(inputId="indicator_review_reporting_date",
                                                                                 label="Reporting Date",
                                                                                 choices=review_asof_date,
                                                                                 selected=review_asof_date,
                                                                                 options=list(placeholder="Select date..."))),
                                                           column(3,
                                                                  selectizeInput(inputId="indicator_review_audit_method",
                                                                                 label="Audit Method",
                                                                                 choices=setNames(c("rec","calc"),
                                                                                                  c("Recursive Calculations","This indicator only")),
                                                                                 selected='calc',
                                                                                 options=list(placeholder="Select method..."))),
                                                           column(2,style='padding-left:10px;margin-top:25px;',
                                                                  actionButton(inputId="action_indicator_review_start",
                                                                               label="Start Review",
                                                                               icon=icon("flask"),
                                                                               class="btn-success"))),
                                                  div(id="indicator_review_log_container",style="width:100%;height:300px;background-color:black;color:limegreen;overflow-y:auto;padding:3px;",
                                                      div(id="indicator_review_log"))
                                              ))
  showModal(modal_indicator_review_start)
}

observeEvent(input$action_indicator_review, {
  
  if (!isTruthy(SELECTED_PROGRAM_ID())) return (showNotification(h2("Please select an RSF Program in the upper-left drop down menu")))
  
  selected_indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  if (empty(selected_indicator)) return (showNotification(h2("Error: no indicator is actively selected")))

  formulas <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()
  if (empty(formulas)) return (showNotification(h2("Error: no formulas exist for indicator ",selected_indicator$indicator_id)))
  
  selected_formula_id <- as.numeric(input$action_indicator_review)
  if (!isTruthy(selected_formula_id)) return (NULL)
  formulas <- formulas[formula_id==selected_formula_id]
  if (empty(formulas)) return (NULL)
  
  #this will fail when testing a quasi-calculated indicator (ie, is_calculated will only be true for the rsf_program_facility indicators that see it has a subscribed formula_id)
  #if (!isTruthy(selected_indicator$is_calculated)) return (NULL)  

  
  show_modal_indicator_review(rsf_pfcbl_id = NA,
                              review_asof_date = NA,
                              review_indicator_id = unique(formulas$indicator_id))
  
  
},ignoreInit=TRUE)

observeEvent(input$indicator_review_facility_list, {
  
  facility_ids <- as.numeric(input$indicator_review_facility_list)
  #SELECTED_REVIEW_CLIENT_ID(client_rsf_pfcbl_id)
  
  dates <- NULL
  
  if (isTruthy(facility_ids)) {
    dates <- DBPOOL %>% dbGetQuery("
                                    select distinct
                                      rpr.reporting_asof_date::text
                                    from p_rsf.rsf_pfcbl_ids ids
                                    inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                    where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                      and rpr.reporting_asof_date <= now()::date",
                                   params=list(paste0(facility_ids,collapse=",")))
    
    dates <- c("",sort(dates$reporting_asof_date))
  } else {
    
    #program <- SELECTED_PROGRAM()
    dates <- c("",sort(as.character(SELECTED_PROGRAM_VALID_REPORTING_DATES())))
  }
  
  selected_date <- as.character(input$indicator_review_reporting_date)
  if (!isTruthy(selected_date) ||
      !selected_date %in% dates) {
    selected_date <- dates[[length(dates)]]
  }
  
  #Indicator review reporting dates filters review for calculated indicators, for relevant years where selected client to be reviewed has reported.
  updateSelectizeInput(session=session,
                       inputId="indicator_review_reporting_date",
                       choices=dates,
                       selected = selected_date)
})

observeEvent(input$action_indicator_review_start, {
  
  review_indicator_id <- INDICATOR_REVIEW_INDICATOR_ID()
  rsf_program_id <- SELECTED_PROGRAM_ID()
  review_rsf_pfcbl_id <- as.numeric(input$indicator_review_facility_list)
  indicator_review_reporting_date <- input$indicator_review_reporting_date
  
  if (!isTruthy(review_indicator_id) || !isTruthy(rsf_program_id)) return (NULL)
  if (!isTruthy(review_rsf_pfcbl_id)) review_rsf_pfcbl_id <- as.numeric(NA)
  
  if (!isTruthy(indicator_review_reporting_date)) return (showNotification(type="error",h3("Reporting date must be selected")))
  else indicator_review_reporting_date <- as.Date(indicator_review_reporting_date)
  
  method <- input$indicator_review_audit_method
  if (!method %in% c("rec","calc")) method <- "rec"

   status_message(class="",
                 "Gathering information for calculation review...\n")
  
  disable(id="action_indicator_review_start")
  disable(id="indicator_review_facility_list")
  disable(id="indicator_review_reporting_date")

  result <- tryCatch({
    result <- DBPOOL %>% rsf_indicators_calculate_do_test(rsf_pfcbl_id.family=ifelse(all(is.na(review_rsf_pfcbl_id),na.rm=T),
                                                                                     rsf_program_id,
                                                                                     review_rsf_pfcbl_id),
                                                          indicator_id=review_indicator_id,
                                                          reporting_current_date=indicator_review_reporting_date,
                                                          all_parameters=(method=="rec"),
                                                          status_message=status_message)
    result    
  },
  error = function(err) {
    status_message(class="error",
                   conditionMessage(err))
    NULL
  },
  warning = function(war) {
    status_message(class="error",
                   conditionMessage(war))
    NULL
  })
  
  if (!is.null(result)) {
    
    
    status_message(class="info","SUCCESS\n")  
    INDICATOR_REVIEW_RESULTS(result)
    enable("action_indicator_review_next")
    status_message(class="info","\n\nCalculations completed.  Please click NEXT to review results.\n")
  } else {
    status_message(class="info","Review failed due to a problem in the formula.  Review messages for a '^' that attempts to identify the location that is causing the problem.\n")
    updateActionButton(session=session,inputId="action_indicator_review_next",label="Close")
    enable(id="action_indicator_review_next")
    INDICATOR_REVIEW_RESULTS(data.table())
    INDICATOR_REVIEW_INDICATOR_ID(as.numeric(NA))
    status_message(class="info","\n\nPlease click NEXT to return to indicator screen and correct calculation formula.\n")
  }
  
},ignoreInit = TRUE)

observeEvent(input$action_indicator_review_next, {
  
  results <- INDICATOR_REVIEW_RESULTS()
  if (!empty(results)) {
    indicator_review_reporting_date <- input$indicator_review_reporting_date
    indicator_review_reporting_date <- as.Date(indicator_review_reporting_date)
    
    selected_facility_review <- as.numeric(input$indicator_review_facility_list)
    
    
    facility_choices <- c("",setNames(SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id,
                                      SELECTED_PROGRAM_FACILITIES_LIST()$facility_name))
    
    if (!all(is.na(selected_facility_review))) {
      review_facilities <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_pfcbl_id %in% selected_facility_review,
                                                        .(rsf_pfcbl_id,
                                                           facility_name)]
      
      facility_choices <- c("",setNames(review_facilities$rsf_pfcbl_id,
                                        review_facilities$facility_name))
    }

    m <- modalDialog(title="Indicator Review Results",size="l",
          div(style="background-color:white;padding:2px;height:700px;",
              fluidRow(column(8,selectizeInput(inputId="indicator_review_results_filter_facilities",
                                                label="Filter Projects",
                                                width="100%",
                                                choices=facility_choices,
                                                selected="",
                                                multiple=TRUE,
                                                options=list(placeholder="Filter by project, or leave blank for all"))),
                       column(2,selectizeInput(inputId="indicator_review_results_filter_changed",
                                               label="Filter Changed",
                                               choices=c(`No Filter`="",
                                                         `Changes`="TRUE",
                                                         `Unchanged`="FALSE"))),
                       column(2,align="right",
                              downloadButton(outputId="indicator_review_results_download",label="Download",icon=icon("file-excel"),class="btn-success"))),
              fluidRow(column(12,uiOutput(outputId="indicator_review_formula_text"))),
              fluidRow(column(12,DT::dataTableOutput(outputId="indicator_review_results_table",height="625px")))))
    
    showModal(m)
  } else { removeModal() }
  
},ignoreInit = TRUE)



output$indicator_review_formula_text <- renderUI({
  
  review_indiator_id <- as.numeric(INDICATOR_REVIEW_INDICATOR_ID())
  results <- INDICATOR_REVIEW_RESULTS()
  
  if (empty(results)) return (HTML(""))
  
  indicator_formula_ids <- unique(results$formula_id)
  
  formulas <- DBPOOL %>% dbGetQuery("
    select 
      indf.formula,
      indf.formula_title
    from p_rsf.indicator_formulas indf
    where indf.formula_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(paste0(indicator_formula_ids)))
  
  setDT(formulas)
  
  ui <- tagList()
  for (i in 1:nrow(formulas)) {
    ui[[length(ui)+1]] <- div(formulas[i,formula_title],": ",formulas[i,formula])
  }
  
  return (ui)
})

output$indicator_review_program_name <- renderText({
  program <- SELECTED_PROGRAM()
  if (isTruthy(program)) return (program$program_name)
  else return ("No RSF Program Selected")
})

output$indicator_review_results_table <- DT::renderDataTable({
  
  dt <- INDICATOR_REVIEW_RESULTS_VIEW()
  
  if (all(is.na(dt)) || empty(dt)) dt <- data.frame(error="No results available.  Check your error messages and ensure this RSF Program is subscribed to this indicator")
  
  DT::datatable(dt,
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
                  pageLength=15
                )) %>%
    formatStyle(columns=c(0),width="50px") %>%
    formatStyle(columns=c(1:ncol(dt)),whiteSpace="nowrap",maxWidth="200px;",overflow="hidden",textOverflow="ellipsis")
  
})

output$indicator_review_results_download <- downloadHandler(
  filename = function() { 
    nickname <- SELECTED_PROGRAM()$program_nickname
    indicator_review_id <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()$indicator_id
    indicator_review_reporting_date <- as.character(input$indicator_review_reporting_date)
    
    paste0(nickname,"-ind",indicator_review_id,"-",indicator_review_reporting_date,".xlsx")
  },
  content = function(file) {
    
    #browser()
    dt <- INDICATOR_REVIEW_RESULTS_VIEW()
    pname <- SELECTED_PROGRAM()$program_name
    review_indicator_id <- INDICATOR_REVIEW_INDICATOR_ID()
    review_indicator <- RSF_INDICATORS()[indicator_id==review_indicator_id]
    formula_ids <- unique(dt$formula_id)
    #formula <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()$formula
    #formula_sort <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()$formula_sort

    formulas <- DBPOOL %>% dbGetQuery("select 
                                       formula || case when formula_sort is not null then concat('[SORTED BY] ',formula_sort)
                                                       else '' end as formula,
                                       formula_id
                                       from p_rsf.indicator_formulas indf
                                       where indf.formula_id = any(select unnest(string_to_array($1::text,','))::int)",
                                      params=list(paste0(formula_ids,collapse=",")))
    formula <- paste0(formulas$formula,collapse=" \n")
    
    
    requirements_names <- DBPOOL %>% dbGetQuery("select 
                                               indf.indicator_id,
                                               NULLIF(array_to_string(array_agg(distinct pind.indicator_name order by pind.indicator_name),','),'') as formula_indicator_names_requirements
                                             from p_rsf.indicator_formulas indf
                                             left join lateral unnest(indf.formula_indicator_id_requirements) as required_indicator_id on true
                                             left join p_rsf.indicators pind on pind.indicator_id = required_indicator_id
                                             where indf.formula_indicator_id_requirements is not null
                                               and indf.formula_id = any(select unnest(string_to_array($1::text,','))::int)
                                             group by indf.indicator_id",
                                             params=list(paste0(formula_ids,collapse=",")))
    formula_requirements <- NA
    
    headers <- data.frame(`Program Name`=pname,
                          `Report Created Date`=today(),
                          `Indicator Review For`=review_indicator$indicator_name,
                          `Formula`=formula,
                          check.names = FALSE)

    if (!empty(requirements_names)) {
      formula_requirements <- sort(unique(unlist(strsplit(requirements_names$formula_indicator_names_requirements,","))))
      headers$`Calculation Parameter Requirements` = paste0(formula_requirements,collapse=", ")
    }  
    

    wb <- openxlsx::createWorkbook(creator="IFC RSF",title="Indicator Data Review")
    
    openxlsx::addWorksheet(wb=wb,sheetName="Indicator Data")
    openxlsx::writeData(wb=wb,sheet=1,x=t(headers),rowNames=TRUE,colNames = FALSE)
    openxlsx::writeDataTable(wb=wb,sheet=1,x=dt,startRow=length(headers)+2,tableName="rsf_indicator_data")
    openxlsx::setColWidths(wb,sheet=1,cols=c(1:ncol(dt)),widths=22)
    
    openxlsx::saveWorkbook(wb=wb,file=file,overwrite=TRUE)
  },
  contentType="application/xlsx"
)