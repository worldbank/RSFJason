
EVENT_GUIDANCE_APPLIED <- reactiveVal(0)
INDICATOR_FLAGS_SELECTED_EVALUATION_IDS <- reactiveVal(c())

SERVER_DATASETS_REVIEW_FLAGS_REVERSION_CHECK_NAMES <- eventReactive(LOGGEDIN(), {
  
  if (!isTruthy(LOGGEDIN())) return (NULL)
  
  cnames <- DBPOOL %>% dbGetQuery("
    select 
      ic.check_name,
      ic.indicator_check_id
    from p_rsf.indicator_checks ic
    where ic.check_name in ('sys_calculator_overwrote_manual_calculation','sys_data_status_modified','waiver_value_vs_sys_calculator')")
  
  setDT(cnames)
  if (nrow(cnames) != 3) {
    stop("System cannot find the following flags in database: sys_calculator_overwrote_manual_calculation, sys_data_status_modified, waiver_value_vs_sys_calculator.  Did a name change?  It must be kept for system use.")
  }
  
  return (cnames)
})


SERVER_DATASETS_REVIEW_FLAGS_REVERSIONS <- function(evaluation_ids) {
  
  if (length(evaluation_ids)==0) return (NULL)
  
  reversions <- DBPOOL %>% dbGetQuery("
    select 
    chk.evaluation_id,
    coalesce(rd.data_value,'{MISSING}') || coalesce(' ' || rd.data_unit,'') as data_value,
    rd.data_id,
    revert.data_id as revert_data_id,
    revert.data_value as revert_value,
    revert.users_name
    from p_rsf.rsf_data_checks chk
    inner join p_rsf.rsf_data rd on rd.data_id = chk.data_id
    inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
    left join lateral (select 
                       rdU.data_id,
                       coalesce(rdU.data_value,'{MISSING}') || coalesce(' ' || rdU.data_unit,'') as data_value,
                       vai.users_name
                       from p_rsf.reporting_cohorts rcU 
                       inner join p_rsf.rsf_data rdU on rdU.reporting_cohort_id = rcU.reporting_cohort_id
                       inner join p_rsf.view_account_info vai on vai.account_id = rcU.reporting_user_id
                       where rcU.parent_reporting_cohort_id = rc.parent_reporting_cohort_id
                       and rdU.rsf_pfcbl_id = rd.rsf_pfcbl_id
                       and rdU.indicator_id = rd.indicator_id
                       and rcU.is_reported_cohort = true
                       and rcU.is_calculated_cohort = false
                       order by rdU.data_id desc
                       limit 1) revert on true 
    where chk.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(paste0(unique(evaluation_ids),collapse=",")))
  
  setDT(reversions)
  return(reversions)
  
}

SELECTED_COHORT_SELECTED_INDICATOR_REVIEW_FLAGS <- eventReactive(c(SELECTED_COHORT_INDICATOR_FLAGS_FILTERED(),
                                                                   input$action_indicator_flags_review), {

 selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
 indicator_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
 
 if (!isTruthy(selected_indicator_flag_id)) return (NULL)
 if (!isTruthy(indicator_flags)) return (NULL)
 if (!selected_indicator_flag_id %in% indicator_flags$indicator_flag_id) return(NULL)
 
 indicator_flags <- SELECTED_COHORT_FLAGS()[indicator_flag_id==selected_indicator_flag_id]
 return (indicator_flags)
},ignoreNULL=FALSE)

SERVER_DATASETS_REVIEW_FLAGS_QUERY_DETAILS <- function(evaluation_ids) {
  
  evaluation_ids <- paste0(unique(evaluation_ids),collapse=",")
  cohort_flag_details <- DBPOOL %>% dbGetQuery("select 
    rdc.evaluation_id,
    rdc.rsf_pfcbl_id,
    rdc.check_asof_date,
    rdc.check_status,
    rdc.check_status_comment,
    rdc.check_message,
    rdc.check_status_user_id,
    vai.users_name as check_status_users_name,
    nids.rsf_full_name as entity_name
    from p_rsf.rsf_data_checks rdc
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = rdc.rsf_pfcbl_id
    left join p_rsf.view_account_info vai on vai.account_id = rdc.check_status_user_id
    where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(evaluation_ids))
  
  setDT(cohort_flag_details)
  return (cohort_flag_details)
}

showModal_indicator_check_guidance_new <- function(for_indicator_id,
                                                   indicator_check_id,
                                                   rsf_pfcbl_id,
                                                   close_to_modal=NULL) {
  rsf_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(rsf_program_id)) return (NULL)
  
  indicator <- RSF_INDICATORS()[indicator_id==for_indicator_id]
  if (!isTruthy(indicator)) return (NULL)
  
  id <- paste0("guidance_0")
  ns <- NS(id)
  
  indicator_html <- format_html_indicator(indicator_name=indicator$indicator_name,
                                          data_category=indicator$data_category,
                                          data_type=indicator$data_type,
                                          is_system=indicator$is_system,
                                          is_calculated=indicator$is_calculated)
  
  for_name <- DBPOOL %>% dbGetQuery("select 
                                      sn.sys_name,
                                      sn.pfcbl_category
                                     from p_rsf.view_rsf_pfcbl_id_current_sys_names sn 
                                     where sn.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                                                 from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                                                 where ft.from_rsf_pfcbl_id = $1::int)
                                       and sn.pfcbl_category in ('global','program','facility')",
                                    params=list(rsf_pfcbl_id))
  
  setDT(for_name)
  
  if (indicator$data_type == "global") for_name <- for_name[pfcbl_category=="global",sys_name]
  else if (indicator$data_type == "program") for_name <- for_name[pfcbl_category=="program",sys_name]
  else for_name <- for_name[pfcbl_category=="facility",sys_name]
  
  check <- DBPOOL %>% dbGetQuery("select
                                    ic.indicator_check_id,
                                    ic.check_name,
                                    ic.check_class,
                                    ic.check_type,
                                    ic.is_system,
                                    coalesce(ic.auto_resolve_system_check,false) as auto_resolve,
                                    coalesce(ic.variance_tolerance_allowed,false)::bool as variance_tolerance_allowed
                                 from p_rsf.indicator_checks ic
                                 where ic.indicator_check_id = $1::int",
                                 params=list(indicator_check_id))
  
  default_auto_resolve <- as.logical(check$auto_resolve)
  default_check_class <- as.character(check$check_class)
  
  check_html <- format_html_check(check_name=check$check_name,
                                  check_class=check$check_class,
                                  check_type=check$check_type,
                                  is_subscribed=TRUE, #Just formatting
                                  is_system=check$is_system)
  
  level.choices <- c()
  level.selected <- NULL
  if (rsf_program_id==0) {
    level.choices <- c(`Global Specific`="program") #because when its global program selected, we only want it for THIS program and not for everyone
    level.selected <- "program"
  } else {
    program_name <- SELECTED_PROGRAM()$program_nickname
    
    level.choices <- c("global","program","facility")
    level.choices <- setNames(level.choices,
                              c("Universal",
                                paste0("All ",program_name," Facilities"),
                                "Facility Specific"))
    
    level.selected <- ifelse(indicator$data_type %in% c("global","program"),
                             "program",
                             "facility")
  }
  
  set_level <- selectizeInput(inputId=ns("guidance_set_program_level"),
                              label="Set Guidance Level",
                              choices=level.choices,
                              selected=level.selected,
                              width="250px")
  
  #level_note <- "When selected, guidance is applied at the entire program level"
  
  if (indicator$data_type %in% c("global","program")) {
    set_level <- disabled(set_level)
    level_note <- "This program-level indicator must apply guidance at the entire program level"
  }
  
  guidance_class_choices <- c(`Critical`='critical',
                              `Error`='error',
                              `Warning`='warning',
                              `Info`='info')
  
  guidance_class_choices[which(guidance_class_choices==default_check_class)] <- NA
  names(guidance_class_choices)[which(is.na(guidance_class_choices))] <- paste0(names(guidance_class_choices)[which(is.na(guidance_class_choices))],
                                                                                " [Default]")
  toleranceInput <- textInput(inputId=ns("tolerance_variance"),
                              label="Apply if Variance Below:",
                              value="",
                              placeholder="0%")
  
  if (check$variance_tolerance_allowed==FALSE) toleranceInput <- disabled(toleranceInput)
  
  ui <- tagList(fluidRow(align="top",style="padding-top:5px;",
                         column(12,align="top",
                                uiOutput(outputId=ns("new_guidance_name")))),
                fluidRow(align="top",style="margin-top:5px;",
                         column(2,set_level),
                         column(10,
                                div(style="margin-top:25px",
                                    textOutput(outputId=ns("level_description"))))),
                fluidRow(align="top",
                         column(12,
                                fluidRow(align="top",
                                         column(6,
                                                textAreaInput(inputId=ns("guidance_text"),
                                                              label="Guidance Text",
                                                              value="",
                                                              placeholder = paste0("Create and apply new guidance comment"),
                                                              width="100%",
                                                              rows=1)),
                                         column(2,
                                                selectizeInput(inputId=ns("guidance_resolving"),
                                                               label="Resolve Action",
                                                               choices=c(`Auto-Resolve`="RESOLVE",
                                                                         `User Review`="REVIEW",
                                                                         `Always Ignore`="IGNORE"),
                                                               selected=default_auto_resolve,
                                                               multiple=FALSE,
                                                               width="100%")),
                                         column(2,
                                                selectizeInput(inputId=ns("guidance_class"),
                                                               label="Flag Class",
                                                               choices=guidance_class_choices,
                                                               selected=NA,
                                                               multiple=FALSE,
                                                               width="100%")),
                                         
                                         column(1,
                                                toleranceInput),
                                         
                                         column(1,style="margin-top:25px;",
                                                actionButton(inputId=ns("guidance_apply"),
                                                             label="Apply",
                                                             class="btn-success"))))))
  
  ui <- div(style="padding:5px;border:solid black 1px;background-color:gainsboro;",
            ui)
  
  
  registerModule(id=id,
                 .module=server_datasets_guidance_module(id=id,
                                                         guidance_id=as.numeric(NA),
                                                         indicator_id=for_indicator_id,
                                                         indicator_check_id=indicator_check_id,
                                                         for_rsf_pfcbl_id=rsf_pfcbl_id,
                                                         for_pfcbl_category=as.character(NA),
                                                         user_id=USER_ID(),
                                                         INDICATOR_FLAGS_SELECTED_EVALUATION_IDS=INDICATOR_FLAGS_SELECTED_EVALUATION_IDS,
                                                         EVENT_GUIDANCE_APPLIED=EVENT_GUIDANCE_APPLIED))
  m <- modalDialog(id="view_indicator_check_guidance",
                   div(
                     fluidPage(
                       fluidRow(column(12,style="display:inline-block",tags$label("Guidance For"),
                                       div(style="display:inline-block;",HTML(indicator_html),
                                           div(style="display:inline-block;",HTML(check_html)))))),
                     fluidRow(style="padding-top:10px;",column(12,ui))),
                   
                   title=HTML(paste0("Indicator Check Guidance For: ",for_name)),
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  actionButton(inputId="action_guidance_new_cancel",
                                               label="Cancel",
                                               class="btn-primary btn-danger"))),
                   size="l")
  showModal(m)
  
}


showModal_indicator_check_guidance_edit <- function(for_indicator_id,
                                                    indicator_check_id,
                                                    rsf_pfcbl_id) {
  #browser()
  indicator <- RSF_INDICATORS()[indicator_id==for_indicator_id]
  if (!isTruthy(indicator)) return (NULL)
  
  indicator_html <- format_html_indicator(indicator_name=indicator$indicator_name,
                                          data_category=indicator$data_category,
                                          data_type=indicator$data_type,
                                          is_system=indicator$is_system,
                                          is_calculated=indicator$is_calculated)
  
  for_name <- DBPOOL %>% dbGetQuery("select 
                                      sn.sys_name,
                                      sn.pfcbl_category
                                     from p_rsf.view_rsf_pfcbl_id_current_sys_names sn 
                                     where sn.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
                                                                 from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                                                 where ft.from_rsf_pfcbl_id = $1::int)
                                       and sn.pfcbl_category in ('global','program','facility')",
                                    params=list(rsf_pfcbl_id))
  
  setDT(for_name)
  
  if (indicator$data_type == "global") for_name <- for_name[pfcbl_category=="global",sys_name]
  else if (indicator$data_type == "program") for_name <- for_name[pfcbl_category=="program",sys_name]
  else for_name <- for_name[pfcbl_category=="facility",sys_name]
  
  
  check <- DBPOOL %>% dbGetQuery("select
                                    ic.indicator_check_id,
                                    ic.check_name,
                                    ic.check_class,
                                    ic.check_type,
                                    ic.is_system,
                                    coalesce(ic.auto_resolve_system_check,false) as auto_resolve,
                                    coalesce(ic.variance_tolerance_allowed,false)::bool as variance_tolerance_allowed 
                                 from p_rsf.indicator_checks ic
                                 where ic.indicator_check_id = $1::int",
                                 params=list(indicator_check_id))
  
  check_html <- format_html_check(check_name=check$check_name,
                                  check_class=check$check_class,
                                  check_type=check$check_type,
                                  is_subscribed=TRUE,
                                  is_system=check$is_system)
  
  guidance <- DBPOOL %>% dbGetQuery("
                                    select
                                    	icg.indicator_check_guidance_id,
                                    	icg.guidance,
                                    	icg.is_resolving_guidance,
                                      icg.is_ignoring_guidance,
                                    	icg.for_pfcbl_category,
                                    	icg.overwrite_check_class,
                                      coalesce(icg.variance_threshold,0) as variance_threshold,
                                    	icg.for_indicator_id as indicator_id,
                                    	icg.indicator_check_id,
                                    	array_to_string(array_agg(nids.rsf_full_name),',') as subscribed_names
                                    from p_rsf.indicator_check_guidance icg

                                    left join p_rsf.rsf_program_facility_check_guidance pcg on pcg.indicator_check_guidance_id = icg.indicator_check_guidance_id
                                    left join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ft.rsf_pfcbl_id
                                    where icg.for_indicator_id = $1::int
                                      and icg.indicator_check_id = $2::int
                                      and (icg.
                                    group by
                                    	icg.indicator_check_guidance_id,
                                    	icg.guidance,
                                    	icg.is_resolving_guidance,
                                    	icg.for_pfcbl_category,
                                    	icg.overwrite_check_class,
                                    	icg.for_indicator_id,
                                    	icg.indicator_check_id",
                                    params=list(for_indicator_id,
                                                indicator_check_id,
                                                rsf_pfcbl_id))
  setDT(guidance)
  
  ui <- NULL
  if (!empty(guidance)) {
    guidance[nchar(subscribed_names)==0,subscribed_names:="GUIDANCE NOT USED"]
    
    
    default_auto_resolve <- as.logical(check$auto_resolve)
    default_check_class <- as.character(check$check_class)
    
    
    indicator_is_program_level <- indicator$data_category %in% c("global","program")
    
    setcolorder(guidance,
                neworder = c("indicator_check_guidance_id",
                             "guidance",
                             "is_resolving_guidance",
                             "is_ignoring_guidance",
                             "for_pfcbl_category",
                             "indicator_id",
                             "indicator_check_id",
                             "subscribed_names"))
    
    guidance[,ui_id:=1:.N]
    guidance[,subscribed_names:=mapply(str_split,string=subscribed_names,pattern=",")]
    guidance[,subscribed_html_names:=paste0("<div>",
                                            paste0(unlist(mapply(format_html_indicator,
                                                                 indicator_name=subscribed_names,
                                                                 data_category=for_pfcbl_category,
                                                                 data_type="",
                                                                 is_system=FALSE,
                                                                 is_calculated=FALSE,
                                                                 options_group_name=NA)),
                                                   collapse="&nbsp; "),
                                            "</div>"),
             by=.(ui_id)]
    
    
    
    guidance_class_choices <- c(`Critical`='critical',
                                `Error`='error',
                                `Warning`='warning',
                                `Info`='info')
    
    guidance_class_choices[which(guidance_class_choices==default_check_class)] <- NA
    names(guidance_class_choices)[which(is.na(guidance_class_choices))] <- paste0(names(guidance_class_choices)[which(is.na(guidance_class_choices))],
                                                                                  " [Default]")
    
    ui_list <- list()
    
    for (g in 1:nrow(guidance)) {
      guide <- guidance[g]
      
      id <- paste0("guidance_",g)
      ns <- NS(id)
      
      toleranceValue <- NULL
      if (check$variance_tolerance_allowed==FALSE) {
        toleranceValue <- 0.0
      } else {
        toleranceValue <- as.numeric(guide$variance_threshold)
        if (!isTruthy(toleranceValue)) toleranceValue <- 0.0
      }
      
      toleranceValue <- as.numeric(toleranceValue) * 100
      toleranceValue <- paste0(toleranceValue,"%")
      
      toleranceInput <- textInput(inputId=ns("tolerance_variance"),
                                  label=NULL,
                                  value=as.character(toleranceValue),
                                  placeholder="0% Variance")
      
      if (check$variance_tolerance_allowed==FALSE) toleranceInput <- disabled(toleranceInput)
      
      
      resolving.selected <- fcase(guide$is_ignoring_guidance==TRUE,"IGNORE",
                                  guide$is_resolving_guidance==TRUE,"RESOLVE",
                                  default="REVIEW")
      
      {
        ui_list[length(ui_list)+1] <- tagList(
          fluidRow(align="top",
                   column(12,
                          fluidRow(align="top",
                                   column(12,HTML(guide$subscribed_html_names))),
                          fluidRow(align="top",
                                   column(6,
                                          textAreaInput(inputId=ns("guidance_text"),
                                                        label=NULL,
                                                        value=guide$guidance,
                                                        placeholder = paste0("Enter new text for this ",
                                                                             tools::toTitleCase(guide$for_pfcbl_category),
                                                                             "Guidance."),
                                                        width="100%",
                                                        rows=1)),
                                   column(2,
                                          selectizeInput(inputId=ns("guidance_resolving"),
                                                         label=NULL,
                                                         choices=c(`Auto-Resolve`="RESOLVE",
                                                                   `User Review`="REVIEW",
                                                                   `Always Ignore`="IGNORE"),
                                                         selected=resolving.selected,
                                                         multiple=FALSE,
                                                         width="100%")),
                                   column(2,
                                          selectizeInput(inputId=ns("guidance_class"),
                                                         label=NULL,
                                                         choices=guidance_class_choices,
                                                         selected=guide$overwrite_check_class,
                                                         multiple=FALSE,
                                                         width="100%")),
                                   column(1,toleranceInput),
                                   column(1,
                                          actionButton(inputId=ns("guidance_apply"),
                                                       label="Apply",
                                                       class="btn-success"))))))
      }
      
      registerModule(id=id,
                     .module=server_datasets_guidance_module(id=id,
                                                             guidance_id=guide$indicator_check_guidance_id,
                                                             indicator_id=guide$indicator_id,
                                                             indicator_check_id=guide$indicator_check_id,
                                                             for_rsf_pfcbl_id=rsf_pfcbl_id,
                                                             for_pfcbl_category=guide$for_pfcbl_category,
                                                             user_id=USER_ID(),
                                                             INDICATOR_FLAGS_SELECTED_EVALUATION_IDS=INDICATOR_FLAGS_SELECTED_EVALUATION_IDS,
                                                             EVENT_GUIDANCE_APPLIED=EVENT_GUIDANCE_APPLIED))
    }
    ui <- do.call(what=shiny::tagList,
                  args=ui_list)
    ui <- div(style="padding:5px;border:solid black 1px;background-color:gainsboro;",
              ui)
  } else { #else guidance is empty
    ui <- div(style="padding:5px;background-color:gainsboro;",
              fluidRow(column(12,
                              p("No guidance available.  Create New guidance to proceed."))))
  }
  
  m <- modalDialog(id="view_indicator_check_guidance",
                   div(#style="max-height:600px;overflow:auto;",
                     fluidPage(
                       fluidRow(column(12,style="display:inline-block",tags$label("Guidance For"),
                                       div(style="display:inline-block;",HTML(indicator_html),
                                           div(style="display:inline-block;",HTML(check_html)))))),
                     fluidRow(style="padding-top:10px;",
                              column(3,
                                     actionButton(inputId="guidance_create_new",
                                                  label="Create New Guidance",
                                                  class="btn-primary")),
                              column(9,"Note: to permanently delete a guidance option, delete its text and submit 'apply' with an empty guidance field.")),
                     fluidRow(style="padding-top:10px;",
                              column(12,ui))),
                   
                   title=HTML(paste0("Indicator Check Guidance For: ",for_name)),
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  actionButton(inputId="action_guidance_edit_cancel",
                                               label="Cancel",
                                               class="btn-primary btn-danger"))),
                   size="l")
  showModal(m)
}

#Action click to review cohort indicator flag details for resolutions: raises modal panel
observeEvent(input$action_indicator_flags_review, {
  
  cohort_info <- SELECTED_COHORT_INFO()
  selected_indicator_flag_id <- input$action_indicator_flags_review
  
  if (!isTruthy(cohort_info)) return(NULL)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  cohort_flags <- SELECTED_COHORT_SELECTED_INDICATOR_REVIEW_FLAGS()
  cohort_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  
  if (!isTruthy(cohort_flags) || !isTruthy(cohort_indicator_flag)) {
    return (showNotification(type="error",
                             h3("Unable to review flags as no flags exist for this selection.  Please try again by clicking on the review icon")))
  }
  
  if (nrow(cohort_flags) >= 10) {
    show_modal_spinner(spin = "circle",
                       color = "blue",
                       text = "Loading...",
                       session = shiny::getDefaultReactiveDomain())
  }
  
  
  check_definition <- DBPOOL %>% dbGetQuery("select 
                                             ic.definition
                                             from p_rsf.indicator_checks ic
                                             where ic.indicator_check_id = any(select unnest(string_to_array($1::text,','))::int)",
                                            params=list(paste0(na.omit(unique(c(cohort_indicator_flag$indicator_check_id,
                                                                                cohort_indicator_flag$consolidated_from_indicator_check_id))))))
  check_definition <- paste0(check_definition$definition,collapse=" {AND ALSO} ")
  
  check_html <-  cohort_indicator_flag$check_html
  check_formula_html <- cohort_indicator_flag$check_formula_html
  
  indicator_html <- cohort_indicator_flag$indicator_html
  formula_html <- cohort_indicator_flag$formula_html
  
  status.choices <- c(Active="active",
                      Resolved="resolved")
  
  check_names <- SERVER_DATASETS_REVIEW_FLAGS_REVERSION_CHECK_NAMES()
  
  if (cohort_indicator_flag$check_name=="sys_calculator_overwrote_manual_calculation") {
    status.choices <- c(Active="active",
                        Resolved="resolved",
                        `Revert with Waiver`='revert')
    
  } else if (cohort_indicator_flag$check_name=="sys_data_status_modified") {
    status.choices <- c(Active="active",
                        Resolved="resolved",
                        `Remove Waiver`='remove')
  }
  
  status_review.choices <- c(All="all",
                             None="none",
                             Active="active",
                             Resolved="resolved",
                             New="new")
  
  status_review.selected <- "new"
  
  
  if (!any(cohort_flags$check_status=="resolved")) {
    status_review.choices <- status_review.choices[-which(status_review.choices=="resolved")]
    status_review.selected <- "active"
  } else if (!any(cohort_flags$check_status=="active")) {
    status_review.choices <- status_review.choices[-which(status_review.choices=="active")]
    status_review.selected <- "resolved"
  } else {
    status_review.selected <- "new"
  }
  
  status.selected <- "active"
  if (all(cohort_flags$check_status=="resolved")) status.selected = "resolved"

  placeholder <- "Apply update comment to all marked flags..."
  if (any(cohort_indicator_flag$check_class=="critical")) {
    status.choices <- c()
    status.selected <- ""
    placeholder <- "Critical flags must be resolved by deleting and re-uploading corrected datasets"
  }
  
  indicator_formula_review_ui <- NULL
  check_formula_review_ui <- NULL
  if (!is.na(cohort_indicator_flag$indicator_formula_id)) {
    indicator_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_indicator",
                                                label="Audit Calculation",
                                                class="btn-primary",
                                                icon=icon("calculator"))
  }
  
  #System checks will not have a formula to review
  if (!is.na(cohort_indicator_flag$check_formula_id)) {
    check_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_check",
                                            label="Audit Check",
                                            class="btn-primary",
                                            icon=icon("flag"))
  }
  
  m <- modalDialog(id="view_indicator_flags_review",
                   title=HTML(paste0("Review Flags: ",
                                     cohort_info$rsf_full_name," ",
                                     format_asof_date_label(cohort_info$reporting_asof_date)," ",
                                     "[upload #",cohort_info$reporting_cohort_id,"]")),
                   div(style="max-height:600px;width:100%;overflow-y:auto;",
                       fluidPage(
                         fluidRow(column(2,tags$label("Flagged Indicator")),
                                  column(6,style="display:inline-block",
                                         div(style="display:flex;flex-flow:row nowrap;",
                                             div(HTML(indicator_html)),
                                             div(HTML(formula_html))),
                                         div(style="display:flex;flex-flow:row nowrap;",
                                             div(HTML(check_html)),
                                             div(HTML(check_formula_html)))
                                  ),
                                  column(4,
                                         div(style="display:flex;flex-flow:row nowrap;column-gap:15px;",
                                           actionButton(inputId="action_review_indicator_flags_view_dashboard",
                                                        label="View in Dashboard",
                                                        class="btn-primary",
                                                        icon=icon("eye")),
                                           
                                           indicator_formula_review_ui,
                                           
                                           check_formula_review_ui
                                         )
                                  )),
                         fluidRow(column(2,tags$label("Definition")),
                                  column(10,div(style='display:inline-block',icon("info",style="color:blue"),check_definition))),
                         fluidRow(column(2,div(style='display:inline-block;',
                                               tags$label("Resolution Guidance"),
                                               icon("edit",
                                                    title='Edit Guidance',
                                                    #class="btn-primary",
                                                    style="color:blue;",
                                                    class="pointer",
                                                    onclick=paste0('Shiny.setInputValue("action_edit_indicator_flag_guidance","', as.numeric(Sys.time()),'",{priority:"event"})')))),
                                  column(10,uiOutput(outputId="view_indicator_flag_guidance"))),
                         
                         fluidRow(style="padding-top:10px",
                                  column(2,
                                         radioGroupButtons(
                                           inputId="indicator_flags_selected",
                                           width="200px",
                                           label="Select Status Checks:",
                                           choices=status_review.choices,
                                           selected = status_review.selected,
                                           size="xs")),
                                  column(7,
                                         textAreaInput(inputId="indicator_flags_status_message",
                                                       label="Set Status Message:",
                                                       width="100%",
                                                       placeholder=placeholder,
                                                       rows = 1)
                                  ),
                                  column(3,
                                         selectizeInput(inputId="indicator_flags_status",
                                                        label="Set Status To:",
                                                        choices=status.choices,
                                                        selected=status.selected,
                                                        width = "100%"
                                         ))),
                         
                         fluidRow(style="width:100%;border-spacing:5px 2px;border-top:solid black 2px;vertical-align:top;",
                                  column(12,
                                         style="height:100%;",
                                         dataTableOutput(outputId="server_datasets_review_flags_dataset",
                                                         width="100%")))
                       )
                   ),
                   
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  actionButton(inputId="action_indicator_flags_review_cancel",
                                               label="Cancel",
                                               class="btn-primary btn-danger")),
                              div(style="display:inline-block;float:right;",
                                  actionButton(inputId="action_indicator_flags_review_save",
                                               label="Save & Exit",
                                               class="btn-primary btn-success"))),
                   size="l")
  showModal(m)
})

observeEvent(input$server_datasets_review_flags_selected, {
  evaluation_id <- as.numeric(input$server_datasets_review_flags_selected)
  if (!isTruthy(evaluation_id)) return(NULL)
  if (!evaluation_id %in% SELECTED_COHORT_SELECTED_INDICATOR_REVIEW_FLAGS()$evaluation_id) return (NULL)
  
  if (evaluation_id %in% INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()) {
    INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()[-which(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()==evaluation_id)])
  } else {
    INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(unique(c(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(),evaluation_id)))
  }
  print(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS())
})

#Closes review modal and brings up guidance modal for the selected indicator/check currently under review
observeEvent(input$action_edit_indicator_flag_guidance, {
  
  #In case any existing guidance modules haven't been cleaned up properly...
  lapply(grep("^guidance_",registeredModules(),value=T),destroyModule)
  #removeModal()
  
  selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  cohort_indicator_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
  if (!selected_indicator_flag_id %in% cohort_indicator_flags$indicator_flag_id) return (NULL)
  
  cohort <- COHORTS_LIST()[reporting_cohort_id==SELECTED_COHORT_ID()]
  if (!isTruthy(cohort)) return (NULL)
  
  indicator_flag <- cohort_indicator_flags[indicator_flag_id==selected_indicator_flag_id]
  
  for_indicator_id <- indicator_flag$indicator_id
  indicator_check_id <- indicator_flag$indicator_check_id
  if (!is.na(indicator_flag$consolidated_from_indicator_id) && 
      !is.na(indicator_flag$consolidated_from_indicator_check_id)) {
    for_indicator_id <- indicator_flag$consolidated_from_indicator_id
    indicator_check_id <- indicator_flag$consolidated_from_indicator_check_id
  }
  
  showModal_indicator_check_guidance_edit(for_indicator_id=as.numeric(for_indicator_id),
                                          indicator_check_id=as.numeric(indicator_check_id),
                                          rsf_pfcbl_id=cohort$reporting_rsf_pfcbl_id)
})

#Action button: go to dashboard.  If flag applies to a calculated indicator, import parameters, too
observeEvent(input$action_review_indicator_flags_view_dashboard, {
  cohort_info <- SELECTED_COHORT_INFO()
  #cohort_id <- SELECTED_COHORT_ID()
  
  if (empty(cohort_info)) return(NULL)
  
  selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(indicator_flag)) return (NULL)
  

  flagged_indicator_id <- unique(indicator_flag$consolidated_from_indicator_id)
  flagged_indicator_check_id <- unique(indicator_flag$consolidated_from_indicator_check_id)
  
  if (!isTruthy(flagged_indicator_id)) flagged_indicator_id <- unique(indicator_flag$indicator_id)
  if (!isTruthy(flagged_indicator_check_id)) flagged_indicator_check_id <- unique(indicator_flag$indicator_check_id)

  check_indicator_ids <- DBPOOL %>% dbGetQuery("
    select unnest(formula_indicator_ids) as indicator_id 
    from p_rsf.indicator_check_formulas 
    where check_formula_id = $1::int",
    params=list(indicator_flag$check_formula_id))
  
  for_indicator_names <- RSF_INDICATORS()[indicator_id==flagged_indicator_id,indicator_name]
  for_indicator_names <- c(":include:IDs",for_indicator_names)
  
  if (isTruthy(as.numeric(indicator_flag$indicator_formula_id))) for_indicator_names <- c(for_indicator_names,":expand:calculations-shallow")
  if (!empty(check_indicator_ids)) {
    for_indicator_names <- c(for_indicator_names,
                             RSF_INDICATORS()[indicator_id %in% check_indicator_ids$indicator_id,indicator_name])
  }
  
  dashboard_parameters <- SERVER_DASHBOARD_RUN_OPTIONS_INIT
  dashboard_parameters$flags_filter <- "any"
  dashboard_parameters$flags_display <- "active"
  dashboard_parameters$format_unchanged <- "black"
  
  dashboard_parameters$format_pivot <- "DATA"
  # if (length(for_indicator_names) >= length(unlist(indicator_flag$evaluation_ids))) {
  #   dashboard_parameters$format_pivot <- "NAME"
  # } else {
  #   dashboard_parameters$format_pivot <- "DATA"
  # }
  
  for_client_sys_names <- SELECTED_PROGRAM_CLIENTS_LIST()[rsf_program_id %in% cohort_info$rsf_program_id &
                                                          rsf_facility_id %in% cohort_info$rsf_facility_id,
                                                          rsf_pfcbl_id]
  
  SERVER_DASHBOARD_DO_LOAD(for_client_sys_names=for_client_sys_names,
                           for_indicator_names=for_indicator_names,
                           for_asof_dates=cohort_info$reporting_asof_date,
                           dashboard_parameters=dashboard_parameters)
})

observeEvent(input$action_review_indicator_flags_audit_indicator, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  selected_cohort <- SELECTED_COHORT_INFO()
  #browser()
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  cohort_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_indicator_id <- as.numeric(cohort_indicator_flag$indicator_id)
  
  if (!isTruthy(review_indicator_id)) return (NULL)
  
  show_modal_indicator_review(clientest_rsf_pfcbl_id=selected_cohort$clientest_rsf_pfcbl_id,
                              review_asof_date=selected_cohort$reporting_asof_date,
                              review_indicator_id=review_indicator_id)
  
},ignoreInit = TRUE)

observeEvent(input$action_review_indicator_flags_audit_check, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  selected_cohort <- SELECTED_COHORT_INFO()
  #browser()
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  cohort_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_check_formula_id <- as.numeric(cohort_indicator_flag$check_formula_id)
  if (!isTruthy(review_check_formula_id)) return (NULL)
  
  show_modal_server_admin_checks_review(clientest_rsf_pfcbl_id=selected_cohort$clientest_rsf_pfcbl_id,
                                        review_asof_date=selected_cohort$reporting_asof_date,
                                        review_check_formula_id=review_check_formula_id)
  
},ignoreInit = TRUE)

#saves bulk comments and status for all flags of same type on this _indicator_
observeEvent(input$action_indicator_flags_review_save, {
  
  selected_evaluation_ids <- INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()
  
  if (!isTruthy(selected_evaluation_ids)) return(showNotification(type="error",h2("Nothing is checked to update, nothing to save. Please check an item to update; or select cancel to close the window")))
  
  check_status_comment_updated <- input$indicator_flags_status_message
  check_status_updated <- input$indicator_flags_status
  
  if (!isTruthy(check_status_comment_updated)) return (showNotification(type="error",h2("A status comment is required to update flag status")))
  if (!isTruthy(check_status_updated) || 
      !check_status_updated %in% c("active",
                                   "resolved",
                                   "revert",       #applies a MANUAL_CALCULATION flag to the data point and reverts the system calculation
                                   "remove")) {    #deletes the check with the flag and re-calculates using system
    return (showNotification(type="error",h2("Invalid status selected.")))
  } 
  
  if (isTruthy(selected_evaluation_ids) & length(selected_evaluation_ids) > 0) {
    #browser()
    #Update the flags -- and set "reverts" to be "resolved" as they'll be subsequently deleted.
    if (check_status_updated %in% c("active","resolved","revert")) {
      withProgress(message="Saving updates...",value=0.25, {
      
        cohort_indicator_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
        update_flags <- cohort_indicator_flags[,
                                               .(evaluation_id=unlist(evaluation_ids,recursive = F)),
                                               by=.(indicator_id,
                                                    indicator_check_id,
                                                    check_class,
                                                    indicator_flag_id)
        ][evaluation_id %in% as.numeric(selected_evaluation_ids)]
        
        incProgress(amount=0.5,message="Uploading to database...")
        
        if (!empty(update_flags)) {
          
          update_flags[,`:=`(check_status_updated=check_status_updated,
                             check_status_comment_updated=check_status_comment_updated)]
          
          #So the correct waiver messages flow from the one being reverted to the new flag
          #however, this is necessary because it grants access to the flag message, which will be concatenated into the flag status message.
          update_flags[check_status_updated=="revert",
                       check_status_updated:="resolved"]
          
          update_flags <- update_flags[,.(evaluation_id,
                                          indicator_check_id,
                                          check_class,
                                          check_status_updated,
                                          check_status_comment_updated)]
          
          saved <-  DBPOOL %>% db_data_update_flags(user_id=USER_ID(),
                                                    flags=update_flags)
        }
        
        incProgress(amount=1.0,message="Completed")
      })
    }
    
    if (check_status_updated == "revert") {

      data_status_flag_id <- SERVER_DATASETS_REVIEW_FLAGS_REVERSION_CHECK_NAMES()[check_name=="sys_data_status_modified",indicator_check_id]

      withProgress(message="Reverting calculations...",value=0.25, {
        
        reversions <- SERVER_DATASETS_REVIEW_FLAGS_REVERSIONS(evaluation_ids=selected_evaluation_ids)
        
        incProgress(amount=0.25,message="Uploading to database...")

#conn <- poolCheckout(DBPOOL)
        poolWithTransaction(DBPOOL,function(conn) {
          
          dbExecute(conn,"
                    create temp table _temp_reversions(evaluation_id int,
                                                        data_value text,
                                                        data_id int,
                                                        revert_data_id int,
                                                        revert_value text)
                    on commit drop")
          
          dbAppendTable(conn,
                        name="_temp_reversions",
                        value=reversions[,.(evaluation_id,
                                            data_value,
                                            data_id,
                                            revert_data_id,
                                            revert_value)])
          
          
          
          dbExecute(conn,"
                     insert into p_rsf.rsf_data_checks(data_id,
                                            rsf_pfcbl_id,
                                            indicator_id,
                                            check_asof_date,
                                            indicator_check_id,
                                            check_formula_id,
                                            status_time,
                                            check_message,
                                            check_status,
                                            check_status_comment,
                                            check_status_user_id,
                                            indicator_check_guidance_id,
                                            check_data_id_is_current,
                                            consolidated_from_indicator_id,
                                            consolidated_from_indicator_check_id,
                                            data_sys_flags)						
          select 
          rd.data_id,
          rd.rsf_pfcbl_id,
          rd.indicator_id,
          rd.reporting_asof_date as check_asof_date,
          ic.indicator_check_id,
          NULL as check_formula_id,
          now() as status_time,
          concat('Reversion from system-calculated {',tr.data_value,'} to manually-reported {',tr.revert_value,'} and data status set to: MANUALLY CALCULATED' ) as check_message,
          case when ic.auto_resolve_system_check = true then 'resolved' else 'active' end as check_status,
          rdc.check_status_comment,
          $2::text as check_status_user_id,
          NULL as indicator_check_guidance_id,
          true as check_data_id_is_current, -- not now, but should become so by end of transaction!
          NULL as consolidated_from_indicator_id,
          NULL as consolidated_from_indicator_check_id,
          4 as data_sys_flags  -- bit 4 is manually calculated
          
          from _temp_reversions	tr
          inner join p_rsf.indicator_checks ic on ic.indicator_check_id = $1
          inner join p_rsf.rsf_data_checks rdc on rdc.evaluation_id = tr.evaluation_id
          inner join p_rsf.rsf_data rd on rd.data_id = tr.revert_data_id",
          params=list(data_status_flag_id,
                      USER_ID()))
          
          #Because inserting the flag will delete the data overwrite check and archive it--but archive is not meaningful now (and we don't want to "restore" the reversion decision without actually reverting it)
          dbExecute(conn,"
            delete from p_rsf.rsf_data_checks_archive dca
            using _temp_reversions tr
            where dca.archive_id = tr.evaluation_id")
        })
        
        # Flags:
        # 0: reserved
        # 1: reserved
        # 2: deleted (won't be present in rsf_data_current)
        # 4: manual overwrite (if calculated, accept; no overwrite)

        
        

      })
    
    
    }
    
    if (check_status_updated == "remove") {

      withProgress(message="Removing waiver flag...",value=0.25, {
        
        remove_data_ids <- DBPOOL %>% dbGetQuery("
          select
            rdc.data_id,
            rdc.data_sys_flags
          from p_rsf.rsf_data_checks rdc
          where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
          params=list(paste0(selected_evaluation_ids,collapse=",")))
        
        data_sys_flags <- unique(remove_data_ids$data_sys_flags)
        
        if (length(data_sys_flags) !=1) {
          stop(paste0("Remove flags can only be performed on flags of the same type for each 'remove' request.  This request include multiple flags: ",
                      paste0(sort(data_sys_flags),collapse=",")))
        }
        
        incProgress(amount=0.25,message="Removing from database...")
        
        DBPOOL %>% dbExecute("
          delete from p_rsf.rsf_data_checks rdc
          where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
          params=list(paste0(selected_evaluation_ids,collapse=",")))
        
        
        DBPOOL %>% dbExecute("
          delete from p_rsf.rsf_data_checks_archive dca
          where dca.archive_id = any(select unnest(string_to_array($1::text,','))::int)",
          params=list(paste0(selected_evaluation_ids,collapse=",")))
        
        incProgress(amount=0.25,message="Resetting data flags...")
        
        DBPOOL %>% dbExecute("
          update p_rsf.rsf_data rd
          set data_sys_flags = NULLIF(data_sys_flags # $1::int,0)
          where rd.data_id = any(select unnest(string_to_array($2::text,','))::int)",
          params=list(data_sys_flags,
                      paste0(unique(remove_data_ids$data_id),collapse=",")))
      })      
      
    }
    
    
    if (check_status_updated %in% c("revert","remove")) {
      cohort_info <- SELECTED_COHORT_INFO()
      #Set the overwritten data point do manual overwrite
      
      withProgress(message="Reverting calculations...",value=0.25, {
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Recalculating affected data: ",dots))
        }
        
        incProgress(amount=0.25,message="Recalculating data...")
        DBPOOL %>% rsf_program_calculate(rsf_program_id = SELECTED_PROGRAM_ID(),
                                         rsf_indicators = RSF_INDICATORS(),
                                         rsf_pfcbl_id.family = cohort_info$reporting_rsf_pfcbl_id,
                                         status_message=progress_status_message)
      })
      
      withProgress(message="Reverting calculations...",value=0.25, {
        
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Rechecking affected data: ",dots))
        }
        incProgress(amount=0.25,message="Rechecking data...")
        DBPOOL %>% rsf_program_check(rsf_program_id=SELECTED_PROGRAM_ID(),
                                     rsf_indicators=RSF_INDICATORS(),
                                     rsf_pfcbl_id.family=cohort_info$reporting_rsf_pfcbl_id,
                                     check_future=TRUE,
                                     check_consolidation_threshold=NA,
                                     reference_asof_date=NULL,
                                     status_message= progress_status_message)
      })
    }
    
    REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
    
    
  }
  
  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(c()) #NA is intentional and different from c()
  removeModal()
})

#closes modal "view_data_flags_review"
observeEvent(input$action_indicator_flags_review_cancel, {
  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(c()) #NA is intentional and different from c()
  removeModal()
})

observeEvent(input$guidance_create_new, {
  
  lapply(grep("^guidance_",registeredModules(),value=T),destroyModule)
  removeModal()
  
  selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  cohort_indicator_flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()
  if (!selected_indicator_flag_id %in% cohort_indicator_flags$indicator_flag_id) return (NULL)
  
  cohort <- COHORTS_LIST()[reporting_cohort_id==SELECTED_COHORT_ID()]
  if (!isTruthy(cohort)) return (NULL)
  
  indicator_flag <- cohort_indicator_flags[indicator_flag_id==selected_indicator_flag_id,
                                           .(indicator_id,
                                             indicator_check_id)]
  
  showModal_indicator_check_guidance_new(for_indicator_id=indicator_flag$indicator_id,
                                         indicator_check_id=indicator_flag$indicator_check_id,
                                         rsf_pfcbl_id=cohort$reporting_rsf_pfcbl_id,
                                         close_to_modal=NULL)
  
})

observeEvent(input$action_guidance_edit_cancel, {
  lapply(grep("^guidance_",registeredModules(),value=T),destroyModule)
  removeModal()
  
  if (isTruthy(input$action_indicator_flags_review)) {
    print("sending event")
    shinyjs::runjs(paste0("Shiny.setInputValue(\"action_indicator_flags_review\",\"",input$action_indicator_flags_review,"\",{priority:\"event\"})"))
  }
})

observeEvent(input$action_guidance_new_cancel, {
  lapply(grep("^guidance_",registeredModules(),value=T),destroyModule)
  removeModal()
  
  if (isTruthy(input$action_indicator_flags_review)) {
    shinyjs::runjs(paste0("Shiny.setInputValue(\"action_indicator_flags_review\",\"",input$action_indicator_flags_review,"\",{priority:\"event\"})"))
  }
})

observeEvent(EVENT_GUIDANCE_APPLIED(), {
  removeModal()
  lapply(grep("^guidance_",registeredModules(),value=T),destroyModule)
  REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
  shinyjs::runjs(paste0("Shiny.setInputValue(\"action_indicator_flags_review\",\"",input$action_indicator_flags_review,"\",{priority:\"event\"})"))
})

output$view_indicator_flag_guidance <- renderUI({
  
  selected_cohort <- SELECTED_COHORT_INFO()
  if (!isTruthy(selected_cohort)) return (NULL)
  
  review_flags <- SELECTED_COHORT_SELECTED_INDICATOR_REVIEW_FLAGS()
  if (!isTruthy(review_flags)) return(NULL)
  
  applied_guidance_id <- na.omit(unique(review_flags$indicator_check_guidance_id))
  guidance_for <- NULL
  if (length(applied_guidance_id)==1) {
    
    guidance <- DBPOOL %>% dbGetQuery("
      select 
        icg.guidance,
        icg.is_resolving_guidance,
        icg.is_ignoring_guidance,
        icg.for_pfcbl_category,
        icg.overwrite_check_class,
        coalesce(vai.users_name,'UNKNOWN') as users_name,
        fcg.rsf_pfcbl_id,
        nids.rsf_full_name
      from p_rsf.indicator_check_guidance icg
      inner join p_rsf.rsf_program_facility_check_guidance fcg on fcg.indicator_check_guidance_id = icg.indicator_check_guidance_id
      inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = fcg.rsf_pfcbl_id
      left join p_rsf.view_account_info vai on vai.account_id = icg.user_id
      where icg.indicator_check_guidance_id = $1::int
        and fcg.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id
      	                           from p_rsf.view_rsf_pfcbl_id_family_tree ft
      														 where ft.from_rsf_pfcbl_id = $2::int)",
      params=list(applied_guidance_id,
                  selected_cohort$reporting_rsf_pfcbl_id))
    
    if (!empty(guidance)) {
      is_msg <- c()
      if (!is.na(guidance$overwrite_check_class)) {
        class <- guidance$overwrite_check_class
        if (class=="critical") class <- "<span style='font-weight:bold;color:brickred;'>CRITICAL</span>"
        else if (class=="error") class <- "<span style='font-weight:bold;color:red;'>ERROR</span>"
        else if (class=="warning") class <- "<span style='font-weight:bold;color:yellow;'>WARNING</span>"
        else if (class=="info") class <- "<span style='font-weight:bold;color:blue;'>INFO</span>"
        
        class <- paste0("SET AS ",class)
        is_msg <- c(is_msg,class)
      }
      
      if (guidance$is_ignoring_guidance) {
        is_msg <- c(is_msg,"SET TO <span style='font-weight:bold;color:brickred;'>IGNORE</span")
      } else if (guidance$is_resolving_guidance) {
        is_msg <- c(is_msg,"SET TO <span style='font-weight:bold;color:forestgreen;'>AUTO RESOLVE</span>")
      }
      
      is_msg <- paste0(is_msg,collapse=" AND")
      if (nchar(is_msg) >0) is_msg <- paste0(is_msg," BECAUSE: ")
      else is_msg <- "<b>GUIDANCE</b>:"
      
      guidance_for <- paste0("<u>",guidance$rsf_full_name,"</u> ",is_msg,
                             guidance$guidance,
                             " (",format_name_abbreviation(guidance$users_name),")")
    }
  } else if (length(applied_guidance_id)==0) {
    guidance_for <- "No guidance specified. Edit to change."
  } else {
    guidance_for <- "<b>Warning: Multiple different checks have conflicting guidance applied, perhaps due to different users applying difference guidance over time.  Recommend to review and reapply guidance for these checks.</b>"
  } 
  
  ui <- HTML(guidance_for)
  
  return(ui)
  
})

output$server_datasets_review_flags_dataset <- DT::renderDataTable({
  
  evaluations <- SELECTED_COHORT_SELECTED_INDICATOR_REVIEW_FLAGS()[,.(evaluation_id,check_class,check_status,rsf_pfcbl_id)]
  if (empty(evaluations)) {
    INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(c())
    return (DT::datatable(data.frame(Error="This selection has no flags to display"),
                          rownames=FALSE,
                          fillContainer = TRUE,
                          options=list(
                            dom="t",
                            autoWidth=TRUE,
                            paging=FALSE
                          )))
  }
  
  indicator_flags_selected <- req(input$indicator_flags_selected)
  indicator_flags_status <- input$indicator_flags_status
  
  cohort_flag_details <- SERVER_DATASETS_REVIEW_FLAGS_QUERY_DETAILS(evaluations$evaluation_id)
  
  # cohort_flag_details <- DBPOOL %>% dbGetQuery("select 
  #   rdc.evaluation_id,
  #   rdc.rsf_pfcbl_id,
  #   rdc.check_status,
  #   rdc.check_status_comment,
  #   rdc.check_message,
  #   rdc.check_status_user_id,
  #   vai.users_name as check_status_users_name,
  #   nids.rsf_full_name as entity_name
  #   from p_rsf.rsf_data_checks rdc
  #   inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = rdc.rsf_pfcbl_id
  #   left join p_rsf.view_account_info vai on vai.account_id = rdc.check_status_user_id
  #   where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
  #   params=list(paste0(unique(evaluations$evaluation_id),collapse=",")))
  # 
  # setDT(cohort_flag_details)
  
  evaluations <- evaluations[cohort_flag_details,
                             on=.(evaluation_id),
                             nomatch=NULL]
  
  evaluations[,selected:=fcase(indicator_flags_selected=="all" & nchar(check_status) > 0,TRUE,
                               indicator_flags_selected=="none"  & nchar(check_status) > 0,FALSE,
                               indicator_flags_selected=="resolved" & check_status=="resolved",TRUE,
                               indicator_flags_selected=="active" & check_status=="active",TRUE,
                               indicator_flags_selected=="new" & is.na(check_status_comment),TRUE,
                               default=FALSE)]
  
  evaluations[is.na(check_status_comment),
              check_status_comment:="{MISSING}"]
  
  
  evaluations[,
              apply_html:=paste0("<input type='checkbox' name='apply_flag_actions' id='",paste0("flag_",evaluation_id),"' value=",evaluation_id," onmousedown='event.stopPropagation();' ",
                                 ifelse(selected==TRUE, #"NEW" flags are those who've never had a comment will be auto-selected, else not.
                                        "checked=true ",
                                        ""),
                                 "onclick='Shiny.setInputValue(\"server_datasets_review_flags_selected\",",evaluation_id,",{priority:\"event\"})' />")]
  
  defs <- list(list(className = 'dt-left', targets = c(0,1,2,3,4,5)),  #Zero-based targets
               list(className = 'dt-center', targets = c(6)))
  
  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(unique(evaluations[selected==TRUE,evaluation_id])) #Refresh for new review
  
  if (indicator_flags_status %in% "revert") {
    
    reversions <- SERVER_DATASETS_REVIEW_FLAGS_REVERSIONS(evaluations$evaluation_id)
    
    evaluations <- evaluations[reversions,
                               on=.(evaluation_id),
                               nomatch=NULL]
    
    evaluations <- evaluations[,.(SYSID=rsf_pfcbl_id,
                                  NAME=entity_name,
                                  Message=check_message,
                                  `System Value`=data_value,
                                  `Revert to Value`=revert_value,
                                  `Reported By`=sapply(users_name,format_name_abbreviation),
                                  Revert=apply_html)]
    
  } else {
    evaluations <- evaluations[,.(SYSID=rsf_pfcbl_id,
                                  NAME=entity_name,
                                  Message=check_message,
                                  Comment=check_status_comment,
                                  Status=check_status,
                                  User=sapply(check_status_users_name,format_name_abbreviation),
                                  Apply=apply_html)]
  }
  DT::datatable(evaluations,
                rownames = FALSE,
                fillContainer=TRUE,
                height = "100%",
                escape = FALSE,
                options=list(
                  dom="t",
                  ordering=FALSE,  
                  paging=FALSE,
                  columnDefs=defs 
                )) %>% 
    formatStyle(columns=c(0,1,4,5,6),whiteSpace="nowrap")
})

output$datasets_review_download_flags_action <- downloadHandler(
  filename = function() {
    selected_id <- input$cohort_collection_selected_id
    cohort <- NULL
    if (identical(selected_id,"all") || is.na(suppressWarnings(as.numeric(selected_id)))) {
      cohort <- SELECTED_COHORT_INFO()
    } else {
      cohort <- COHORTS_LIST()[reporting_cohort_id==as.numeric(selected_id)]
    }
    paste0("Flags Report for ",file_path_sans_ext(cohort$source_name),".xlsx")
  },
  content=function(file) {
    
    
    withProgress(message="Downloading file",value=0.5, {
      
      flags <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[,
                                                           .(pfcbl_category_rank,
                                                             check_rank,
                                                             indicator_id,
                                                             indicator_name,
                                                             formula_title,
                                                             check_name,
                                                             check_type,
                                                             check_class,
                                                             check_formula_title,
                                                             evaluation_ids)]
      flags <- flags[,
                     .(evaluation_id=unlist(evaluation_ids,recursive=F)),
                     by=.(pfcbl_category_rank,
                          check_rank,
                          indicator_id,
                          indicator_name,
                          formula_title,
                          check_name,
                          check_type,
                          check_class,
                          check_formula_title)]
      
      evaluations <- SERVER_DATASETS_REVIEW_FLAGS_QUERY_DETAILS(flags$evaluation_id)
      
      flags <- flags[evaluations,
                     on=.(evaluation_id),
                     nomatch=NULL]
      
      setorder(flags,
               pfcbl_category_rank,
               check_rank,
               entity_name,
               check_type,
               check_name)
      
      flags <- flags[,
                     .(FLAGID=evaluation_id,
                       SYSID=rsf_pfcbl_id,
                       CHECK_DATE=check_asof_date,
                       NAME=entity_name,
                       indicator_name,
                       indicator_formula=formula_title,
                       check_formula=check_formula_title,
                       type=check_type,
                       class=check_class,
                       CHECK=check_name,
                       MESSAGE=check_message,
                       STATUS=check_status,
                       comment=check_status_comment,
                       user=check_status_users_name)]
      
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb,
                             sheetName="FLAGS")
      openxlsx::writeDataTable(wb=wb,
                               sheet="FLAGS",
                               x=flags)
      openxlsx::saveWorkbook(wb=wb,
                             file=file,
                             overwrite=TRUE)
      })
  }
)
