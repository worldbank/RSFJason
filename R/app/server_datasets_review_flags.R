

INDICATOR_FLAGS_SELECTED_EVALUATION_IDS <- reactiveVal(c())

SERVER_DATASETS_REVIEW_FLAGS_REVERSION_CHECK_NAMES <- eventReactive(LOGGEDIN(), {
  
  if (!isTruthy(LOGGEDIN())) return (NULL)
  
  cnames <- DBPOOL %>% dbGetQuery("
    select 
      ic.check_name,
      ic.indicator_check_id
    from p_rsf.indicator_checks ic
    where ic.check_name in ('sys_calculator_overwrote_manual_calculation',
                            'sys_calculator_vs_missing_calculation',
                            'sys_data_status_modified',
                            'waiver_value_vs_sys_calculator')")
  
  setDT(cnames)
  if (nrow(cnames) != 4) {
    stop("System cannot find the following flags in database: sys_calculator_vs_missing_calculation, sys_calculator_overwrote_missing_calculation, sys_data_status_modified, waiver_value_vs_sys_calculator.  Did a name change?  It must be kept for system use.")
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

showModal_indicator_check_config <- function(for_rsf_pfcbl_id,
                                                    for_indicator_id,
                                                    indicator_check_id) {
  #browser()
  indicator <- RSF_INDICATORS()[indicator_id==for_indicator_id]
  if (!isTruthy(indicator)) return (NULL)
  
  indicator_html <- format_html_indicator(indicator_name=indicator$indicator_name,
                                          data_category=indicator$data_category,
                                          data_type=indicator$data_type,
                                          is_system=indicator$is_system,
                                          is_calculated=indicator$is_calculated)
  
  for_name <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_pfcbl_id==for_rsf_pfcbl_id,facility_name]
  
  config <- DBPOOL %>% dbGetQuery("
    select
      ic.check_name,
      ic.check_class,
      ic.check_type,
      ic.is_system,
      coalesce(scc.config_auto_resolve,ic.auto_resolve_system_check) as config_auto_resolve,
      coalesce(scc.config_check_class,ic.check_class) as config_check_class,
      coalesce(scc.config_threshold,0) as config_threshold,
      coalesce(ic.variance_tolerance_allowed,false)::bool as variance_tolerance_allowed,
      coalesce(scc.config_comments,'') as config_comments,
      scc.comments_user_id,
      vai.users_name
    from p_rsf.indicator_checks ic
    left join p_rsf.rsf_setup_checks_config scc on scc.rsf_pfcbl_id = $1::int
                                               and scc.for_indicator_id = $2::int
                                               and scc.indicator_check_id = ic.indicator_check_id
    left join p_rsf.view_account_info vai on vai.account_id = scc.comments_user_id
    where ic.indicator_check_id = $3::int",
    params=list(for_rsf_pfcbl_id,
                for_indicator_id,
                indicator_check_id))
  
  setDT(config)

  check_html <- format_html_check(check_name=config$check_name,
                                  check_class=config$check_class,
                                  check_type=config$check_type,
                                  is_subscribed=TRUE,
                                  is_system=config$is_system)
  
  
  toleranceInput <- {
    toleranceValue <- NULL
    if (config$variance_tolerance_allowed==FALSE) {
      toleranceValue <- 0.0
    } else {
      toleranceValue <- as.numeric(config$config_threshold)
      if (!isTruthy(toleranceValue)) toleranceValue <- 0.0
    }
    
    toleranceValue <- as.numeric(toleranceValue) * 100
    toleranceValue <- paste0(toleranceValue,"%")
    
    ttInput <- textInput(inputId="indicator_check_edit_config__tolerance",
                                label="Auto-Resolve Below Variance",
                                value=as.character(toleranceValue),
                                placeholder="eg, '2.5%', '3 DAYS'")
    
    if (config$variance_tolerance_allowed==FALSE) ttInput <- disabled(ttInput)
    
    ttInput
  }  

  m <- modalDialog(id="view_indicator_check_edit_config",
                   div(
                     fluidRow(column(10,style="display:inline-block",tags$label("Configure System Flag"),
                                     div(style="display:inline-block;",HTML(indicator_html),
                                         div(style="display:inline-block;",HTML(check_html)))),
                              column(2,
                                     actionButton(inputId="indicator_check_edit_config__action_delete",
                                                  label="Delete Config",
                                                  class="btn-danger"))),
                     fluidRow(style="padding-top:10px;",
                              
                              column(3,
                                     selectizeInput(inputId="indicator_check_edit_config__resolving",
                                                    label="Default Review",
                                                    choices=c(`User Review`="FALSE",
                                                              `Auto-Resolve`="TRUE"),
                                                    selected=toupper(paste0(config$config_auto_resolve)),
                                                    multiple=FALSE,
                                                    width="100%")),
                              column(3,
                                     selectizeInput(inputId="indicator_check_edit_config__class",
                                                    label="Flag Severity",
                                                    choices=c(`Critical`='critical',
                                                              `Error`='error',
                                                              `Warning`='warning',
                                                              `Info`='info'),
                                                    selected=tolower(config$config_check_class),
                                                    multiple=FALSE,
                                                    width="100%")),
                              column(3,toleranceInput),

                              column(3,
                                     #hidden so modal can be launched by differerent UIs.  On save, need to verify the rsf_pfcbl_id is available to user and is a system check
                                     hidden(textInput(inputId="indicator_check_edit_config__ids",label=NULL,value=paste0(for_rsf_pfcbl_id,"-",for_indicator_id,"-",indicator_check_id))))),
                                     
                   
                     fluidRow(style="padding-top:10px;",
                              column(12,
                                     textAreaInput(inputId="indicator_check_edit_config__comments",
                                                   label=paste0("Configuration Comments",
                                                                ifelse(is.na(config$users_name),"",
                                                                       paste0(" [",config$users_name,"]"))),
                                                   value=config$config_comments)))
                   ),
                   
                   title=HTML(paste0("Flag Config For: ",for_name)),
                   footer=div(style="display:inline-block;width:100%;",
                              div(style="display:inline-block;float:left;",
                                  actionButton(inputId="indicator_check_edit_config__action_cancel",
                                               label="Cancel",
                                               class="btn-primary")),
                              div(style="display:inline-block;float:right;",
                                  actionButton(inputId="indicator_check_edit_config__action_submit",
                                               label="Save & Close",
                                               class="btn-primary btn-success"))),
                   size="m")
  showModal(m)
}

observeEvent(input$indicator_check_edit_config__action_delete, {
  
  ids <- as.numeric(unlist(strsplit(input$indicator_check_edit_config__ids,split="-")))
  
  if (length(ids) != 3) return (NULL)
  if (!ids[[1]] %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id ||
      !ids[[2]] %in% RSF_INDICATORS()$indicator_id ||
      !ids[[3]] %in% RSF_CHECKS()$indicator_check_id) {
    showNotification(type="error",
                     ui=h3("Failed to configure check: invalid IDs or user does not have permissions"))
    return (NULL)
  }
  
  DBPOOL %>% dbExecute("delete from p_rsf.rsf_setup_checks_config scc
                        where scc.rsf_pfcbl_id = $1::int
                          and scc.for_indicator_id = $2::int
                          and scc.indicator_check_id = $3::int",
                       params=list(ids[[1]],
                                   ids[[2]],
                                   ids[[3]]))
  
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1)
  REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
  removeModal()
  
})

observeEvent(input$indicator_check_edit_config__action_submit, {
  
  ids <- as.numeric(unlist(strsplit(input$indicator_check_edit_config__ids,split="-")))
  
  if (length(ids) != 3) return (NULL)
  if (!ids[[1]] %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id ||
      !ids[[2]] %in% RSF_INDICATORS()$indicator_id ||
      !ids[[3]] %in% RSF_CHECKS()$indicator_check_id) {
    showNotification(type="error",
                     ui=h3("Failed to configure check: invalid IDs or user does not have permissions"))
    return (NULL)
  }

  config_comments <- input$indicator_check_edit_config__comments
  
  config_class <- input$indicator_check_edit_config__class
  
  config_resolving <- as.logical(input$indicator_check_edit_config__resolving)
  
  config_threshold <- input$indicator_check_edit_config__tolerance
  config_threshold <- as.numeric(gsub("[^[:digit:]\\.]","",config_threshold))
  
  #For days, variance is in "DAYS"
  if (RSF_INDICATORS()[indicator_id == ids[2],data_type] %in% c("date")) {
    config_threshold <- round(config_threshold)
  } else {
    config_threshold <- round(config_threshold / 100,2)    
  }


  DBPOOL %>% dbExecute("
    insert into p_rsf.rsf_setup_checks_config(rsf_pfcbl_id,
                                            for_indicator_id,
                                            indicator_check_id,
                                            rsf_program_id,
                                            rsf_facility_id,
                                            config_auto_resolve,
                                            config_check_class,
                                            config_threshold,
                                            config_comments,
                                            comments_user_id)
    select 
      ids.rsf_pfcbl_id,
      ind.indicator_id,
      ic.indicator_check_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      coalesce($4::bool,false) as config_auto_resolve,
      coalesce($5::text,ic.check_class) as config_check_class,
      case when ic.variance_tolerance_allowed is true 
           then coalesce($6::numeric,0)
           else NULL
      end as config_threshold,
      $7::text as config_comments,
      $8::text as comments_user_id
    from p_rsf.rsf_pfcbl_ids ids,
         p_rsf.indicators ind,
         p_rsf.indicator_checks ic
    where ids.rsf_pfcbl_id = $1::int 
      and ind.indicator_id = $2::int
      and ic.indicator_check_id = $3::int
      and ic.is_system is true -- can only config system checks
    on conflict(rsf_pfcbl_id,for_indicator_id,indicator_check_id)
    do update
    set config_auto_resolve = EXCLUDED.config_auto_resolve,
        config_check_class = EXCLUDED.config_check_class,
        config_threshold = EXCLUDED.config_threshold,
        config_comments = EXCLUDED.config_comments,
        comments_user_id = EXCLUDED.comments_user_id",
    params=list(ids[[1]],
                ids[[2]],
                ids[[3]],
                config_resolving,
                config_class,
                config_threshold,
                config_comments,
                USER_ID()))
  
  if (config_resolving == TRUE || 
     (!is.na(config_threshold) && (config_threshold > 0))) {
    
      DBPOOL %>% dbExecute("
      with resolve as (

        select 
          rdc.evaluation_id,
          scc.config_comments,
          scc.comments_user_id
        from p_rsf.rsf_setup_checks_config scc
        inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = scc.rsf_pfcbl_id
        inner join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                            and rdc.indicator_id = scc.for_indicator_id
                                            and rdc.indicator_check_id = scc.indicator_check_id
        left join lateral (select ((regexp_match(check_message,'\\(([[:digit:]\\.]+)[:space:]?(%|DAYS) variance\\)$'))[1]) as val,
                                           ((regexp_match(check_message,'\\([[:digit:]\\.]+[:space:]?(.*) variance\\)$'))[1]) as unit) as var 
                                           on rdc.check_message ~ 'variance'
                                           and var.val is not null
                                           and public.isnumeric(var.val) is true                                          
        where scc.rsf_pfcbl_id = $1::int
          and scc.for_indicator_id = $2::int
          and scc.indicator_check_id = $3::int
          and rdc.check_data_id_is_current is true
          and rdc.check_status = 'active'
          and
          (
            (scc.config_auto_resolve is true)
            or
            (case when var.unit ~* 'days' then var.val::numeric else var.val::numeric/100 end < coalesce(scc.config_threshold,0))
          )
      )
      update p_rsf.rsf_data_checks rdc
      set check_status = 'resolved',
          check_status_comment = concat('Resolved by Flag Configuration: ',res.config_comments),
          check_status_user_id = res.comments_user_id
      from resolve res
      where res.evaluation_id = rdc.evaluation_id",
       params=list(ids[[1]],
                   ids[[2]],
                   ids[[3]]))
  }
  
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1)
  REFRESH_SELECTED_COHORT_DATA(REFRESH_SELECTED_COHORT_DATA()+1)
  removeModal()
  
                       
})

#Action click to review cohort indicator flag details for resolutions: raises modal panel
observeEvent(input$action_indicator_flags_review, {
  
  cohort_group <- SELECTED_IMPORT_COHORT_GROUP()
  selected_indicator_flag_id <- input$action_indicator_flags_review
  
  if (!isTruthy(cohort_group)) return(NULL)
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
  
  check_config <- DBPOOL %>% dbGetQuery("
    select 
      concat('[',coalesce(vai.users_name,'UNKNOWN'),']: ',scc.config_comments) as config_comments
    from p_rsf.view_rsf_setup_check_config scc
    left join p_rsf.view_account_info vai on vai.account_id = scc.comments_user_id
    where scc.rsf_pfcbl_id = $1::int
      and scc.for_indicator_id = $2::int
      and scc.indicator_check_id = $3::int",
    params=list(cohort_group$import_rsf_pfcbl_id,
                cohort_indicator_flag$indicator_id,
                cohort_indicator_flag$indicator_check_id))
  
  check_definition <- DBPOOL %>% dbGetQuery("select 
                                              ic.definition
                                             from p_rsf.indicator_checks ic
                                             where ic.indicator_check_id = any(select unnest(string_to_array($1::text,','))::int)",
                                            params=list(paste0(na.omit(unique(c(cohort_indicator_flag$indicator_check_id))))))
  
  check_definition <- paste0(check_definition$definition,collapse=" {AND ALSO} ")
  
  if (!empty(check_config)) {
    check_config <- div(icon("edit",style="color:black"),
                        check_config$config_comments)
  } else {
    check_config <- NULL
  }
  check_html <-  cohort_indicator_flag$check_html
  check_formula_html <- cohort_indicator_flag$check_formula_html
  
  indicator_html <- cohort_indicator_flag$indicator_html
  formula_html <- cohort_indicator_flag$formula_html
  
  status.choices <- c(Active="active",
                      Resolved="resolved")
  
  check_names <- SERVER_DATASETS_REVIEW_FLAGS_REVERSION_CHECK_NAMES()
  
  if (cohort_indicator_flag$check_name %in% c("sys_calculator_overwrote_manual_calculation",
                                              "sys_calculator_vs_missing_calculation")) {
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
  
  check_formula_setup_ui <- div(style="width:100px","")
  indicator_setup_ui <- div(style="width:100px","System Metric")
                                                
  if (!is.na(cohort_indicator_flag$indicator_is_system) &&
      cohort_indicator_flag$indicator_is_system != TRUE) {
    
    indicator_setup_ui <- div(style="width:100px",
                              HTML(paste("<a href='#' ",
                                         " onclick=\"Shiny.setInputValue('action_indicator_flags__setup_indicator',",
                                                                          as.numeric(Sys.time()),
                                                                           ",{priority:'event'})\">",
                                         "Setup Metric <i class='far fa-eye'></i></a>")))
    
  }
  
  if (!is.na(cohort_indicator_flag$indicator_formula_id)) {
    indicator_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_indicator",
                                                label="Audit Calculation",
                                                class="btn-primary",
                                                icon=icon("calculator"))
    
    
  }
  
  #System checks will not have a formula to review (or setup)
  if (!is.na(cohort_indicator_flag$check_formula_id)) {
    check_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_check",
                                            label="Audit Check",
                                            class="btn-primary",
                                            icon=icon("flag"))
    
    check_formula_setup_ui <- div(style="width:100px",
                              HTML(paste("<a href='#' ",
                                         " onclick=\"Shiny.setInputValue('action_indicator_flags__setup_check',",
                                         as.numeric(Sys.time()),
                                         ",{priority:'event'})\">",
                                         "Setup Check <i class='far fa-eye'></i></a>")))
  
  } else {
    #System checks have no formula and are not "setup" strictly speaking.
    #But being able to configure calculation overwrites is often expected
    check_formula_setup_ui <- div(style="width:100px",
                                  HTML(paste("<a href='#' ",
                                             " onclick=\"Shiny.setInputValue('action_indicator_flags__config_check',",
                                             as.numeric(Sys.time()),
                                             ",{priority:'event'})\">",
                                             "Config Flag <i class='far fa-edit'></i></a>")))
    
  }
  
  m <- modalDialog(id="view_indicator_flags_review",
                   title=HTML(paste0("Review Flags: ",
                                     cohort_group$entity_name," ",
                                     format_asof_date_label(cohort_group$cohort_asof_date)," ",
                                     "[upload #",cohort_group$import_id,"]")),
                   div(style="max-height:600px;width:100%;overflow-y:auto;",
                       fluidPage(
                         fluidRow(column(8,style="display:inline-block",
                                         div(style="display:flex;flex-flow:row nowrap;",
                                             indicator_setup_ui,
                                             div(HTML(indicator_html)),
                                             div(HTML(formula_html))),
                                         
                                         div(style="display:flex;flex-flow:row nowrap;",
                                             check_formula_setup_ui,
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
                         fluidRow(style="padding-top:10px;",
                                  column(12,
                                         div(style='display:inline-block',icon("info",style="color:blue"),check_definition),
                                         check_config)),

                         
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
  #print(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS())
})

#When clicked, navigates to the Setup page and auto-filtered for the selected check.
observeEvent(input$action_indicator_flags__setup_check, {
  
  selected_indicator_flag_id <- as.numeric(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  setup_indicator_check <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(setup_indicator_check)) return (NULL)
  
  
  
  selected_id <- NULL
  if (!(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id)) {
    selected_id <- DBPOOL %>% dbGetQuery("selected coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id
                                          from p_rsf.rsf_pfcbl_ids ids
                                          where ids.rsf_pfcbl_id = $1::int",
                                         params=list(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id))
    selected_id <- as.numeric(unlist(selected_id))
    
  } else {
    selected_id <- SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id
    
  }
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__checks_program_facilities",
                       selected=selected_id)
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__checks_monitoring_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__checks_category_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__checks_type_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__checks_search_filter",
                       selected=paste0(setup_indicator_check$check_name," ",setup_indicator_check$check_formula_title))
  
  if (!"tabset_setup_program" %in% input$sidebarMenu)  {
    updateTabItems(session=session,
                   inputId="sidebarMenu",
                   selected="setup")
  }
  
  updateTabsetPanel(session=session,
                    inputId="tabset_setup_program",
                    selected="setup_checks")
  
  removeModal()
})

#When clicked, navigates to the Setup page and auto-filtered for the selected check.
observeEvent(input$action_indicator_flags__config_check, {
  
  selected_indicator_flag_id <- as.numeric(input$action_indicator_flags_review)
  

  if (!isTruthy(selected_indicator_flag_id)) return(NULL)

  config_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]

  if (empty(config_indicator_flag)) return (NULL)

  selected_id <- NULL
  if (!(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id)) {
    selected_id <- DBPOOL %>% dbGetQuery("selected coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id
                                          from p_rsf.rsf_pfcbl_ids ids
                                          where ids.rsf_pfcbl_id = $1::int",
                                         params=list(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id))
    selected_id <- as.numeric(unlist(selected_id))
    
  } else {
    selected_id <- SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id
    
  }
  
  for_indicator_id <- config_indicator_flag$indicator_id
  indicator_check_id <- config_indicator_flag$indicator_check_id
  
  showModal_indicator_check_config(for_rsf_pfcbl_id=selected_id,
                                   for_indicator_id=config_indicator_flag$indicator_id,
                                   indicator_check_id=config_indicator_flag$indicator_check_id)
    
})

#When clicked, navigates to the Setup page and auto-filtered for the selected indicator.

observeEvent(input$action_indicator_flags__setup_indicator, {
  
 
  selected_indicator_flag_id <- as.numeric(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  setup_indicator <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(setup_indicator)) return (NULL)
  
  selected_id <- NULL
  if (!(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id)) {
    selected_id <- DBPOOL %>% dbGetQuery("selected coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id
                                          from p_rsf.rsf_pfcbl_ids ids
                                          where ids.rsf_pfcbl_id = $1::int",
                                         params=list(SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id))
    selected_id <- as.numeric(unlist(selected_id))
    
  } else {
    selected_id <- SELECTED_IMPORT_COHORT_GROUP()$import_rsf_pfcbl_id
    
  }
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_program_facilities",
                       selected=selected_id)
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_monitoring_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_category_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_search_filter",
                       selected="")
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_search_filter",
                       selected=setup_indicator$indicator_name)
  
  if (!"tabset_setup_program" %in% input$sidebarMenu)  {
    updateTabItems(session=session,
                   inputId="sidebarMenu",
                   selected="setup")
  }
  
  updateTabsetPanel(session=session,
                    inputId="tabset_setup_program",
                    selected="setup_indicators")
  
  removeModal()
  
})

#Action button: go to dashboard.  If flag applies to a calculated indicator, import parameters, too
observeEvent(input$action_review_indicator_flags_view_dashboard, {
  cohort_group <- SELECTED_IMPORT_COHORT_GROUP()
  
  if (empty(cohort_group)) return(NULL)
  
  selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(indicator_flag)) return (NULL)
  

  flagged_indicator_id <- unique(indicator_flag$indicator_id)
  flagged_indicator_check_id <- unique(indicator_flag$indicator_check_id)

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
  
  for_facility_sys_names <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_facility_id %in% cohort_group$rsf_facility_id,
                                                             rsf_pfcbl_id]
  
  SERVER_DASHBOARD_DO_LOAD(for_facility_sys_names=for_facility_sys_names,
                           for_indicator_names=for_indicator_names,
                           for_asof_dates=cohort_group$cohort_asof_date,
                           dashboard_parameters=dashboard_parameters)
})

observeEvent(input$action_review_indicator_flags_audit_indicator, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  selected_cohort <- SELECTED_IMPORT_COHORT_GROUP()
  #browser()
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  cohort_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_indicator_id <- as.numeric(cohort_indicator_flag$indicator_id)
  
  if (!isTruthy(review_indicator_id)) return (NULL)
  
  show_modal_indicator_review(rsf_pfcbl_id=selected_cohort$import_rsf_pfcbl_id,
                              review_asof_date=selected_cohort$cohort_asof_date,
                              review_indicator_id=review_indicator_id)
  
},ignoreInit = TRUE)

observeEvent(input$action_review_indicator_flags_audit_check, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  selected_cohort <- SELECTED_IMPORT_COHORT_GROUP()
  #browser()
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  cohort_indicator_flag <- SELECTED_COHORT_INDICATOR_FLAGS_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_check_formula_id <- as.numeric(cohort_indicator_flag$check_formula_id)
  if (!isTruthy(review_check_formula_id)) return (NULL)
  
  show_modal_server_admin_checks_review(rsf_pfcbl_id=selected_cohort$import_rsf_pfcbl_id,
                                        review_asof_date=selected_cohort$cohort_asof_date,
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
                                            check_data_id_is_current
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
          true as check_data_id_is_current, -- not now, but should become so by end of transaction!
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
      cohort_group <- SELECTED_IMPORT_COHORT_GROUP()
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
                                         rsf_pfcbl_id.family = cohort_group$import_rsf_pfcbl_id,
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
                                     rsf_pfcbl_id.family=cohort_group$import_rsf_pfcbl_id,
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
    
    cohort <- SELECTED_IMPORT_COHORT_GROUP()
    
    paste0("Flags Report for ",cohort$entity_name," ",cohort$reporting_asof_date_label,".xlsx")
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
                       #SYSID=rsf_pfcbl_id,
                       CHECK_DATE=check_asof_date,
                       NAME=entity_name,
                       #indicator_name,
                       #indicator_formula=formula_title,
                       type=check_type,
                       class=check_class,
                       MESSAGE=check_message,
                       CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
                                                               check_formula_title)),
                       
                       STATUS=check_status,
                       comment=check_status_comment,
                       user=check_status_users_name)]
    
      wb <- openxlsx::createWorkbook()
      wrap_style <- createStyle(wrapText = TRUE)

      openxlsx::addWorksheet(wb,
                             sheetName="FLAGS")
      openxlsx::writeDataTable(wb=wb,
                               sheet="FLAGS",
                               x=flags)
      openxlsx::setColWidths(wb,
        sheet="FLAGS",
        cols=c(1,2,3,4,5,6,7,8,9,10),
        widths = c(10,
                   13, #check date 
                   35, #entity name
                   17, #check type
                   10, #check class
                   90, #check message
                   90, #check
                   10,
                   10,
                   10))
      
      addStyle(wb,
               sheet="FLAGS",
               wrap_style,
               rows = 1:nrow(flags),
               cols = 6)
      
      openxlsx::saveWorkbook(wb=wb,
                             file=file,
                             overwrite=TRUE)
      })
  }
)
