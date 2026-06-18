

INDICATOR_FLAGS_SELECTED_EVALUATION_IDS <- reactiveVal(c())



SERVER_DATASETS_REVIEW_FLAGS_SELECTED_FLAG_EVALUATIONS <- eventReactive(c(IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED(),
                                                                          input$action_indicator_flags_review), {

 selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
 indicator_flags <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()
 
 if (!isTruthy(selected_indicator_flag_id)) return (NULL)
 if (!isTruthy(indicator_flags)) return (NULL)
 if (!selected_indicator_flag_id %in% indicator_flags$indicator_flag_id) return(NULL)
 
 #IMPORT_FLAGS_SELECTED has all the underlying flag data.  Whereas IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED has reflects what remains after menu selections to focus on indicator names, titles, summary info
 indicator_flags <- IMPORT_FLAGS_SELECTED()[indicator_flag_id==selected_indicator_flag_id]
 
 #this should be in order of rank
 setorder(indicator_flags,
          rsf_pfcbl_id)
 
 return (indicator_flags)

},ignoreNULL=FALSE)

#This is to avoid downloading potentially a lot of text data for entity names, check comments and status messages in IMPORT_FLAGS_SELECTED()
SERVER_DATASETS_REVIEW_FLAGS_db_EVALUATION_DETAILS <- function(evaluation_ids) {
  
  evaluation_ids <- paste0(unique(evaluation_ids),collapse=",")
  flag_evaluations <- DBPOOL %>% dbGetQuery("select 
    rdc.evaluation_id,
    rdc.rsf_pfcbl_id,
    rdc.indicator_id,
    rdc.indicator_check_id,
    rdc.check_asof_date,
    rdc.check_formula_id,
    rdc.check_status,
    rdc.check_status_comment,
    rdc.check_message,
    rdc.check_status_user_id,
    vai.users_name as check_status_users_name,
    nids.rsf_full_name as entity_name,
    nids.pfcbl_name
    from p_rsf.rsf_data_checks rdc
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = rdc.rsf_pfcbl_id
    left join p_rsf.view_account_info vai on vai.account_id = rdc.check_status_user_id
    where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(evaluation_ids))
  
  setDT(flag_evaluations)
  
 
  
  return (flag_evaluations)
}

SERVER_DATASETS_REVIEW_FLAGS_SETUP_CHECK <- eventReactive(c(input$indicator_check_edit_config__ids,
                                                            input$indicator_check_edit_setup__action_submit), {
                                                          
  ids <- as.numeric(unlist(strsplit(input$indicator_check_edit_config__ids,split="-")))
  
  if (length(ids) != 4) return (NULL)
  if (!ids[[1]] %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id ||
      !ids[[2]] %in% RSF_INDICATORS()$indicator_id ||
      !ids[[4]] %in% RSF_CHECK_FORMULAS()$check_formula_id) {
    
    return (NULL)
  }
  
  subscription <- DBPOOL %>% dbGetQuery("
    select
      scs.category_manager_rsf_pfcbl_id as rsf_pfcbl_id,
      scs.check_formula_id,
      scs.is_subscribed,
      scs.is_auto_subscribed,
      scs.subscription_comments,
      vai.users_name
    from p_rsf.view_rsf_setup_check_subscriptions scs
    left join p_rsf.view_account_info vai on vai.account_id = scs.comments_user_id
    where scs.rsf_pfcbl_id = $1::int
      and scs.check_formula_id = $2::int",
    params=list(ids[[1]],
                ids[[4]]))
  
  return (subscription)
  

},ignoreInit=FALSE,ignoreNULL=FALSE)

INDICATOR_FLAGS_SELECTED_SYS_FLAG_STATUS_VIEW <- eventReactive(input$indicator_flags_status, {
  
  if (input$indicator_flags_status %in% "revert") { "revert" }
  else { "regular" }
  
})


showModal_indicator_check_config <- function(for_rsf_pfcbl_id,
                                             for_indicator_id,
                                             indicator_check_id,
                                             check_formula_id) {
  
  indicator <- RSF_INDICATORS()[indicator_id==for_indicator_id]
  if (!isTruthy(indicator)) return (NULL)
  
  indicator_html <- format_html_indicator(indicator_name=indicator$indicator_name,
                                          data_category=indicator$data_category,
                                          data_type=indicator$data_type,
                                          is_system=indicator$is_system,
                                          is_calculated=indicator$is_calculated)
  
  for_name <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_pfcbl_id==for_rsf_pfcbl_id,facility_name]
  
  if (!isTruthy(check_formula_id)) { check_formula_id <- 0 } #a non-ID that will return NULL
  

  setup <- DBPOOL %>% dbGetQuery("
    select
      scs.rsf_pfcbl_id,
      scs.pfcbl_category,
      scs.check_formula_id,
      scs.is_subscribed,
      scs.is_auto_subscribed,
      scs.subscription_comments,
      scs.comments_user_id,
      icf.check_formula_title,
      vai.users_name
    from p_rsf.view_rsf_setup_check_subscriptions scs
    inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = scs.check_formula_id
    left join p_rsf.view_account_info vai on vai.account_id = scs.comments_user_id
    where scs.rsf_pfcbl_id = $1::int
      and scs.check_formula_id = $2::int",
    params=list(for_rsf_pfcbl_id,
                check_formula_id))                                 
  
  #Config allows both system and user checks to be customized
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
                                               and scc.check_formula_id is not distinct from NULLIF($4::text,'0')::int
    left join p_rsf.view_account_info vai on vai.account_id = scc.comments_user_id
    where ic.indicator_check_id = $3::int",
    params=list(for_rsf_pfcbl_id,
                for_indicator_id,
                indicator_check_id,
                check_formula_id))
  
  setDT(config)

  check_html <- format_html_check(check_name=config$check_name,
                                  check_class=config$check_class,
                                  check_type=config$check_type,
                                  is_subscribed=TRUE,
                                  is_system=config$is_system)
  check_formula_html <- NULL
  if (!empty(setup) && !is.na(setup$check_formula_title)) {
  check_formula_html <- format_html_indicator(
                             indicator_name=gsub("'","%39;",setup$check_formula_title),
                             data_category="formula",
                             data_type="",
                             is_system=FALSE,
                             is_calculated=FALSE,
                             is_subscribed=TRUE,
                             id=setup$check_formula_id)
  }
  
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

  setup_ui <- NULL
  if (!empty(setup)) {
    scomments <- setup$subscription_comments
    if (!isTruthy(scomments)) scomments <- ""
    
    setup_ui <- tagList(
      fluidRow(style="padding-top:10px;",
        column(9,
               textAreaInput(inputId="indicator_check_edit_setup__comments",
                             label=paste0("Setup Comments",
                                          ifelse(is.na(setup$users_name),"",
                                                 paste0(" [",setup$users_name,"]"))),
                             value=scomments,
                             placeholder="Enter RSA reference or Business Rule for this check")
        ),
        column(3,style="padding-top:24px",
               uiOutput(outputId="indicator_check_edit_setup__subscription_ui"))))
  }
    

  m <- modalDialog(id="view_indicator_check_edit_config",
                   div(
                     fluidRow(column(12,
                                     div(style="display:flex;flex-direction:row;",
                                         tags$label(paste0("Configure ",ifelse(config$is_system,"System","")," Flag")),
                                         div(style="padding-left:10px;",HTML(check_html)),
                                         div(style="padding-left:5px;",HTML(check_formula_html))),
                                     div(style="display:flex;flex-direction:row;",
                                         tags$label("On"),
                                         div(style="padding-left:10px;",HTML(indicator_html))))),
                     
                     fluidRow(style="border-bottom:solid black 1px;padding-bottom:10px;",
                              column(12,
                              #hidden so modal can be launched by differerent UIs.  On save, need to verify the rsf_pfcbl_id is available to user and is a system check
                              hidden(textInput(inputId="indicator_check_edit_config__ids",
                                               label=NULL,
                                               value=paste0(for_rsf_pfcbl_id,"-",
                                                            for_indicator_id,"-",
                                                            indicator_check_id,"-",
                                                            check_formula_id))))),
                     
                     setup_ui,
                     
                     fluidRow(style="padding-top:10px;",
                              column(3,
                                     selectizeInput(inputId="indicator_check_edit_config__resolving",
                                                    label="Flag Status",
                                                    choices=c(`User Review`="FALSE",
                                                              `Always Resolve`="TRUE"),
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
                                     div(style="padding-top:25px",
                                     actionButton(inputId="indicator_check_edit_config__action_delete",
                                                  label="Delete Config",
                                                  class="btn-danger")))),
                   
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
                                  modalButton("Cancel")),
                              div(style="display:inline-block;float:right;",
                                  actionButton(inputId="indicator_check_edit_config__action_submit",
                                               label="Save & Close",
                                               class="btn-primary btn-success"))),
                   size="m")
  showModal(m)
}



#An alternative to managing the check subscription via the SETUP interface.
observeEvent(input$indicator_check_edit_setup__action_submit, {

  setup_check <- SERVER_DATASETS_REVIEW_FLAGS_SETUP_CHECK()
  
  if (empty(setup_check)) {
    
    showNotification(type="error",
                     ui=h3("Failed to setup check: invalid IDs or user does not have permissions"))
    return (NULL)
  }
 
  existing_comments <- as.character(setup_check$subscription_comments)
  new_comments <- as.character(input$indicator_check_edit_setup__comments)

  if (!isTruthy(existing_comments)) { existing_comments <- "" }
  if (!isTruthy(new_comments)) { new_comments <- "" }
  
  if (setup_check$is_subscribed==TRUE & (new_comments==existing_comments || nchar(new_comments) < 3)) {
    
    showNotification(type="error",
                     ui=h3("Please update setup comments about why check is deactivated to turn off this check"))
    return (NULL)
  }
  
  DBPOOL %>% dbExecute("
    insert into p_rsf.rsf_setup_checks(rsf_pfcbl_id,
                                       check_formula_id,
                                       indicator_check_id,
                                       rsf_program_id,
                                       rsf_facility_id,
                                       is_subscribed,
                                       is_auto_subscribed,
                                       subscription_comments,
                                       comments_user_id,
                                       auto_subscribed_by_reporting_cohort_id)
    select
      scs.category_manager_rsf_pfcbl_id as rsf_pfcbl_id,
      scs.check_formula_id,
      scs.indicator_check_id,
      scs.rsf_program_id,
      scs.rsf_facility_id,
      (not scs.is_subscribed) as is_subscribed,
      false as is_auto_subscribed,
      $3::text as subscription_comments,
      $4::text as comments_user_id,
      NULL as auto_subscribed_by_reporting_cohort_id
    from p_rsf.view_rsf_setup_check_subscriptions scs
    
    where scs.rsf_pfcbl_id = $1::int
      and scs.check_formula_id = $2::int
      
    on conflict(rsf_pfcbl_id,check_formula_id)
    do update
    set is_subscribed = EXCLUDED.is_subscribed,
        indicator_check_id = EXCLUDED.indicator_check_id,
        is_auto_subscribed = EXCLUDED.is_auto_subscribed,
        subscription_comments = EXCLUDED.subscription_comments,
        comments_user_id = EXCLUDED.comments_user_Id,
        auto_subscribed_by_reporting_cohort_id = EXCLUDED.auto_subscribed_by_reporting_cohort_id",
    params=list(setup_check$rsf_pfcbl_id,
                setup_check$check_formula_id,
                new_comments,
                USER_ID()))
  
  updateTextAreaInput(session=session,
                      inputId="indicator_check_edit_setup__comments",
                      label=paste0("Setup Comments [",USER_NAME(),"]"))
  
})

observeEvent(input$indicator_check_edit_config__action_delete, {
  
  ids <- as.numeric(unlist(strsplit(input$indicator_check_edit_config__ids,split="-")))
  
  if (length(ids) != 4) return (NULL)
  if (!ids[[1]] %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id ||
      !ids[[2]] %in% RSF_INDICATORS()$indicator_id ||
      !ids[[3]] %in% RSF_CHECKS()$indicator_check_id) {
    showNotification(type="error",
                     ui=h3("Failed to configure check: invalid IDs or user does not have permissions"))
    return (NULL)
  }
  
  check_formula_id <- as.numeric(ids[[4]])
  if (!(check_formula_id %in% RSF_CHECK_FORMULAS()$check_formula_id)) {
    check_formula_id <- NA
  }
  
  DBPOOL %>% dbExecute("delete from p_rsf.rsf_setup_checks_config scc
                        where scc.rsf_pfcbl_id = $1::int
                          and scc.for_indicator_id = $2::int
                          and scc.indicator_check_id = $3::int
                          and scc.check_formula_id = (NULLIF($4::text,'NA')::int)",
                       params=list(ids[[1]],
                                   ids[[2]],
                                   ids[[3]],
                                   check_formula_id))
  
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1)
  IMPORT_LIST__REFRESH(IMPORT_LIST__REFRESH()+1)
  removeModal()
})

observeEvent(input$indicator_check_edit_config__action_submit, {
  
  ids <- as.numeric(unlist(strsplit(input$indicator_check_edit_config__ids,split="-")))
  
  if (length(ids) != 4) return (NULL)
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
  config_threshold <- suppressWarnings(as.numeric(gsub("[^[:digit:]\\.]","",config_threshold)))
  
  if (is.na(config_threshold)) config_threshold <- 0
  #For days, variance is in "DAYS"
  if (RSF_INDICATORS()[indicator_id == ids[2],data_type] %in% c("date")) {
    config_threshold <- round(config_threshold)
  } else {
    config_threshold <- round(config_threshold / 100,2)    
  }

  setup_check <- SERVER_DATASETS_REVIEW_FLAGS_SETUP_CHECK()
  check_formula_id <- NA
  #Expected to be empty/NULL when system check is being configured (Ie, there is no setup available for system checks)
  if (!empty(setup_check)) {
    existing_comments <- as.character(setup_check$subscription_comments)
    new_comments <- as.character(input$indicator_check_edit_setup__comments)
    check_formula_id <- setup_check$check_formula_id
    
    if (!isTruthy(existing_comments)) { existing_comments <- "" }
    if (!isTruthy(new_comments)) { new_comments <- "" }

    #means that the user changed the setup comments, but did not click to change monitoring the check
    #so save the updated user comments
    if (existing_comments != new_comments) {
      DBPOOL %>% dbExecute("
        update p_rsf.rsf_setup_checks rsc
        set is_auto_subscribed = false,
            auto_subscribed_by_reporting_cohort_id = NULL,
            subscription_comments = NULLIF($3::text,''),
            comments_user_id = $4::text
        where rsc.rsf_pfcbl_id = $1::int
          and rsc.check_formula_id = $2::int",
        params=list(setup_check$rsf_pfcbl_id,
                    setup_check$check_formula_id,
                    new_comments,
                    USER_ID()))
    }  
  }
   
  DBPOOL %>% dbExecute("
    insert into p_rsf.rsf_setup_checks_config(rsf_pfcbl_id,
                                            for_indicator_id,
                                            indicator_check_id,
                                            check_formula_id,
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
      NULLIF($9::text,'NA')::int as check_formula_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      coalesce($4::bool,false) as config_auto_resolve,
      coalesce($5::text,ic.check_class) as config_check_class,
      case when ic.variance_tolerance_allowed is true 
           then coalesce($6::numeric,0)
           else 0
      end as config_threshold,
      $7::text as config_comments,
      $8::text as comments_user_id
    from p_rsf.rsf_pfcbl_ids ids,
         p_rsf.indicators ind,
         p_rsf.indicator_checks ic
    where ids.rsf_pfcbl_id = $1::int 
      and ind.indicator_id = $2::int
      and ic.indicator_check_id = $3::int
      
    on conflict on constraint rsf_setup_checks_config_uids_ucnst -- this uses postgresql 15 nulls not distinct to accommodate null check_formula_id for sys checks
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
                USER_ID(),
                check_formula_id))
  
  #will retroactively update active flags that are within the variance specified by the config
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
                                            and rdc.check_formula_id is not distinct from $4::int
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
            (case when var.unit ~* 'days' then var.val::numeric else var.val::numeric end < coalesce(scc.config_threshold,0))
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
                   ids[[3]],
                   check_formula_id))
    
    
    DBPOOL %>% dbExecute("
      with resolve as (

        select 
          dca.archive_id,
          scc.config_comments,
          scc.comments_user_id
        from p_rsf.rsf_setup_checks_config scc
        inner join p_rsf.view_rsf_pfcbl_id_family_tree ft on ft.from_rsf_pfcbl_id = scc.rsf_pfcbl_id
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
        inner join p_rsf.rsf_data_checks_archive dca on dca.sys_name = sn.sys_name
                                                    and dca.indicator_id = scc.for_indicator_id
                                                    and dca.indicator_check_id = scc.indicator_check_id
                                                    and dca.check_formula_id is not distinct from $4::int
        left join lateral (select ((regexp_match(check_message,'\\(([[:digit:]\\.]+)[:space:]?(%|DAYS) variance\\)$'))[1]) as val,
                                           ((regexp_match(check_message,'\\([[:digit:]\\.]+[:space:]?(.*) variance\\)$'))[1]) as unit) as var 
                                           on dca.check_message ~ 'variance'
                                           and var.val is not null
                                           and public.isnumeric(var.val) is true                                          
        where scc.rsf_pfcbl_id = $1::int
          and scc.for_indicator_id = $2::int
          and scc.indicator_check_id = $3::int
          and dca.check_status = 'active'
          and
          (
            (scc.config_auto_resolve is true)
            or
            (case when var.unit ~* 'days' then var.val::numeric else var.val::numeric end < coalesce(scc.config_threshold,0))
          )
      )
      update p_rsf.rsf_data_checks_archive dca
      set check_status = 'resolved',
          check_status_comment = concat('Resolved by Flag Configuration: ',res.config_comments),
          check_status_user_id = res.comments_user_id
      from resolve res
      where res.archive_id = dca.archive_id",
                         params=list(ids[[1]],
                                     ids[[2]],
                                     ids[[3]],
                                     check_formula_id))
  }
  
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1)
  IMPORT_LIST__REFRESH(IMPORT_LIST__REFRESH()+1)
  removeModal()
})

#Action click to review cohort indicator flag details for resolutions: raises modal panel
observeEvent(input$action_indicator_flags_review, {
  
  import <- IMPORT_SELECTED()
  selected_indicator_flag_id <- input$action_indicator_flags_review
  
  if (!isTruthy(import)) return(NULL)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  flag_evaluations <- SERVER_DATASETS_REVIEW_FLAGS_SELECTED_FLAG_EVALUATIONS()
  flag_selected <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  
  if (!isTruthy(flag_evaluations) || !isTruthy(flag_selected)) {
    return (showNotification(type="error",
                             h3("Unable to review flags as no flags exist for this selection.  Please try again by clicking on the review icon")))
  }
  
  if (nrow(flag_evaluations) >= 10) {
    show_modal_spinner(spin = "circle",
                       color = "blue",
                       text = "Loading...",
                       session = shiny::getDefaultReactiveDomain())
  }
  
  check_definition <- NULL
  setup_definition <- NULL
  config_definition <- NULL
  
  check_config <- DBPOOL %>% dbGetQuery("
    select 
      concat('[',coalesce(vai.users_name,'UNKNOWN'),']: ',scc.config_comments) as config_comments
    from p_rsf.view_rsf_setup_check_config scc
    left join p_rsf.view_account_info vai on vai.account_id = scc.comments_user_id
    where scc.rsf_pfcbl_id = $1::int
      and scc.for_indicator_id = $2::int
      and scc.indicator_check_id = $3::int
      and scc.check_formula_id is not distinct from (NULLIF($4::text,'NA')::int)",
    params=list(import$import_rsf_pfcbl_id,
                flag_selected$indicator_id,
                flag_selected$indicator_check_id,
                flag_selected$check_formula_id))
  
  if (!empty(check_config)) {
    config_definition <- div(icon("gears",style="color:black"),
                             check_config$config_comments)
  } else {
    config_definition <- NULL
  }
  
  
  check_definition <- DBPOOL %>% dbGetQuery("
    select 
      ic.definition
    from p_rsf.indicator_checks ic
    where ic.indicator_check_id = any(select unnest(string_to_array($1::text,','))::int)",
    params=list(flag_selected$indicator_check_id))
  
  check_definition <- unlist(check_definition$definition)
  
  if (nchar(trim(check_definition))==0) { check_definition <- NULL
  } else {
    check_definition <- div(icon("info",style="color:blue"),
                            check_definition)
  }
  
  #user-defined checks will have a formula_id and system checks will not
  if (!is.na(flag_selected$check_formula_id)) {
    
    setup_definition <- DBPOOL %>% dbGetQuery("
      select 
        coalesce(vai.users_name,'SYSTEM') as name,
        scs.subscription_comments
      from p_rsf.view_rsf_setup_check_subscriptions scs
      left join p_rsf.view_account_info vai on vai.account_id = scs.comments_user_id
      where scs.rsf_pfcbl_id = $1::int
        and scs.check_formula_id = $2::int",
    params=list(import$import_rsf_pfcbl_id,
                flag_selected$check_formula_id))
    
    if (!empty(setup_definition)) {
      setup_definition <- div(icon("toggle-off",style="color:green"),
                              paste0(setup_definition$subscription_comments," [",format_name_abbreviation(setup_definition$name),"] "))
    } else {
      setup_definition <- NULL
    }
  }
  
  definition <- div(check_definition,
                    setup_definition,
                    config_definition)
  
  
  check_html <-  flag_selected$check_html
  check_formula_html <- flag_selected$check_formula_html
  
  indicator_html <- flag_selected$indicator_html
  formula_html <- flag_selected$formula_html
  
  status.choices <- c(Active="active",
                      Resolved="resolved")
  

  placeholder <- "Apply update comment to all marked flags..."
  if (any(flag_selected$check_class=="critical")) { #Allows us to "resolve" 
    
    if (!any(flag_selected$check_is_system)) {
      status.choices <- c(Active="active")
      status.selected <- "active"
      #Can always submit "active" with
      placeholder <- "Critical flags must be resolved by deleting and re-uploading corrected datasets"
    } else {
      status.choices <- c(Active="active",
                          Ignore="resolved")
      
      status.selected <- "active"
      placeholder <- "Ignoring critical flags may result in incorrect data calculations and results reporting. Ignore with caution!"
    }
  }
  
  if (any(!is.na(flag_evaluations$data_flag_value) & flag_evaluations$data_flag_value > 0)) {
    
    data_flags <- unique(flag_evaluations[data_flag_value>0,.(data_flag_name,data_flag_value)])
    
    if (nrow(data_flags) > 1) {
      showNotification(type="error",
                       h3("Multiple data flags present in checks (which should not be possbile) -- selecting first entry: ",data_flags))
      data_flags <- data_flags[1]
    }
    
    data_flags <- fcase(data_flags$data_flag_name=="MANUAL","Reject System Calculation",
                        data_flags$data_flag_name=="CALCULATE","Use System Calculation",
                        data_flags$data_flag_name=="CORRECTION","Apply Historic Correction",
                        default=paste0("ERROR: tag unknown: ",dat_flags$data_flag_name))
    
    data_flags <- setNames("applyflag",data_flags)
    status.choices <- c(status.choices,
                        data_flags)
  
  }
  
  if (any(!is.na(flag_evaluations$data_sys_flags))) {
    
    data_flags <- unique(flag_evaluations[,.(data_sys_flags)])

    if (nrow(data_flags) > 1) {
      showNotification(type="error",
                       h3("Multiple data flags present in checks (which should not be possbile) -- selecting first entry: ",data_flags))
      data_flags <- data_flags[1]
    }
    
    data_flags <- SERVER_DATASETS_FLAGS_DATA_SYS_FLAGS()[sapply(data_flag_value,bitwAnd,b=data_flags$data_sys_flags) > 0]
    
    data_flags <- fcase(data_flags$data_flag_name=="MANUAL","Undo Reject Calculation",
                        data_flags$data_flag_name=="CALCULATE","Undo System Calculation",
                        data_flags$data_flag_name=="CORRECTION","Undo Historic Correction",
                        default=paste0("ERROR: tag unknown: ",data_flags$data_flag_name))
    
    data_flags <- setNames("removeflag",data_flags)
    status.choices <- c(status.choices,
                        data_flags)
    
  }
  
  status.selected <- ""
  #status.selected <- "new"
  
  status_review.choices <- c(All="all",
                             None="none",
                             New="new")

  filter_selected <- "None"
 
  
  if (all(input$cohort_view_flagged_data=="ACTIVE")) {
    filter_selected <- "Active"
  } else if (all(input$cohort_view_flagged_data=="RESOLVED")) {
    filter_selected <- "Resolved"
  }
  
  
  
  
  indicator_formula_review_ui <- NULL
  check_formula_review_ui <- NULL
  
  check_formula_setup_ui <- div(style="width:100px","")
  indicator_setup_ui <- div(style="width:100px","System Metric")
                                                
  if (!is.na(flag_selected$indicator_is_system) &&
      flag_selected$indicator_is_system != TRUE) {
    
    indicator_setup_ui <- div(style="width:100px",
                              HTML(paste("<a href='#' ",
                                         " onclick=\"Shiny.setInputValue('action_indicator_flags__setup_indicator',",
                                                                          as.numeric(Sys.time()),
                                                                           ",{priority:'event'})\">",
                                         "Setup Metric <i class='far fa-eye'></i></a>")))
    
  }
  
  if (!is.na(flag_selected$indicator_formula_id)) {
    indicator_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_indicator",
                                                label="Audit Calculation",
                                                class="btn-primary",
                                                icon=icon("calculator"))
    
    
  }
  
  #System checks will not have a formula to review (or setup)
  if (!is.na(flag_selected$check_formula_id)) {
    check_formula_review_ui <- actionButton(inputId="action_review_indicator_flags_audit_check",
                                            label="Audit Check",
                                            class="btn-primary",
                                            icon=icon("flag"))
  }
  
  {
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
                                     import$entity_name," ",
                                     format_asof_date_label(import$reporting_asof_date)," ",
                                     "[upload #",import$import_id,"]")),
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
                                         div(style='display:inline-block',
                                             definition))),
                         
                         fluidRow(style="padding-top:10px;width:100%",
                                  column(1,
                                        selectizeInput(inputId="indicator_flags_status_filter",
                                                       label="Filter",
                                                       choices=c(ALL="None","Active","Resolved","New"),
                                                       selected=filter_selected)),
                                  column(7,
                                         textAreaInput(inputId="indicator_flags_status_message",
                                                       label="Set Status Message:",
                                                       width="100%",
                                                       placeholder=placeholder,
                                                       rows = 1)
                                  ),
                                  column(4,style="display:flex;flex-direction: row;flex-wrap:nowrap",
                                         div(style='width:190px;white-space:nowrap;justify-content:left;',
                                             selectizeInput(inputId="indicator_flags_status",
                                                        label="Set Status To:",
                                                        choices=status.choices,
                                                        selected=status.selected,
                                                        width = "100%")),
                                         
                                         div(style='text-align:center;justify-content:right;flex-grow:1',
                                             radioGroupButtons(
                                               inputId="indicator_flags_selected",
                                               label="Auto-Select",
                                               choices=status_review.choices,
                                               selected = "new",
                                               size="xs"))
                                         )),
                         
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
                                               class="btn-primary btn-default")),
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
  if (!evaluation_id %in% SERVER_DATASETS_REVIEW_FLAGS_SELECTED_FLAG_EVALUATIONS()$evaluation_id) return (NULL)
  
  if (evaluation_id %in% INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()) {
    INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()[-which(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS()==evaluation_id)])
  } else {
    INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(unique(c(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(),evaluation_id)))
  }
  #print(INDICATOR_FLAGS_SELECTED_EVALUATION_IDS())
})

#When clicked, navigates to the Setup page and auto-filtered for the selected check.
observeEvent(input$action_indicator_flags__config_check, {
  
  selected_indicator_flag_id <- as.numeric(input$action_indicator_flags_review)
  

  if (!isTruthy(selected_indicator_flag_id)) return(NULL)

  config_indicator_flag <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]

  if (empty(config_indicator_flag)) return (NULL)

  selected_id <- NULL
  if (!(IMPORT_SELECTED()$import_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id)) {
    selected_id <- DBPOOL %>% dbGetQuery("selected coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id
                                          from p_rsf.rsf_pfcbl_ids ids
                                          where ids.rsf_pfcbl_id = $1::int",
                                         params=list(IMPORT_SELECTED()$import_rsf_pfcbl_id))
    selected_id <- as.numeric(unlist(selected_id))
    
  } else {
    selected_id <- IMPORT_SELECTED()$import_rsf_pfcbl_id
    
  }
  
  showModal_indicator_check_config(for_rsf_pfcbl_id=selected_id,
                                   for_indicator_id=config_indicator_flag$indicator_id,
                                   indicator_check_id=config_indicator_flag$indicator_check_id,
                                   check_formula_id=config_indicator_flag$check_formula_id)
    
})

#When clicked, navigates to the Setup page and auto-filtered for the selected indicator.

observeEvent(input$action_indicator_flags__setup_indicator, {
  
 
  selected_indicator_flag_id <- as.numeric(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  setup_indicator <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(setup_indicator)) return (NULL)
  
  selected_id <- NULL
  if (!(IMPORT_SELECTED()$import_rsf_pfcbl_id %in% SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()$rsf_pfcbl_id)) {
    selected_id <- DBPOOL %>% dbGetQuery("selected coalesce(ids.rsf_facility_id,ids.rsf_program_id) as rsf_pfcbl_id
                                          from p_rsf.rsf_pfcbl_ids ids
                                          where ids.rsf_pfcbl_id = $1::int",
                                         params=list(IMPORT_SELECTED()$import_rsf_pfcbl_id))
    selected_id <- as.numeric(unlist(selected_id))
    
  } else {
    selected_id <- IMPORT_SELECTED()$import_rsf_pfcbl_id
    
  }

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
  
  import <- IMPORT_SELECTED()
  
  if (empty(import)) return(NULL)
  
  selected_indicator_flag_id <- as.character(input$action_indicator_flags_review)
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  
  indicator_flag <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  if (empty(indicator_flag)) return (NULL)

  #for flags that reference the indicator explicitly in the message (many do) but especially for rsf_X_reporting flags
  #and for missing_term or indicator_not_found flags, to support setting these values.
  evaluations <- SERVER_DATASETS_REVIEW_FLAGS_db_EVALUATION_DETAILS(unlist(indicator_flag$evaluation_ids)) #This is a bit inefficient
  message_references <- lapply(RSF_INDICATORS()$indicator_name,
         function(ind,messages) {
           if (any(grepl(ind,messages))) { ind }
         },
         messages=unlist(evaluations$check_message))
  
  message_references <- unique(unlist(message_references))
  
  
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
  if (length(message_references)) {
    for_indicator_names <- c(for_indicator_names,
                             message_references)
  }
  
  dashboard_parameters <- SERVER_DASHBOARD_RUN_OPTIONS_INIT
  #dashboard_parameters$flags_filter <- "any"
  #dashboard_parameters$flags_display <- "active"
  dashboard_parameters$format_unchanged <- "black"
  
  dashboard_parameters$format_pivot <- "DATA"
  
  if (length(flagged_entities <- unique(evaluations$pfcbl_name))) {
    dashboard_parameters$name_filter <- trimws(gsub("^[a-z]+:","",flagged_entities)) #dashbaord name filter extracts the "loan:" "borrower:" etc from pfcbl name
  }
    
  for_facility_sys_names <- SELECTED_PROGRAM_FACILITIES_LIST()[rsf_facility_id %in% import$rsf_facility_id,
                                                             rsf_pfcbl_id]
  
  
  SERVER_DASHBOARD_DO_LOAD(for_facility_sys_names=for_facility_sys_names,
                           for_indicator_names=for_indicator_names,
                           for_asof_dates=import$reporting_asof_date,
                           dashboard_parameters=dashboard_parameters)
})

observeEvent(input$action_review_indicator_flags_audit_indicator, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  import <- IMPORT_SELECTED()
  #browser()
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  flag_selected <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_indicator_id <- as.numeric(flag_selected$indicator_id)
  
  if (!isTruthy(review_indicator_id)) return (NULL)
  
  show_modal_indicator_review(rsf_pfcbl_id=import$import_rsf_pfcbl_id,
                              review_asof_date=import$reporting_asof_date,
                              review_indicator_id=review_indicator_id)
  
},ignoreInit = TRUE)

observeEvent(input$action_review_indicator_flags_audit_check, {
  
  
  selected_indicator_flag_id <- input$action_indicator_flags_review
  import <- IMPORT_SELECTED()
  
  if (!isTruthy(selected_indicator_flag_id)) return(NULL)
  flag_selected <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id==selected_indicator_flag_id]
  review_check_formula_id <- as.numeric(flag_selected$check_formula_id)
  if (!isTruthy(review_check_formula_id)) return (NULL)
  
  show_modal_server_admin_checks_review(rsf_pfcbl_id=import$import_rsf_pfcbl_id,
                                        review_asof_date=import$reporting_asof_date,
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
      !check_status_updated %in% c("active",   #STANDARD
                                   "resolved", #STANDARD  
                                   "applyflag",   #Applies assigned flags on the check
                                   "removeflag"  #Removes any flags on the check
                                   )) {    
    return (showNotification(type="error",h2("Invalid status selected.")))
  } 
  
  if (isTruthy(selected_evaluation_ids) & length(selected_evaluation_ids) > 0) {
    #browser()
    #Update the flags -- and set "reverts" to be "resolved" as they'll be subsequently deleted.
   
      withProgress(message="Saving updates...",value=0.25, {
      
        update_flags <- IMPORT_FLAGS_SELECTED()[evaluation_id %in% selected_evaluation_ids,
                                                .(evaluation_id,
                                                 indicator_id,
                                                 indicator_check_id,
                                                 check_class,
                                                 indicator_flag_id,
                                                 data_sys_flags,
                                                 data_flag_value)]
        
        
        incProgress(amount=0.5,message="Uploading to database...")
        
        if (!empty(update_flags)) {

          update_flags[,csu:=check_status_updated]
          update_flags[,`:=`(check_status_updated=fcase(csu=="active","active",
                                                        csu=="resolved","resolved",
                                                        csu=="applyflag","resolved",
                                                        csu=="removeflag","active"),
                             check_status_comment_updated=check_status_comment_updated,
                             
                             data_sys_flags=fcase(csu=="active",as.numeric(NA),     #no change
                                                  csu=="resolved",as.numeric(NA),   #no change
                                                  csu=="applyflag",as.numeric(data_flag_value), #change!
                                                  csu=="removeflag",as.numeric(0)))]            #change!
          

          update_flags <- update_flags[,.(evaluation_id,
                                          indicator_check_id,
                                          check_class,
                                          check_status_updated,
                                          check_status_comment_updated,
                                          data_sys_flags=data_sys_flags)]
          
          saved <- tryCatch({
            saved <-  DBPOOL %>% db_data_update_flags(user_id=USER_ID(),  
                                                      flags=update_flags)
          },error = function(e) {
            showNotification(duration=NULL,ui=h3("This update could not be saved: ",conditionMessage(e)))
          },warning = function(w) {
            showNotification(duration=NULL,ui=h3("This update could not be saved: ",conditionMessage(w)))
          })
          
                   
        }
        
        incProgress(amount=1.0,message="Completed")
      })
    }

  if (check_status_updated %in% c("applyflag","removeflag")) {
    import <- IMPORT_SELECTED()
    #Set the overwritten data point do manual overwrite
    
    withProgress(message="Recalculating...",value=0.25, {
      progress_status_message <- function(class,...) {
        dots <- list(...)
        dots <- paste0(unlist(dots),collapse=" ")
        incProgress(amount=0,
                    message=paste0("Recalculating affected data: ",dots))
      }
      
      incProgress(amount=0.25,message="Recalculating data...")
      
      DBPOOL %>% rsf_program_calculate(rsf_indicators=RSF_INDICATORS(),
                                       rsf_pfcbl_id.family=import$import_rsf_pfcbl_id,
                                       for_import_id=import$import_id,
                                       calculate_future=FALSE,
                                       reference_asof_date=import$reporting_asof_date,
                                       status_message=progress_status_message)
    })
    
    withProgress(message="Rechecking...",value=0.25, {
      
      progress_status_message <- function(class,...) {
        dots <- list(...)
        dots <- paste0(unlist(dots),collapse=" ")
        incProgress(amount=0,
                    message=paste0("Rechecking affected data: ",dots))
      }
      
      incProgress(amount=0.25,message="Rechecking data...")
      DBPOOL %>% rsf_program_check(rsf_indicators=RSF_INDICATORS(),
                                   rsf_pfcbl_id.family=import$import_rsf_pfcbl_id,
                                   check_future=FALSE,
                                   check_consolidation_threshold=NA,
                                   reference_asof_date=import$reporting_asof_date,
                                   status_message=progress_status_message)
    })
  }
    
  IMPORT_LIST__REFRESH(IMPORT_LIST__REFRESH()+1)
  
  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(c()) #NA is intentional and different from c()
  removeModal()
})

#closes modal "view_data_flags_review"
observeEvent(input$action_indicator_flags_review_cancel, {
  INDICATOR_FLAGS_SELECTED_EVALUATION_IDS(c()) #NA is intentional and different from c()
  removeModal()
})

output$indicator_check_edit_setup__subscription_ui <- renderUI({
  
  #action_number <- as.numeric(input$indicator_check_edit_setup__action_submit) #force update on submit.
  #if (empty(IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED())) return (NULL)
  
  #config_indicator_flag <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[indicator_flag_id== as.numeric(input$action_indicator_flags_review)][!is.na(check_formula_id)]
  setup_check <- SERVER_DATASETS_REVIEW_FLAGS_SETUP_CHECK()
  
  # if (empty(config_indicator_flag)) {    
  #   return (NULL)
  # }
  
  # setup_check <- DBPOOL %>% dbGetQuery("
  #   select
  #     scs.is_subscribed::bool
  #   from p_rsf.view_rsf_setup_check_subscriptions scs
  #   where scs.rsf_pfcbl_id = $1::int
  #     and scs.check_formula_id = $2::int",
  # params=list(IMPORT_SELECTED()$import_rsf_pfcbl_id,
  #             config_indicator_flag$check_formula_id))                 
  
  #setup_check <- SERVER_DATASETS_REVIEW_FLAGS_SETUP_CHECK()
  
  label <- ""
  class <- ""
  if (setup_check$is_subscribed==TRUE) {
    label <- "Stop Checking Flag"
    class <- "btn-primary btn-danger"
  } else if (setup_check$is_subscribed==FALSE) {
    label <- "Start Checking Flag"
    class <- "btn-primary btn-success"
  }
  
  actionButton(inputId="indicator_check_edit_setup__action_submit",
               label=label,
               class=class)
})

output$server_datasets_review_flags_dataset <- DT::renderDataTable({
  
  evaluations <- SERVER_DATASETS_REVIEW_FLAGS_SELECTED_FLAG_EVALUATIONS()[,.(evaluation_id,check_class,check_status,rsf_pfcbl_id)]
  
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
  
  data_flags_view <- INDICATOR_FLAGS_SELECTED_SYS_FLAG_STATUS_VIEW()
  
  indicator_flags_status_filter <- req(input$indicator_flags_status_filter)
  indicator_flags_selected <- input$indicator_flags_selected
  
  
  cohort_flag_details <- SERVER_DATASETS_REVIEW_FLAGS_db_EVALUATION_DETAILS(evaluations$evaluation_id)
  
  
  evaluations <- evaluations[cohort_flag_details,
                             on=.(evaluation_id),
                             nomatch=NULL]
  
  if (indicator_flags_status_filter %in% c("Active","Resolved","New")) {
    
    if (indicator_flags_status_filter=="Active") {
      evaluations <- evaluations[check_status=="active"]
    } else if (indicator_flags_status_filter=="Resolved") {
      evaluations <- evaluations[check_status=="resolved"]
    } else if (indicator_flags_status_filter=="New") {
      evaluations <- evaluations[is.na(check_status_comment)]
    }
  }
  
  if (any(indicator_flags_selected=="all",na.rm=T)) {
    evaluations[,selected:=TRUE]
  
  } else if (any(indicator_flags_selected=="none",na.rm=T)) { 
    evaluations[,selected:=FALSE]
  } else {
    evaluations[,selected := is.na(check_status_comment)]
  }
  
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
  
  if (data_flags_view %in% "revert") {
    
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
  
  setorder(evaluations,
           SYSID)
  
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
                )) #%>%  formatStyle(columns=c(0,1,4,5,6),whiteSpace="nowrap")
})

output$datasets_review_download_flags_action <- downloadHandler(
  filename = function() {
    
    import <- IMPORT_SELECTED()
    whentxt <- toupper(format(today(),format="%d%b"))
    f <- import$source_name
    #f <- "#3 51197 - OTP Leasing - 1Q26 - v2.xlsx"
    #f <- "#3 51197 - OTP Leasing - 1Q26 - CHK18MAY v2.xlsx"
    if (grepl("CHK\\d+[A-Z]{3}[^[:alnum:]]",f)) {
      f <- gsub("^(.*CHK)(\\d+[A-Z]{3})([^[:alnum:]].*)$",paste0("\\1",whentxt,"\\3"),f,ignore.case = T)
      
      
      #f <- gsub("CHK(\\d+)",paste0("CHK",chk+1),f)
    } else {
      
      if (grepl("v\\d",f,ignore.case = T)) {
        f <- gsub("(v\\d+)",paste0(" - CHK",whentxt," \\1"),f)
      } else {
        f <- paste0(file_path_sans_ext(f)," - CHK",whentxt,".",file_ext(f))
      }
    }
    f <- gsub("\\s+"," ",f)
    f <- gsub("\\s+\\.xlsx",".xlsx",f)
    f <- gsub("\\-\\s?\\-","-",f)
  },
  content=function(file) {
    
    import <- IMPORT_SELECTED()
    withProgress(message="Downloading file",value=0.5, {
      
      flags <- {
        flags <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[,
                                                            .(pfcbl_category_rank,
                                                              check_rank,
                                                              indicator_id,
                                                              indicator_name,
                                                              formula_title,
                                                              check_formula_id,
                                                              indicator_check_id,
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
                            check_formula_id,
                            indicator_check_id,
                            check_name,
                            check_type,
                            check_class,
                            check_formula_title)]
        
        evaluations <- SERVER_DATASETS_REVIEW_FLAGS_db_EVALUATION_DETAILS(flags$evaluation_id)
        
        flags <- flags[evaluations,
                       on=.(evaluation_id),
                       nomatch=NULL]

        
        
        if (is.null(flags) || empty(flags)) flags <- data.table(evaluation_id=numeric(0),
                                                                entity_name=character(0),
                                                                rsf_pfcbl_id=numeric(0),
                                                                check_asof_date=as.Date(numeric(0)),
                                                                pfcbl_category_rank=numeric(0),
                                                                check_rank=numeric(0),
                                                                indicator_id=numeric(0),
                                                                indicator_name=character(0),
                                                                formula_title=character(0),
                                                                check_formula_id=numeric(0),
                                                                indicator_check_id=numeric(0),
                                                                check_name=character(0),
                                                                check_type=character(0),
                                                                check_class=character(0),
                                                                check_formula_title=character(0),
                                                                check_status=character(0),
                                                                check_stats_comment=character(0))
        
        #Because the IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED filters the SUMMARY object only and the summary object contains all evaluation_ids as a list
        #regardless of the status.  So need to re-filter on the status selection.
        view_data_flags <- toupper(input$cohort_view_flagged_data)
        if (!isTruthy(view_data_flags)) view_data_flags <- ""
        if (view_data_flags=="ACTIVE") {
          flags <- flags[check_status=="active"]
        } else if (view_data_flags=="RESOLVED") {
          flags <- flags[check_status=="resolved"]
        } else if (view_data_flags=="NEW") {
          flags <- flags[check_status=="active" & (is.na(check_status_comment) || nchar(check_stats_comment)==0)]
        }
        
        setorder(flags,
                 pfcbl_category_rank,
                 check_rank,
                 entity_name,
                 check_type,
                 check_name)
      }
      
      if (empty(flags)) {
        showNotification(type="message",
                         ui=h3("There are no displayed flags to download: an empty sheet will be inserted for Current Flags"))
#        flags <- NULL
#        return (NULL)
      }
      
      wbflags <- NULL
      
      if (import$file_extension=="xlsx") {
        
        outpath <- DBPOOL %>% db_import_download_file(import_id=IMPORT_SELECTED()$import_id)
        
        if (!is.null(outpath)) {
          
          file.copy(from=outpath,
                    to=file,
                    overwrite = TRUE)
          
          if (file.exists(outpath)) file.remove(outpath)
        
          
          if (import$template_name=="IFC-QR-TEMPLATE2025") {
            
            lookup <- db_export_get_template(pool=DBPOOL,
                                             template_name="IFC-QR-TEMPLATE2025")
            
            wbflags <- parse_template_IFC_QR2025(pool=DBPOOL,
                                                 template_file=file,
                                                 template_lookup=lookup,
                                                 rsf_indicators=db_indicators_get_labels(DBPOOL),
                                                 return.insert_flags=flags,
                                                 return.next_date=NULL,
                                                 status_message = function(...) {},
                                                 CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
            
          
          
          } else {
            
            wbflags <- openxlsx2::wb_load(file=file)
            
            if (any(wbflags$sheet_names=="Current Flags")) {
              wbflags$remove_worksheet(sheet="Current Flags")
            }
            wbflags$add_worksheet(sheet="Current Flags")
            sorder <- which(wbflags$sheet_names=="Current Flags")
            wbflags$set_order(c(sorder,wbflags$sheetOrder[-sorder]))
            wbflags$add_data_table(sheet="Current Flags",
                                   table_name="RSF_current_flags",
                                   dims="B1",
                                   x=flags[,
                                           .(FLAGID=evaluation_id,
                                             CHECK_DATE=check_asof_date,
                                             NAME=entity_name,
                                             type=check_type,
                                             class=check_class,
                                             MESSAGE=check_message,
                                             CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
                                                                                     check_formula_title)),
                                             STATUS=check_status,
                                             comment=check_status_comment,
                                             user=check_status_users_name)])
            wbflags$set_active_sheet(sheet="Current Flags")
            wbflags$set_col_widths(sheet="Current Flags",
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
            
          }
          
          openxlsx2::wb_save(wbflags,file=file,overwrite = T)
          #wbflags$save(file=file,overwrite=TRUE)
        }
      
      
      } else if (import$file_extension %in% c("pdf","txt")) {

        setorder(flags,
                 entity_name,
                 check_status,
                 indicator_name,
                 check_name,
                 check_formula_title,
                 check_class,
                 check_message)
        
        wbflags <- paste0(flags$entity_name,": ",
               flags$indicator_name,": ",ifelse(is.na(flags$check_formula_title),flags$check_name,flags$check_formula_title),
               " [",toupper(flags$check_status),":",flags$evaluation_id,"] [",flags$check_type,":",toupper(flags$check_class),"]\n",
               flags$check_message,"\n\n")
        
        writeLines(text=wbflags,
                   con=file)
        
      }
      
      #catch-all 
      # if (is.null(wbflags)) {
      #   
      #   wbflags <- openxlsx2::wb_workbook(creator="RSF Jason")
      #   wbflags$add_worksheet(sheet="Current Flags")
      #   wbflags$add_data_table(sheet="Current Flags",
      #                          table_name="RSF_current_flags",
      #                          x=flags[,
      #                                  .(FLAGID=evaluation_id,
      #                                    CHECK_DATE=check_asof_date,
      #                                    NAME=entity_name,
      #                                    type=check_type,
      #                                    class=check_class,
      #                                    MESSAGE=check_message,
      #                                    CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
      #                                                                            check_formula_title)),
      #                                    STATUS=check_status,
      #                                    comment=check_status_comment,
      #                                    user=check_status_users_name)])
      #   
      #   wbflags$set_active_sheet(sheet="Current Flags")
      #   wbflags$set_col_widths(sheet="Current Flags",
      #                          cols=c(1,2,3,4,5,6,7,8,9,10),
      #                          widths = c(10,
      #                                     13, #check date 
      #                                     35, #entity name
      #                                     17, #check type
      #                                     10, #check class
      #                                     90, #check message
      #                                     90, #check
      #                                     10,
      #                                     10,
      #                                     10))
      #   wbflags$save(file=file,
      #                overwrite=TRUE)
      # }
  
      
        
      
      
      incProgress(amount=1.0,message="Completed")
    })
  })


output$datasets_review_download_zeroversion_action <- downloadHandler(
  filename = function() {
    
    import <- IMPORT_SELECTED()
    whentxt <- toupper(format(today(),format="%d%b"))
    f <- import$source_name
    #f <- "#3 51197 - OTP Leasing - 1Q26 - v2.xlsx"
    #f <- "#3 51197 - OTP Leasing - 1Q26 - CHK18MAY v2.xlsx"
    if (grepl("CHK\\d+[A-Z]{3}[^[:alnum:]]",f)) {
      f <- gsub("^(.*CHK)(\\d+[A-Z]{3})([^[:alnum:]].*)$",paste0("\\1",whentxt,"\\3"),f,ignore.case = T)
      
      
      #f <- gsub("CHK(\\d+)",paste0("CHK",chk+1),f)
    } else {
      
      if (grepl("v\\d",f,ignore.case = T)) {
        f <- gsub("(v\\d+)",paste0(" - CHK",whentxt," \\1"),f)
      } else {
        f <- paste0(file_path_sans_ext(f)," - CHK",whentxt,".",file_ext(f))
      }
    }
    f <- gsub("\\s+"," ",f)
    f <- gsub("\\s+\\.xlsx",".xlsx",f)
    f <- gsub("\\-\\s?\\-","-",f)
  },
  content=function(file) {
    
    import <- IMPORT_SELECTED()
    withProgress(message="Downloading file",value=0.5, {
      
      flags <- {
        flags <- IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED()[,
                                                          .(pfcbl_category_rank,
                                                            check_rank,
                                                            indicator_id,
                                                            indicator_name,
                                                            formula_title,
                                                            check_formula_id,
                                                            indicator_check_id,
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
                            check_formula_id,
                            indicator_check_id,
                            check_name,
                            check_type,
                            check_class,
                            check_formula_title)]
        
        evaluations <- SERVER_DATASETS_REVIEW_FLAGS_db_EVALUATION_DETAILS(flags$evaluation_id)
        
        flags <- flags[evaluations,
                       on=.(evaluation_id),
                       nomatch=NULL]
        
        
        
        if (is.null(flags) || empty(flags)) flags <- data.table(evaluation_id=numeric(0),
                                                                entity_name=character(0),
                                                                rsf_pfcbl_id=numeric(0),
                                                                check_asof_date=as.Date(numeric(0)),
                                                                pfcbl_category_rank=numeric(0),
                                                                check_rank=numeric(0),
                                                                indicator_id=numeric(0),
                                                                indicator_name=character(0),
                                                                formula_title=character(0),
                                                                check_formula_id=numeric(0),
                                                                indicator_check_id=numeric(0),
                                                                check_name=character(0),
                                                                check_type=character(0),
                                                                check_class=character(0),
                                                                check_formula_title=character(0),
                                                                check_status=character(0),
                                                                check_stats_comment=character(0))
        
        #Because the IMPORT_FLAGS_SELECTED_SUMMARY_FILTERED filters the SUMMARY object only and the summary object contains all evaluation_ids as a list
        #regardless of the status.  So need to re-filter on the status selection.
        view_data_flags <- toupper(input$cohort_view_flagged_data)
        if (!isTruthy(view_data_flags)) view_data_flags <- ""
        if (view_data_flags=="ACTIVE") {
          flags <- flags[check_status=="active"]
        } else if (view_data_flags=="RESOLVED") {
          flags <- flags[check_status=="resolved"]
        } else if (view_data_flags=="NEW") {
          flags <- flags[check_status=="active" & (is.na(check_status_comment) || nchar(check_stats_comment)==0)]
        }
        
        setorder(flags,
                 pfcbl_category_rank,
                 check_rank,
                 entity_name,
                 check_type,
                 check_name)
      }
      
      if (empty(flags)) {
        showNotification(type="message",
                         ui=h3("There are no displayed flags to download: an empty sheet will be inserted for Current Flags"))
        #        flags <- NULL
        #        return (NULL)
      }
      
      wbflags <- NULL
      
      if (import$file_extension=="xlsx") {
        
        outpath <- DBPOOL %>% db_import_download_file(import_id=IMPORT_SELECTED()$import_id)
        
        if (!is.null(outpath)) {
          
          file.copy(from=outpath,
                    to=file,
                    overwrite = TRUE)
          
          if (file.exists(outpath)) file.remove(outpath)
          
          
          if (import$template_name=="IFC-QR-TEMPLATE2025") {
            
            lookup <- db_export_get_template(pool=DBPOOL,
                                             template_name="IFC-QR-TEMPLATE2025")
            
            wbflags <- parse_template_IFC_QR2025(pool=DBPOOL,
                                                 template_file=file,
                                                 template_lookup=lookup,
                                                 rsf_indicators=db_indicators_get_labels(DBPOOL),
                                                 return.insert_flags=flags,
                                                 return.next_date=NULL,
                                                 status_message = function(...) {},
                                                 CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
            
            
            
          } else {
            
            wbflags <- openxlsx2::wb_load(file=file)
            
            if (any(wbflags$sheet_names=="Current Flags")) {
              wbflags$remove_worksheet(sheet="Current Flags")
            }
            wbflags$add_worksheet(sheet="Current Flags")
            sorder <- which(wbflags$sheet_names=="Current Flags")
            wbflags$set_order(c(sorder,wbflags$sheetOrder[-sorder]))
            wbflags$add_data_table(sheet="Current Flags",
                                   table_name="RSF_current_flags",
                                   dims="B1",
                                   x=flags[,
                                           .(FLAGID=evaluation_id,
                                             CHECK_DATE=check_asof_date,
                                             NAME=entity_name,
                                             type=check_type,
                                             class=check_class,
                                             MESSAGE=check_message,
                                             CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
                                                                                     check_formula_title)),
                                             STATUS=check_status,
                                             comment=check_status_comment,
                                             user=check_status_users_name)])
            wbflags$set_active_sheet(sheet="Current Flags")
            wbflags$set_col_widths(sheet="Current Flags",
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
            
          }
          
          openxlsx2::wb_save(wbflags,file=file,overwrite = T)
          #wbflags$save(file=file,overwrite=TRUE)
        }
        
        
      } else if (import$file_extension %in% c("pdf","txt")) {
        
        setorder(flags,
                 entity_name,
                 check_status,
                 indicator_name,
                 check_name,
                 check_formula_title,
                 check_class,
                 check_message)
        
        wbflags <- paste0(flags$entity_name,": ",
                          flags$indicator_name,": ",ifelse(is.na(flags$check_formula_title),flags$check_name,flags$check_formula_title),
                          " [",toupper(flags$check_status),":",flags$evaluation_id,"] [",flags$check_type,":",toupper(flags$check_class),"]\n",
                          flags$check_message,"\n\n")
        
        writeLines(text=wbflags,
                   con=file)
        
      }
      
      #catch-all 
      # if (is.null(wbflags)) {
      #   
      #   wbflags <- openxlsx2::wb_workbook(creator="RSF Jason")
      #   wbflags$add_worksheet(sheet="Current Flags")
      #   wbflags$add_data_table(sheet="Current Flags",
      #                          table_name="RSF_current_flags",
      #                          x=flags[,
      #                                  .(FLAGID=evaluation_id,
      #                                    CHECK_DATE=check_asof_date,
      #                                    NAME=entity_name,
      #                                    type=check_type,
      #                                    class=check_class,
      #                                    MESSAGE=check_message,
      #                                    CHECK=paste0(indicator_name,": ",ifelse(is.na(check_formula_title),check_name, #system checks only have a check_name
      #                                                                            check_formula_title)),
      #                                    STATUS=check_status,
      #                                    comment=check_status_comment,
      #                                    user=check_status_users_name)])
      #   
      #   wbflags$set_active_sheet(sheet="Current Flags")
      #   wbflags$set_col_widths(sheet="Current Flags",
      #                          cols=c(1,2,3,4,5,6,7,8,9,10),
      #                          widths = c(10,
      #                                     13, #check date 
      #                                     35, #entity name
      #                                     17, #check type
      #                                     10, #check class
      #                                     90, #check message
      #                                     90, #check
      #                                     10,
      #                                     10,
      #                                     10))
      #   wbflags$save(file=file,
      #                overwrite=TRUE)
      # }
      
      
      
      
      
      incProgress(amount=1.0,message="Completed")
    })
  })

