SERVER_SETUP_INDICATORS_LIST_REFRESH <- reactiveVal(0)
SERVER_SETUP_INDICATORS_TOGGLE_SELECTED <- reactiveVal(c())
SERVER_SETUP_INDICATORS_LIST <- eventReactive(c(RSF_INDICATORS(),
                                                input$ui_setup__indicator_program_facilities,
                                                SERVER_SETUP_INDICATORS_LIST_REFRESH()), {
  
  if (empty(RSF_INDICATORS())) return (NULL)
                                                
  
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  
  if (!isTruthy(selected_rsf_pfcbl_id)) {
    selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
  }
    
  monitored_indicators <- DBPOOL %>% dbGetQuery("
    select 
      fis.*,
      indf.formula_title,
      indf.is_primary_default,
      coalesce(has.reported,false) as has_reported,
      case when fis.is_auto_subscribed = true then NULL::bool
           else fis.is_subscribed end
      as user_subscription,
      ifv.indicator_id is NOT NULL as calculation_has_variety
    from p_rsf.view_rsf_program_facility_indicator_subscriptions fis
    left join p_rsf.indicator_formulas indf on indf.formula_id = fis.formula_id
    left join p_rsf.view_rsf_program_facility_indicator_formula_variety ifv on ifv.indicator_id = fis.indicator_id
    left join lateral (select exists(select * from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                     inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                     where ft.from_rsf_pfcbl_id = fis.rsf_pfcbl_id
                                       and rdc.indicator_id = fis.indicator_id
                                       and rdc.data_value is NOT NULL) as reported) as has on true
    where fis.rsf_pfcbl_id = $1::int
      and fis.is_system = false
      --and fis.is_static_nonreporting = false
    order by fis.data_category_rank,fis.indicator_name",
    params=list(selected_rsf_pfcbl_id))
                                                  
  setDT(monitored_indicators)
  monitored_indicators[RSF_INDICATORS(),
                       `:=`(definition=gsub("'","%39;",i.definition),
                            indicator_default_is_calculated=i.is_calculated),
                       on=.(indicator_id)]
  
  monitored_indicators[is.na(indicator_default_is_calculated),
                       indicator_default_is_calculated:=FALSE]
  
  monitored_indicators[,indicator_html_id:=paste0("indicatorSubscription-",selected_rsf_pfcbl_id,"-",indicator_id)]
  monitored_indicators[,formula_html_id:=paste0("formulaSubscription-",selected_rsf_pfcbl_id,"-",indicator_id)]
  
  monitored_indicators[,indicator_name_html:=mapply(format_html_indicator,
                                                    indicator_name=indicator_name,
                                                    data_category=data_category,
                                                    data_type=data_type,
                                                    is_system=is_system,
                                                    is_calculated=is_calculated,
                                                    is_subscribed=is_subscribed,
                                                    user_subscription=user_subscription,
                                                    id=indicator_html_id)]

  monitored_indicators[,indicator_html:=paste0("<div title='",definition,"'>",
                                               indicator_name_html,
                                               "</div>")]
  
  monitored_indicators[,formula_html:=""] 
  monitored_indicators[,formula_name_html:=""]
  
  monitored_indicators[(is_calculated==TRUE & is.na(formula_id)),
                       formula_title:=paste0("Manually Reported: No Formula",
                                             ifelse(indicator_default_is_calculated==FALSE," [DEFAULT]",""))]
  
  monitored_indicators[!is.na(formula_title),
                       formula_title:=gsub("'","%39;",formula_title)]
  
  monitored_indicators[!is.na(formula_title),
                       formula_name_html:=mapply(format_html_indicator,
                                                 indicator_name=formula_title,
                                                 data_category=fcase(is.na(formula_id),"noformula",
                                                                     is_primary_default==TRUE,"formula",
                                                                     is_primary_default==FALSE,"customformula",
                                                                     default="none"),
                                                 data_type=data_type,
                                                 is_system=is_system,
                                                 is_calculated=TRUE,
                                                 is_subscribed=is_subscribed,
                                                 id=formula_html_id)]

  monitored_indicators[,formula_view:=""]
  monitored_indicators[!is.na(formula_title),
                       formula_view:=paste0("<div onmousedown='event.stopPropagation();' style='display:inline-block;'>",
                                             "<i class='fa fa-solid fa-calculator icon-edit pointer' ",
                                             "title='Click to assign or change calculation formula' ",
                                             "onclick='Shiny.setInputValue(\"server_setup_indicators__formula_subscription\",\"",formula_html_id,"\",{priority:\"event\"})'>",
                                             "</i>",
                                            "</div>")]
  
  monitored_indicators[,
                       toggle_box:=paste0("<input type='checkbox' name='indicator_subscription_toggle_checkbox' ",
                                          "value=",indicator_html_id," ",
                                          "onmousedown='event.stopPropagation();' ",
                                          "onclick='Shiny.setInputValue(\"server_setup_indicators__toggle_select\",\"",formula_html_id,"\",{priority:\"event\"})' />")] 

  return (monitored_indicators)

},
ignoreInit  = FALSE,ignoreNULL=FALSE) %>% debounce(millis=100)

SERVER_SETUP_INDICATORS_MODE_IS_SETUP <- eventReactive(input$ui_setup__indicator_setup_filter, {

  sf <- as.logical(input$ui_setup__indicator_setup_filter)
  if (is.na(sf)) { 
    
    return (FALSE)
    
  } else {
    
    if (sf==TRUE) {
      
      updateSelectizeInput(session=session,
                           inputId="ui_setup__indicator_calculated_filter",
                           selected = c("reported","formulas"))
      
      updateSelectizeInput(session=session,
                           inputId="ui_setup__indicator_monitoring_filter",
                           selected = c("all"))
    }
    
    return (sf)
  }
},ignoreNULL=FALSE,ignoreInit = FALSE)

SERVER_SETUP_INDICATORS_LIST_FILTERED <- eventReactive(c(SERVER_SETUP_INDICATORS_LIST(),
                                                         input$ui_setup__indicator_monitoring_filter,
                                                         input$ui_setup__indicator_category_filter,
                                                         input$ui_setup__indicator_search_filter,
                                                         input$ui_setup__indicator_calculated_filter,
                                                         SERVER_SETUP_INDICATORS_MODE_IS_SETUP()), {
  
  setup <- as.logical(input$ui_setup__indicator_setup_filter)
  mfilter <- input$ui_setup__indicator_monitoring_filter
  cfilter <- tolower(input$ui_setup__indicator_category_filter)
  sfilter <- tolower(input$ui_setup__indicator_search_filter)
  xfilter <- tolower(input$ui_setup__indicator_calculated_filter)
  
  monitored_indicators <- SERVER_SETUP_INDICATORS_LIST()
  SERVER_SETUP_INDICATORS_TOGGLE_SELECTED(c())
  if (empty(monitored_indicators)) return (NULL)
  
  if (isTruthy(sfilter)) {
   
   rsf_labels <- rbindlist(RSF_INDICATORS()$labels)
   sfilter <- unlist(strsplit(x=sfilter,split="[^[:alnum:]]"))
   smatches <- lapply(sfilter,function(x) {
     grepl(pattern=x,
           x=rsf_labels$label,
           ignore.case=T)
   })
   
   smatches <- which(Reduce(`&`,smatches))
   if (length(smatches)==0) {
     return(NULL)
   } else {
     monitored_indicators <- monitored_indicators[indicator_id %in% rsf_labels[smatches,indicator_id]]
   }
  }
  
  if (isTruthy(mfilter) && mfilter=="subscribed") {
   monitored_indicators <- monitored_indicators[is_subscribed==TRUE]
  } else if (isTruthy(mfilter) && mfilter=="unsubscribed") {
   monitored_indicators <- monitored_indicators[is_subscribed==FALSE]
  } else if (isTruthy(mfilter) && mfilter=="unreported") {
   monitored_indicators <- monitored_indicators[has_reported==FALSE & is_subscribed==TRUE]
  } else if (isTruthy(mfilter) && mfilter=="auto") {
    monitored_indicators <- monitored_indicators[default_subscription==TRUE]
  } else if (isTruthy(mfilter) && mfilter=="setup") {
    monitored_indicators <- monitored_indicators[default_subscription==FALSE]
  }
  
  xfilter_selected <- FALSE
  if (!isTruthy(xfilter) || any(xfilter=="") || any(xfilter=="all")) xfilter_selected <- TRUE
  
  if (isTruthy(xfilter) && any(xfilter=="calculated")) {
    xfilter_selected <- xfilter_selected | monitored_indicators$is_calculated==TRUE
  } 
  if (isTruthy(xfilter) && any(xfilter=="reported")) {
    xfilter_selected <- xfilter_selected | monitored_indicators$is_calculated==FALSE
  }
  if (isTruthy(xfilter) && any(xfilter=="formulas")) {
    xfilter_selected <- xfilter_selected | monitored_indicators$calculation_has_variety==TRUE
  }
  monitored_indicators <- monitored_indicators[xfilter_selected]
  
  if (SERVER_SETUP_INDICATORS_MODE_IS_SETUP()) {
    cfilter <- c("facility","client")
  }
  
  if (isTruthy(cfilter) && any(cfilter %in% monitored_indicators$data_category)) {
   monitored_indicators <- monitored_indicators[data_category %in% cfilter]
  }
  
  if (SERVER_SETUP_INDICATORS_MODE_IS_SETUP()) {
    selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
    setup_data <- DBPOOL %>% dbGetQuery("
      select 
        ids.rsf_pfcbl_id,
        fis.indicator_id,
        fis.is_calculated is false and pfcbl_category = 'facililty' as is_editable,
        ids.created_in_reporting_asof_date,
        case when rdc.data_id is NULL then '{MISSING}'
             when rdc.data_value is NULL then '{NOTHING}'
        		 when rdc.data_unit is NOT NULL then rdc.data_value || ' ' || rdc.data_unit
        		 else rdc.data_value end as data_value_text,
        rdc.data_id
        from p_rsf.rsf_pfcbl_ids ids
        inner join p_rsf.view_rsf_program_facility_indicator_subscriptions fis on fis.rsf_pfcbl_id = ids.rsf_pfcbl_id
        inner join p_rsf.indicators ind on ind.indicator_id = fis.indicator_id
        left join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                            and rdc.indicator_id = fis.indicator_id
                                            and rdc.reporting_asof_date = ids.created_in_reporting_asof_date
      where ids.rsf_pfcbl_id = $1::int
        and ids.pfcbl_category = 'facility'
      	and ind.data_category in ('facility','client')",
      params=list(selected_rsf_pfcbl_id))
    
    setDT(setup_data)
    monitored_indicators <- setup_data[monitored_indicators,
                                       on=.(rsf_pfcbl_id,
                                            indicator_id),
                                       nomatch=NULL]

  }
  
  return (monitored_indicators)
},
ignoreInit = FALSE, ignoreNULL=FALSE) %>% debounce(250)
####################
###########OBSERVERS
####################

observeEvent(input$ui_setup__indicator_search_filter, {
  search <- input$ui_setup__indicator_search_filter
  
  if (isTruthy(search) && nchar(search) >=3) {
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_monitoring_filter",
                         selected="")
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_category_filter",
                         selected="")
    
    updateSelectizeInput(session=session,
                         inputId="ui_setup__indicator_calculated_filter",
                         selected="")
    
  }
  
},ignoreNULL=FALSE,ignoreInit=TRUE)

observeEvent(input$action_setup_program_recalculate_reset, {
  program <- SELECTED_PROGRAM()
  if (!isTruthy(program)) return(NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- program()$rsf_pfcbl_id
  
  withProgress(message="Resetting all calculations takes a minute or two...",
               value=0.5,{
    DBPOOL %>% dbExecute("
      delete from p_rsf.rsf_data_calculation_evaluations dce
      where dce.rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
      																					from p_rsf.rsf_pfcbl_id_family fam
      																					where fam.parent_rsf_pfcbl_id = $1::int)
      and not exists(select * from p_rsf.view_rsf_pfcbl_indicator_subscriptions pis 
                     where pis.rsf_pfcbl_id = dce.rsf_pfcbl_id
      							   and pis.indicator_id = dce.indicator_id
      								 and pis.is_calculated = true
                       and pis.is_subscribed = true)",
      params=list(selected_rsf_pfcbl_id))
    
    DBPOOL %>% dbExecute("
                         with calculations as materialized (
                          select distinct
                          tip.to_calculate_rsf_pfcbl_id,
                          tip.to_calculate_indicator_id,
                          tip.to_calculate_formula_id,
                          tip.reporting_asof_date
                          from p_rsf.compute_calculation_triggered_by_parameter tip
                          where tip.from_parameter_pfcbl_id = any (select ft.to_family_rsf_pfcbl_id
                                                                   from p_rsf.view_rsf_pfcbl_id_family_tree ft
                          																				 where ft.from_rsf_pfcbl_id = $1::int)
                        )
                        insert into p_rsf.rsf_data_calculation_evaluations(rsf_pfcbl_id,indicator_id,calculation_asof_date)
                        select 
                          calcs.to_calculate_rsf_pfcbl_id,
                          calcs.to_calculate_indicator_id,
                          calcs.reporting_asof_date
                        from calculations calcs
                        inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = calcs.to_calculate_rsf_pfcbl_id
                        																													 and pis.formula_id = calcs.to_calculate_formula_id
                        where pis.is_subscribed = true
                          and pis.is_calculated = true
                          and calcs.to_calculate_rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id
                                                                    from p_rsf.rsf_pfcbl_id_family fam
                                                                    where fam.parent_rsf_pfcbl_id = $1::int)
                          and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
                        	           where rpr.rsf_pfcbl_id = calcs.to_calculate_rsf_pfcbl_id
                        						   and rpr.reporting_asof_date = calcs.reporting_asof_date)
                        
                        union all
                        
                        -- if ever reported, manually or calculated--recalculate it.
                        select 
                        	rd.rsf_pfcbl_id,
                        	rd.indicator_id,
                        	rd.reporting_asof_date
                        from p_rsf.rsf_data_current rd
                        inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rd.rsf_pfcbl_id
                        																													 and pis.indicator_id = rd.indicator_id
                        where pis.is_subscribed = true
                        	and pis.is_calculated = true
                        	and rd.rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id
        																						from p_rsf.rsf_pfcbl_id_family fam
        																						where fam.parent_rsf_pfcbl_id = $1::int)
                        	and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
                        						 where rpr.rsf_pfcbl_id = rd.rsf_pfcbl_id
                        							 and rpr.reporting_asof_date = rd.reporting_asof_date)
  
                        on conflict do nothing;",
                        params=list(selected_rsf_pfcbl_id)) #so we don't trigger Global
               })  
})

observeEvent(input$server_setup_indicators__recalculate_pending, {
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()
  
  if (empty(program)) return (NULL)
 
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  if (is.na(selected_rsf_pfcbl_id) ||
      selected_rsf_pfcbl_id==program$rsf_pfcbl_id) {
    facilities <- facilities[is.na(rsf_facility_id)==FALSE] #all facilities of the program.
  } else {
    facilities <- facilities[rsf_pfcbl_id == selected_rsf_pfcbl_id]
  }

  facilities <- facilities[,.(rsf_program_id,
                              rsf_pfcbl_id,
                              nickname)]
  
  #Not necessary, but for the progress messaging will show calculating GLOBAL instead of the facility's name
  if (SELECTED_PROGRAM_ID() != 0) {
    facilities <- rbindlist(list(data.table(rsf_program_id=0,
                                            rsf_pfcbl_id=0,
                                            nickname="GLOBAL"),
                                 facilities))
  }
  
  removeModal()
  #Recalculats on a per-client basis to ensure each client is up to date before recalculating the next
  for (i in 1:nrow(facilities)) {
    facility <- facilities[i]
    withProgress(value=((i-1)/nrow(facilities)),
                 message=paste0("Recalculating ",facility$nickname,": "), 
                 {
                   progress_status_message <- function(class,...) {
                     dots <- list(...)
                     dots <- paste0(unlist(dots),collapse=" ")
                     incProgress(amount=0,
                                 message=paste0("Recalculating ",facility$nickname,": ",dots))
                   }

                   DBPOOL %>% rsf_program_calculate(rsf_program_id=SELECTED_PROGRAM_ID(),
                                                    rsf_indicators=RSF_INDICATORS(),
                                                    rsf_pfcbl_id.family=facility$rsf_pfcbl_id,
                                                    calculate_future=FALSE,
                                                    reference_asof_date=today(),
                                                    status_message=progress_status_message)
                 })
  }
})

observeEvent(input$ui_setup__indicators_recalculate, {
  
  
  fp <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
  nickname <- fp[rsf_pfcbl_id==selected_rsf_pfcbl_id,nickname]
  
  m <-modalDialog(id="recalculate_program_indicators",
                  div(style="background-color:white;color:black;font-size:16px;padding:5px;height:150px;width:100%;",
                      #fluidRow(column(12,p("Warning: Recalculating indicators can be very time consuming.")))
                      fluidRow(column(12,style="width:100%",
                                      p(nickname," has ",
                                        textOutput(outputId="server_setup_indicators__recalculate_pendingcount",inline=T),
                                        " calculations pending."),
                                      p("Select 'Reset Calculations' to invalidate and re-run all computations for ",nickname))
                      ),
                      fluidRow(column(6,style="float:left",
                                      actionButton(inputId="server_setup_indicators__recalculate_pending",
                                                   label="Run Pending Calculations",
                                                   class="btn-success")),
                               column(6,style="float:right",
                                      actionButton(inputId="action_setup_program_recalculate_reset",
                                                   label="Reset Calculations",
                                                   class="btn-danger"))
                      ),
                  ),
                  title=HTML(paste0("Recalculate Data for ",nickname)),
                  footer=modalButton("Close"),
                  size="s")
  
  showModal(m)
})

observeEvent(input$server_setup_indicators__formula_subscription_edit, {

  formula_click <- input$server_setup_indicators__formula_subscription
  if (!isTruthy(formula_click)) return (NULL)
  
  click_ids <- strsplit(formula_click,"-")[[1]]
  if (length(click_ids) != 3) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(click_ids[[2]])
  selected_indicator_id <- as.numeric(click_ids[[3]])
  
  updateTabItems(session=session,
                 inputId="sidebarMenu",
                 selected="system")
  
  updateTabsetPanel(session=session,
                    inputId="tabset_admin_system",
                    selected="Indicators")
  
  updateSelectizeInput(session=session,
                       inputId="admin_system_selected_indicator",
                       selected=selected_indicator_id)
  removeModal()
})

observeEvent(input$server_setup_indicators__formula_subscription_apply, {
  
  formula_click <- input$server_setup_indicators__formula_subscription
  selected_facility_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  
  if (!isTruthy(formula_click)) return (NULL)
  if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
  if (!isTruthy(selected_facility_id)) return (NULL)
  
  click_ids <- strsplit(formula_click,"-")[[1]]
  
  if (length(click_ids) != 3) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(click_ids[[2]])
  selected_indicator_id <- as.numeric(click_ids[[3]])
  
  if (selected_rsf_pfcbl_id != selected_facility_id) {
    return(showNotification(type="error",
                            ui=h3("An error has occured. The requested formula does not match the selected facility")))
  }
  formula_id <- as.numeric(input$server_setup_indicators__formula_subscription.select)
  
  success <- DBPOOL %>% dbGetQuery("
    insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                      indicator_id,
                                                      formula_id,
                                                      rsf_program_id,
                                                      rsf_facility_id,
                                                      is_subscribed,
                                                      is_auto_subscribed,
                                                      subscription_comments,
                                                      comments_user_id)
    select
      ids.rsf_pfcbl_id,
      $3::int as indicator_id,
      (select indf.formula_id 
			 from p_rsf.indicator_formulas indf
			 where indf.indicator_id = $3::int
			   and indf.formula_id = $2::int) as formula_id,
      ids.rsf_program_id,
      ids.rsf_facility_id,
      true as is_subscribed,
      false as is_auto_subscribed,
      'Formula set by ' || $4::text || ' on ' || timeofday()::date as subscription_comments,
      $5::text as comments_user_id
    from p_rsf.rsf_pfcbl_ids ids
    where ids.rsf_pfcbl_id = $1::int
    on conflict (rsf_pfcbl_id,indicator_id)
    do update
    set
      formula_id = EXCLUDED.formula_id,
      is_subscribed = EXCLUDED.is_subscribed,
      is_auto_subscribed = EXCLUDED.is_auto_subscribed,
      subscription_comments = concat(rsf_program_facility_indicators.subscription_comments || '; ',EXCLUDED.subscription_comments),
      comments_user_id = EXCLUDED.comments_user_id
      
    returning formula_id;",
    params=list(selected_rsf_pfcbl_id,
                formula_id,
                selected_indicator_id,
                USER_NAME(),
                USER_ID()))
    
  if (empty(success) || !identical(as.numeric(success$formula_id),as.numeric(formula_id))) {
    return(showNotification(type="error",
                            ui=h3("An error has occured. The formula calculation was not successfully applied.  Try logging out and try again.")))
  } 
  # else {
  #   
  #   formula <- DBPOOL %>% dbGetQuery("
  #     select 
  #       indf.formula_title
  #     from p_rsf.indicator_formulas indf
  #     where indf.formula_id = $1::int",
  #     params=list(formula_id))
  # 
  #   shinyjs::html(selector=paste0("#",formula_click,"-text"),
  #                 html=formula$formula_title)
  #   
  # }
  shiny::removeModal()
  SERVER_SETUP_INDICATORS_LIST_REFRESH(SERVER_SETUP_INDICATORS_LIST_REFRESH()+1)
})

observeEvent(input$server_setup_indicators__formula_subscription, {
  
  formula_click <- input$server_setup_indicators__formula_subscription
  selected_facility_id <- as.numeric(input$ui_setup__indicator_program_facilities)

  if (!isTruthy(formula_click)) return (NULL)
  if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
  if (!isTruthy(selected_facility_id)) return (NULL)
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  selected_facility <- NULL
  
  #Because newly created programs without facilities trying to setup indicators will crash
  if (empty(facilities)) {
    selected_facility <- setNames(c(program$rsf_pfcbl_id),
                                  c(paste0("program:",program$program_nickname," (all facilities)")))
    
  } else {
    selected_facility <- setNames(c(program$rsf_pfcbl_id,
                                    facilities$rsf_pfcbl_id),
                                  c(paste0("program:",program$program_nickname," (all facilities)"),
                                    paste0("facility:",facilities$facility_nickname)))
  }
  selected_facility <- selected_facility[which(selected_facility==selected_facility_id)]
  selected_facility <- names(selected_facility)
  
  
  click_ids <- strsplit(formula_click,"-")[[1]]
  
  if (length(click_ids) != 3) return (NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(click_ids[[2]])
  selected_indicator_id <- as.numeric(click_ids[[3]])
  
  formulas <- DBPOOL %>% dbGetQuery("select
                                       indf.formula_id,
                                       indf.formula_title,
                                       indf.is_primary_default
                                    from p_rsf.indicator_formulas indf
                                    where indf.indicator_id = $1::int
                                    order by
                                      indf.is_primary_default desc,
                                      indf.formula_title",
                                    params=list(selected_indicator_id))
  setDT(formulas)
  
  default_calculated <- (RSF_INDICATORS()[indicator_id==selected_indicator_id,is_calculated])
  if (is.na(default_calculated)) default_calculated <- FALSE
  
  if (default_calculated==FALSE ||
      !any(formulas$is_primary_default)) {
  
    formulas <- rbindlist(list(formulas,
                               data.table(formula_id=as.numeric(NA),
                                          formula_title=paste0("Manually Reported: No Formula",
                                                               ifelse(default_calculated==FALSE," [DEFAULT]","")),
                                          is_primary_default=FALSE)))  
  } 
  
  current_formula <- DBPOOL %>% dbGetQuery("
    select 
      fis.formula_id,
      fis.is_subscribed
      from p_rsf.view_rsf_program_facility_indicator_subscriptions fis
    where fis.rsf_pfcbl_id = $1::int
      and fis.indicator_id = $2::int",
    params=list(selected_rsf_pfcbl_id,
                selected_indicator_id))
  
  if (empty(current_formula) || as.logical(current_formula$is_subscribed)==FALSE) {
    return(showNotification(type="error",
                            ui=h3(paste0(selected_facility," is not monitoring this indicator and therefore its calculation definition is not applicable."))))
  }
  
  current_formula <- current_formula$formula_id
  
  formulas[is_primary_default==TRUE,
           formula_title:=paste0(formula_title," [DEFAULT]")]
  
  choices <- setNames(formulas$formula_id,
                      formulas$formula_title)
  
  
  m <-modalDialog(id="program_indicators_formula_subscription",
                  div(style="background-color:white;color:black;font-size:16px;padding:5px;height:250px;width:100%;",
                      fluidRow(column(12,style="width:100%",
                                      selectizeInput(inputId="server_setup_indicators__formula_subscription.select",
                                                     label="Selected Calculation Formula",
                                                     choices=choices,
                                                     selected=current_formula))
                      ),
                      fluidRow(column(12,style="width:100%",
                                      tags$label("Formula Definition:"),
                                      textOutput(outputId="server_setup_indicators__formula_subscription.view_formula"))),
                      
                      fluidRow(column(12,style="width:100%;padding-top:10px;",
                                      tags$label("Formula Notes:"),
                                      textOutput(outputId="server_setup_indicators__formula_subscription.view_formula_notes")))
                  ),
                  title=HTML(paste0("Select Indicator Calculation Formula for ",selected_facility)),
                  footer=div(style="display:flex;flex-flow:row nowrap;",
                            div(style="padding-left:20px;flex-grow:1;text-align:left;",
                                modalButton("Cancel")),
                            div(style="padding-left:20px",
                                actionButton(inputId="server_setup_indicators__formula_subscription_apply",
                                             label="Apply Formula",
                                             class="btn-success")),
                            div(style="padding-left:20px",
                                actionButton(inputId="server_setup_indicators__formula_subscription_edit",
                                             label="Edit Indicator Formulas",
                                             icon=icon("up-right-from-square"),
                                             class="btn-primary"))),
                  size="s")
  
  showModal(m)
})

observeEvent(input$server_setup_indicators__toggle_select, {
  
  selected <- input$server_setup_indicators__toggle_select
  
  if (any(selected %in% SERVER_SETUP_INDICATORS_TOGGLE_SELECTED())) {
    SERVER_SETUP_INDICATORS_TOGGLE_SELECTED()[!(selected %in% SERVER_SETUP_INDICATORS_TOGGLE_SELECTED())]
  } else {
    SERVER_SETUP_INDICATORS_TOGGLE_SELECTED(c(SERVER_SETUP_INDICATORS_TOGGLE_SELECTED(),selected))
  }
  
})

observeEvent(input$server_setup_indicators__toggle_subscriptions, {

  if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
  
  selected <- SERVER_SETUP_INDICATORS_TOGGLE_SELECTED()
  if (length(selected)==0) return (NULL)
  
  selected_ids <- strsplit(selected,"-")
  results <- c()
  withProgress(value=0.5,message = "Updating Program Indicator Monitoring...", {
    
    for (s in 1:length(selected_ids)) {
      toggle <- selected_ids[[s]]
      
      if (length(toggle) != 3) return (NULL)

      selected_rsf_pfcbl_id <- as.numeric(toggle[[2]])
      selected_indicator_id <- as.numeric(toggle[[3]])
      
      subscription_status <- DBPOOL %>% db_program_toggle_indicator_subscription(rsf_program_id = SELECTED_PROGRAM_ID(),
                                                                                 rsf_pfcbl_id = selected_rsf_pfcbl_id,
                                                                                 indicator_id = selected_indicator_id)
      results <- c(results,subscription_status)
    }
  })

  if (anyNA(results)) {
    showNotification(type="error",
                     ui=h3("Monitoring for this indicator is controlled by the system and cannot be modified")) 
  }
  
  SERVER_SETUP_INDICATORS_TOGGLE_SELECTED(c())
  
    # } else {
    #   if (subscription_status==TRUE) {
    #     shinyjs::removeClass(selector=paste0("#",click),
    #                          class="unsubscribed")
    #     
    #     shinyjs::removeClass(selector=paste0("#",formula_click),
    #                          class="unsubscribed")
    #     
    #     shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
    #                          class="fa-regular fa-circle-xmark")
    # 
    #     shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
    #                          class="fa-regular fa-circle-question")
    #     
    #     shinyjs::addClass(selector=paste0("#",paste0(click,"subscription")),
    #                       class="fa-regular fa-circle-check")
    #     
    #     
    # 
    #   } else {
    #     shinyjs::addClass(selector=paste0("#",click),
    #                       class="unsubscribed")
    #     
    #     shinyjs::addClass(selector=paste0("#",formula_click),
    #                       class="unsubscribed")
    # 
    #     shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
    #                          class="fa-regular fa-circle-check")
    #     
    #     shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
    #                          class="fa-regular fa-circle-question")
    #     
    #     shinyjs::addClass(selector=paste0("#",paste0(click,"subscription")),
    #                       class="fa-regular fa-circle-xmark")
    #     
    #   }

  #SERVER_DASHBOARD.INDICATORS_REFRESH(SERVER_DASHBOARD.INDICATORS_REFRESH()+1)
  SERVER_SETUP_INDICATORS_LIST_REFRESH(SERVER_SETUP_INDICATORS_LIST_REFRESH()+1)
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1) #Because which indicators are subscribed/subscribable affects check auto-subscribe
  
}, ignoreInit = TRUE)

##################
###########OUTPUTS
##################

output$server_setup_indicators__formula_subscription.view_formula <- renderText({
  formula_select <- as.numeric(input$server_setup_indicators__formula_subscription.select)
  if (!isTruthy(formula_select)) return (NULL)
  
  formula <- DBPOOL %>% dbGetQuery("
    select
      indf.formula
    from p_rsf.indicator_formulas indf
    where indf.formula_id = $1::int",
    params=list(formula_select))
  
  formula <- formula$formula
  if (!isTruthy(formula)) formula <- "This formula is undefined"
  return (formula)
})

output$server_setup_indicators__formula_subscription.view_formula_notes <- renderText({
  formula_select <- as.numeric(input$server_setup_indicators__formula_subscription.select)
  if (!isTruthy(formula_select)) return (NULL)
  
  formula <- DBPOOL %>% dbGetQuery("
    select
      indf.formula_notes
    from p_rsf.indicator_formulas indf
    where indf.formula_id = $1::int",
    params=list(formula_select))
  
  notes <- formula$formula_notes
  if (!isTruthy(notes)) notes <- "No notes available for this formula"
  return (notes)
})

output$server_setup_indicators__recalculate_pendingcount <- renderText({
  rsf_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(rsf_program_id)) return("0")
  
  input$ui_setup__indicators_recalculate
  input$action_setup_program_recalculate_pending
  input$action_setup_program_recalculate_reset
  input$setup_program_recalculate_indicators
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id

  pc <- DBPOOL %>% dbGetQuery("select count(*) as pending_count
                              from p_rsf.rsf_data_calculation_evaluations dce 
                              where dce.rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
                                                           from p_rsf.rsf_pfcbl_id_family fam
                                                           where fam.parent_rsf_pfcbl_id = $1::int)",
                              params=list(selected_rsf_pfcbl_id))
  
  pc <- format(as.numeric(pc$pending_count),big.mark=",")
  if (!isTruthy(pc)) pc <- "0"
  return (pc)
})

#Takes a while and not very informative
# output$server_setup_indicators.recalculate_verifiedcount <- renderText({
#   rsf_program_id <- SELECTED_PROGRAM_ID()
#   if (!isTruthy(rsf_program_id)) return("0")
#   
#   input$ui_setup__indicators_recalculate #modal button
#   input$action_setup_program_recalculate_pending
#   input$action_setup_program_recalculate_reset
#   input$setup_program_recalculate_indicators
#   
#   selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
#   if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
#   
#   tc <- DBPOOL %>% dbGetQuery("select count(*) as verified_count
#                               from p_rsf.rsf_data_current rdc
#                               inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rdc.rsf_pfcbl_id
#                                                                                          and pis.indicator_id = rdc.indicator_id
#                               where pis.is_calculated = true
#                                 and pis.is_calculated = true
#                                 and rdc.rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
#                                                            from p_rsf.rsf_pfcbl_id_family fam
#                                                            where fam.parent_rsf_pfcbl_id = $1::int)",
#                               params=list(selected_rsf_pfcbl_id))
#   
#   tc <- format(as.numeric(tc$verified_count),big.mark=",")
#   if (!isTruthy(tc)) tc <- "0"
#   return (tc)
# })
observeEvent(input$ui_setup__indicators_monitored_table_cell_edit, {
  
  clicked_cell <- input$ui_setup__indicators_monitored_table_cell_edit
  
  
  if (!isTruthy(clicked_cell) || length(clicked_cell) == 0) return (NULL)
  monitored_indicator <- SERVER_SETUP_INDICATORS_LIST_FILTERED()[clicked_cell$row]
  
  monitored_indicator <- as.list(monitored_indicator)
  
  is_setup_mode <- SERVER_SETUP_INDICATORS_MODE_IS_SETUP()
  
  order_col <- 5
  notes_col <- 4
  
  if (is_setup_mode) {
    order_col <- 6
    notes_col <- 5
    
    #Edited the cell's setup value!
    if (clicked_cell$col==4) {
      
      withProgress(session=session,
                   message="Saving Setup Data", {
                     
        ind <- RSF_INDICATORS()[indicator_id==monitored_indicator$indicator_id]
        
        setup_data <- as.data.table(monitored_indicator)[,.(reporting_template_row_group="SETUP1",
                                                            rsf_pfcbl_id,
                                                            indicator_id,
                                                            indicator_name,
                                                            indicator_sys_category=ind$indicator_sys_category,
                                                            data_category=ind$data_category,
                                                            reporting_asof_date=created_in_reporting_asof_date,
                                                            reporting_submitted_data_unit=as.character(NA),
                                                            reporting_submitted_data_value=as.character(clicked_cell$value),
                                                            reporting_submitted_data_formula=as.character(NA)
                                                            )]
        
        setup_data <- parse_data_formats(template_data=setup_data,
                                         rsf_indicators=RSF_INDICATORS())
        
        data_flags <- rbindlist(setup_data$data_flags_new)
        
        if (!empty(data_flags)) {
          data_flags[RSF_CHECKS(),
                     check_class:=i.check_class,
                     on=.(check_name)]
          data_flags <- data_flags[check_class != "info"]
        }
        
        if (!empty(data_flags)) {
          
          flags <- paste0(paste0(data_flags$check_name,": ",data_flags$check_message),
                          collapse="
                           ")
          
          showNotification(duration=NULL,
                            ui=h3(div(paste0("Failed to update value to ",as.character(clicked_cell$value))),
                                  div(paste0("Errors Found: ")),
                                  div(flags)))
        } else {
          
          
          valid_cols <- c("reporting_asof_date",
                          "rsf_pfcbl_id",
                          "indicator_id",
                          "data_value",
                          "data_unit",
                          "data_submitted",
                          "data_source_row_id")
          
          setup_data <- setup_data[,
                                   .(reporting_asof_date,
                                     rsf_pfcbl_id,
                                     indicator_id,
                                     data_value,
                                     data_unit,
                                     data_submitted,
                                     data_source_row_id=as.character(NA))]
          
          reporting_cohort <- DBPOOL %>% dbGetQuery("
            select 
              coalesce(rc.parent_reporting_cohort_id,rc.reporting_cohort_id) as reporting_cohort_id, 
              rc.reporting_user_id,
              rc.reporting_asof_date,
              rc.rsf_program_id
            from p_Rsf.rsf_pfcbl_ids ids
            inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id= ids.created_by_reporting_cohort_id
            where ids.rsf_pfcbl_Id = $1::int",
            params=list(setup_data$rsf_pfcbl_id))
          
          setDT(reporting_cohort)
          DBPOOL %>% db_add_update_data_user(reporting_cohort,
                                             cohort_upload_data=setup_data,
                                             template_has_static_row_ids=FALSE)
        }
      
      })
    }
  }
  
  #regardless of it being setup mode, user edited ordering or made notes.
  if (clicked_cell$col %in% c(order_col,notes_col)) {
    if (clicked_cell$col==order_col) { 
      monitored_indicator[["sort_preference"]] <- as.numeric(clicked_cell$value)
      
      
    } else if (clicked_cell$col==notes_col) {
      monitored_indicator[["subscription_comments"]] <- as.character(clicked_cell$value)
    }
    
    DBPOOL %>% dbExecute("
    insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                      indicator_id,
                                                      formula_id,
                                                      rsf_program_id,
                                                      rsf_facility_id,
                                                      is_subscribed,
                                                      is_auto_subscribed,
                                                      sort_preference,
                                                      subscription_comments)
            select 
              ids.rsf_pfcbl_id,
              ind.indicator_id,
              indf.formula_id as formula_id,
              ids.rsf_program_id,
              ids.rsf_facility_id,
              $4::bool as is_subscribed,
              false as is_auto_subscribed,
              $5::int2 as sort_preference,
              $6::text as subscription_comments
            from p_rsf.rsf_pfcbl_ids ids
            cross join p_rsf.indicators ind
            left join p_rsf.indicator_formulas indf on indf.indicator_id = ind.indicator_id
                                                   and indf.formula_id = $3::int
            where ids.rsf_pfcbl_id = $1::int
              and ind.indicator_id = $2::int
            on conflict (rsf_pfcbl_id,indicator_id)
            do update
            set formula_id = EXCLUDED.formula_id,
                is_subscribed = EXCLUDED.is_subscribed,
                is_auto_subscribed = EXCLUDED.is_auto_subscribed,
                sort_preference = EXCLUDED.sort_preference,
                subscription_comments = EXCLUDED.subscription_comments",
    params=list(monitored_indicator$rsf_pfcbl_id,
                monitored_indicator$indicator_id,
                monitored_indicator$formula_id,
                monitored_indicator$is_subscribed,
                monitored_indicator$sort_preference,
                monitored_indicator$subscription_comments))
    
  }
})

output$ui_setup__indicators_monitored_table <- DT::renderDataTable({
  
  
  selected_rsf_program_id <- SELECTED_PROGRAM_ID() ##Reactive on selected program_id
  monitored_indicators <- SERVER_SETUP_INDICATORS_LIST_FILTERED()
  is_setup_mode <- isolate(SERVER_SETUP_INDICATORS_MODE_IS_SETUP())
  
  if (!isTruthy(selected_rsf_program_id)) {
    return (DT::datatable(data.frame(Error="A Program must be selected first."),
                          rownames=FALSE,
                          fillContainer = TRUE,
                          options=list(
                            dom="t",
                            autoWidth=TRUE,
                            paging=FALSE
                          )))
  }
  
  if (empty(monitored_indicators)) {
    return (DT::datatable(data.frame(Error="No indicators found for this filter or selection"),
                          rownames=FALSE,
                          fillContainer = TRUE,
                          options=list(
                            dom="t",
                            autoWidth=TRUE,
                            paging=FALSE
                          )))
  }
  
  toggleButton <- paste0("<div>
                          <i class='fa fa-solid fa-shuffle icon-action pointer' 
                             title='Toggle Subscription...' 
                             onclick='event.stopPropagation();Shiny.setInputValue(\"server_setup_indicators__toggle_subscriptions\",-1,{priority:\"event\"})'>
                        </i></div>")
  
  padding <- max(c(1,nchar(as.character(monitored_indicators$sort_preference))),na.rm=T)
  
  if (is_setup_mode) {
    #browser()
    reporting_asof_date <- ymd(unique(as.character(monitored_indicators$created_in_reporting_asof_date)))
    setorder(monitored_indicators,
             data_category_rank,
             sort_preference,
             na.last=TRUE)
    
    monitored_indicators <- monitored_indicators[,
                                                 .(toggle_box,
                                                   indicator_html,
                                                   formula_name_html,
                                                   formula_view,
                                                   data_value_text,
                                                   subscription_comments=ifelse(is_inherited==TRUE,
                                                                                "[program setting]",
                                                                                subscription_comments),
                                                   sort_preference=ifelse(is.na(sort_preference),
                                                                          "",
                                                                          str_pad(as.character(sort_preference),
                                                                                  width=padding,
                                                                                  pad="0"))
                                                 )]
    dd <- DT::datatable(monitored_indicators,
                        rownames = FALSE,
                        fillContainer=TRUE,
                        #extensions = "Select",
                        selection = "none",
                        colnames=c(toggleButton,
                                   "Indicator Name",
                                   "Calculation",
                                   "Formula",
                                   paste0("Value@",format_asof_date_label(reporting_asof_date)),
                                   "Notes",
                                   "Order"),
                        editable=list(target = 'cell',
                                      disable = list(columns = c(0,1,2,3),
                                                     rows=which(monitored_indicators$is_editable==FALSE))), #c(0,1)))
                        escape = FALSE, #Shouldn't be any HTML escapable text
                        options=list(
                          dom="t",
                          bSort=T,
                          scrollY="70vh",
                          ordering=F,
                          paging=F,
                          columnDefs = list(
                            list(targets = c(0,1), className = 'dt-left')))) %>%
   
      formatStyle(columns="data_value_text",
                  target="cell",
                  `width` = "150px",
                  `white-space`="nowrap") %>%
      
      formatStyle(columns="sort_preference",
                  target="cell",
                  `width` = "50px",
                  `white-space`="nowrap") %>%
      
      formatStyle(columns="subscription_comments",
                  target="cell",
                  `width` = "200px",
                  `white-space`="normal",
                  `text-overflow`="ellipsis",
                  `overflow`="hidden")
    
  } else {
    
    monitored_indicators <- monitored_indicators[,
                                                 .(toggle_box,
                                                   indicator_html,
                                                   formula_name_html,
                                                   formula_view,
                                                   subscription_comments=ifelse(is_inherited==TRUE,
                                                                                "[program setting]",
                                                                                subscription_comments),
                                                   sort_preference=ifelse(is.na(sort_preference),
                                                                          "",
                                                                          str_pad(as.character(sort_preference),
                                                                                  width=padding,
                                                                                  pad="0"))
                                                   )]
    
    dd <- DT::datatable(monitored_indicators,
                  rownames = FALSE,
                  fillContainer=TRUE,
                  #extensions = "Select",
                  selection = "none",
                  colnames=c(toggleButton,"Indicator Name","Calculation","Formula","Notes","Order"),
                  editable=list(target = 'cell', disable = list(columns = c(0,1,2,3))), #c(0,1)))
                  escape = FALSE, #Shouldn't be any HTML escapable text
                  options=list(
                    dom="t",
                    bSort=T,
                    scrollY="70vh",
                    ordering=T,
                    paging=F,
                    columnDefs = list(
                      list(targets = c(0,2,3,4), orderable = FALSE),
                      list(targets = c(0,1), className = 'dt-left')))) %>%
      
      formatStyle(columns="sort_preference",
                  target="cell",
                  `width` = "50px",
                  `white-space`="nowrap") %>%
      
      formatStyle(columns="subscription_comments",
                  target="cell",
                  `width` = "200px",
                  `white-space`="normal",
                  `text-overflow`="ellipsis",
                  `overflow`="hidden")
  }
  
  return (dd)
  
})
