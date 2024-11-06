####################
###########REACTIVES
####################
SERVER_SETUP_CHECKS_TOGGLE_SELECTED <- reactiveVal(c())
SERVER_SETUP_CHECKS_LIST_REFRESH <- reactiveVal(0)

SERVER_SETUP_CHECKS__FILTERED_CHECKS <- eventReactive(c(RSF_CHECKS(),
                                                        SERVER_SETUP_CHECKS_LIST_REFRESH(),
                                                        input$ui_setup__checks_program_facilities,
                                                        input$ui_setup__checks_monitoring_filter,
                                                        input$ui_setup__checks_category_filter,
                                                        input$ui_setup__checks_search_filter,
                                                        input$ui_setup__checks_type_filter), {

   if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
   if (empty(RSF_CHECKS())) return (NULL)
   
   selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
   if (!isTruthy(selected_rsf_pfcbl_id)) {
     selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
   }
   
   SERVER_SETUP_CHECKS_TOGGLE_SELECTED(c())
   
   monitored_checks <- DBPOOL %>% dbGetQuery("
    select 
      fcs.rsf_pfcbl_id,
      fcs.indicator_check_id,
      fcs.check_formula_id,
      fcs.rank_id,
      fcs.check_formula_title,
      fcs.check_pfcbl_category,
      fcs.check_name,
      fcs.check_type,
      fcs.check_class,
      ict.check_type_name,
      coalesce(has.reported,false) as has_flag,
      fcs.is_subscribed,
      fcs.is_auto_subscribed,
      case when fcs.is_auto_subscribed = true then NULL::bool
           else fcs.is_subscribed end as user_subscription
    from p_rsf.view_rsf_program_facility_check_subscriptions fcs
    inner join p_rsf.indicator_check_types ict on ict.check_type = fcs.check_type
    left join (select distinct
    						ft.from_rsf_pfcbl_id as rsf_pfcbl_id,
    						rdc.check_formula_id,
    						true as reported
    						from p_rsf.view_rsf_pfcbl_id_family_tree ft
    						inner join p_rsf.rsf_data_checks rdc on rdc.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
    						where ft.from_rsf_pfcbl_id = $1::int
    						  and rdc.check_formula_id is not null) as has on has.rsf_pfcbl_id = fcs.rsf_pfcbl_id
    																												  and has.check_formula_id = fcs.check_formula_id
    where fcs.rsf_pfcbl_id = $1::int
    order by 
      fcs.data_category_rank desc,
      case when fcs.check_class = 'critical' then 1
           when fcs.check_class = 'error' then 2
           when fcs.check_class = 'warning' then 3
           else 4 end,
      fcs.check_name,
      fcs.check_formula_title",
    params=list(selected_rsf_pfcbl_id))
                                                           
   setDT(monitored_checks)
  
   monitored_checks[RSF_CHECK_TYPES(),
                    `:=`(type_class=i.type_class,
                         type_class_label=i.type_class_label),
                    on=.(check_type)]
   
   monitored_checks[RSF_CHECKS(),
                    definition:=gsub("'","%39;",i.definition),
                    on=.(indicator_check_id)]
   
   monitored_checks[,formula_html_id:=paste0("checkFormula-",rsf_pfcbl_id,"-",check_formula_id)]
   
   monitored_checks[,pfcbl_name_html:=mapply(format_html_indicator,
                                             indicator_name=check_pfcbl_category,
                                             data_category=check_pfcbl_category,
                                             data_type="",
                                             is_system=FALSE,
                                             is_calculated=FALSE)]
   
   monitored_checks[,check_name_html:=mapply(format_html_check,
                                             check_name=check_name,
                                             check_class=check_class,
                                             check_type=check_type,
                                             is_subscribed=is_subscribed,
                                             user_subscription=user_subscription,
                                             id=formula_html_id)]
   
   # monitored_checks[,check_name_html:=paste0("<div onmousedown='event.stopPropagation();'
   #                                             style='display:inline-block;'
   #                                             class='pointer'
   #                                             title='",definition,"'
   #                                             onclick='Shiny.setInputValue(\"server_setup_checks__toggle_subscription\",\"",formula_html_id,"\",{priority:\"event\"})'>",
   #                                             check_name_html,
   #                                             "</div>")]

   monitored_checks[,check_name_html:=paste0("<div title='",definition,"'>",
                                             check_name_html,
                                             "</div>")]
   
   monitored_checks[,
                    toggle_box:=paste0("<input type='checkbox' name='indicator_subscription_toggle_checkbox' ",
                                       "value=",formula_html_id," ",
                                       "onmousedown='event.stopPropagation();' ",
                                       "onclick='Shiny.setInputValue(\"server_setup_checks__toggle_select\",\"",formula_html_id,"\",{priority:\"event\"})' />")] 

   monitored_checks[!is.na(check_formula_title),
                    check_formula_title:=gsub("'","%39;",check_formula_title)]
   
   monitored_checks[!is.na(check_formula_title),
                    check_formula_title_html:=mapply(format_html_indicator,
                                                     indicator_name=check_formula_title,
                                                     data_category="formula",
                                                     data_type="",
                                                     is_system=FALSE,
                                                     is_calculated=FALSE,
                                                     is_subscribed=is_subscribed,
                                                     id=formula_html_id)]
   
   monitored_checks[,
                    view_check_html:=paste0("<div style='display:inline-block;'> ",
                                            "<div onmousedown='event.stopPropagation();' style='display:inline-block;'> ",
                                            "<i class='fa fa-solid fa-up-right-from-square icon-view pointer' ",
                                            "title='View' onclick='Shiny.setInputValue(\"server_setup_checks__view\",",check_formula_id,",{priority:\"event\"})'></i>",
                                            "</div></div>")]
  
  mfilter <- input$ui_setup__checks_monitoring_filter
  cfilter <- tolower(input$ui_setup__checks_category_filter)
  sfilter <- tolower(input$ui_setup__checks_search_filter)
  tfilter <- tolower(input$ui_setup__checks_type_filter)
  
  if (isTruthy(sfilter)) {
    
    sfilter <- unlist(strsplit(x=sfilter,split="[^[:alnum:]]"))
    
    smatches <- lapply(sfilter,function(x,dm) {
      sapply(as.data.frame(
        t(matrix(
          grepl(pattern=x,x=dm),
          ncol=5))),
        any)
      
    },dm=unlist(monitored_checks[,.(check_name,
                                    check_formula_title,
                                    check_class,
                                    check_type,
                                    check_type_name)],
                recursive = F))
    
    smatches <- which(Reduce(`&`,smatches))
    
    if (length(smatches)==0) {
      return(NULL)
    } else {
      monitored_checks <- monitored_checks[smatches]
    }
  }
  
  if (isTruthy(mfilter)) {
    if (mfilter=="subscribed") {
      monitored_checks <- monitored_checks[is_subscribed==TRUE]
    } else if (mfilter=="unsubscribed") {
      monitored_checks <- monitored_checks[is_subscribed==FALSE]
    } else if (mfilter=="unreported") {
      monitored_checks <- monitored_checks[has_flag==FALSE & is_subscribed==TRUE]
    }
  }
  
  if (isTruthy(cfilter) && any(cfilter==monitored_checks$check_pfcbl_category)) {
    monitored_checks <- monitored_checks[check_pfcbl_category==cfilter]
  }
  
  if (isTruthy(tfilter) && any(tfilter==monitored_checks$type_class)) {
    
    monitored_checks <- monitored_checks[type_class %in% tfilter]
  }
  
  return (monitored_checks)
  

},ignoreInit=FALSE,ignoreNULL=FALSE)

####################
###########OBSERVERS
####################

observeEvent(input$ui_setup__checks_recheck, {
  
  rsf_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(rsf_program_id)) return (NULL)
  
  fp <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
  nickname <- fp[rsf_pfcbl_id==selected_rsf_pfcbl_id,nickname]
  
  
  m <-modalDialog(id="recheck_program_indicators",
                  div(style="background-color:white;color:black;font-size:16px;padding:5px;height:150px;width:100%;",
                      #fluidRow(column(12,p("Warning: Recalculating indicators can be very time consuming.")))
                      fluidRow(column(12,style="width:100%",
                                      p(nickname," has ",
                                        textOutput(outputId="server_setup_checks__recheck_pending_UI",inline=T),
                                        " checks pending."),
                                      p("Select 'Reset Checks' to invalidate and re-run all checks for ",nickname))
                      ),
                      fluidRow(column(6,style="float:left",
                                      actionButton(inputId="server_setup_checks__recheck_run",
                                                   label="Check Pending Checks",
                                                   class="btn-success")),
                               column(6,style="float:right",
                                      actionButton(inputId="server_setup_checks__recheck_reset",
                                                   label="Reset Checks",
                                                   class="btn-danger"))
                      ),
                  ),
                  title=HTML(paste0("Recheck Data for ",nickname)),
                  footer=modalButton("Close"),
                  size="s")
  
  showModal(m)
  
})

observeEvent(input$server_setup_checks__recheck_reset, {
  program <- SELECTED_PROGRAM()
  if (!isTruthy(program)) return(NULL)
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- program()$rsf_pfcbl_id
  
  withProgress(message="Resetting all checks takes a minute or two...",
               value=0.5,{
    DBPOOL %>% dbExecute("
      delete from p_rsf.rsf_data_check_evaluations dce
      where dce.rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
      																					from p_rsf.rsf_pfcbl_id_family fam
      																					where fam.parent_rsf_pfcbl_id = $1::int)
      and not exists(select * from p_rsf.view_rsf_pfcbl_check_subscriptions pcs 
                     where pcs.rsf_pfcbl_id = dce.rsf_pfcbl_id
      							   and pcs.check_formula_id = dce.check_formula_id
      								 and pcs.is_subscribed = true)",
      params=list(selected_rsf_pfcbl_id))
    
    DBPOOL %>% dbExecute("with checks as materialized (
                    			select
                    				tip.to_check_rsf_pfcbl_id,
                    				tip.to_check_formula_id,
                    				tip.reporting_asof_date			
                    			from p_rsf.compute_check_triggered_by_parameter tip
                    			where tip.from_parameter_pfcbl_id = any (select ft.to_family_rsf_pfcbl_id
                                                                   from p_rsf.view_rsf_pfcbl_id_family_tree ft
                          																				 where ft.from_rsf_pfcbl_id = $1::int)
                            and tip.is_calculation_trigger_parameter = true
                    		)
                    		insert into p_rsf.rsf_data_check_evaluations(rsf_pfcbl_id,check_formula_id,check_asof_date)
                    		select 
                    			chk.to_check_rsf_pfcbl_id,
                    			chk.to_check_formula_id,
                    			chk.reporting_asof_date
                    		from checks chk
  											inner join p_rsf.view_rsf_pfcbl_check_subscriptions pcs on pcs.rsf_pfcbl_id = chk.to_check_rsf_pfcbl_id
  																																						 and pcs.check_formula_id = chk.to_check_formula_id
  																																						 
                        where pcs.is_subscribed = true
                          and chk.to_check_rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
                                                                    from p_rsf.rsf_pfcbl_id_family fam
                                                                    where fam.parent_rsf_pfcbl_id = $1::int)
                          and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
                        	           where rpr.rsf_pfcbl_id = chk.to_check_rsf_pfcbl_id
                        						   and rpr.reporting_asof_date = chk.reporting_asof_date)														
  																		 
                    		on conflict do nothing;",
                    		params=list(selected_rsf_pfcbl_id))
  })
})

observeEvent(input$server_setup_checks__recheck_run, {
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  
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
  
  for (i in 1:nrow(facilities)) {
    facility <- facilities[i]
    withProgress(value=((i-1)/nrow(facilities)),
                 message=paste0("Rechecking ",facility$nickname,": "), 
                 {
                   progress_status_message <- function(class,...) {
                     dots <- list(...)
                     dots <- paste0(unlist(dots),collapse=" ")
                     incProgress(amount=0,
                                 message=paste0("Rechecking ",facility$nickname,": ",dots))
                   }
                   
                   DBPOOL %>% rsf_program_check(rsf_program_id=SELECTED_PROGRAM_ID(),
                                                rsf_indicators=RSF_INDICATORS(),
                                                rsf_pfcbl_id.family=facility$rsf_pfcbl_id,
                                                check_future=TRUE,
                                                check_consolidation_threshold=NA,
                                                reference_asof_date=NULL,
                                                status_message=progress_status_message)
                 })
  }
  
})

observeEvent(input$server_setup_checks__toggle_select, {
  
  selected <- input$server_setup_checks__toggle_select
  
  if (any(selected %in% SERVER_SETUP_CHECKS_TOGGLE_SELECTED())) {
    SERVER_SETUP_CHECKS_TOGGLE_SELECTED()[!(selected %in% SERVER_SETUP_CHECKS_TOGGLE_SELECTED())]
  } else {
    SERVER_SETUP_CHECKS_TOGGLE_SELECTED(c(SERVER_SETUP_CHECKS_TOGGLE_SELECTED(),selected))
  }
})

observeEvent(input$server_setup_checks__toggle_subscriptions, {
  
  if (!isTruthy(SELECTED_PROGRAM_ID())) return (NULL)
  

  selected <- SERVER_SETUP_CHECKS_TOGGLE_SELECTED()
  if (length(selected)==0) return (NULL)
  
  selected_ids <- strsplit(selected,"-")
  results <- c()
  withProgress(value=0.5,message = "Updating Program Checks Monitoring...", {
    
    for (s in 1:length(selected_ids)) {
      toggle <- selected_ids[[s]]
      
      if (length(toggle) != 3) return (NULL)
      
      selected_rsf_pfcbl_id <- as.numeric(toggle[[2]])
      selected_formula_id <- as.numeric(toggle[[3]])
      
      subscription_status <- DBPOOL %>% db_program_toggle_check_subscription(rsf_pfcbl_id = selected_rsf_pfcbl_id,
                                                                             check_formula_id = selected_formula_id)
      results <- c(results,subscription_status)
    }
  })
  
  if (anyNA(results)) {
    showNotification(type="error",
                     ui=h3("Monitoring for this indicator is controlled by the system and cannot be modified")) 
  }
  
  SERVER_SETUP_CHECKS_TOGGLE_SELECTED(c())  
  SERVER_SETUP_CHECKS_LIST_REFRESH(SERVER_SETUP_CHECKS_LIST_REFRESH()+1)
  # if (length(click_ids) != 3) return (NULL)
  # 
  # selected_rsf_pfcbl_id <- as.numeric(click_ids[[2]])
  # selected_formula_id <- as.numeric(click_ids[[3]])
  # 
  # if (selected_rsf_pfcbl_id != as.numeric(input$ui_setup__checks_program_facilities)) return (NULL)

  # withProgress(value=0.5,message = "Updating Program Indicator Monitoring...", {
  #   
  #   subscription_status <- DBPOOL %>% db_program_toggle_check_subscription(rsf_pfcbl_id=selected_rsf_pfcbl_id,
  #                                                                          check_formula_id=selected_formula_id)
  #   if (is.na(subscription_status)) {
  #     showNotification(type="error",
  #                      ui=h3("Failed to change subscription status.  Ensure program is monitoring the indicators that this check flags"))
  #   } else {
  #     if (subscription_status==TRUE) {
  #       shinyjs::removeClass(selector=paste0("#",click),
  #                            class="unsubscribed")
  # 
  #       shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-question")
  #       
  #       shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-xmark")
  #       
  #       shinyjs::addClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-check")
  #       
  #       
  #     } else {
  #       shinyjs::addClass(selector=paste0("#",click),
  #                         class="unsubscribed")
  #       
  #       shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-question")
  #       
  #       shinyjs::removeClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-check")
  #       
  #       shinyjs::addClass(selector=paste0("#",paste0(click,"subscription")),
  #                            class="fa-regular fa-circle-xmark")
  #       
  #       
  #     }
  #   }
  # })
})

observeEvent(input$server_setup_checks__view, {
  selected_check_formula_id <- as.numeric(input$server_setup_checks__view)
  if (!isTruthy(selected_check_formula_id)) return (NULL)
  
  selected_indicator_check_id <- DBPOOL %>% dbGetQuery("
    select icf.indicator_check_id
    from p_rsf.indicator_check_formulas icf
    where icf.check_formula_id = $1::int",
    params=list(selected_check_formula_id))
  
  selected_indicator_check_id <- unlist(selected_indicator_check_id)
  
  SERVER_ADMIN_CHECKS.DISPLAY_CHECK_FORMULA <- reactiveVal(selected_check_formula_id)
  
  updateSelectizeInput(session=session,
                       inputId="server_admin_checks__selected_check",
                       selected=selected_indicator_check_id)
  
  updateTabsetPanel(session=session,
                    inputId="tabset_admin_system",
                    selected="Checks")
  
  updateTabItems(session=session,
                 inputId="sidebarMenu",
                 selected="system")
  
},ignoreInit = TRUE)

##################
###########OUTPUTS
##################

output$server_setup_checks__recheck_pending_UI <- renderText({
  
  req(SELECTED_PROGRAM_ID())
  rsf_program_id <- SELECTED_PROGRAM_ID()
  
  input$ui_setup__checks_recheck
  input$server_setup_checks__recheck
  input$server_setup_checks__reset
  input$setup_program_recheck_indicators
  
  selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__checks_program_facilities)
  if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
  
  pc <- DBPOOL %>% dbGetQuery("select count(*) as pending_count
                              from p_rsf.rsf_data_check_evaluations dce
                              where dce.rsf_pfcbl_id = any(select distinct fam.child_rsf_pfcbl_id
                                                           from p_rsf.rsf_pfcbl_id_family fam
                                                           where fam.parent_rsf_pfcbl_id = $1::int)",
                              params=list(selected_rsf_pfcbl_id))
  
  pc <- format(as.numeric(pc$pending_count),big.mark=",")
  if (!isTruthy(pc)) pc <- "0"
  return (pc)
})

#This takes a while and isn't very informative
# output$server_setup_checks__recheck_verified_UI <- renderText({
#   SELECTED_PROGRAM_ID()
#   rsf_program_id <- SELECTED_PROGRAM_ID()
#   
#   selected_rsf_pfcbl_id <- as.numeric(input$ui_setup__indicator_program_facilities)
#   if (!isTruthy(selected_rsf_pfcbl_id)) selected_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
#   
#   input$ui_setup__checks_recheck
#   input$server_setup_checks__recheck
#   input$server_setup_checks__reset
#   input$setup_program_recheck_indicators
#   
#   tc <- withProgress(message="Counting verified checks takes a minute or two...",
#                      value=0.5,{
#   
#     tc <- DBPOOL %>% dbGetQuery("
#       with checks as materialized (
#         select
#         	tip.to_check_rsf_pfcbl_id,
#         	tip.to_check_formula_id,
#         	tip.reporting_asof_date			
#         from p_rsf.compute_check_triggered_by_parameter tip
#         where tip.from_parameter_pfcbl_id = any (select ft.to_family_rsf_pfcbl_id
#         																				 from p_rsf.view_rsf_pfcbl_id_family_tree ft
#         																				 where ft.from_rsf_pfcbl_id = $1::int)
#         	and tip.is_calculation_trigger_parameter = true
#       )
#       select count(*) as verified_count
#       from (
#         select distinct
#         chk.to_check_rsf_pfcbl_id,
#         chk.to_check_formula_id,
#         chk.reporting_asof_date
#         from checks chk
#         inner join p_rsf.view_rsf_pfcbl_check_subscriptions pcs on pcs.rsf_pfcbl_id = chk.to_check_rsf_pfcbl_id
#         																										 and pcs.check_formula_id = chk.to_check_formula_id																										 
#         where pcs.is_subscribed = true
#         and chk.to_check_rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id
#         																		from p_rsf.rsf_pfcbl_id_family fam
#         																		where fam.parent_rsf_pfcbl_id = $1::int)
#         and exists(select * from p_rsf.rsf_pfcbl_reporting rpr
#         					 where rpr.rsf_pfcbl_id = chk.to_check_rsf_pfcbl_id
#         						 and rpr.reporting_asof_date = chk.reporting_asof_date)
#       )",
#     params=list(selected_rsf_pfcbl_id))
#   })
#   
#   tc <- format(as.numeric(tc$verified_count),big.mark=",")
#   if (!isTruthy(tc)) tc <- "0"
#   return (tc)
# })

output$ui_setup__checks_monitored_table <- DT::renderDataTable({
  
  
  selected_rsf_program_id <- SELECTED_PROGRAM_ID() ##Reactive on selected program_id
  display_checks <- SERVER_SETUP_CHECKS__FILTERED_CHECKS()
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

  if (empty(display_checks)) {
    return (DT::datatable(data.frame(Error="No checks found for this filter or selection"),
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
                             onclick='event.stopPropagation();Shiny.setInputValue(\"server_setup_checks__toggle_subscriptions\",-1,{priority:\"event\"})'>
                        </i></div>")
  
  display_checks <- display_checks[,
                                   .(toggle_box,
                                     pfcbl_name_html,
                                     type_class_label,
                                     check_name_html,
                                     check_formula_title_html,
                                     view_check_html)]
  DT::datatable(display_checks,
                rownames = FALSE,
                fillContainer=TRUE,
                colnames=c(toggleButton,"Applied On","Check Type","Check Name","Formula Title","Goto Check"),
                escape = FALSE, #Shouldn't be any HTML escapable text
                options=list(
                  dom="t",
                  bSort=T,
                  scrollY="70vh",
                  #scrollCollapse=TRUE,
                  ordering=F,
                  paging=F,
                  columnDefs = list(list(className = 'dt-left', targets = c(0,1)))))
  
})
