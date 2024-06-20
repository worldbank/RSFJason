SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK <- reactiveVal(NA)
SERVER_ADMIN_CHECKS.DISPLAY_CHECK_FORMULA <- reactiveVal(NA)
SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS <- reactiveVal(data.table()) #data.table of formulas
SERVER_ADMIN_CHECKS.MODULES <- reactiveVal(list())

RSF_CHECKS <- eventReactive(SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(), {
  
  if (!LOGGEDIN()) return (NULL)
                
  Shiny.destroyList(SERVER_ADMIN_CHECKS.MODULES)
  
  checks <- DBPOOL %>% dbGetQuery("
  select 
    ic.indicator_check_id,
    ic.check_name,
    ic.is_system,
    ic.check_class,
  	ic.check_type,
  	ic.check_pfcbl_category, 
    ic.grouping,
    ic.subgrouping,
    ic.definition,
    ic.auto_resolve_system_check
  from p_rsf.indicator_checks ic")

  if (empty(checks)) return (NULL)
  
  setDT(checks)
  
  if (isTruthy(input$server_admin_indicators__search)) {
    updateTextInput(session=session,
                    inputId="server_admin_checks__search",
                    value="")
  }
  
  return (checks)
}, ignoreInit=FALSE)

RSF_CHECK_TYPES <- eventReactive(RSF_CHECKS(), {
  
  if (is.null(RSF_CHECKS)) return (NULL)
  
  types <- DBPOOL %>% dbGetQuery("
    select
      check_type,
      check_type_name,
      apply_on
   from p_rsf.indicator_check_types ict")
  
  setDT(types)
  types[,type_class:=gsub("^([a-z]+)_.*","\\1",check_type)]
  types[,type_class_label:=fcase(type_class=="contract","Contract Compliance",
                                 type_class=="business","Business Rules",
                                 type_class=="data",    "Data Validity",
                                 default="Unclassified")]
  
  types[,sort_order:=fcase(type_class=="contract",1,
                           type_class=="business",2,
                           type_class=="data",3,
                           default=4)]
  setorder(types,
           sort_order,
           check_type_name)
  
  return (types)
  
},ignoreNULL = FALSE)

SERVER_ADMIN_CHECKS.SELECTED_CHECK <- eventReactive(c(RSF_CHECKS(),
                                                      input$server_admin_checks__selected_check), {

  all_checks <- RSF_CHECKS()                                                        
  selected_check_id <- as.numeric(input$server_admin_checks__selected_check)
  
  removeModal() 
  print(paste("SERVER_ADMIN_CHECKS.SELECTED_CHECK loading: ",selected_check_id))
  
  Shiny.removeInputs(moduleIds=SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()$module_id)  
  Shiny.destroyList(SERVER_ADMIN_CHECKS.MODULES)
  
  #Reset with a blank list
  SERVER_ADMIN_CHECKS.MODULES(list())
  
  selected_check <- NULL
  if (selected_check_id %in% all_checks$indicator_check_id) {
    selected_check <- all_checks[indicator_check_id==selected_check_id]
  }
  
  if (empty(selected_check)) {
    
    SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(data.table())
    
  
  } else {
    
    formulas <- DBPOOL %>% dbGetQuery("
      select 
        icf.check_formula_id,
        icf.indicator_check_id,
        icf.formula,
        icf.formula_result_message,
        icf.formula_comments,
        coalesce(icf.auto_resolve,false) as auto_resolve,
        icf.formula_fx_date,
        icf.check_formula_title
      from p_rsf.indicator_check_formulas icf
      where icf.indicator_check_id = $1::int",
      params=list(selected_check_id))  
    

    setDT(formulas)
    
    formulas[,module_id:=paste0("SERVER_ADMIN_CHECKS__formula",check_formula_id)]
    formulas[,edited:=FALSE]
    SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(formulas)
  }
  return (selected_check)  
  
},ignoreInit=FALSE,ignoreNULL = FALSE)

observeEvent(LOGGEDIN(), {
  if (isTruthy(LOGGEDIN())) {
    SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(0)
  } else {
    SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(NA)
  }
},ignoreInit = TRUE)

observeEvent(RSF_CHECKS(), {

  all_checks <- RSF_CHECKS()
  if (!isTruthy(all_checks)) return (NULL)

  selected_id <- abs(as.numeric(SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK()))
  
  #print(paste0("observeEvent(RSF_CHECKS() loading: ",selected_id))
  
  if (!isTruthy(selected_id)) selected_id <- ""
  if (!selected_id %in% all_checks$indicator_check_id) selected_id <- ""
  
  all_checks <- all_checks[order(check_name)]
  
  choices <- setNames(all_checks$indicator_check_id,all_checks$check_name)
  
  updateSelectizeInput(session=session,
                       inputId="server_admin_checks__selected_check",
                       choices=choices,
                       selected=selected_id)
  
}, ignoreInit = FALSE, ignoreNULL=FALSE, priority=100) #Refresh on init

##set system indicators drop down menu.  Updated when new indicators are selected
observeEvent(c(RSF_CHECKS(),
               input$server_admin_checks__search), {
                 
  all_checks <- RSF_CHECKS()
  
  if (!isTruthy(all_checks)) return (NULL)
  
  selected_id <- abs(as.numeric(SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK()))
  all_checks <- all_checks[order(check_name)]
  keywords <- as.character(input$server_admin_checks__search)
  
  if (!isTruthy(selected_id)) selected_id <- ""
  if (!selected_id %in% all_checks$indicator_check_id) selected_id <- ""
  
  choices <- ""
  
  if (isTruthy(keywords)) {
   keywords <- unlist(strsplit(keywords,"[^[:alpha:]]+"))
   labels <- rbindlist(list(all_checks[,.(indicator_check_id,label=check_name)],
                            all_checks[,.(indicator_check_id,label=definition)],
                            all_checks[,.(indicator_check_id,label=check_type)],
                            all_checks[,.(indicator_check_id,label=check_class)]))
   
   matches <- lapply(keywords,grepl,x=labels$label,ignore.case=TRUE)
   matches <- which(Reduce(`&`,matches))
   
   if (length(matches) > 0) {
     labels <- labels[matches]
     
     priorities <- lapply(strsplit(tolower(labels$label),"[^[:alpha:]]+"),
                          pmatch,
                          table=tolower(keywords),
                          duplicates.ok=FALSE,
                          nomatch=0)
     labels[,
            is_sorted:=sapply(priorities,
                              function(x) {
                                if (!any(x>0)) as.numeric(NA)
                                else !is.unsorted(x[1:which.max(rev(x)>0)])
                              })]
     labels[,
            is_sorted:=fcase(is_sorted==TRUE,1,
                             is_sorted==FALSE,2,
                             default=3)]
     labels[,
            match_priority:=sapply(lapply(lapply(priorities,diff),abs),sum)]
     
     labels[,
            name_len:=sapply(label,nchar)]
     
     setorder(labels,
              is_sorted,
              match_priority,
              name_len)
     labels[,
            match_priority:=1:.N]
     
     labels <- labels[,
                      .(match_priority=min(match_priority)),
                      by=.(indicator_check_id)]
     
     setorder(labels,
              match_priority,
              indicator_check_id)
     
     selected_id <- labels$indicator_check_id[1]
     
     labels <- all_checks[labels,
                              on=.(indicator_check_id),
                              nomatch=NULL][order(match_priority),
                                            .(indicator_check_id,
                                              check_name)]
     
     choices <- setNames(labels$indicator_check_id,
                         labels$check_name)
     
     
   } else {
     choices <- c(`No matching checks found`="")
     selected_id <- ""
   }
  } else {
   choices <- setNames(all_checks$indicator_check_id,
                       all_checks$check_name)
  }
  
  updateSelectizeInput(session=session,
                      inputId="server_admin_checks__selected_check",
                      choices=choices,
                      selected=selected_id)
  
  }, ignoreInit = FALSE, ignoreNULL=FALSE, priority=100) #Refresh on init


observeEvent(input$server_admin_checks__clear, {
  
  updateTextInput(session=session,
                  inputId="server_admin_checks__selected_check",
                  value="")
  
},ignoreInit=TRUE)

observeEvent(input$server_admin_checks__create_check, {

  if (LOGGEDIN()==FALSE) return (as.numeric(NA))
  
  if (!isTruthy(input$server_admin_checks__create_check)) return (as.numeric(NA))
  
  show_modal_spinner(
    spin = "circle",
    color = "blue",
    text = "Loading...",
    session = shiny::getDefaultReactiveDomain()
  )
  
  new_check_id <- DBPOOL %>% db_check_create()
  if (!isTruthy(new_check_id)) {
    removeModal()
    showNotification(type="error",
                     h3("Failed to create a new indicator"))
    return (as.numeric(NA))
  }
  SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(new_check_id)
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$server_admin_checks__edit_check_save, { 

  check <- SERVER_ADMIN_CHECKS.SELECTED_CHECK()
  if (!isTruthy(check)) return (NULL)
  
  check_formulas <- SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()

  check_name <- normalizeSystemName(input$server_admin_checks__edit_check_name)
  check_class <- input$server_admin_checks__edit_check_class
  check_type <- input$server_admin_checks__edit_check_type
  check_category <- input$server_admin_checks__edit_check_category
  
  grouping <- input$server_admin_checks__edit_check_grouping
  subgrouping <- input$server_admin_checks__edit_check_subgrouping
  definition <- input$server_admin_checks__edit_check_definition

  if (!isTruthy(check_name)) return(showNotification(type="error",h3("Check name is a required field")))
  
  if (!isTruthy(check_class) ||
      !check_class %in% c("critical","error","warning","info")) return(showNotification(type="error",h3("Check class is a required field")))
  
  if (!isTruthy(check_type)) check_type <- "none"
  
  if (check$is_system==FALSE &&
      !check_category %in% c("program","facility","client","borrower","loan")) return(showNotification(type="error",h3("Check category is a required field")))
  
  name_prefix <- ""
  if (check$is_system==TRUE) name_prefix <- "sys_" #sys can be applied across any category and therefore not expected within the name.
  else name_prefix <- paste0(fcase(check_category=="program","(program|progarms)",
                                   check_category=="facility","(facility|facilities)",
                                   check_category=="client","(client|clients)",
                                   check_category=="borrower","(borrower|borrowers)",
                                   check_category=="loan","(loan|loans)"),
                             "_")
  
  if (!grepl(paste0("^",name_prefix),check_name)) {
    check_name <- paste0(check_category,"_",check_name)
  }
  
  if (!isTruthy(definition)) definition <- ""

  if (!isTruthy(grouping)) grouping <- NA
  if (!isTruthy(subgrouping)) subgrouping <- NA
  
  check_data <- data.table(indicator_check_id=check$indicator_check_id,
                           check_name=check_name,
                           check_class=check_class,
                           check_pfcbl_category=check_category,
                           check_type=check_type,
                           grouping=grouping,
                           subgrouping=subgrouping,
                           definition=definition)

  withProgress(message="Saving check...",value=0.5, {

    tryCatch({
      DBPOOL %>% db_check_update(check_data=check_data,
                                 check_formulas=check_formulas,
                                 is_system_check=check$is_system)
      
      
      new_subscriptions <- DBPOOL %>% dbGetQuery("
        with new_subscriptions as MATERIALIZED (
          insert into p_rsf.rsf_program_facility_checks(rsf_pfcbl_id,
                                                        check_formula_id,
                                                        indicator_check_id,
                                                        rsf_program_id,
                                                        rsf_facility_id,
                                                        is_subscribed)
          select 
            fcs.rsf_pfcbl_id,
            fcs.check_formula_id,
            fcs.indicator_check_id,
            fcs.rsf_program_id,
            fcs.rsf_facility_id,
            true as is_subscribed
          from p_rsf.view_rsf_program_settings rps
          inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_program_id = rps.rsf_program_id
          inner join p_rsf.view_rsf_program_facility_check_subscribable fcs on fcs.rsf_pfcbl_id = ids.rsf_pfcbl_id
          where setting_name = 'auto_monitor_new_checks' and setting_value = 'true'
            and ids.pfcbl_category = 'program'
          	and fcs.is_subscribable = true
          	and fcs.indicator_check_id = $1::int
          on conflict
          do nothing
          returning rsf_pfcbl_id,check_formula_id
        )
        select 
          ns.rsf_pfcbl_id,
          ns.check_formula_id,
          coalesce(nids.nickname,nids.name) as rsf_name,
          ic.check_name,
          icf.check_formula_title,
          ic.check_class,
          ic.check_type
        from new_subscriptions ns
        inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ns.rsf_pfcbl_id
        inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = ns.check_formula_id
        inner join p_Rsf.indicator_checks ic on ic.indicator_check_id = icf.indicator_check_id
        order by 
          nids.rsf_full_name,
          ic.check_name,
          icf.check_formula_title",
        params=list(check_data$indicator_check_id))
      
      setDT(new_subscriptions)
      
      if (!empty(new_subscriptions)) {
        new_subscriptions <- new_subscriptions[,
                                               .(snames=paste0(rsf_name,collapse=", ")),
                                               by=.(check_name,check_class,check_type,check_formula_title)]
        ui <- tagList()
        for (i in 1:nrow(new_subscriptions)) {
          ns <- new_subscriptions[i]
          ui[[length(ui)+1]] <- div(div(HTML(format_html_check(check_name=paste0(ns$check_name,":",ns$check_formula_title),
                                                               check_class=ns$check_class,
                                                               check_type=ns$check_type,
                                                               is_subscribed=TRUE,
                                                               is_system=FALSE))),
                                            div(style="font-weight:bold;",paste0(ns$snames)))
        }
        
        showNotification(type="message",
                         duration=35,
                         ui=div(style="font-size:18px;",
                                div(style="font-weight:bold;padding-bottom:10px;",
                                    "The following RSF Programs are set to auto-subscribe to new/modified checks and are now monitoring:"),
                                div(style="padding:15px 15px 15px 15px;border-top:solid black 1px;border-bottom:solid black 1px;",
                                    ui),
                                div(style="padding-top:15px;",
                                    "Change subscription or auto-subscribe behavior in RSF Setup")))
      }

      #already loaded, so reload to refresh the global indicators from the database
      if (check$indicator_check_id %in% SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK()) {
        SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(-check$indicator_check_id)
      } else {
        SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(check$indicator_check_id)
      }
    },
    error=function(e) {
      showNotification(type="error",
                       duration = NULL,
                       ui=div(h3("Save failed: verify that indicator formula names are correct and valid"),
                              h3(paste0("Error: ",conditionMessage(e)))))
    },
    warning=function(w) {
      showNotification(type="warning",
                       duration = NULL,
                       ui=div(h3("Save failed: verify that indicator formula names are correct and valid"),
                              h3(paste0("Error: ",conditionMessage(w)))))
    })
    
    incProgress(amount=1.0)                 
  })
})

observeEvent(input$server_admin_checks__edit_check_delete, {

  check <- SERVER_ADMIN_CHECKS.SELECTED_CHECK()

  if (check$is_system==TRUE) {
    return(showNotification(type="error",
                            h3("System checks cannot be deleted")))
  }
  
  used <- dbGetQuery(pool,"
          select sn.sys_name
          from p_rsf.rsf_program_facility_checks pfc
          inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = pfc.rsf_pfcbl_id
          where pfc.indicator_check_id = $1::int
            and pfc.is_subscribed = true",
          params=list(check$indicator_check_id))
  
  setDT(used)
  if (!empty(used)) {
    
    usedby <- tagList()
    for (i in 1:nrow(used)) {
      usedby[[length(usedby)+1]] <- div(used[i,sys_name])
    }
    showNotification(type="error",
                     ui=div(h3("Failed to delete check: '",input$check_formula_title,"' because it is actively used by:"),
                            div(style="display:flex;flex-direction:row",usedby),
                            h4("Go to: Setup Tab and remove this check from these entities, before it can be deleted.")),
                     duration=20)
    
  } else {

    withProgress(message="Deleting check...",value=0.5, {
      
      DBPOOL %>% dbExecute("
        delete from p_rsf.indicator_checks 
        where indicator_check_id = $1::int",
        params=list(check$indicator_check_id))

      SERVER_ADMIN_CHECKS.LOAD_RSF_CHECK(-1 * check$indicator_check_id)      
      incProgress(amount=1.0,message="Completed")
    })
  }
})

observeEvent(input$server_admin_checks__edit_check_add_formulas, {
  
  selected_check <- SERVER_ADMIN_CHECKS.SELECTED_CHECK()
  if (!isTruthy(selected_check)) return (NULL)
  
  new_formula <- data.table(check_formula_id=(-1 * as.numeric(input$server_admin_checks__edit_check_add_formulas)), #set when saved, pseudo for now.
                            indicator_check_id=selected_check$indicator_check_id,
                            formula=as.character(NA),
                            formula_result_message=as.character(NA),
                            formula_comments=as.character(NA),
                            auto_resolve=FALSE,
                            formula_fx_date="calculation",
                            check_formula_title=as.character(NA))
  
  new_formula[,module_id:=paste0("SERVER_ADMIN_CHECKS__formula",check_formula_id)]
  new_formula[,edited:=FALSE]
  
  setcolorder(new_formula,
              neworder = names(SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS()))
  
  formulas <- rbindlist(list(SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(),
                             new_formula))
  
  SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS(formulas)

  start.collapsed <- nrow(formulas) > 1  
  
  id <- new_formula$module_id
  ui <- server_admin_checks_formulas.module_ui(id=ns(id),
                                               formula=as.list(new_formula),
                                               rsf_indicators=RSF_INDICATORS(),
                                               start.collapsed=FALSE)
  
  server_admin_checks_formulas.module_server(id=ns(id),
                                             module_id=id,
                                             pool=DBPOOL,
                                             SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS=SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS,
                                             SERVER_ADMIN_CHECKS.MODULES=SERVER_ADMIN_CHECKS.MODULES)

  insertUI(selector = "#server_admin_checks__edit_check_formulas",
           where="afterBegin",
           ui=ui)
  
  
  
})

output$server_admin_checks__edit_name_UI <- renderUI({
  if (!isTruthy(SERVER_ADMIN_CHECKS.SELECTED_CHECK())) return (NULL)
  check_name <- input$server_admin_checks__edit_check_name
  check_class <- input$server_admin_checks__edit_check_class
  check_type <- input$server_admin_checks__edit_check_type
  if (!isTruthy(check_class)) check_class <- "info"
  is_system <- SERVER_ADMIN_CHECKS.SELECTED_CHECK()$is_system

  check_name <- normalizeSystemName(check_name)
  html <- format_html_check(check_name=check_name,
                            check_class=check_class,
                            check_type=check_type,
                            is_system=is_system)
  return (HTML(html))
})

output$server_admin_checks__edit_check_UI <- renderUI({
  
  check <- SERVER_ADMIN_CHECKS.SELECTED_CHECK()

  if (empty(check)) return (h2("Please select a check to edit"))
  
  editable <- TRUE #System names are requied by system, not to be changed by users.
  is_system <- as.logical(check$is_system)
  if (!isTruthy(is_system)) is_system <- FALSE
  
  check_name <- check$check_name

  class_choices <- c("critical","error","warning","info")
  class_selected <- check$check_class
  
  check_type_choices <- RSF_CHECK_TYPES()
  check_type_choices <- setNames(check_type_choices$check_type,
                                 check_type_choices$check_type_name)
  
  check_type_selected <- check$check_type
  if (!isTruthy(check_type_selected)) check_type_selected <- "none"
  
  pfcbl_choices <- c(`Program`='program',
                     `Facility`='facility',
                     `Client`='client',
                     `Borrower`='borrower',
                     `Loan`='loan')

  pfcbl_category_selected <- check$check_pfcbl_category
  if (!isTruthy(pfcbl_category_selected)) pfcbl_category_selected <- "loan"

  if (is_system==TRUE) {
    pfcbl_choices <- c(`System`="system")
    pfcbl_category_selected <- "system"
  }  
  
  grouping_choices <- c(`{ None }`="",
                        `Program`='program',
                        `Facility`='facility',
                        `Client`='client',
                        `Borrower`='borrower',
                        `Loan`='loan')
  
  grouping_selected <- check$grouping
  if (!isTruthy(grouping_selected)) grouping_selected <- "{ None }"
  
  
  check_subgrouping <- check$subgrouping
  if (!isTruthy(check_subgrouping)) check_subgrouping <- ""

  definition <- check$definition[[1]]
  if (!isTruthy(definition)) definition <- ""
  
  system_auto_resolve_selected <- check$auto_resolve_system_check
  if (is_system==FALSE) system_auto_resolve_selected <- ""
  
  formulas  <- isolate({ SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS() })
  display_formula <- isolate({ as.numeric(SERVER_ADMIN_CHECKS.DISPLAY_CHECK_FORMULA()) })
  
  if (isTruthy(display_formula)) {
    SERVER_ADMIN_CHECKS.DISPLAY_CHECK_FORMULA(as.numeric(NA))
  }
  
  ui_formulas <- tagList()
  if (!empty(formulas)) {
    for (i in 1:nrow(formulas)) {
      formula <- formulas[i]
      id <- formula$module_id
      display_formula <- isTruthy(display_formula) && formula$check_formula_id==display_formula
      ui <- server_admin_checks_formulas.module_ui(id=ns(id),
                                                   formula=formula,
                                                   rsf_indicators=RSF_INDICATORS(),
                                                   start.collapsed=display_formula)
      ui_formulas[[length(ui_formulas)+1]] <- ui

      server_admin_checks_formulas.module_server(id=ns(id),
                                                 module_id=id,
                                                 pool=DBPOOL,
                                                 SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS=SERVER_ADMIN_CHECKS.SELECTED_CHECK_FORMULAS,
                                                 SERVER_ADMIN_CHECKS.MODULES=SERVER_ADMIN_CHECKS.MODULES)
    }
  }

  ui <- div(style='width:955px;background-color:gainsboro;padding:10px;margin: 0 auto;',
            fluidRow(align="center",style="padding-top:5px;padding-bottom:5px;",
                     column(12,uiOutput(outputId="server_admin_checks__edit_name_UI"))),
            
            fluidRow(column(5,
                            enabled(state=editable & !is_system,
                                      textInput(inputId="server_admin_checks__edit_check_name",
                                         width="100%",
                                         label = tags$label("Check Name",
                                                            style='margin:0px;',
                                                            tags$i(class='fas fa-question icon-question',
                                                                   title="Check name may use only: upper and lower-case letters and numbers; and underscore punctiation. Names will be auto-formatted.")),
                                         value=check_name))),
                     column(2,
                            enabled(state=editable & !is_system,
                            selectizeInput(inputId="server_admin_checks__edit_check_class",
                                           label="Class",
                                           choices=class_choices,
                                           selected=class_selected))),
                     column(2,
                            enabled(state=editable & !is_system,
                                    selectizeInput(inputId="server_admin_checks__edit_check_category",
                                           label="Category",
                                           choices=pfcbl_choices,
                                           selected=pfcbl_category_selected))),
                     column(3,
                            selectizeInput(inputId="server_admin_checks__edit_check_type",
                                           label="Check Type",
                                           choices=check_type_choices,
                                           selected=check_type_selected))
            ),
            fluidRow(column(12,
                            textAreaInput(inputId="server_admin_checks__edit_check_definition",
                                          label="Definition",
                                          value=definition,
                                          width="100%"))),
            ifelse(is_system==FALSE,
                  list(tagList(
                    fluidRow(
                             column(4,enabled(state=editable & !is_system,
                                              selectizeInput(inputId="server_admin_checks__edit_check_grouping",
                                                     label="Formula Grouping",
                                                     choices=grouping_choices,
                                                     selected=grouping_selected,
                                                     options=list(placeholder="Advanced Formulas: Grouping")))),
                             column(8,enabled(state=editable & !is_system,
                                              textInput(inputId="server_admin_checks__edit_check_subgrouping",
                                                        label="Sub-Grouping",
                                                        width="100%",
                                                        value=check_subgrouping,
                                                        placeholder="Advanced Formulas: Sub-Grouping. Blank by default.")))
                    ),
                    fluidRow(align="left",style="padding-bottom:5px;",
                            column(12,
                                   div(actionButton(inputId="server_admin_checks__edit_check_add_formulas",
                                                    label="Add Formula",
                                                    icon=icon("plus-square"),class="btn btn-primary")),
                                   div(id="server_admin_checks__edit_check_formulas",style="padding-top:10px;",
                                       ui_formulas)
                    ))
                  )),
                  list(tagList(
                    fluidRow(
                      column(4,
                             selectizeInput(inputId="server_admin_checks__edit_check_system_auto_resolve",
                                                      label="Auto-Resolve",
                                                      choices=c(`Yes`=TRUE,
                                                                `No`=FALSE),
                                                      selected=system_auto_resolve_selected)))
                  ))
            ),
            fluidRow(style='padding-top:10px;',
                     column(12,div(style='border-top:solid gray 1px;',
                                   fluidRow(
                                     column(6,align="left",
                                      div(style="padding-top:5px;",actionButton(inputId="server_admin_checks__edit_check_delete",class="btn-danger",label="Delete Check",icon=icon("trash")))
                                     ),
                                     column(6,align="right",
                                      div(style="padding-top:5px;",actionButton(inputId="server_admin_checks__edit_check_save",class="btn-success",label="Save",icon=icon("save"))))))))
  )  
  return (ui)
})

output$server_admin_checks__download_checks <- downloadHandler(
  
  filename = function() { "RSF Checks List.xlsx" },
  content = function(file) {
    
    checks <- RSF_CHECKS()
    
    if (!isTruthy(checks)) return (showNotification(type="error",h2("Unable to download checks while system is loading.  Try again shortly")))
    
    excelwb <- withProgress(message="Downloading checks...",value=0.5,
                            {
                              
                              
                              checks_dt <- checks[,.(ID=indicator_check_id,
                                                     Name=check_name,
                                                     Class=check_class,
                                                     Definition=definition,
                                                     is_system,
                                                     #is_varying=is_indicator_varying,
                                                     formula_grouping=grouping,
                                                     formula_subgrouping=subgrouping)]
                              
                              formulas_dt <- rbindlist(unlist(checks$formulas,recursive = F))
                              formulas_dt <- formulas_dt[,.(ID=indicator_check_id,`Applied on Indicator`=for_indicator_name,formula,formula_result_message,auto_resolve,formula_comments)]
                              
                              checks_dt <- formulas_dt[checks_dt,on=.(ID),nomatch=NA]
                              setcolorder(checks_dt,neworder=c("ID","Name","Class","Applied on Indicator","Definition","is_system","is_varying","formula_grouping","formula_subgrouping",
                                                               "formula","formula_result_message","auto_resolve","formula_comments"))
                              
                              excelwb <- openxlsx::createWorkbook()
                              addWorksheet(excelwb,sheetName="RSF Checks")
                              writeDataTable(excelwb,sheet=1,x=checks_dt)
                              setColWidths(excelwb,sheet=1,cols=c(1:14), widths=c(8,35,10,35,30,10,10,10,15,30,30,10,30))
                              setColWidths(excelwb,sheet=1,cols=c(15:ncol(indicator_dt)), widths=c(30))
                              
                              excelwb
                            })
    
    openxlsx::saveWorkbook(excelwb,file)
  },
  contentType = "application/xlsx")