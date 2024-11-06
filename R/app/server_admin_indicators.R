
SERVER_ADMIN_INDICATORS.MODULES <- reactiveVal(list())
LOAD_SYSTEM_INDICATOR <- reactiveVal(NA)
SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS <- reactiveVal(data.table())

SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS <- reactiveVal(data.table()) #data.table of formulas
SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING <- reactiveVal(0)
SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID <- reactiveVal(NA)
#RSF_INDICATORS is used by all the admin screens to have a full list of indicators, etc
RSF_INDICATORS <- eventReactive(c(LOGGEDIN(),
                                  LOAD_SYSTEM_INDICATOR()), #will be called when a new indicator is created or deleted -- so refresh
{

  if (!LOGGEDIN()) return (NULL)

  indicators <- DBPOOL %>% db_indicators_get_labels()

  if (empty(indicators)) return (NULL)

  if (any(indicators$redundancy_error,na.rm=T)) {
    bad_indicators <- indicators[redundancy_error==TRUE,
                                 .(indicator_name,
                                   labels)]
    bad_indicators <- bad_indicators[,unlist(labels,recursive=F),
                                     by=.(indicator_name)][redundancy_error==TRUE]
    
    if (nrow(bad_indicators) > 1) {
      setorder(bad_indicators,
               label_normalized,
               -is_primary)
      ui <- tagList()
      for (i in 1:nrow(bad_indicators)) {
        ui[[length(ui)+1]] <- p(paste(unlist(bad_indicators[i,.(indicator_name,
                                                           key=paste0(key_type,"=",label_key),
                                                           paste0("'",label,"'"),
                                                           primary=ifelse(is_primary," (primary)"," (alias) <- should this one be deleted?"))]),collapse=" "))
      }
      showNotification(type="error",
                       duration=NULL,
                       ui=div(h3("Error: Redundant indicator titles have been added for different indicators.  These MUST be corrected in Indicator Admin"),
                              ui,
                              h4("If a template is using redundant lables (this is bad practice), these may be specified in RSF Setup -> Template Setup, where header instructions may be added to dis-ambiguate these labels")))
    }
  }
  return (indicators)
  
}, ignoreInit=FALSE) %>% debounce(100)

observeEvent(LOAD_SYSTEM_INDICATOR(), {
  
  Shiny.destroyList(SYSTEM_OPTIONS_MODULES)
  #If we systematically load or refresh our indicator list, clear-out searches that may already exist.
  #Eg, if we create a new indicator, don't have a filter on
  
  if (isTruthy(input$server_admin_indicators__search)) {
    updateTextInput(session=session,
                    inputId="server_admin_indicators__search",
                    value="")
  }
},priority=100)

SERVER_ADMIN_INDICATORS_DO_RECALCULATE <- function(rsf_pfcbl_ids=NA,
                                                   indicator_id,
                                                   reset=FALSE) {
  
  if (any(is.na(rsf_pfcbl_ids))) {
    if (reset==TRUE) {
      all_ids <- DBPOOL %>% dbGetQuery("
        select pfi.rsf_pfcbl_id
        from p_rsf.rsf_program_facility_indicators pfi 
        where pfi.is_subscribed = true
          and pfi.indicator_id = $1::int",
      params=list(indicator_id))
      rsf_pfcbl_ids <- all_ids$rsf_pfcbl_id
    } 
    else {

        all_ids <- DBPOOL %>% dbGetQuery("
          select distinct dce.rsf_pfcbl_id from p_rsf.rsf_data_calculation_evaluations dce
          where dce.indicator_id = $1::int",
          params=list(indicator_id))
            rsf_pfcbl_ids <- all_ids$rsf_pfcbl_id
    }
  }
  
  if (length(rsf_pfcbl_ids) > 0) {
    withProgress(message="Recalculating",
                 value=(1/length(rsf_pfcbl_ids))/2, {
      
      for (id in rsf_pfcbl_ids) {
        
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Recalculating affected data: ",dots))
        }
        
        pid <- DBPOOL %>% dbGetQuery("
          select ids.rsf_program_id 
          from p_rsf.rsf_pfcbl_ids ids
          where ids.rsf_pfcbl_id = $1::int",
          params=list(id))
        pid <- pid$rsf_program_id
        
        if (reset==TRUE) {
          DBPOOL %>% dbExecute("
          select p_rsf.rsf_pfcbl_indicator_recalculate(v_rsf_pfcbl_id => $1::int,
                                                        v_indicator_id => $2::int)",
          params=list(id,
                      indicator_id))
        }
        
        DBPOOL %>% rsf_program_calculate(rsf_program_id=pid,
                                         rsf_indicators=RSF_INDICATORS(),
                                         rsf_pfcbl_id.family=id,
                                         calculate_future=TRUE,
                                         reference_asof_date=NULL,
                                         status_message=progress_status_message)
        
        incProgress(amount=(1/length(rsf_pfcbl_ids)))
      }
    })
  }
}

SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR <- eventReactive(c(RSF_INDICATORS(),
                                                                     input$admin_system_selected_indicator), {

  selected_indicator_id <- as.numeric(input$admin_system_selected_indicator)
  all_indicators <- RSF_INDICATORS()

  removeModal() 

  Shiny.removeInputs(moduleIds=SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()$module_id)  
  Shiny.destroyList(SERVER_ADMIN_INDICATORS.MODULES)
  
  #Reset with a blank list
  SERVER_ADMIN_INDICATORS.MODULES(list())
  
  selected_indicator <- NULL
  if (selected_indicator_id %in% all_indicators$indicator_id) {
    selected_indicator <- all_indicators[indicator_id==selected_indicator_id]
  }
  
  if (empty(selected_indicator)) {
    SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(data.table())
    SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS(data.table())
  
  } else {
    
    labels <- rbindlist(selected_indicator$labels)
    
    labels <- labels[,
                     .(indicator_id,
                       label_key,
                       label,
                       key_type,
                       key_type_template_id,
                       is_primary)]
    #must be a new indicator without any labels.  SYS will exist by its default
    if (!any(labels$label_key=="EN")) {
      labels <- rbindlist(list(labels,
                               data.table(indicator_id=selected_indicator$indicator_id,
                                          label_key="EN",
                                          label="",
                                          key_type="language",
                                          key_type_template_id=NA,
                                          is_primary=TRUE)))
    }
    
    labels[,label_id:=selected_indicator$label_id]
    labels[,
           module_id:=paste0("IND",indicator_id,"_LAB",.GRP),
           by=.(indicator_id,
                label_id,
                label_key)]
    
    setorder(labels,
             module_id,
             -is_primary)
    
    labels[,label_rank:=1:.N,
           by=.(module_id)]
    
    labels <- labels[label_key != "SYS"] #This is the indicator system name, not a label for user purposes.
    
    
    
    formulas <- DBPOOL %>% dbGetQuery("
      select 
        indf.overwrite as formula_overwrite,
        indf.indicator_id,
        indf.formula,
        indf.formula_sort,
        
        indf.formula_fx_date,
        unit_ind.indicator_name as formula_unit_set_by_indicator_name,
        indf.formula_id,
        indf.formula_title,
        indf.is_primary_default,
        indf.formula_notes,
        ind.is_system,
        ind.indicator_name,
        --ind.is_periodic_or_flow_reporting,
        exists(select * from p_rsf.rsf_program_facility_indicators pfi
               where pfi.formula_id = indf.formula_id) as is_applied
      
      from p_rsf.indicator_formulas indf
      inner join p_rsf.indicators ind on ind.indicator_id = indf.indicator_id
      left join p_rsf.indicators unit_ind on unit_ind.indicator_id = indf.formula_unit_set_by_indicator_id
      where indf.indicator_id = $1::int",
      params=list(selected_indicator$indicator_id))
    
    setDT(formulas)
    
    formulas[,module_id:=paste0("SERVER_ADMIN_INDICATORS__formula",formula_id)]
    formulas[,edited:=FALSE]
    
    SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS(labels)
    SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(formulas)
    
    pending <- DBPOOL %>% dbGetQuery("
      select count(*)::int as pending
      from p_rsf.rsf_data_calculation_evaluations dce
      where dce.indicator_id = $1::int",
      params=list(selected_indicator_id))
    
    pending <- pending$pending
    if (!isTruthy(pending)) pending <- 0
    SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING(pending)
    
  }
  
  return (selected_indicator)
},
ignoreInit=FALSE,ignoreNULL = FALSE)

indicator_type_unit_choices <- function(indicator_type,current_unit) {
  #Doesn't use full list because full list is SPAMMY.  But it lets users type-in their own input, as needed.
  
  unit_choices <- c(`{ Not applicable }`="")
  current_unit <- trimws(toupper(current_unit))
  
  if (indicator_type=="currency") unit_choices <- c(unit_choices,
                                                    `Currency: LCU`="LCU",
                                                    `Currency: USD`="USD",
                                                    `Currency: EUR`="EUR",
                                                    setNames(current_unit,paste0("Currency: ",current_unit)))
  
  else if (indicator_type=="currency_ratio") unit_choices <- c(unit_choices,
                                                               `FX: USD/LCU`="USD/LCU",
                                                               `FX: EUR/LCU`="EUR/LCU",
                                                               `FX: EUR/USD`="EUR/USD",
                                                               setNames(current_unit,paste0("FX: ",current_unit)))
  
  else if (indicator_type=="number") unit_choices <- c(unit_choices,
                                                       `Time: Days`="DAYS",
                                                       `Time: Months`="MONTHS",
                                                       `Time: Years`="YEARS",
                                                       setNames(current_unit,paste0("Custom: ",current_unit)))
  
  unit_choices <- unique(unit_choices)
  
  return (unit_choices)
}

module_ui_system_indicator_label <- function(id,
                                             label,
                                             keys_list) {
  ns <- NS(id)

  lkey <- unique(label$label_key)
  primary_label <- label[is_primary==TRUE,label]
  secondary_labels <- label[is_primary==FALSE,label]
  #print(paste0("Drawing module_ui_system_indicator_label for id=",id," key=",lkey))
  
  if (length(secondary_labels)==0) secondary_labels <- c("")
  
  slabels <- tagList()
  for(i in 1:length(secondary_labels)) {
    slabel <- secondary_labels[i]
    if (is.na(slabel)) slabel <- ""
    id <- paste0("label",(i+1))    
    slabel_ui <- fluidRow(column(12,style="375px;",class="options-groups-labels",textInput(inputId=ns(id),label=NULL,width="100%",value=slabel,placeholder="No alternative defined")))
    slabels[[length(slabels)+1]] <- slabel_ui
    #print(paste0("REGISTERING SECONDARY LABEL: ",id))
  }
  
  ui <- fluidRow(column(2,style='width:150px;padding:0 3px 0 15px;',selectizeInput(inputId=ns("label_key"),label=NULL,width="100%",choices=keys_list,selected=toupper(lkey),options=list(placeholder="Select..."))),
                 column(4,style='width:375px;padding:0 3px 0 3px;',textInput(inputId=ns("label1"),label=NULL,width="100%",value=primary_label,placeholder="Please enter a label...")),
                 column(4,style='width:375px;padding:0 0px 5px; 3px;',id=ns("location_indicator_secondary_labels"),slabels),
                 column(2,style="width:20px; float:left;margin-top:4px;margin-left:-8px",
                        actionButton(ns("label_secondary_add"),label=NULL,title="Add Alternative Label",icon=icon("plus"),class="btn-primary btn-circle", style="font-size:10px; width:20px; height:20px; padding: 0px 5px; line-height:0px;")))
  return (ui)
}

module_session_system_indicator_label <- function(id, 
                                                  module_id, 
                                                  INDICATOR_LABELS,
                                                  SERVER_ADMIN_INDICATORS.MODULES) {

  init.count <- isolate({ max(2,sum(INDICATOR_LABELS()$module_id==module_id)) })
  modules <- isolate({ SERVER_ADMIN_INDICATORS.MODULES() })
  
  moduleServer(id,function(input,output, session) 
  {
    
    this.id <- reactiveVal(module_id)
    this.labelCount <- reactiveVal(init.count) #2 because always at least fields for primary and secondary labels (2)
      
    o1 <- observeEvent(input$label_key, { 
      lkey <- input$label_key
      
      all_labels <- INDICATOR_LABELS()
      other_labels <- all_labels[module_id != this.id()]
      all_keys <- SYSTEM_ALL_LABEL_KEYS()[,label_key]
      if (!lkey %in% all_keys) {
        showNotification(type="error",h2("Error: ",lkey," is not a valid label choice"))
        return (NULL)
      } else if (any(other_labels$label_key==lkey)) {
        
        showNotification(type="error",h2("Error: ",lkey," is already selected and cannot be selected twice."))
        return (NULL)
      }
      
      all_labels[module_id==this.id(),label_key:=lkey]
      INDICATOR_LABELS(all_labels)
      
    },ignoreInit=TRUE)
    
    o2 <- observeEvent(sapply(1:this.labelCount(),function(x) input[[paste0("label",x)]]),{
  

      all_labels <- INDICATOR_LABELS()
      
      for(rank in 1:this.labelCount()) {
        slabel <- input[[paste0("label",rank)]]
        if (!isTruthy(slabel)) slabel <- ""
        if (empty(all_labels[module_id==this.id() & label_rank==rank])) {
          new_label <- all_labels[module_id==this.id(),
                                  .(label="",
                                    is_primary=FALSE,
                                    label_rank=max(label_rank)+1),
                                  by=.(module_id,
                                       label_id,
                                       indicator_id,
                                       label_key,
                                       key_type,
                                       key_type_template_id)]
          
          setcolorder(new_label,
                      neworder=names(all_labels))
          
          all_labels <- rbindlist(list(all_labels,new_label))
        }
        all_labels[module_id==this.id() & label_rank==rank,label:=slabel]
      }
      INDICATOR_LABELS(all_labels)
      
    },ignoreInit=TRUE)
  
    o3 <- observeEvent(input$label_secondary_add, {
      
      this.labelCount(this.labelCount()+1)
      
      id <- paste0("label",this.labelCount())   
      new_ui <- fluidRow(column(12,class="options-groups-labels",textInput(inputId=session$ns(id),label=NULL,width="375px",value="",placeholder="No alternative defined")))
      
      insertUI(selector=paste0("#",session$ns("location_indicator_secondary_labels")),where="beforeEnd",ui=new_ui)
    }, ignoreInit=TRUE)
    

    modules[[length(modules)+1]] <- o1
    modules[[length(modules)+1]] <- o2
    modules[[length(modules)+1]] <- o3
    
    SERVER_ADMIN_INDICATORS.MODULES(modules)
    
  })  
}

observeEvent(LOGGEDIN(), {
  if (isTruthy(LOGGEDIN())) {
    LOAD_SYSTEM_INDICATOR(0)
  } else {
    LOAD_SYSTEM_INDICATOR(NA)
  }
},ignoreInit = TRUE)

##set system indicators drop down menu.  Updated when new indicators are selected
observeEvent(c(RSF_INDICATORS(),
               input$server_admin_indicators__search), {
  
  all_indicators <- RSF_INDICATORS()
  
  if (!isTruthy(all_indicators)) return (NULL)
  
  selected_id <- abs(as.numeric(LOAD_SYSTEM_INDICATOR()))
  all_indicators <- all_indicators[order(indicator_name)]
  keywords <- as.character(input$server_admin_indicators__search)
  
  if (!isTruthy(selected_id)) selected_id <- ""
  if (!selected_id %in% all_indicators$indicator_id) selected_id <- ""
  
  choices <- ""
  
  if (isTruthy(keywords)) {
    keywords <- unlist(strsplit(keywords,"[^[:alpha:]]+"))
    labels <- rbindlist(all_indicators$labels)
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
                       by=.(indicator_id)]
      
      setorder(labels,
               match_priority,
               indicator_id)
      
      selected_id <- labels$indicator_id[1]
      
      labels <- all_indicators[labels,
                     on=.(indicator_id),
                     nomatch=NULL][order(match_priority),
                                   .(indicator_id,
                                     indicator_name)]
      
      choices <- setNames(labels$indicator_id,labels$indicator_name)
      
    
    } else {
      choices <- c(`No matching indicators found`="")
      selected_id <- ""
    }
  } else {
    choices <- setNames(all_indicators$indicator_id,all_indicators$indicator_name)
  }
  
  updateSelectizeInput(session=session,
                       inputId="admin_system_selected_indicator",
                       choices=choices,
                       selected=selected_id)

}
, ignoreInit = FALSE, ignoreNULL=FALSE, priority=100) #Refresh on init


observeEvent(input$server_admin_indicators__clear, {
  
    updateTextInput(session=session,
                    inputId="server_admin_indicators__search",
                    value="")
  
},ignoreInit=TRUE)

observeEvent(input$server_admin_indicators__create_indicator, {
  
  if (LOGGEDIN()==FALSE) return (as.numeric(NA))
  
  if (!isTruthy(input$server_admin_indicators__create_indicator)) return (as.numeric(NA))
  
  show_modal_spinner(
    spin = "circle",
    color = "blue",
    text = "Loading...",
    session = shiny::getDefaultReactiveDomain()
  )
  
  new_indicator <- DBPOOL %>% db_indicator_create()
  if (is.null(new_indicator)) { #will return NULL if empty
    removeModal()
    showNotification(type="error",
                     h3("Failed to create a new indicator"))
    return (as.numeric(NA))
  }
  #print(paste0("NEW INDICATOR=",new_indicator$indicator_id))
  #return (new_indicator$indicator_id)
  LOAD_SYSTEM_INDICATOR(new_indicator$indicator_id)
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$server_admin_indicators__delete_prompt, {
  
  selected_indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  deletable <- DBPOOL %>% dbGetQuery("select
                                      not exists(
                                        select *
                                        from p_rsf.rsf_data rd
                                        inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
                                        inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                        where rd.indicator_id = $1::int
                                        	and ids.created_by_reporting_cohort_id <> rd.reporting_cohort_id)",
                                     params=list(selected_indicator$indicator_id))
  deletable <- unlist(deletable)
  
  if (deletable==TRUE) {
    SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID(selected_indicator$indicator_id)
    shinyjs::runjs(paste0("Shiny.setInputValue(\"server_admin_indicators__delete_action\",",selected_indicator$indicator_id,",{priority:\"event\"})"))
  } else {
    
    SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID(selected_indicator$indicator_id)  
    ddata <- DBPOOL %>% dbGetQuery("
      with dcounts as materialized (
        select
        	rc.reporting_rsf_pfcbl_id as rsf_pfcbl_id,
        	rc.rsf_program_id,
        	count(distinct data_id) filter (where rc.is_calculated_cohort=true) as calculated_data_count,
        	count(distinct data_id) filter (where rc.is_reported_cohort=true) as reported_data_count
        from p_rsf.rsf_data rd
        inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
        where rd.indicator_id = $1::int
        group by rc.reporting_rsf_pfcbl_id,rc.rsf_program_id
      )
      select nids.rsf_full_name,calculated_data_count,reported_data_count
      from dcounts dc
      inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = dc.rsf_pfcbl_id
      order by dc.rsf_program_id,nids.rsf_full_name",
      params=list(selected_indicator$indicator_id))
    
    setDT(ddata)
    dp <- sum(ddata$calculated_data_count,ddata$reported_data_count)
    
    delete_confirm <- modalDialog(title="Confirm Delete Indicator?",
                                  footer=div(style="display:flex;flex-flow:row nowrap;justify-content: space-between;",
                                             modalButton("Cancel: No actions"),
                                             actionButton(inputId="server_admin_indicators__delete_confirm",
                                                                  label=paste0("PERMANENTLY DELETE indicator and all ",dp," data points"),
                                                                  class="btn-danger")),
                                          div(style="background-color:white;padding:2px;height:400px;",
                                              p("WARNING: This indicator has reported data:"),
                                              p(paste0(sum(ddata$calculated_data_count)," data calculated by the system")),
                                              p(paste0(sum(ddata$reported_data_count)," data calculated by the system")),
                                              p("This action will DELETE this indicator and ALL data from the system and ALL ",
                                                "formulas (if this is a calculated indicator)."),
                                              p("This action CANNOT BE UNDONE: Proceed only with absolute certainty"),
                                              p("Enter password for account '",USER_ACCOUNT$user_login,"' to confirm delete action:"),
                                              passwordInput(inputId="server_admin_indicators__delete_pwconfirm",
                                                            label="Account Password",
                                                            value="",
                                                            width="100%")))
    showModal(delete_confirm)
    
    
  }
  
})

observeEvent(input$server_admin_indicators__delete_confirm, {
  if (isTruthy(input$server_admin_indicators__delete_confirm)) {
    did <- SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID()  
    pwconfirm <- input$server_admin_indicators__delete_pwconfirm
    
    result <- db_user_login(pool=DBPOOL_APPLICATIONS,
                            application_hashid=RSF_MANAGEMENT_APPLICATION_ID,
                            username=USER_ACCOUNT$user_login,
                            password=pwconfirm)

    if (is.null(result)) {
      showNotification(type="error",
                       h3("Delete failed: Login credentials could not be validated"))
    } else {
      shinyjs::runjs(paste0("Shiny.setInputValue(\"server_admin_indicators__delete_action\",",did,",{priority:\"event\"})"))
    }
  }
},ignoreInit = TRUE)

observeEvent(input$server_admin_indicators__delete_action, {


  confirm_id <- as.numeric(input$server_admin_indicators__delete_action)
  delete_id <- SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID()
  indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  
  if (!isTruthy(confirm_id)) return(NULL)
  if (!isTruthy(SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID())) return (NULL)
  if (empty(indicator)) return (NULL)
  if (!(indicator$indicator_id == confirm_id && confirm_id == delete_id)) {
    return(showNotification(type="error",
                            h3("Selected indicator does not match deleted indicator: delete failed")))
  }

  deleted <- withProgress({
    incProgress(amount=0.5)
    deleted <- DBPOOL %>% db_indicator_delete(indicator_id=indicator$indicator_id)
    
    if (deleted) {
      LOAD_SYSTEM_INDICATOR(-1 * indicator$indicator_id) #Negative unload
    }
    deleted
  },message="Deleting indicator...")

  if (!deleted) showNotification(type="error","Delete failed. This indicator has active data and can only be removed after data is deleted.")
  SERVER_ADMIN_INDICATORS_DELETE_INDICATOR_ID(as.numeric(NA))
})

observeEvent(input$server_admin_indicators__save_indicator, {

  indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  
  if (!isTruthy(indicator)) return (NULL)
  all_options <- SYSTEM_ALL_OPTIONS()
  labels <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS()
  lables <- unique(labels[,.(label_id,
                             label_key,
                             is_primary,
                             label)])
  
  if (!any(toupper(labels$label_key)=="EN")) {
    showNotification(h3("Save failed: An English language label is required."),type="error")
    return(NULL)
  } 
  if (!isTruthy(labels[label_key=="EN" & is_primary==TRUE,label])) {
    showNotification(h3("Save failed: An English language Primary Label is required."),type="error")
    return(NULL)
  }

  labels[is.na(label),label:=""]
  labels[is.na(label_key),label_key:=""]
  labels[,
         action:=fcase(any(is_primary==TRUE & label==""),"delete",
                       default="save"),
         by=.(label_id,
              label_key)]
  
  if (empty(labels[action=="save" & label_key=="EN"])) {
    showNotification(h3("Save failed: An English language Primary Label is required."),type="error")
    return(NULL)
  }
  labels <- labels[action=="save"]
  labels[,action:=NULL]

  formulas <- SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()
  if (!empty(formulas)) {
    formulas <- formulas[edited==TRUE]
    
    if (anyNA(formulas$formula)) {
      return(showNotification(type="error",
                              ui=h3("Formula cannot be left blank.  Delete the formula instead.")))
    }
  }
  
  indicator_name <- input$admin_system_edit_indicator_name
  data_category <- tolower(input$admin_system_edit_indicator_category)
  data_type <- tolower(input$admin_system_edit_indicator_type)
  data_unit <- toupper(input$admin_system_edit_indicator_type_unit)
  data_frequency <- as.logical(input$admin_system_edit_indicator_data_frequency)
  default_value <- input$admin_system_edit_indicator_default
  definition <- input$admin_system_edit_indicator_definition
  default_subscription <- as.logical(input$admin_system_edit_indicator_default_subscribed)
  
  indicator_name <- normalizeIndicatorName(x=indicator_name,
                                           category=data_category,
                                           data_unit=data_unit,
                                           is_system=indicator$is_system,
                                           data_type=data_type)
  
  if (!isTruthy(data_unit)) data_unit <- as.character(NA)
  if (!isTruthy(data_frequency)) data_frequency <- FALSE
  if (isTruthy(default_value)) default_value <- paste0(default_value,collapse=" & ") #For multi-select options defaults, when multiple are selected
  else default_value <- as.character(NA)
  
  options_id <- as.numeric(input$admin_system_edit_indicator_options)
  options_allows_blanks <- as.logical(input$group_allows_blanks)
  options_allows_multi <- as.logical(input$group_allows_multi)
  
  if (!isTruthy(indicator_name)) return(showNotification(h3("Save failed: Indicator name is required."),type="error"))
  if (!isTruthy(data_category)) return(showNotification(h3("Save failed: Category is required."),type="error"))
  if (!isTruthy(data_type)) return(showNotification(h3("Save failed: Data type is required."),type="error"))
  if (!data_category %in% c("global","program","facility","client","borrower","loan")) return(showNotification(h3("Save failed: Valid data category is required."),type="error"))
  if (!data_type %in% c("number","currency","currency_ratio","percent","date","text","logical")) return(showNotification(h3("Save failed: Valid data type is required."),type="error"))
  if (!isTruthy(definition)) definition <-  "Definition undefined"
  if (is.na(default_subscription)) default_subscription <- TRUE
  
  if (!identical(indicator$data_category,data_category) |
      !identical(indicator$data_type,data_type) |
      !identical(indicator$data_unit,data_unit)) {
    
    editable <- DBPOOL %>% dbGetQuery("select NOT exists(select * from p_rsf.rsf_data rd where rd.indicator_id = $1::int)
                                          and NOT exists(select * from p_rsf.indicators ind where ind.indicator_id = $1::int and is_system=true)",
                                      params=list(indicator$indicator_id))
    if (!any(unlist(editable))) {
      return(showNotification(type="error",
                              ui=h3("Indicators with saved data points cannot change data category, type or unit")))
    }
  }
  
  if (!isTruthy(options_id) || !options_id %in% all_options$options_group_id) {
    options_id <- NA
    options_allows_blanks <- NA
    options_allows_multi <- NA
  }
  
  success <- withProgress(message="Saving changes...",value=0.3, {
    
    
    success <- tryCatch({
      DBPOOL %>% db_indicator_update(indicator=data.table(indicator_id=indicator$indicator_id,
                                                          indicator_name=indicator_name,
                                                          data_category=data_category,
                                                          data_type=data_type,
                                                          data_unit=data_unit,
                                                          data_frequency=data_frequency,
                                                          default_value=default_value,
                                                          default_subscription=default_subscription,
                                                          options_group_id=options_id,
                                                          options_group_allows_blanks=options_allows_blanks,
                                                          options_group_allows_multiples=options_allows_multi,
                                                          definition=definition),
                                         labels=labels[,
                                                       .(label_id,
                                                          label_key,
                                                          is_primary,
                                                          label)],
                                         formulas=formulas[,
                                                           .(indicator_id,
                                                             formula_id,
                                                             formula,
                                                             formula_sort,
                                                             formula_overwrite,
                                                             formula_fx_date,
                                                             formula_title,
                                                             formula_notes,
                                                             formula_unit_set_by_indicator_name,
                                                             is_primary_default)])
      TRUE
    },
    warning = function(w) {
      showNotification(type="warning",
                       ui=h3("Failed to update indicator: ",conditionMessage(w)),
                       duration=NULL)
      FALSE
    },
    error = function(e) {
      showNotification(type="warning",
                       ui=h3("Failed to update indicator: ",conditionMessage(e)),
                       duration=NULL)
      FALSE
    })
    
    if (!success) return (FALSE)
    
    incProgress(amount=0.7,message="Updating")
      
    #already loaded, so reload to refresh the global indicators from the database
    if (indicator$indicator_id %in% LOAD_SYSTEM_INDICATOR()) {
      LOAD_SYSTEM_INDICATOR(-indicator$indicator_id)
    } else {
      LOAD_SYSTEM_INDICATOR(indicator$indicator_id)
    }
      
    monitoring <- DBPOOL %>% dbGetQuery("select exists(select * from p_rsf.rsf_program_facility_indicators pfi
                                                       where pfi.indicator_id = $1::int)::bool as monitored",
                                        params=list(indicator$indicator_id))
    monitoring <- unlist(monitoring)
    if (!any(monitoring)) showNotification(type="warning",
                                           h3("Attention: Indicator is not monitored.  Click program name(s) below to manage status: ORANGE=Unmonitored, GREEN=Monitored"))

    incProgress(amount=1.0,message="Completed")
    return (TRUE)
  })  
  
  if (success) {
    
    pending <- DBPOOL %>% dbGetQuery("
      select count(*)::int as pending
      from p_rsf.rsf_data_calculation_evaluations dce
      where dce.indicator_id = $1::int",
      params=list(indicator$indicator_id))
    
    pending <- pending$pending
    if (!isTruthy(pending)) pending <- 0
    SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING(pending)
  # SERVER_ADMIN_INDICATORS_DO_RECALCULATE(rsf_pfcbl_ids=NA, #all
  #                                        indicator_id=indicator$indicator_id)
  }
})

observeEvent(input$server_admin_indicators__add_label, {
  
  if (!isTruthy(SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR())) return (NULL)
  
  labels <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS()
  label_keys <- SYSTEM_ALL_LABEL_KEYS()
  next_label_key <- label_keys$label_key[!label_keys[key_type %in% c("language","ifc"),label_key] %in% unique(labels$label_key)][1]

  new_label <- labels[,.(label_key=next_label_key,
                         label="",
                         key_type=NA,            #don't care what this is as this is temporary, so just NA
                         key_type_template_id=NA,
                         is_primary=TRUE,
                         label_rank=1,
                         module_id=paste0("IND",indicator_id,"_LAB",length(unique(module_id))+1)),
                      by=.(indicator_id,label_id)]
  
  setcolorder(new_label,
              neworder=names(labels))
  
  id <- new_label$module_id
  ui <- module_ui_system_indicator_label(id=ns(id),
                                         label=new_label,
                                         keys_list=SYSTEM_ALL_LABEL_KEYS_LIST())

  all_labels <- rbindlist(list(labels,new_label))
  SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS(all_labels)
  module_session_system_indicator_label(id=ns(id),
                                        module_id=id,
                                        INDICATOR_LABELS=SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS,
                                        SERVER_ADMIN_INDICATORS.MODULES=SERVER_ADMIN_INDICATORS.MODULES)
  
  insertUI(selector = "#location_admin_system_edit_indicator_labels",
           where="beforeEnd",
           ui=ui,
           immediate=TRUE)
})

observeEvent(input$server_admin_indicators__add_formula, {

  selected_indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  if (!isTruthy(selected_indicator)) return (NULL)
 #print(paste0("New formula click #",as.numeric(input$server_admin_indicators__add_formula)))
  
  new_formula <- data.table(formula_overwrite="allow",
                            indicator_id=selected_indicator$indicator_id,
                            formula=as.character(NA),
                            formula_sort=as.character(NA),
                            formula_fx_date="calculation",
                            formula_unit_set_by_indicator_name=as.character(NA),
                            formula_id=(-1 * as.numeric(input$server_admin_indicators__add_formula)), #set when saved, pseudo for now.
                            formula_title=as.character(NA),
                            is_primary_default=nrow(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS())==0,
                            formula_notes=as.character(NA),
                            is_system=selected_indicator$is_system,
                            indicator_name=selected_indicator$indicator_name,
                            is_applied=FALSE)
  
  new_formula[,module_id:=paste0("SERVER_ADMIN_INDICATORS__formula",formula_id)]
  new_formula[,edited:=FALSE]
  setcolorder(new_formula,
              neworder = names(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS()))
  
  formulas <- rbindlist(list(SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(),
                             new_formula))
  
  start.collapsed <- nrow(formulas) > 1
  
  SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS(formulas)
  
  id <- new_formula$module_id
  ui <- server_admin_indicator_formulas.module_ui_indicator_formula(id=ns(id),
                                                                    formula=as.list(new_formula),
                                                                    rsf_indicators=RSF_INDICATORS(),
                                                                    start.collapsed=start.collapsed)
  
  server_admin_indicator_formulas.module_session_indicator_formula(id=ns(id),
                                                                   module_id=id,
                                                                   pool=DBPOOL,
                                                                   SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS=SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS,
                                                                   SERVER_ADMIN_INDICATORS.MODULES=SERVER_ADMIN_INDICATORS.MODULES)
  
  insertUI(selector="#server_admin_indicators__formulas",
           where="afterBegin",
           ui=ui)
    
}, ignoreInit=TRUE)

output$admin_system_display_indicator_html_name <- renderUI({
  
  all_options <- isolate({ SYSTEM_ALL_OPTIONS() })
  indicator_name <- input$admin_system_edit_indicator_name
  data_category <- input$admin_system_edit_indicator_category
  data_type <- input$admin_system_edit_indicator_type
  data_unit <- input$admin_system_edit_indicator_type_unit
  
  if (!isTruthy(data_unit)) data_unit <- NA
  
  is_calculated <- toupper(as.character(input$admin_system_edit_indicator_calculated))
  if (isTruthy(is_calculated) && is_calculated %in% c("TRUE")) is_calculated <- TRUE
  else is_calculated <- FALSE

  options_id <- as.numeric(input$admin_system_edit_indicator_options)

  if (!isTruthy(options_id)) options_group_name <- NA
  else options_group_name <- all_options[options_group_id==options_id,options_group_name]
  
  if (length(options_group_name)==0) options_group_name <- NA

  indicator_name <- normalizeIndicatorName(x=indicator_name,
                                           category=data_category,
                                           data_unit=data_unit,
                                           data_type=data_type)
  
  html <- format_html_indicator(indicator_name=indicator_name,
                                data_category=data_category,
                                data_type=data_type,
                                is_system=FALSE,
                                is_calculated=is_calculated,
                                options_group_name=options_group_name,
                                is_subscribed = TRUE)
  return (HTML(html))
})

output$admin_system_edit_indicator_options_UI <- renderUI({
  ui <- NULL
  
  indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  all_options <- SYSTEM_ALL_OPTIONS()
  selected_option <- as.numeric(input$admin_system_edit_indicator_options) #Reactive on selecting a options group or not to draw appropriate form field
  
  if (!isTruthy(indicator)) return (tagList())
  if (!isTruthy(selected_option) || !selected_option %in% all_options$options_group_id) selected_option <- -1 #Selected "None"
  
  if (selected_option==-1) {
    ui <- fluidRow(column(12,""))
  } else {
    blanks_label = tags$label("Allows Blanks",style='margin:0px;font-weight:bold;white-space:nowrap;',tags$i(class='fas fa-question icon-question',title="When NOT checked, system will enforce input data to be a valid options choice and disallow blanks as a valid input and use the indicator default value instead of blank.  When checked, blank inputs will be ignored."))
    multi_label = tags$label("Multi-Select",style='margin:0px;font-weight:bold;white-space:nowrap;',tags$i(class='fas fa-question icon-question',title="When checked, input data can be a list of comma-delimited options to indicate multiple options simultaneously for the input value. Multiple-select can ONLY be selected for TEXT-type options. It is recommended to disallow multiple selections."))
    
    allows_blanks <- as.logical(indicator$indicator_options_group_allows_blanks)
    allows_multi <- as.logical(indicator$indicator_options_group_allows_multiples)
    
    if (!isTruthy(allows_blanks)) allows_blanks <- FALSE
    if (!isTruthy(allows_multi)) allows_multi <- FALSE
    
    ui <- fluidRow(column(6,align="center",blanks_label,checkboxInput(inputId="group_allows_blanks",width="25px",value = allows_blanks, label=NULL)),
                   column(6,align="center",multi_label,checkboxInput(inputId="group_allows_multi", width="25px",value = allows_multi, label=NULL)))
  }
  return (ui)
})

output$admin_system_edit_indicator_default_UI <- renderUI({
  
  indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  
  all_options <- SYSTEM_ALL_OPTIONS() #Reactive on new options being updated (although this panel shouldn't be visible at that time)
  selected_option <- as.numeric(input$admin_system_edit_indicator_options) #Reactive on selecting a options group or not to draw appropriate form field
  
  if (!isTruthy(indicator)) return (tagList())
  if (!isTruthy(selected_option) || !selected_option %in% all_options$options_group_id) selected_option <- -1 #Selected "None"
  
  default_label <- paste0("When uploaded data fields are blank or missing, the default value will be used instead. Defaults may cause flags and checks to go unreported, or register an incorrect change to a previously reported reported value. Use only if certain, else leave default blank.")
  default_value <- trimws(unlist(strsplit(indicator$default_value,"&")),whitespace="[ \\t\\r\\n\\v\\h\\s]")
  none <- "{ No default value }"
  
  ui <- NULL
  
  #options group UI
  if (selected_option==-1) {
    if (!isTruthy(default_value)) default_value <- character(0)
    label = tags$label("Default Value",style='margin:0px;',tags$i(class='fas fa-question icon-question',title=default_label))
    
    ui <- textInput(inputId="admin_system_edit_indicator_default",label=label,value=default_value,placeholder=none)
    
  } 
  else {
    label = tags$label("Default Selection",style='margin:0px;',tags$i(class='fas fa-question icon-question',title=default_label))
    
    option_name <- all_options[options_group_id==selected_option,options_group_name]
    option_values <- unique(all_options[options_group_id==selected_option,labels[[1]][label_key=='EN',.(options_group_key,primary_label)]])
    
    option_values <- rbindlist(list(data.table(options_group_key=NA,primary_label=none),option_values))
    
    allows_multi <- as.logical(indicator$indicator_options_group_allows_multiples)
    
    if (!isTruthy(allows_multi)) allows_multi <- FALSE
    if (!isTruthy(default_value)) default_value <- none
    
    options_choices <- setNames(as.character(option_values$options_group_key),as.character(option_values$primary_label))
    
    ui <- selectizeInput(inputId="admin_system_edit_indicator_default",
                         label = label,
                         choices = options_choices,
                         multiple = allows_multi,
                         selected = default_value,
                         options = list(placeholder=none))
  } 
})

observeEvent(input$indicator_admin_program_subscription, {
  
  withProgress(value=0.5,message = "Updating Program Indicator Monitoring...", {
    indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
    subscribe_program_id <- as.numeric(input$indicator_admin_program_subscription)
    
    if (!subscribe_program_id %in% USER_PROGRAMS()$rsf_program_id) {
      showNotification(type="error",
                       ui=h3("An error has occurred: Unable to locate subscription for this program and indicator"))
      return (NULL)
    }
    rsf_pfcbl_id <- DBPOOL %>% dbGetQuery("
      select ids.rsf_pfcbl_id
      from p_rsf.rsf_pfcbl_ids ids
      where ids.rsf_program_id = $1::int
        and ids.pfcbl_category in ('global','program')
    ",params=list(subscribe_program_id))
    
    subscription_status <- DBPOOL %>% db_program_toggle_indicator_subscription(rsf_program_id = subscribe_program_id,
                                                                               rsf_pfcbl_id = rsf_pfcbl_id$rsf_pfcbl_id,
                                                                               indicator_id = indicator$indicator_id)
    
    if (subscription_status==TRUE) {
      shinyjs::removeClass(selector=paste0("#indicator_admin_program_subscription_",subscribe_program_id),
                           class="btn-warning")
      
      shinyjs::addClass(selector=paste0("#indicator_admin_program_subscription_",subscribe_program_id),
                        class="btn-success")
    } else {
      shinyjs::removeClass(selector=paste0("#indicator_admin_program_subscription_",subscribe_program_id),
                           class="btn-success")
      
      shinyjs::addClass(selector=paste0("#indicator_admin_program_subscription_",subscribe_program_id),
                        class="btn-warning")
    }
  })
})

observeEvent(input$admin_system_edit_indicator_type, {
  
  indicator <- SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR()
  all_options <- SYSTEM_ALL_OPTIONS() #Reactive on updates to OPTIONS
  
  if (!isTruthy(indicator)) return (NULL)
  
  data_type <- tolower(input$admin_system_edit_indicator_type)
  
  all_options <- all_options[tolower(options_group_data_type)==tolower(data_type),.(options_group_id,options_group_name)]
  all_options <- rbind(data.frame(options_group_id="",options_group_name="{ Not applicable }"),all_options)
  
  selected_option <- indicator$options_group_name
  
  if (!isTruthy(selected_option) || !selected_option %in% all_options$options_group_name) selected_option <- ""
  else selected_option <- as.character(all_options$options_group_id[all_options$options_group_name==selected_option])
  
  options_choices <- setNames(as.character(all_options$options_group_id),all_options$options_group_name)
  
  updateSelectizeInput(session=session,
                       inputId="admin_system_edit_indicator_options",
                       choices=options_choices,
                       selected=selected_option)
  
  selected_unit <- toupper(indicator$data_unit)
  if (!isTruthy(selected_unit)) selected_unit <- ""
  
  unit_options <- indicator_type_unit_choices(data_type,
                                              selected_unit)
  
  
  updateSelectizeInput(session=session,
                       inputId="admin_system_edit_indicator_type_unit",
                       choices=unit_options,
                       selected=selected_unit)
  
  
  #enable(id="admin_system_edit_indicator_calculated")
  
  removeModal()
}, ignoreInit=FALSE, priority = -1)

observeEvent(input$server_admin_indicators__recalculate_pending, {
  indicator <- req(SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR())
  
  tryCatch({
    SERVER_ADMIN_INDICATORS_DO_RECALCULATE(indicator_id=indicator$indicator_id)  
    SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING(0)
  },
  warning=function(w) { showNotification(type="warning",
                                         duration=NULL,
                                         ui=h3(conditionMessage(w)))
  },
  error=function(e) {
    showNotification(type="error",
                     duration=NULL,
                     ui=h3(conditionMessage(e)))
  })
  
  
})

observeEvent(SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING(), {
  
  pending <- SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING()
  if (!isTruthy(pending)) pending <- 0
  if (pending > 0) showElement(id="server_admin_indicators__calculations_pending")
  else hideElement(id="server_admin_indicators__calculations_pending")
})

output$server_admin_indicators__recalculate_count <- renderText({
  paste0("Recalculate ",SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING()," Pending")
})

output$admin_system_edit_indicator <- renderUI({
  
  indicator <- req(SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR())
  formulas <- isolate({ SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS() })    #not reactive: don't want to refresh when modules make updates or add new.
  labels <- isolate({ SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS() }) #not reactive: don't want to refresh when modules make updates or add new.
  pending <- isolate({ SERVER_ADMIN_INDICATORS_CALCULATIONS_PENDING() })
  
  if (!isTruthy(pending)) pending <- 0
  
  #print(paste0("admin_system_edit_indicator called: ",indicator$indicator_id))
  
  if (!isTruthy(indicator) || empty(indicator)) return (h2("Please select an indicator to edit"))
  
  #If we add/edit/delete labels, we don't want to redraw the UI.  Only redraw when selected system indicator changes.
  
  
  
  keys_list <- SYSTEM_ALL_LABEL_KEYS_LIST()
  
  data_points <- DBPOOL %>% dbGetQuery("select coalesce(count(distinct data_id),0)::int as indicator_data
                                        from p_rsf.rsf_data rd 
                                        where rd.indicator_id=$1::int",
                                       params=list(indicator$indicator_id))
  
  data_points <- unlist(data_points)
  if (length(data_points)==0) data_points <- 0
  editable <- data_points == 0
  

  indicator_name <- indicator$indicator_name
  indicator_name_label <- paste0("Indicator name may use only: upper and lower-case letters and numbers; and underscore punctiation. Names will be auto-formatted.")
  secondary_labels <- paste0("One label per line.",
                             "Enter alternate phrases, (mis)spellings or aliases associated with the primary label.",
                             "These include permutations found in template files, including misspellings and typos, which the system will recognize and associate with this indicator.",
                             "Deleting an entry could prevent the system from reading a template correctly. Modify with care.")
  
  data_type <- indicator$data_type
  
  selected_frequency <- as.logical(indicator$is_periodic_or_flow_reporting)
  if (!isTruthy(selected_frequency)) selected_frequency <- FALSE
  
  selected_monitoring <- as.logical(indicator$default_subscription)
  if (is.na(selected_monitoring)) selected_monitoring <- TRUE
  
  category_choices <- c(Global="global",
                        Program="program",
                        Facility="facility",
                        Client="client",
                        Borrower="borrower",
                        Loan="loan")
  
  type_choices <- c(Number="number",
                    Currency="currency",
                    `Currency Ratio`="currency_ratio",
                    Percent="percent",
                    Date="date",
                    Text="text",
                    `True/False`="logical")
  
  selected_category <- indicator$data_category
  selected_type <- indicator$data_type
  
  options_group_id <- indicator$options_group_id
  options_group_name <- indicator$options_group_name
  
  if (!isTruthy(options_group_id)) options_group_id <- -1
  if (!isTruthy(options_group_name)) options_group_name <- '{ Not applicable }'
  
  selected_options_group_id <- options_group_id
  
  selected_unit <- toupper(indicator$data_unit)
  if (!isTruthy(selected_unit)) selected_unit <- ""
  
  unit_choices <- indicator_type_unit_choices(indicator_type=data_type,
                                              current_unit=selected_unit)
  
  
  
  
  program_subscriptions <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_program_id,
      ids.rsf_pfcbl_id,
      nids.rsf_name as name,
      coalesce(pfi.is_subscribed,false) as is_subscribed
    from p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    left join p_rsf.rsf_program_facility_indicators pfi on pfi.rsf_pfcbl_id = ids.rsf_pfcbl_id
    																									 and pfi.indicator_id = $1::int
    where ids.pfcbl_category in ('global','program')",
    params=list(indicator$indicator_id))
  
  setDT(program_subscriptions)
  
  program_subscriptions[,class:="btn "]
  program_subscriptions[is_subscribed==TRUE,
                        class:=paste0(class,"btn-success")]
  
  program_subscriptions[is_subscribed==FALSE,
                        class:=paste0(class,"btn-warning")]
  
  if (selected_type=="global") {
    program_subscriptions <- program_subscriptions[rsf_program_id==0]
  } else {
    program_subscriptions <- program_subscriptions[rsf_program_id!=0]
  }
  
  ps<-split(program_subscriptions,
            by="rsf_program_id")
  
  ps <- lapply(ps,
               function(x) {
                 HTML(paste0('<button 
                      id="indicator_admin_program_subscription_',x$rsf_program_id,'" 
                      class="',paste0(x$class," action-button"),'" 
                      onclick="',paste0("Shiny.setInputValue('indicator_admin_program_subscription',",x$rsf_program_id,",{priority:'event'})"),'">',
                      x$name,"</button>"))
               })
  
  program_subscriptions <- tagList(fluidRow(column(12,div(tagList(ps)))))
  
  definition <- indicator$definition
  
  label_headers <- fluidRow(align="left",
                            column(2,style="width:150px;padding:0 3px 0 15px;",tags$label("For")),
                            column(4,style="width:375px;padding:0 3px 0 3px;",tags$label("Primary Label")),
                            column(4,style="width:375px;padding:0 3px 0 3px;",tags$label("Alternative Labels",style='margin:0px;',tags$i(class='fas fa-question icon-question',title=secondary_labels))),
                            column(2,""))
  ui_labels <- tagList()
  ui_labels[[length(ui_labels)+1]] <- label_headers
  
  label_keys <- unique(labels$label_key) #Defaults to EN when not available
  #browser()
  for (i in 1:length(label_keys)) {
    lkey <- label_keys[i]
    if (lkey=="SYS") next;
    
    ind_label <- labels[label_key==lkey] 
    
    id <- unique(ind_label$module_id)

    ui <- module_ui_system_indicator_label(id=ns(id),
                                           label=ind_label,
                                           keys_list=keys_list)
    
    ui_labels[[length(ui_labels)+1]] <- ui
    module_session_system_indicator_label(id=ns(id),
                                          module_id=id,
                                          INDICATOR_LABELS=SERVER_ADMIN_INDICATORS.SELECTED_SYSTEM_INDICATOR_LABELS,
                                          SERVER_ADMIN_INDICATORS.MODULES=SERVER_ADMIN_INDICATORS.MODULES)
  }
  
  ui_formulas <- tagList()
  if (!empty(formulas)) {
    for (i in 1:nrow(formulas)) {
      formula <- formulas[i]
      id <- formula$module_id
      ui <- server_admin_indicator_formulas.module_ui_indicator_formula(id=ns(id),
                                                                        formula=as.list(formula),
                                                                        rsf_indicators=RSF_INDICATORS())
      ui_formulas[[length(ui_formulas)+1]] <- ui
      
      server_admin_indicator_formulas.module_session_indicator_formula(id=ns(id),
                                                                       module_id=id,
                                                                       pool=DBPOOL,
                                                                       SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS=SERVER_ADMIN_INDICATORS.SELECTED_INDICATOR_FORMULAS,
                                                                       SERVER_ADMIN_INDICATORS.MODULES=SERVER_ADMIN_INDICATORS.MODULES)
    }
  }
  
  limited <- NULL
  if (!editable) limited <- fluidRow(style="padding-bottom:5px;",
                                     column(12,div(style='padding_left:15px;',
                                                   icon("exclamation-triangle",class="icon-orange"),
                                                   paste0("Indicator has ",data_points," data points saved. Editing options that affect existing data are disabled."))))
  
  recalculate_pending <- div(id="server_admin_indicators__calculations_pending",
                             actionButton(inputId="server_admin_indicators__recalculate_pending",
                                          label=textOutput(outputId="server_admin_indicators__recalculate_count",inline = TRUE),
                                          icon=icon("calculator"),
                                          class="btn btn-warning"))
  
  if (pending==0) recalculate_pending <- hidden(recalculate_pending)

  ui <- div(style='width:1000px;background-color:gainsboro;padding:10px;margin: 0 auto;',
            fluidRow(align="center",style="padding-top:5px;padding-bottom:5px;",column(12,uiOutput(outputId="admin_system_display_indicator_html_name"))),
            limited,
            fluidRow(column(12,textInput(inputId="admin_system_edit_indicator_name",
                                         width="100%",
                                         label = tags$label("Indicator Name",style='margin:0px;',tags$i(class='fas fa-question icon-question',title=indicator_name_label)),
                                         value=indicator_name))
            ),
            fluidRow(align="center",
                     column(3,align="left",
                            enabled(state=editable,
                                    selectizeInput(inputId="admin_system_edit_indicator_category",
                                                   label = "Data Category",
                                                   choices = category_choices,
                                                   selected = selected_category,
                                                   options = list(placeholder="Select...")))),
                     column(2,align="left",
                            enabled(state=editable,
                                    selectizeInput(inputId="admin_system_edit_indicator_type",
                                                   label = "Data Type",
                                                   choices = type_choices,
                                                   selected = selected_type,
                                                   options = list(placeholder="Select...")))),
                     column(3,align="left",
                            enabled(state=editable,
                                    selectizeInput(inputId="admin_system_edit_indicator_type_unit",
                                                   label = "Data Unit",
                                                   choices = unit_choices,
                                                   selected = selected_unit,
                                                   options = list(placeholder="{ Not applicable }",
                                                                  create=TRUE,
                                                                  persist=TRUE)))),
                     column(2,align="left",
                            enabled(state=editable,
                                    selectizeInput(inputId="admin_system_edit_indicator_data_frequency",
                                                   label = "Data Frequency",
                                                   choices = c('Normal'=FALSE,
                                                               'Periodic'=TRUE),
                                                   selected = selected_frequency,
                                                   options = list(placeholder="Normal")))),
                     column(2,align="left",
                              selectizeInput(inputId="admin_system_edit_indicator_default_subscribed",
                                             label="Monitoring",
                                             selected=selected_monitoring,
                                             choices=c("Auto"=TRUE,
                                                       "RSF Setup"=FALSE)))
                     ),
            fluidRow(align="left",
                     column(4,align="left",
                            enabled(state=editable,
                                    selectizeInput(inputId="admin_system_edit_indicator_options",
                                                   label = "Choices Group",
                                                   choices = setNames(options_group_id,options_group_name),
                                                   selected = selected_options_group_id,
                                                   options = list(placeholder="{ Not applicable }")))),
                     column(4,align="left",
                            uiOutput(outputId="admin_system_edit_indicator_default_UI")),
                     column(4,align="left",
                            uiOutput(outputId="admin_system_edit_indicator_options_UI"))
            ),
            fluidRow(align="left",style="padding-bottom:5px;",
                     column(12,
                            div(style="display:flex;flex-direction:row;",
                                div(actionButton(inputId="server_admin_indicators__add_formula",
                                                 label="Add Formula",
                                                 icon=icon("plus-square"),
                                                 class="btn btn-primary")),
                                div(id="server_admin_indicators__quasi_calculation"),
                                div(style="padding-left:20px;",recalculate_pending)),
                            
                            div(id="server_admin_indicators__formulas",style="padding-top:10px;",
                                ui_formulas))),
            fluidRow(align="left",
                     column(12,
                            textAreaInput(inputId="admin_system_edit_indicator_definition",rows="2",width="937px",label="Definition",value=definition))),
            fluidRow(align="left",
                     column(12,
                            div(actionButton(inputId="server_admin_indicators__add_label",label="Add Label",icon=icon("puzzle-piece"),class="btn-primary btn-sm")),
                            div(id="location_admin_system_edit_indicator_labels",ui_labels)
                     )
            ),
            fluidRow(align="left",
                     style='padding-bottom:5px;',
                     column(12,div(tags$label("Programs monitoring this Indicator (Click to subscribe/unsubscribe)"),
                                   program_subscriptions))
            ),
            #recalculate_ui,
            fluidRow(style='padding-top:10px;',div(style='border-top:solid gray 1px;margin-left:10px;margin-right:10px;',
                                                   column(6,align="left",style="padding-top:5px;",
                                                          actionButton(inputId="server_admin_indicators__delete_prompt",
                                                                               class="btn-danger",
                                                                               label="Delete",
                                                                               icon=icon("minus-circle"))
                                                   ),
                                                   column(6,align="right",style="padding-top:5px;",
                                                          actionButton(inputId="server_admin_indicators__save_indicator",class="btn-success",label="Save",icon=icon("save")))))
  )  
  
  return(ui)
})


output$admin_system_download_indicators <- downloadHandler(
  filename = function() { "RSF Indicators List.xlsx" },
  content = function(file) {
    
    
    indicators <- RSF_INDICATORS()
    
    if (!isTruthy(indicators)) return (showNotification(type="error",h2("Unable to download indicators while system is loading.  Try again shortly")))
    
    excelwb <- withProgress(message="Downloading indicators...",value=0.5,
                            {
                              
                              
                              indicator_dt <- indicators[,.(ID=indicator_id,
                                                            Name=indicator_name,
                                                            Category=data_category,
                                                            Unit=data_unit,
                                                            `Choice Group`=options_group_name,
                                                            `Group Allows Blanks`=indicator_options_group_allows_blanks,
                                                            `Group Allows Multiples`=indicator_options_group_allows_multiples,
                                                            `Default Value`=default_value,
                                                            definition,
                                                            is_system,
                                                            is_calculated,
                                                            formula,
                                                            formula_sort,
                                                            formula_overwrite
                              )]
                              labels_dt <- rbindlist(indicators$labels)
                              labels_dt <- labels_dt[,.(secondary_labels=paste0(secondary_labels,collapse="; ")),by=.(ID=indicator_id,label_key,primary_label)]
                              
                              labels_dt <- dcast.data.table(labels_dt,ID ~ label_key,value.var = c("primary_label","secondary_labels"))
                              
                              indicator_dt <- indicator_dt[labels_dt,on=.(ID)]
                              
                              excelwb <- openxlsx::createWorkbook()
                              addWorksheet(excelwb,sheetName="RSF Indicators")
                              writeDataTable(excelwb,sheet=1,x=indicator_dt)
                              setColWidths(excelwb,sheet=1,cols=c(1:14), widths=c(8,35,10,8,15,10,10,10,30,5,5,15,15,8))
                              setColWidths(excelwb,sheet=1,cols=c(15:ncol(indicator_dt)), widths=c(30))
                              
                              excelwb
                            })
    
    openxlsx::saveWorkbook(excelwb,file)
  },
  contentType = "application/xlsx")
