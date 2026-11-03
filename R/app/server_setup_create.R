
SERVER_SETUP_CREATE__COLLECTIONS_MEMBERSHIPS <- eventReactive(list(SELECTED_PROGRAM(),
                                                                   input$setup_program_create_collection_search), {
 selected_program <- SELECTED_PROGRAM()
 
 if (!LOGGEDIN()) return (NULL)
 if (empty(selected_program)) return (NULL)
 if (!isTRUE(as.logical(selected_program$is_collection))) return (NULL)

 
 search <- trimws(as.character(input$setup_program_create_collection_search))
 
 membership_listing <- DBPOOL %>% dbGetQuery("
     select 
      rp.rsf_program_id,
      ids.rsf_facility_id,
      rf.rsf_facility_id is not null as is_subscribed,
      ids.pfcbl_category,
      coalesce(nids.nickname,nids.name,'RSF' || ids.rsf_facility_id) as facility_name,
      nids.sys_name
    from p_rsf.rsf_pfcbl_ids ids
    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                     and fpg.permission_name = 'LIST'
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    inner join p_rsf.rsf_programs rp on rp.rsf_program_id = $1::int
                                    and rp.is_collection is true
    left join p_rsf.rsf_facilities rf on rf.rsf_facility_id = ids.rsf_pfcbl_id
                                     and rf.rsf_program_id = rp.rsf_program_id
    where ids.pfcbl_category = 'facility'
      and fpg.account_id = $2::text
    order by nids.sys_name,facility_name",
  params=list(selected_program$rsf_program_id,
              USER_ID()))
 
 setDT(membership_listing)
 
 if (length(search) && nchar(search) > 2) {
   membership_listing <- membership_listing[grepl(search,facility_name,ignore.case=T) |
                                            grepl(search,sys_name,ignore.case=T)]
 }
 
 editable <- DBPOOL %>% dbGetQuery("
  select exists(select true
                from users.view_rsf_pfcbl_id_family_permissions_granted ppg
                where ppg.rsf_pfcbl_id = $1::int
                   and ppg.account_id = $2::text
                   and ppg.permission_name = 'WRITE')::bool as editable",
 params=list(selected_program$rsf_program_id,
             USER_ID()))
 
 membership_listing[,
                    toggle_box:=paste0("<input type='checkbox' name='indicator_subscription_toggle_checkbox' ",
                                       "id='",paste0("setup_collections_",rsf_program_id,"-",rsf_facility_id),"' ",
                                       "value=",paste0("setup_collections_",rsf_program_id,"-",rsf_facility_id)," ",
                                       ifelse(is_subscribed==TRUE,"checked='checked' ",""),
                                       ifelse(editable$editable==FALSE,"disabled='disabled' ",""),
                                       "onmousedown='event.stopPropagation();' ",
                                       "onclick='Shiny.setInputValue(\"server_setup_collections__toggle_select\",",rsf_facility_id,",{priority:\"event\"})' />")] 
 
 membership_listing[,editable:=editable$editable]
 return (membership_listing)
 
}) %>% debounce(millis=500)

####################
###########OBSERVERS
####################

#Select new program: update dropdowns for appropriate program
observeEvent(SELECTED_PROGRAM(), {
  
  program <- SELECTED_PROGRAM()
  if (isTRUE(program$is_collection)) {
    showElement(id="setup_program_create_select_collection");
    hideElement(id="setup_program_create_select_what")
    updateTextInput(session=session,
                    inputId="setup_program_create_collection_search",
                    value="")
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_what",
                         selected="",
                         choices="",
                         options=list(placeholder="Include facilities in this portfolio below..."))
    
  } else {

    hideElement(id="setup_program_create_select_collection");
    showElement(id="setup_program_create_select_what")
    updateTextInput(session=session,
                    inputId="setup_program_create_collection_search",
                    value="")
    if (!isTruthy(program) || empty(program) || as.numeric(program$rsf_program_id) <=0) {
      updateSelectizeInput(session=session,
                           inputId="setup_program_create_what",
                           choices=c("",Program="program"),
                           selected="",
                           options=list(placeholder="Select..."))    
    } else {
      updateSelectizeInput(session=session,
                           inputId="setup_program_create_what",
                           choices=c("",
                                     Facility="facility",
                                     Client="client"
                                     ),
                           selected="",
                           options=list(placeholder="Select..."))
    }
  }  
  
}, ignoreNULL = FALSE)

observeEvent(input$setup_program_create_action_button, {
  
  what <- input$setup_program_create_what
  rsf_program_id <- SELECTED_PROGRAM_ID()
  facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
  client_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_client)
  #borrower_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_borrower)
  program_is_collection <- isTRUE(as.logical(input$setup_program_is_collection))
  
  reporting_frequency <- "quarter"
  if (!isTruthy(what)) return (NULL)
  if (what=="program" && isTruthy(as.numeric(rsf_program_id)>0)) {
    return(showNotification(type="error",
                            h3("Error: Selected RSF Program must be blank to create a new Program")))
  }
  if (what=="facility" && !isTruthy(rsf_program_id)) {
    return(showNotification(type="error",
                            h3("Error: RSF Program must be selected to create a new Facility")))
  }
  if (what=="client" && (!isTruthy(rsf_program_id) || !isTruthy(facility_rsf_pfcbl_id))) {
    return(showNotification(type="error",
                            h3("Error: Facility must be selected to create a new Client")))
  }

  setup_indicator_ids <- grep("^setup_program_create_setup_indicator_",names(reactiveValuesToList(input)),value=T)
  setup_indicators <- lapply(setup_indicator_ids,function(x) {
    val <- input[[x]]
    if (length(val) > 1) val <- paste0(val,collapse=" & ")
    if (is.null(val) || is.na(val) || length(val)==0 || nchar(val)==0) val <- as.character(NA) #not using isTruthy because it could submit FALSE
    else val <- as.character(val)
    
    data.table(setup_indicator=x,
               data_value=val)
  })
  setup_indicators <- rbindlist(setup_indicators)
 
  #si <<- as.data.frame(setup_indicators)
  #setup_indicators <- as.data.table(si)
  #what <<- what
  #rsf_program_id <<- rsf_program_id
  #facility_rsf_pfcbl_id <<- facility_rsf_pfcbl_id
  
  setup_indicators[,
                   indicator_id:=as.numeric(gsub("^setup_program_create_setup_indicator_","",setup_indicator))]
  
  indicator_requirements <- DBPOOL %>% db_create_entity_indicator_requirements(entity_type=what)
  
  setDT(indicator_requirements)
  setup_indicators <- setup_indicators[indicator_requirements,
                                       on=.(indicator_id),
                                       nomatch=NULL][,.(indicator_id,
                                                        indicator_name,
                                                        indicator_sys_category,
                                                        data_category,
                                                        data_value,
                                                        default_value,
                                                        is_setup)]
  setup_indicators[,data_value:=trimws(data_value)]
  setup_indicators[!is.na(default_value) & is.na(data_value),
                   data_value:=as.character(default_value)]
  
  
  reporting_asof_date <- as.character(setup_indicators[indicator_sys_category=="entity_creation_date",data_value])
  
  if (!isTruthy(reporting_asof_date) || is.na(suppressWarnings(ymd(reporting_asof_date)))) {
    return(showNotification(type="error",
                            h3("Error: Creation date must be completed")))
    
  }
  
  reporting_asof_date <- ymd(reporting_asof_date)
  if (what != "program") {

    reporting_frequency <- DBPOOL %>% dbGetQuery("select reporting_period from p_rsf.rsf_programs rp
                                                  where rp.rsf_program_id = $1::int",
                                                 rsf_program_id)
    
    reporting_frequency <- reporting_frequency$reporting_period
  } else {
    reporting_frequency <- "quarter" #system basically only enables quarterly reporting
    
  }
  
  reporting_asof_start <- floor_date(reporting_asof_date,reporting_frequency) #first date of current quarter
  reporting_asof_date <- ceiling_date(reporting_asof_date,reporting_frequency) - days(1) #last day of current quarter
  id <- toupper(setup_indicators[indicator_sys_category=="id",data_value])
  
  if (!isTruthy(id) && what != "program") {
    return(showNotification(type="error",
                            ui=h3("The ID number is required: For facillities, the IFC Project ID number.  For Clients, the IFC Partner ID number")))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_local_currency_unit" & data_value %in% c("LCU","LCY")])) {
    return(showNotification(type="error",
                            h3("Error: Local Currency Unit cannot be a generic 'LCU'")))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_completion_date"][ymd(data_value) < reporting_asof_start])) {
    return(showNotification(type="error",
                            h3(paste0("Completion date cannot be before ",reporting_asof_start))))
  }

  if (!empty(setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) < reporting_asof_start])) {
    bad_names <- setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) < reporting_asof_start,
                                                                                  indicator_name]
    return(showNotification(type="error",
                            h3(paste0("Creation date ",(paste0(bad_names,collapse=" & "))," cannot be before start of reporting period ",
                                      as.character(reporting_asof_start)))))
  }
  
  if (!empty(setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) > reporting_asof_date])) {
    bad_names <- setup_indicators[indicator_sys_category=="entity_creation_date"][ymd(data_value) > reporting_asof_date,
                                                                                  indicator_name]
    return(showNotification(type="error",
                            h3(paste0("Creation date ",(paste0(bad_names,collapse=" & "))," cannot be in the future from reporting date ",
                                      as.character(reporting_asof_date)))))
  }
  
  
  if (anyNA(setup_indicators[is_setup=="required",data_value])) {
    missing_values <- setup_indicators[is_setup=="required" & is.na(data_value),indicator_name]
    missing_values <- paste0(missing_values,collapse=", ")
    showNotification(type="error",
                     duration=NULL,
                     div(h3("The following fields are required: "),
                         p(missing_values)))
    return (NULL)
  }
  setup_indicators <- setup_indicators[is.na(data_value)==FALSE]
  nickname <- toupper(setup_indicators[indicator_sys_category=="nickname",data_value])
  
  if (!isTruthy(nickname)) nickname <- paste0(what,"_",trimws(toupper(setup_indicators[indicator_sys_category=="name",data_value])))
  if (!isTruthy(nickname)) nickname <- paste0("NEW_",what)
  
  export_filename <- paste0("#0 ",nickname," (",id,") create_RSF_",toupper(what),".xlsx")
  if (!isTruthy(id)) export_filename <- paste0("#0 ",nickname," create_RSF_",toupper(what),".xlsx")

  rsf_indicators <- DBPOOL %>% db_indicators_get_labels()
  tryCatch({
    withProgress(value=.10,message="Saving datasets...", {
      excelwb <- NULL
      if (what=="program") {

        if (length(setdiff(setup_indicators$indicator_sys_category,
                           c("entity_creation_date",
                             "name",
                             "nickname",
                             "id",
                             "entity_local_currency_unit"))) > 0) {
          stop("Create program only recognizes fields: entity_creation_date, name, nickname, entity_local_currency_unit, id")
        }

        results <- DBPOOL %>% db_program_create(rsf_indicators=rsf_indicators,
                                                program_name=setup_indicators[indicator_sys_category=="name",data_value],
                                                program_nickname=setup_indicators[indicator_sys_category=="nickname",data_value],
                                                program_inception_date=ymd(setup_indicators[indicator_sys_category=="entity_creation_date",data_value]),
                                                program_lcu=setup_indicators[indicator_sys_category=="entity_local_currency_unit",data_value],
                                                program_ifc_project_id=setup_indicators[indicator_sys_category=="id",data_value],
                                                program_is_collection=program_is_collection,
                                                reporting_user_id=USER_ID(),
                                                program_reporting_frequency="quarter",
                                                source_name="Program Created in JASON")
        
        USER_PROGRAMS_REFRESH(USER_PROGRAMS_REFRESH()+1)
        DO_SELECT_PROGRAM_ID(as.numeric(results$rsf_program_id))

        updateTabsetPanel(session=session,
                          inputId="tabset_setup_program",
                          selected = "Setup Indicators")
        
      
      } else {
        
        parent_rsf_pfcbl_id <- NULL
        if (what=="facility") {
          parent_rsf_pfcbl_id <- SELECTED_PROGRAM()$rsf_pfcbl_id
        } else if (what=="client") {
          parent_rsf_pfcbl_id <- facility_rsf_pfcbl_id
        }
        #else if (what=="borrower") parent_rsf_pfcbl_id <- client_rsf_pfcbl_id
        #else if (what=="loan") parent_rsf_pfcbl_id <- borrower_rsf_pfcbl_id

        excelwb <- DBPOOL %>% export_create_entity_to_excel(parent_rsf_pfcbl_id=parent_rsf_pfcbl_id,
                                                            reporting_asof_date=reporting_asof_date,
                                                            entity_data=setup_indicators,
                                                            exporting_user_id=USER_ID())
          
        saveWorkbook(excelwb,
                     file=export_filename,
                     overwrite=TRUE)
        
        incProgress(amount=0.40,message=paste0("Creating new ",what,"..."))
        
        progress_status_message <- function(class,...) {
          dots <- list(...)
          dots <- paste0(unlist(dots),collapse=" ")
          incProgress(amount=0,
                      message=paste0("Uploading data: ",dots))
        }
 
        results <- tryCatch({
          #Template gets its own database pool
          localPool <- dbStart(database=DATABASE_LOCATION)
          ppu <- template_parse_process_and_upload(pool=localPool,
                                            reporting_user_id = USER_ID(),
                                            template_files=export_filename,
                                            source_note="None",
                                            delete_on_error=FALSE,
                                            status_message=progress_status_message,
                                            CALCULATIONS_ENVIRONMENT=CALCULATIONS_ENVIRONMENT)
          poolClose(localPool)
          localPool <- NULL
          ppu
        },
        error=function(e) {
          showNotification(type="error",
                           duration=NULL,
                           ui=h3(paste0("An error has occcurred. Failed to create new ",what,":  ",conditionMessage(e),
                                        "Please review the datasets panel and download the dataset, if available; then delete it.")))
          return(NULL)
          
        },
        warning=function(w) {
          showNotification(type="error",
                           duration=NULL,
                           ui=h3(paste0("An error has occcurred. Failed to create new ",what,":  ",conditionMessage(w),
                                        "Please review the datasets panel and download the dataset, if available; then delete it.")))
          return(NULL)
          
        })
        
        file.remove(export_filename)

        #this utility can only create one at a time, so select last-created by this parent
          created_rsf_pfcbl_ids <- DBPOOL %>% dbGetQuery("
            select 
              ids.rsf_pfcbl_id,
              ids.rsf_program_id,
              ids.rsf_facility_id
            from p_rsf.reporting_cohorts rc
            inner join p_rsf.rsf_pfcbl_ids ids on ids.created_by_reporting_cohort_id = rc.reporting_cohort_id
            where rc.import_id = $1::int
              and ids.pfcbl_category_rank <= 3",
          params=list(results$import_id))
          
          #created_rsf_pfcbl_ids <- unlist(created_rsf_pfcbl_ids)
          
          IMPORT_LIST__REFRESH(IMPORT_LIST__REFRESH()+1)
          SERVER_SETUP_INDICATORS_LIST_REFRESH(SERVER_SETUP_INDICATORS_LIST_REFRESH()+1) #Datasets can create/affect/auto-subscribed and should be up-to-date
          #Will also trigger checks refresh
          SERVER_DASHBOARD_REFRESH(SERVER_DASHBOARD_REFRESH()+1) #Ensure new data is reflected in any reports.
          
          if (!empty(created_rsf_pfcbl_ids)) {
            #Refresh selected program with negative value and force it to re-query its facilities
            DO_SELECT_PROGRAM_ID(-as.numeric(created_rsf_pfcbl_ids$rsf_program_id))
          }
          
          showNotification(ui=h3(paste0("New ",toupper(what)," successfully created!")))
         
      }
    })
  },
  error=function(e) {
    showNotification(type="error",
                     duration=NULL,
                     ui=div(h3(paste0("An error has occurred: failed to create new ",what)),
                            p(conditionMessage(e))))
    
  },
  warning=function(w) {
    showNotification(type="error",
                     duration=NULL,
                     ui=div(h3(paste0("An error has occurred: failed to create new ",what)),
                            p(conditionMessage(w))))
    
    
  })
  
})

observeEvent(input$server_setup_create_setup_indicators, {

  
  recently_created_id <- max(SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id)
  if (!isTruthy(recently_created_id)) return(NULL)
  
  updateTabsetPanel(session=session,
                    inputId="tabset_setup_program",
                    selected = "Setup Indicators")
  
  updateSelectizeInput(session=session,
                       inputId="server_programs__selected_facility",
                       selected=recently_created_id)
  
  showElement(id="ui_setup__indicator_setup_filter_ui",
              anim=FALSE)
  
  updateSelectizeInput(session=session,
                       inputId="ui_setup__indicator_setup_filter",
                       selected=TRUE)
  
  removeModal()  
})

observeEvent(input$setup_program_create_what, {
  what <- input$setup_program_create_what
  program_id <- SELECTED_PROGRAM_ID()
  
  if (isTruthy(program_id) && any(what==c("client"))) {
    
    facilities <- DBPOOL %>% dbGetQuery("select 
                                              nids.rsf_pfcbl_id,
                                              nids.rsf_full_name
                                            from p_rsf.view_current_entity_names_and_ids nids
                                            where rsf_program_id = $1::int
                                              and pfcbl_category = 'facility'
                                            order by nids.rsf_full_name",
                                        params=list(program_id))
    if (empty(facilities)) facilities <- setNames("","No facilities exist")
    else facilities <- c("",setNames(facilities$rsf_pfcbl_id,facilities$rsf_full_name))
    
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_facility",
                         choices=facilities,
                         selected="")
    showElement(id="setup_program_create_selected_facility",anim = TRUE,animType="fade")
  } else {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_facility",
                         choices=c(""),
                         selected="")
    
    hideElement(id="setup_program_create_selected_facility")
    hideElement(id="setup_program_create_selected_client")
  }
}, ignoreNULL = FALSE)

observeEvent(input$setup_program_create_selected_facility, {
  facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
  what <- input$setup_program_create_what
  
  {
    updateSelectizeInput(session=session,
                         inputId="setup_program_create_selected_client",
                         choices=c(""),
                         selected="")
    
    hideElement(id="setup_program_create_selected_client")
    #hideElement(id="setup_program_create_selected_borrower")
    
  }
  
}, ignoreNULL = FALSE)

observeEvent(input$server_setup_collections__toggle_select, {
  
  selected_rsf_program_id <- SELECTED_PROGRAM_ID() ##Reactive on selected program_id
  memberships <- SERVER_SETUP_CREATE__COLLECTIONS_MEMBERSHIPS()
  facility_id <- as.numeric(input$server_setup_collections__toggle_select)
  
  if (!isTruthy(facility_id) || empty(memberships)) return (NULL)
  
  if (!any(facility_id %in% memberships$rsf_facility_id) || !isTRUE(memberships[rsf_facility_id==facility_id,editable])) {
    return(showNotification(type="error",
                            ui=h3("You do not have permission to make this change")))
  } else {
    
    DBPOOL %>% poolWithTransaction(function(conn) {
      
      member <- dbGetQuery(conn,"
        select exists(select true
                      from p_rsf.rsf_facilities rf
                      where rf.rsf_program_id = $1::int
                        and rf.rsf_facility_id = $2::int)::bool as member
      ",params=list(selected_rsf_program_id,
                    facility_id))
      
      if (isTRUE(unlist(member))) {
        
        dbExecute(conn,"
          delete from p_rsf.rsf_facilities rf
          where rf.rsf_program_id = $1::int
            and rf.rsf_facility_id = $2::int
            and exists(select true 
                       from p_rsf.rsf_programs rp
                       where rp.rsf_program_id = $1::int
                         and rp.is_collection is true);    
        ",params=list(selected_rsf_program_id,
                      facility_id))
    
      } else {
        
        dbExecute(conn,"
          insert into p_rsf.rsf_facilities(rsf_program_id,rsf_facility_id) 
          select 
            $1::int,
            $2::int
          where exists(select true 
                       from p_rsf.rsf_programs rp
                       where rp.rsf_program_id = $1::int
                         and rp.is_collection is true)
          on conflict
          do nothing;          
        ",params=list(selected_rsf_program_id,
                      facility_id))
      }
      
    })
    
    REFRESH_PROGRAM_FACILITIES(REFRESH_PROGRAM_FACILITIES()+1)
  }
  
})
##################
###########OUTPUTS
##################

output$setup_program_create_what_text <- renderText({
  what <- input$setup_program_create_what
  what_text <- ""
  if (!isTruthy(what)) what_text <- ""
  else if (what=="program") {
    what_text <- "Create New Program"
  } else if (what=="facility") {
    what_text <- paste0("Create New Facility for RSF Program ",SELECTED_PROGRAM()$program_nickname)
  } else if (what=="client") {
    facility_rsf_pfcbl_id <- as.numeric(input$setup_program_create_selected_facility)
    facility_name <- DBPOOL %>% dbGetQuery("select rsf_full_name
                                             from p_rsf.view_current_entity_names_and_ids nids
                                             where nids.rsf_program_id = $1::int 
                                               and nids.rsf_pfcbl_id = $2::int",
                                           params=list(SELECTED_PROGRAM_ID(),
                                                       facility_rsf_pfcbl_id))
    what_text <- paste0("Create New Client for RSF Facility ",unlist(facility_name))
    
  }
  
  
  return (what_text)
})

output$setup_program_create_ui <- renderUI({
  
  ui <- NULL
  selected_program <- SELECTED_PROGRAM()
  #for regular programs
  if (!isTruthy(selected_program) || !isTRUE(as.logical(selected_program$is_collection))) {
    {
      what <- input$setup_program_create_what
      if (!isTruthy(what) || !what %in% c("program","facility","client")) {
        return (h3("Please select what type of new entity to create from the drop-down menu"))
      }
      
      if (what=="program" && isTruthy(as.numeric(SELECTED_PROGRAM_ID())>0)) {
        
        return (div(h3("A new program cannot be created while ",SELECTED_PROGRAM()$program_nickname," is actively selected."),
                    p("Use back-space to remove any program selection from the main Program choice in upper-left Nav Bar")))
      } else if (what %in% c("facility","client","borrower","loan") && !isTruthy(as.numeric(SELECTED_PROGRAM_ID()))) {
        
        return (div(h3("Please select an RSF Program in the upper-left Nav Bar.")))
      }
      
      
      rsf_indicators <- RSF_INDICATORS()
      indicator_requirements <- DBPOOL %>% db_create_entity_indicator_requirements(entity_type=what)
      
      setDT(indicator_requirements)
      indicator_requirements <- split(indicator_requirements,by="indicator_id")
      
      #TODO Note: doesn't include any OPTIONS/drop down. A bit lazy for the UI, but at present no set-up indicators are choices and this UI is used so rarely that
      #Users can type in what is needed.
      
      ui_requirements <- lapply(indicator_requirements,
                                function(x) {
                                  indicator <- rsf_indicators[indicator_id==x$indicator_id]
                                  placeholder <- x$default_value
                                  if (is.na(x$default_value) && x$data_type=="date") placeholder  <- "Use date format: YYYY-MM-DD"
                                  
                                  if (is.na(placeholder)) placeholder <- NULL
                                  
                                  label <- rbindlist(indicator$labels)[label_key=="EN" & is_primary==TRUE,label]
                                  if (!isTruthy(label)) label <- indicator$indicator_name
                                  label <- paste0(label," (",x$is_setup,")")
                                  
                                  inputId <- paste0("setup_program_create_setup_indicator_",x$indicator_id)
                                  uiOb <- NULL
                                  
                                  if (isTruthy(as.numeric(indicator$options_group_id))) {
                                    
                                    multiple <- as.logical(indicator$indicator_options_group_allows_multiples)
                                    if (is.na(multiple)) multiple <- FALSE
                                    
                                    options <- rbindlist(indicator$options_group)
                                    options <- unique(options[label_key=="EN" & is_primary==TRUE,
                                                              .(options_group_key,label)])[order(label)]
                                    choices <- c("",setNames(options$options_group_key,options$label))
                                    uiOb <- selectizeInput(inputId=inputId,
                                                           label=label,
                                                           choices=choices,
                                                           selected="",
                                                           multiple=multiple,
                                                           options=list(placeholder=placeholder))
                                    
                                    
                                  } else if (indicator$is_data_unit==TRUE & grepl("currency",indicator$indicator_name)) {
                                    currencies <- CALCULATIONS_ENVIRONMENT$VALID_CURRENCIES
                                    currencies <- c("",currencies)
                                    uiOb <- selectizeInput(inputId=inputId,
                                                           label=label,
                                                           choices=currencies,
                                                           selected="",
                                                           multiple=FALSE,
                                                           options=list(placeholder=placeholder))
                                    
                                  } else if (indicator$data_type=="date") {
                                    uiOb <- dateInput(inputId=inputId,
                                                      label=label,
                                                      min="2017-12-31",
                                                      max=as.character(today()),
                                                      value="2020-12-31")
                                    
                                  } else {
                                    uiOb <- textInput(inputId=paste0("setup_program_create_setup_indicator_",x$indicator_id),
                                                      label=label,
                                                      value="",
                                                      placeholder = placeholder)
                                  }
                                  
                                  return(fluidRow(column(12,uiOb)))
                                })
      
      if (what != "program") {
       
        reporting_ui <- tagList()
        ui_requirements <- append(list(reporting_ui),ui_requirements)
      } else {
        
        ui_requirements <- append(list(
          selectizeInput(inputId="setup_program_is_collection",
                      label="Project Type",
                      selected=FALSE,
                      choices=c(`Portfolio Collection`=TRUE,
                                `Pooled Risk Structure`=FALSE),
                      multiple=FALSE)),
                      ui_requirements)
      }
      
      ui_requirements <- tagList(ui_requirements)
      
      ui <- div(style="width:75%;",
                ui_requirements,
                fluidRow(column(12,align="right",
                                actionButton(inputId="setup_program_create_action_button",
                                             label=toTitleCase(paste0("Create ",what)),
                                             class="btn btn-success",
                                             icon=icon("gears"))
                )))
  }
  } else {
    ui <- div(
      
              fluidRow(column(12,
                              "Check which Facilities should be included in this Portfolio Group.  
                              If you are looking for a specific Facility and do not see it, you may not have permissions for that project.")),
              fluidRow(column(12,
                              DT::dataTableOutput(outputId="ui_setup__create_collections_membership")))
              )
  }
  
  return (ui)
})




output$ui_setup__create_collections_membership <- DT::renderDataTable({
  
  
  selected_rsf_program_id <- SELECTED_PROGRAM_ID() ##Reactive on selected program_id
  memberships <- SERVER_SETUP_CREATE__COLLECTIONS_MEMBERSHIPS()
 
  
  if (empty(memberships)) {
    return (DT::datatable(data.frame(Error="There are no available facilities."),
                          rownames=FALSE,
                          fillContainer = TRUE,
                          options=list(
                            dom="t",
                            autoWidth=TRUE,
                            paging=FALSE
                          )))
  }
  
  
    
    memberships <- memberships[,
                               .(toggle_box,
                                 facility_name,
                                 sys_name)]
    
    dd <- DT::datatable(memberships,
                        rownames = FALSE,
                        fillContainer=TRUE,
                        #extensions = "Select",
                        selection = "none",
                        colnames=c("Included?","Facility Name","SYSNAME"),
                        editable=list(target = 'cell', disable = list(columns = c(0,1,2))), #c(0,1)))
                        escape = FALSE, #Shouldn't be any HTML escapable text
                        options=list(
                          dom="t",
                          bSort=T,
                          scrollY="70vh",
                          ordering=T,
                          paging=F,
                          columnDefs = list(
                            list(targets = c(0), className = 'dt-center'),
                            list(targets = c(1,2), className = 'dt-left'))))
  
  return (dd)
  
})

