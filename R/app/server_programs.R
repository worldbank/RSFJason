##############################################################
## Manages Information when user selects PROGRAM from dropdown
##############################################################
USER_PROGRAMS_REFRESH <- reactiveVal(0)
#LOAD_RSF_PFCBL_IDS <- reactiveVal(0)
#LOAD_VALID_REPORTING_DATE <- reactiveVal(NA)
DO_SELECT_PROGRAM_ID <- reactiveVal(as.numeric(NA))
REFRESH_PROGRAM_INDICATORS <- reactiveVal(0)

USER_URL_PARAMETERS <- eventReactive(session$clientData$url_search, {
  if (!isTruthy(session$clientData$url_search)) return (NULL)
  parseQueryString(session$clientData$url_search)
})

USER_PROGRAMS <- eventReactive(list(LOGGEDIN(),
                                    USER_PROGRAMS_REFRESH()), {
                                   
  if (LOGGEDIN()==FALSE) return (data.table())
  
  if(SYS_PRINT_TIMING) debugtime("1: USER_PROGRAMS",reset=TRUE)
  programs <- DBPOOL %>% dbGetQuery("
                                    select 
                                      nids.rsf_pfcbl_id,
                                      nids.rsf_program_id,
                                      nids.rsf_name as program_name,
                                      array_to_string(array_agg(fpg.permission_name),',') as permissions,
                                      nids.sys_name
                                    from p_rsf.view_current_entity_names_and_ids nids
                                    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = nids.rsf_pfcbl_id
                                    where nids.pfcbl_category in ('program','global')
                                      and fpg.account_id = $1::text
                                    group by
                                      nids.rsf_pfcbl_id,
                                      nids.rsf_program_id,
                                      nids.rsf_name,
                                      nids.sys_name
                                    having 'LIST' = any(array_agg(fpg.permission_name))
                                    order by
                                      nids.rsf_program_id = 0,
                                      nids.rsf_name",
                                    params=list(USER_ID()))
  
  if (empty(programs)) programs <- data.frame(rsf_program_id=NA,
                                              permissions="0",
                                              program_name='Error: No defined Programs (or no permissions granted to any programs)')
  setDT(programs)
  programs[,permissions:=mapply(strsplit,x=permissions,split=",",fixed=T)]
  programs
},ignoreNULL=FALSE)


SELECTED_PROGRAM_ID <- eventReactive(list(USER_PROGRAMS(),
                                          DO_SELECT_PROGRAM_ID()), {
  
  rsf_program_id <- as.numeric(abs(DO_SELECT_PROGRAM_ID()))
  cat(paste0("2 SELECTED_PROGRAM_ID=..."))
 
  programs <- USER_PROGRAMS()
  if (!LOGGEDIN()) return (as.numeric(NA))
  if (!isTruthy(rsf_program_id)) return (as.numeric(NA))
  if (empty(programs)) return (as.numeric(NA))
  if (all(is.na(programs$rsf_program_id))) return (as.numeric(NA))
  if (!rsf_program_id %in% programs$rsf_program_id) return (as.numeric(NA))
  
  cat(paste0(DO_SELECT_PROGRAM_ID(),"\n"))
  return (rsf_program_id)
},ignoreNULL=FALSE)

SELECTED_PROGRAM <- eventReactive(SELECTED_PROGRAM_ID(), {
  
  rsf_program_id <- SELECTED_PROGRAM_ID()
  cat(paste0("3 SELECTED_PROGRAM rsf_program_id=",rsf_program_id,"\n"))
  if (!isTruthy(rsf_program_id)) return(data.table())  

  program <- DBPOOL %>% dbGetQuery("
                                   select
                                    nids.rsf_pfcbl_id,
                                    ids.rsf_program_id,
                                    ids.rsf_facility_id,
                                    coalesce(nids.nickname,nids.name,'RSF' || nids.rsf_program_id) as program_nickname,
                                    nids.rsf_name as program_name,
                                    nids.pfcbl_category,
                                    nids.rsf_name,
                                    nids.nickname,
                                    nids.name,
                                    nids.id,
                                    nids.sys_name,
                                    ids.created_in_reporting_asof_date
                                   from p_rsf.view_current_entity_names_and_ids nids
                                   inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nids.rsf_pfcbl_id
                                   where nids.rsf_program_id = $1::int
                                     and nids.pfcbl_category in ('global','program')",
                                   params=list(rsf_program_id))
  
  setDT(program)
  if (empty(program) || !isTruthy(program)) return (showNotification(type="error",ui=h2("The selected program does not exist (or no longer exists)")))
  
  
  return (program)
},ignoreNULL=FALSE)

SELECTED_PROGRAM_VALID_REPORTING_DATES <- eventReactive(SELECTED_PROGRAM_ID(), {
  program_id <- SELECTED_PROGRAM_ID()
  #cat(paste0("4 SELECTED_PROGRAM rsf_program_id=",rsf_program_id,"\n"))
  if (!isTruthy(program_id)) return (c())
  
  #Decided not to use view because if program hasn't reported at all, then its calculations could be stale if the data is extrapolated into a future reporting date
  #eg, for fx rates that rely on extrapolated historic data.
  dates <- DBPOOL %>% dbGetQuery("
   select valid_reporting_date
   from p_rsf.rsf_pfcbl_generate_reporting_dates(v_rsf_pfcbl_id => $1::int,
                                                 v_until_date => (select greatest(timeofday()::date,
                                                                          (select max(ids.deactivated_in_reporting_asof_date)
                                                                           from p_rsf.rsf_pfcbl_ids ids
                                                                           where ids.rsf_pfcbl_id = $1::int))))",
                                 params=list(program_id))
  dates <- dates$valid_reporting_date
  return (dates)
},ignoreNULL=FALSE)

SELECTED_PROGRAM_FACILITIES_LIST <- eventReactive(SELECTED_PROGRAM_ID(), { 
  
  selected_program_id <- SELECTED_PROGRAM_ID()

  if (!isTruthy(selected_program_id)) return (data.table())
  
  facilities <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      coalesce(nids.nickname,nids.name,'RSF' || ids.rsf_facility_id) as facility_name,
      nids.rsf_name,
      nids.nickname,
      nids.name,
      nids.id,
      nids.sys_name,
      ids.created_in_reporting_asof_date,
      array_to_string(array_agg(fpg.permission_name),',') as permissions
    from 
    p_rsf.rsf_pfcbl_ids ids
    inner join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = nids.rsf_pfcbl_id
    where ids.rsf_program_id = $1::int
      and ids.pfcbl_category = 'facility'
      and fpg.account_id = $2::text
    group by
      ids.rsf_program_id,
      ids.rsf_facility_id,
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      coalesce(nids.nickname,nids.name,'RSF' || ids.rsf_facility_id),
      nids.rsf_name,
      nids.nickname,
      nids.name,
      nids.id,
      nids.sys_name,
      ids.created_in_reporting_asof_date
    having 'LIST' = any(array_agg(fpg.permission_name))
    order by ids.pfcbl_category_rank desc,facility_name",
  params=list(selected_program_id,
              USER_ID()))
  
  setDT(facilities)
  facilities[,permissions:=mapply(strsplit,x=permissions,split=",",fixed=T)]
  

  return (facilities)
}, ignoreNULL=FALSE)

SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST <- eventReactive(SELECTED_PROGRAM_FACILITIES_LIST(), {

  selected_program_id <- SELECTED_PROGRAM_ID()
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  
  cat(paste0("SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST: selected_program_id=",SELECTED_PROGRAM_ID()),"\n")
  if (!isTruthy(selected_program_id)) return (data.table())
  
  if (!empty(program)) {
    program <- program[,.(rsf_pfcbl_id,
               rsf_program_id,
               rsf_facility_id=as.numeric(NA),
               nickname=paste0("program:",program_nickname),
               facility_name=paste0("program:",program_nickname),
               pfcbl_category,
               name,
               id,
               sys_name,
               created_in_reporting_asof_date)]
  } else {
    program <- NULL
  }
  
  if (!empty(facilities)) {
    setorder(facilities,
             facility_name)
    facilities <- facilities[,.(rsf_pfcbl_id,
                                rsf_program_id,
                                rsf_facility_id,
                                nickname=facility_name,
                                facility_name,
                                pfcbl_category,
                                name,
                                id,
                                sys_name,
                                created_in_reporting_asof_date),
    ]
  } else {
    facilities <- NULL
  }
  
  pf <- rbindlist(list(program,facilities))
  return (pf)
},ignoreNULL = FALSE)


#Populates select input to filter on client/facility name
observeEvent(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST(), {
  
  cat(paste0("observeEvent(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST() updating menus size:{",
             nrow(SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST())),"}\n")
  
  #Will update even if NULL.
  {
    programs <- USER_PROGRAMS()
    program_choices <- c(`Programs Filter...`="")
    program_selected <- ""
    placeholder <- "Select RSF Program..."
    
    if (!empty(programs)) {
     
      program_choices <- setNames(as.character(programs$rsf_program_id),
                                  programs$program_name)
      
      program_choices <- c(program_choices,
                           `All programs reporting`="-1")
      
      
      if (!isTruthy(SELECTED_PROGRAM_ID())) {
        program_selected <- "-1"
      } else {
        program_selected <- as.character(SELECTED_PROGRAM_ID())
        placeholder <- programs[rsf_program_id==SELECTED_PROGRAM_ID(),program_name]
      }
    }
  }
  
  facilities <- SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()
  
  facilities_choices <- c(`Project Filter...`="")
  facility_selected <- ""
  
  if (!empty(facilities)) {

    facilities_choices <- c(`All Projects`="-1",
                            setNames(as.character(facilities$rsf_pfcbl_id),
                                     facilities$nickname))
    facility_selected <- "-1" #means "ALL" 

    if (!is.null(session$userData$server_programs__selected_facility) &&
        as.numeric(session$userData$server_programs__selected_facility) %in% facilities$rsf_pfcbl_id) {
      facility_selected <- as.character(session$userData$server_programs__selected_facility)
      program_selected <- as.character(facilities[rsf_pfcbl_id==facility_selected,rsf_program_id])
      
    } else if (length(as.numeric(input$server_programs__selected_facility)) &&
               as.numeric(input$server_programs__selected_facility) %in% facilities$rsf_pfcbl_id) {
      facility_selected <- as.character(input$server_programs__selected_facility)
      program_selected <- as.character(facilities[rsf_pfcbl_id==facility_selected,rsf_program_id])
    } 
    
    #selected facility needs to match selected program
    if (facility_selected != "-1") {
      if (!identical(as.numeric(facilities[rsf_facility_id==facility_selected,rsf_program_id]),
                     suppressWarnings(as.numeric(program_selected)))) {
        facility_selected <- "-1"
      }
    }
    
    #print(paste0("Loading and restoring server_programs__selected_facility=",facility_selected))
    
  }

  cat(paste0("Updating server_programs__selected_program: ",
             
             " selected=",as.character(program_selected),"\n"))
 #browser()
  updateSelectizeInput(session,
                       inputId="server_programs__selected_program",
                       choices=program_choices,
                       selected=as.character(program_selected),
                       options = list(placeholder = placeholder))
  
  updateSelectizeInput(session=session,
                       inputId="server_programs__selected_facility",
                       choices = facilities_choices,
                       selected = as.character(facility_selected),
                       options=list(placeholder="Project Filter..."))
    
}, ignoreNULL = FALSE)

observeEvent(input$server_programs__selected_program, {
  id <- suppressWarnings(as.numeric(input$server_programs__selected_program))

  if (!isTruthy(input$server_programs__selected_program)) return (NULL)
  
  cat(paste0("input$server_programs__selected_program={",
             input$server_programs__selected_program,"} ",
             id,
             " vs ",
             abs(DO_SELECT_PROGRAM_ID()),
             " = ",
             identical(as.numeric(id),as.numeric(abs(DO_SELECT_PROGRAM_ID()))),"\n"))


  if (is.na(id) || length(id)==0 || id==-1) id <- as.numeric(NA)
  
  if (!identical(as.numeric(id),
                 as.numeric(abs(DO_SELECT_PROGRAM_ID())))) {
    cat(paste0("SETTING FROM={",DO_SELECT_PROGRAM_ID(),"} TO DO_SELECT_PROGRAM_ID(",as.numeric(abs(id)),")\n"))
    DO_SELECT_PROGRAM_ID(as.numeric(abs(id)))
  } else {
    cat("NOT UPDATING because identical\n")
  }
}, ignoreInit=FALSE, ignoreNULL = TRUE)

# 
# observeEvent(SELECTED_PROGRAM_ID(), {
#   
#   if (!identical(as.numeric(SELECTED_PROGRAM_ID()),
#                  as.numeric(input$server_programs__selected_program)) &&
#       isTruthy(SELECTED_PROGRAM_ID())) {
#     updateSelectizeInput(session,
#                          inputId="server_programs__selected_program",
#                          selected=as.numeric(SELECTED_PROGRAM_ID()))
#   }  
# },priority=9000)

observeEvent(input$server_programs__selected_facility, {
  
  if (!LOGGEDIN()) { return (NULL) }
  if (empty(USER_PROGRAMS()) || empty(SELECTED_PROGRAM_FACILITIES_LIST())) { return(NULL) }
  
  save_id <- as.numeric(input$server_programs__selected_facility)
  if (is.na(save_id) || save_id %in% c(-1)) save_id <- SELECTED_PROGRAM_ID()
  
  print(paste0("Saving user setting server_programs__selected_facility=",save_id))
  
  if (!isTruthy(save_id)) return (NULL)
  
  session$userData$server_programs__selected_facility <- save_id
  
  DBPOOL %>% dbExecute("insert into users.user_settings(account_id,setting_name,setting_value)
                        values($1::text,$2::text,$3::text)
                        on conflict(account_id,setting_name)
                        do update
                        set setting_value = EXCLUDED.setting_value",
                       params=list(USER_ID(),
                                   "server_programs__selected_facility",
                                   as.character(save_id)))
  
},ignoreNULL = TRUE,ignoreInit = TRUE)

observeEvent(list(LOGGEDIN(),
                  USER_URL_PARAMETERS()), {
  
  query <- USER_URL_PARAMETERS()
  cat(paste0("USER_URL_PARAMETERS()=",names(query),"\n"))
  
  if (!LOGGEDIN()) return (NULL)
  
  #1: Load facility based on last-selected saved in user settings
  selected_facility <- DBPOOL %>% dbGetQuery("
    select 
      ids.rsf_pfcbl_id,
      ids.rsf_program_id,
      ids.rsf_facility_id
    from users.user_settings ust
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = (ust.setting_value::numeric)
    where public.isnumeric(ust.setting_value) is true
      and ust.account_id = $1::text
      and ust.setting_name = 'server_programs__selected_facility'",
    params=list(USER_ID()))
  
  cat("selected facility\n")
  print(selected_facility)
  
  #2: If our URL parameter has an ID, they've selected to load a specific ID
  #So overwrite the last-viewed facility selection.
  if (length(query)) {
    qids <- suppressWarnings(as.numeric(trimws(names(query))))
    qids <- qids[!is.na(qids)]
    
    if (length(qids)) {
      qid <- DBPOOL %>% dbGetQuery("
        select 
          ids.rsf_pfcbl_id,
          ids.rsf_program_id,
          ids.rsf_facility_id
        from p_rsf.rsf_data_current_names_and_ids nids
        inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = nids.rsf_pfcbl_id
        where nids.id = trim($1::text)
        
        union all
        
        select 
          ids.rsf_pfcbl_id,
          ids.rsf_program_id,
          ids.rsf_facility_id
        from p_rsf.rsf_pfcbl_ids ids
        where ids.rsf_pfcbl_id = $1::int
        ",
        params=list(qids[[1]]))
      
      if (!empty(qid)) {
        
        selected_facility <- qid[1,]
        
      }
    }
    
    
  }

  #Must be first.
  
  #user entering with a load report request (RID) or load exported report request (EID)
  export_id <- na.omit(suppressWarnings(as.numeric(trimws(query[which(names(query)=="EID")]))))
  #export_id <- 684
  #3: If they have an Export ID specified, then load based on the export's specifications instead.
  selected_export <- NULL
  if (length(export_id)) {
    
    #export_id <- 691
    cat("Loading export ID= ",export_id,"\n")

    if (!is.na(export_id)) {
      selected_export <- DBPOOL %>% dbGetQuery("
      select
        ids.rsf_program_id,
        ids.rsf_facility_id,
        de.export_id,
        de.exporting_title,
        de.exporting_rsf_pf_id as rsf_pfcbl_id,
        de.dashboard_parameters::jsonb,
        de.generated_with_report_id
      from p_rsf.dashboard_exports de
      inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = de.exporting_rsf_pf_id
      where de.export_id = $1::int",
      params=list(export_id))
      
      setDT(selected_export)
      
      selected_facility <- selected_export[,.(rsf_program_id,
                                              rsf_facility_id,
                                              rsf_pfcbl_id)]
    }
  }
  
  if (!empty(selected_facility)) {
    
    f_id <- as.numeric(selected_facility$rsf_facility_id)
    if (is.na(f_id)) f_id <- -1
    
    session$userData$server_programs__selected_facility <- f_id
    DO_SELECT_PROGRAM_ID(as.numeric(selected_facility$rsf_program_id))
    cat("DO SELECT PROGRAM ID: ",as.numeric(selected_facility$rsf_program_id),"\n")
  }
  
  if (!empty(selected_export)) {
    
    cat("Loading report ID= ",selected_export$exporting_title,"\n")

    dashboard_parameters <- jsonlite::fromJSON(selected_export$dashboard_parameters)
    if (!is.na(selected_export$generated_with_report_id)) {
      report <- SERVER_DASHBOARD_REPORTS_LIST()[report_id==selected_export$generated_with_report_id,
                                                .(report_id,
                                                  report_title,
                                                  report_pfcbl_id_filters,
                                                  for_facility_sys_names,
                                                  for_indicator_names,
                                                  for_asof_dates,
                                                  report_parameters)]
      SERVER_DASHBOARD_REPORT_SELECTED(as.list(report))
    }
    
    SERVER_DASHBOARD_DO_LOAD(for_facility_sys_names=dashboard_parameters$for_facility_names,
                             for_indicator_names=dashboard_parameters$for_indicator_names,
                             for_asof_dates=dashboard_parameters$for_asof_dates,
                             dashboard_parameters=dashboard_parameters$dashboard_parameters,
                             do_run=FALSE)
    
  }
  
  # updateQueryString(session=session,
  #                   queryString="",
  #                   mode="replace")
  
  # if (any(toupper(names(query))=="QR",na.rm=T)) {
  #   upnum <- suppressWarnings(as.numeric(input$action_template_upload_new))
  #   if (!length(upnum) || all(is.na(upnum))) {
  #     upnum <- 1
  #   } else {
  #     upnum <- upnum +1
  #   }
  #   
  #   shinyjs::runjs(paste0("Shiny.setInputValue(\"action_template_upload_new\",",upnum,",{priority:\"event\"})"))
  # }
  cat("Completed parsing login parameters\n")
  
},ignoreNULL=FALSE)
