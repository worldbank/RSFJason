##############################################################
## Manages Information when user selects PROGRAM from dropdown
##############################################################
USER_PROGRAMS_REFRESH <- reactiveVal(0)
DO_SELECT_PROGRAM_ID <- reactiveVal(as.numeric(NA))
REFRESH_PROGRAM_INDICATORS <- reactiveVal(0)
REFRESH_PROGRAM_FACILITIES <- reactiveVal(0)

USER_URL_PARAMETERS <- eventReactive(session$clientData$url_search, {
  if (!isTruthy(session$clientData$url_search)) return (NULL)
  parseQueryString(session$clientData$url_search)
})

USER_PROGRAMS <- eventReactive(list(LOGGEDIN(),
                                    USER_PROGRAMS_REFRESH()), {
                                   
  if (LOGGEDIN()==FALSE) return (data.table())
  
  programs <- DBPOOL %>% dbGetQuery("
    select 
      rp.rsf_program_id as rsf_pfcbl_id,
      rp.rsf_program_id,
      coalesce(nids.rsf_name,'UNNAMED' || rp.rsf_program_id) as program_name,
      array_to_string(array_agg(distinct fpg.permission_name),',') as permissions,
      coalesce(nids.sys_name,'program:UNNAMED' || rp.rsf_program_id) as sys_name,
      rp.is_collection
    from p_rsf.rsf_programs rp
    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = rp.rsf_program_id
                                                                     and fpg.account_id = $1::text
    left join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = rp.rsf_program_id
    group by
      rp.rsf_program_id,
      nids.rsf_program_id,
      nids.rsf_name,
      nids.sys_name,
      rp.is_collection
    having 'LIST' = any(array_agg(fpg.permission_name))
    order by
      rp.rsf_program_id = 0,
      rp.is_collection,
      nids.rsf_name,
      rp.rsf_program_id",
    params=list(USER_ID()))
  
  if (empty(programs)) programs <- data.frame(rsf_pfcbl_id=0,
                                              rsf_program_id=0,
                                              program_name='Error: No defined Programs (or no permissions granted to any programs)',
                                              permissions="0",
                                              sys_name="global:Global")

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
    rp.rsf_program_id as rsf_pfcbl_id,
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
    ids.created_in_reporting_asof_date,
    ids.deactivated_in_reporting_asof_date,
    rp.is_collection
  from p_rsf.rsf_programs rp
  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rp.rsf_program_id
  left join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = rp.rsf_program_id
  where rp.rsf_program_id = $1::int",
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

SELECTED_PROGRAM_FACILITIES_LIST <- eventReactive(list(SELECTED_PROGRAM_ID(),
                                                       REFRESH_PROGRAM_FACILITIES()),
{ 
  
  selected_program_id <- SELECTED_PROGRAM_ID()

  if (!isTruthy(selected_program_id)) return (data.table())
  
  facilities <- DBPOOL %>% dbGetQuery("
    select 
      rf.rsf_program_id,
      rf.rsf_facility_id,
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      coalesce(nids.nickname,nids.name,'RSF' || ids.rsf_facility_id) as facility_name,
      nids.rsf_name,
      nids.nickname,
      nids.name,
      nids.id,
      nids.sys_name,
      ids.created_in_reporting_asof_date,
      ids.deactivated_in_reporting_asof_date,
      array_to_string(array_agg(distinct fpg.permission_name),',') as permissions,
      rp.is_collection
    from p_rsf.rsf_facilities rf
    inner join p_rsf.rsf_programs rp on rp.rsf_program_id = rf.rsf_program_id
    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rf.rsf_facility_id
    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = rf.rsf_facility_id
    left join p_rsf.view_current_entity_names_and_ids nids on nids.rsf_pfcbl_id = ids.rsf_pfcbl_id
    where rf.rsf_program_id = $1::int
      and fpg.account_id = $2::text
    group by
      rf.rsf_program_id,
      rf.rsf_facility_id,
      ids.rsf_pfcbl_id,
      ids.pfcbl_category,
      coalesce(nids.nickname,nids.name,'RSF' || ids.rsf_facility_id),
      nids.rsf_name,
      nids.nickname,
      nids.name,
      nids.id,
      nids.sys_name,
      ids.created_in_reporting_asof_date,
      ids.deactivated_in_reporting_asof_date,
      rp.is_collection
    having 'LIST' = any(array_agg(distinct fpg.permission_name))
    order by ids.deactivated_in_reporting_asof_date is not null asc, facility_name",
  params=list(selected_program_id,
              USER_ID()))
  
  setDT(facilities)
  facilities[,permissions:=mapply(strsplit,x=permissions,split=",",fixed=T)]
  

  return (facilities)
}, ignoreNULL=FALSE, ignoreInit = TRUE)

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

SELECTED_PF_ID <- eventReactive(SELECTED_PROGRAM_FACILITIES_LIST(), {
  
    if (as.numeric(input$server_programs__selected_facility) %in% SELECTED_PROGRAM_FACILITIES_LIST()$rsf_pfcbl_id) {
      return (as.numeric(input$server_programs__selected_facility))
    } else if (isTruthy(SELECTED_PROGRAM_ID())) {
      SELECTED_PROGRAM_ID()
    } else {
      0
    }
},ignoreNULL=FALSE)

SELECTED_PF_NAME <- eventReactive(SELECTED_PF_ID(), {
 if (SELECTED_PF_ID()==0) {
   "global:Global"
 } else {
   SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST()[rsf_pfcbl_id==SELECTED_PF_ID(),facility_name]
 }
  
})

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

  if (is.na(id) || length(id)==0 || id==-1) id <- as.numeric(NA)
  
  if (!identical(as.numeric(id),
                 as.numeric(abs(DO_SELECT_PROGRAM_ID())))) {
    cat(paste0("SETTING FROM={",DO_SELECT_PROGRAM_ID(),"} TO DO_SELECT_PROGRAM_ID(",as.numeric(abs(id)),")\n"))
    DO_SELECT_PROGRAM_ID(as.numeric(abs(id)))
    
    if (isTruthy(id)) {
      DBPOOL %>% dbExecute("
        insert into users.user_settings(account_id,setting_name,setting_value)
        values($1::text,$2::text,$3::text)
        on conflict(account_id,setting_name)
        do update
        set setting_value = EXCLUDED.setting_value",
        params=list(USER_ID(),
                    "server_programs__selected_program",
                    as.character(as.numeric(abs(id)))))
    }
  } else {
    
    cat("NOT UPDATING because identical\n")
    
  }
}, ignoreInit=FALSE, ignoreNULL = TRUE)

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

