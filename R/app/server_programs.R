##############################################################
## Manages Information when user selects PROGRAM from dropdown
##############################################################
LOAD_PROGRAM_ID <- reactiveVal(0)
LOAD_RSF_PFCBL_IDS <- reactiveVal(0)
LOAD_VALID_REPORTING_DATE <- reactiveVal(NA)
REFRESH_PROGRAM_INDICATORS <- reactiveVal(0)


USER_PROGRAMS <- eventReactive(c(LOGGEDIN(),
                                 LOAD_PROGRAM_ID()), {
                                   
  if (LOGGEDIN()==FALSE) return (NULL)
  
  if(SYS_PRINT_TIMING) debugtime("reactive: USER_PROGRAMS",reset=TRUE)
  programs <- DBPOOL %>% dbGetQuery("
                                    select 
                                      nids.rsf_pfcbl_id,
                                      nids.rsf_program_id,
                                      nids.rsf_name as program_name,
                                      array_to_string(array_agg(fpg.permission_name),',') as permissions
                                    from p_rsf.view_current_entity_names_and_ids nids
                                    inner join users.view_rsf_pfcbl_id_family_permissions_granted fpg on fpg.rsf_pfcbl_id = nids.rsf_pfcbl_id
                                    where nids.pfcbl_category in ('program','global')
                                      and fpg.account_id = $1::text
                                    group by
                                      nids.rsf_pfcbl_id,
                                      nids.rsf_program_id,
                                      nids.rsf_name
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
})

SELECTED_PROGRAM_ID <- eventReactive(c(input$select_rsf_program_id,
                                       USER_PROGRAMS()), {
  
  rsf_program_id <- as.numeric(input$select_rsf_program_id)
  if(SYS_PRINT_TIMING) debugtime("reactive: SELECTED_PROGRAM_ID='",rsf_program_id,"'")
  
  programs <- USER_PROGRAMS()
  
  if (!isTruthy(programs)) return (NA)
  if (!isTruthy(rsf_program_id)) return (NA)
  if (all(is.na(programs$rsf_program_id))) return (NA)
  if (!rsf_program_id %in% programs$rsf_program_id) return (NA)
  
  rsf_program_id
}, ignoreNULL = FALSE)


SELECTED_PROGRAM <- eventReactive(SELECTED_PROGRAM_ID(), {
  
  rsf_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(rsf_program_id)) return(NULL)  
  
  if(SYS_PRINT_TIMING) debugtime("eventReactive: SELECTED_PROGRAM")
  
  program <- DBPOOL %>% dbGetQuery("
                                   select
                                    nids.rsf_program_id,
                                    nids.rsf_pfcbl_id,
                                    coalesce(nids.nickname,nids.name,'RSF' || nids.rsf_program_id) as program_nickname,
                                    rsf_name as program_name
                                   from p_rsf.view_current_entity_names_and_ids nids
                                   where nids.rsf_program_id = $1::int
                                     and nids.pfcbl_category in ('global','program')",
                                   params=list(rsf_program_id))
  
  setDT(program)
  if (empty(program) || !isTruthy(program)) return (showNotification(type="error",ui=h2("The selected program does not exist (or no longer exists)")))
  
  
  return (program)
})

SELECTED_PROGRAM_VALID_REPORTING_DATES <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                                          LOAD_VALID_REPORTING_DATE()), {
  program <- SELECTED_PROGRAM()
  if (empty(program)) return (NULL)
  
  #Decided not to use view because if program hasn't reported at all, then its calculations could be stale if the data is extrapolated into a future reporting date
  #eg, for fx rates that rely on extrapolated historic data.
  dates <- DBPOOL %>% dbGetQuery("
   select valid_reporting_date
   from p_rsf.rsf_pfcbl_generate_reporting_dates(v_rsf_pfcbl_id => $1::int,
                                                 v_until_date => (select greatest(timeofday()::date,
                                                                          (select max(reporting_asof_date)
                                                                          from p_rsf.rsf_pfcbl_reporting rpr
                                                                          where rpr.rsf_pfcbl_id = $1::int))))",
                                 params=list(program$rsf_program_id))
  dates <- dates$valid_reporting_date
  return (dates)
},ignoreNULL=FALSE)

SELECTED_PROGRAM_FACILITIES_LIST <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                                    LOAD_RSF_PFCBL_IDS()), { 
  
  selected_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(selected_program_id)) return (NULL)
  
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
      ids.created_in_reporting_asof_date
    having 'LIST' = any(array_agg(fpg.permission_name))
    order by facility_name",
  params=list(selected_program_id,
              USER_ID()))
  
  setDT(facilities)
  facilities[,permissions:=mapply(strsplit,x=permissions,split=",",fixed=T)]
  

  return (facilities)
}, ignoreNULL=FALSE)

SELECTED_PROGRAM_FACILITIES_AND_PROGRAM_LIST <- eventReactive(c(SELECTED_PROGRAM_ID(),
                                                                SELECTED_PROGRAM_FACILITIES_LIST()), {
  selected_program_id <- SELECTED_PROGRAM_ID()
  if (!isTruthy(selected_program_id)) return (NULL)
  
  program <- SELECTED_PROGRAM()
  facilities <- SELECTED_PROGRAM_FACILITIES_LIST()
  
  pf <- rbindlist(list(program[,.(rsf_pfcbl_id,
                                  rsf_program_id,
                                  rsf_facility_id=as.numeric(NA),
                                  nickname=program_nickname)],
                 facilities[,.(rsf_pfcbl_id,
                               rsf_program_id,
                               rsf_facility_id,
                               nickname=facility_name)]))
  return (pf)
}, ignoreNULL = FALSE)

observeEvent(USER_PROGRAMS(), {
  
  if (!LOGGEDIN()) return (NULL)
  print("observe: USER_PROGRAMS")
  
  programs <- USER_PROGRAMS()
  
  
  selected_rsf_program <- ""
  
  if (isTruthy(LOAD_PROGRAM_ID()) && LOAD_PROGRAM_ID() > 0)  {
    selected_rsf_program <- LOAD_PROGRAM_ID()
  } 
  
  #print(USER_PROGRAMS())
  #print(selected_rsf_program)
  #if (nrow(programs)==1 && !all(is.na(programs$rsf_program_id))) selected_rsf_program <- programs$rsf_program_id
  
  program.choices <- setNames(programs$rsf_program_id,programs$program_name)
  program.choices <- c("",program.choices)
  
  updateSelectizeInput(session,
                       inputId="select_rsf_program_id",
                       choices=program.choices,
                       selected=selected_rsf_program,
                       options = list(placeholder = 'Select RSF Program...'))
  
}, ignoreNULL = FALSE, ignoreInit = TRUE)