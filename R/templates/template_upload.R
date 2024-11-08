template_upload <- function(pool,
                            template,
                            status_message=function(...) {}) 
{
  t1<-Sys.time() 
  {
    
    SYSTEM_CALCULATOR_ACCOUNT <- CALCULATIONS_ENVIRONMENT$SYSTEM_CALCULATOR_ACCOUNT
  
    print("tempate_upload")
    rsf_program_id <- template$reporting_cohort$rsf_program_id
    if (is.null(rsf_program_id) || is.na(rsf_program_id)) {
      stop("Program ID is undefined")  
    }
    
    if (is.null(template$pfcbl_data) || all(is.na(template$pfcbl_data))) stop("Missing pfcbl_data")
    if (length(template$reporting_cohort$reporting_rsf_pfcbl_id) > 1) {
      stop("Cohort pfcbl_id > 1: Disallowed -- only one reporting cohort ID per dataset.  Create child cohorts for parent cohorts to effect multiple datasets per parent cohort ID")
    }
  
    status_message(class="none",paste0("Uploading data for: ",template$reporting_cohort$source_reference," ...\n"))
  } #Validating Formats

  #Upload input file for archiving and logging before any data are inserted.
  {
    status_message(class="info","\nSaving backup:",basename(template$template_file),"\n"); 
    
    t2 <- Sys.time()
    uploaded <- db_cohort_upload_file(pool=pool,
                                      file_path=template$template_file,
                                      reporting_cohort_id=template$reporting_cohort$reporting_cohort_id)
    
    if (!uploaded) {
      stop(paste0("Failed to save file ",paste0("'",basename(template$template_file),"'")))
    }
    template$backup_time <- Sys.time()-t2
  }
  
  #Do the uploads
  {
    status_message(class="info","\nUploading ",format(nrow(template$pfcbl_data),big.mark=","), " data points\n"); 
    

    #Ensure LCU defintions are present in the data FIRST
    template$pfcbl_data[,reporting_lcu_rank:=0]
    
    template$pfcbl_data[indicator_id %in% template$rsf_indicators[indicator_sys_category=="entity_currency_unit",indicator_id],
                        reporting_lcu_rank:=1]
    
    template$pfcbl_data[indicator_id %in% template$rsf_indicators[indicator_sys_category=="entity_local_currency_unit",indicator_id],
                        reporting_lcu_rank:=2]
    
    template$pfcbl_data[,inserted:=FALSE]
    
    chronologies <- sort(template$pfcbl_data[,unique(reporting_chronology_rank)],decreasing=FALSE) #FROM PAST TO FUTURE (low to high)
    for (chronology_rank in chronologies) {
      #chronology_rank <- chronologies[[1]]
      chronology_pfcbl_data <- template$pfcbl_data[reporting_chronology_rank==chronology_rank]
      current_asof_date <- unique(chronology_pfcbl_data$reporting_asof_date)
      
      if (length(current_asof_date) != 1) stop(paste0("Current asof date has multiple reporting dates: ",paste0(current_asof_date,collapse=",")))
      
      redundancies <- sort(chronology_pfcbl_data[,unique(reporting_redundancy_rank)],decreasing=TRUE) #FROM X to ZERO (high to low)
      
      if (!any(redundancies==0)) stop("No non-zero redundancy ranks in the current data set")
      #high to low: 0==end-of-period data
      for (redundancy_rank in redundancies) {
        #redundancy_rank <- redundancies[1]
        redundancy_pfcbl_data <- chronology_pfcbl_data[reporting_redundancy_rank==redundancy_rank]
        lcus <- sort(redundancy_pfcbl_data[,unique(reporting_lcu_rank)],decreasing=TRUE) #FROM X to ZERO (high to low)
        
        for (lcu_rank in lcus) {
          #lcu_rank <- lcus[1]
          #lcu_rank <- lcus[2]
          lcu_pfcbl_data <- redundancy_pfcbl_data[reporting_lcu_rank==lcu_rank]
          inserted_rows <- db_add_update_data_user(pool=pool,
                                                    reporting_cohort=template$reporting_cohort,
                                                    cohort_upload_data=lcu_pfcbl_data[,
                                                                                      .(reporting_asof_date,
                                                                                        rsf_pfcbl_id,
                                                                                        indicator_id,
                                                                                        data_value,
                                                                                        data_unit,
                                                                                        data_submitted,
                                                                                        data_source_row_id)],
                                                    template_has_static_row_ids=template$template_settings$template_has_static_row_ids,
                                                    is_redundancy_reporting=(redundancy_rank > 0))
          
          template$pfcbl_data[lcu_pfcbl_data[inserted_rows,
                                             .(data_source_row_id,
                                               rsf_pfcbl_id,
                                               indicator_id,
                                               reporting_asof_date)],
                              inserted:=TRUE,
                              on=.(data_source_row_id,
                                   rsf_pfcbl_id,
                                   indicator_id,
                                   reporting_asof_date)]
        }
      }
    }
  }
  
  if(SYS_PRINT_TIMING) debugtime("template_upload"," Data Upload time: ",format(Sys.time()-t1))
  
  #Because guidance can be at the rsf_facility level, must be performed here after setup template has potentially created new facilities.
  #And uploaded their names and IDs to match against sys_name
  #But before the system checker has been run and performed any checks that might be subject to guidance contained in the setup file.
  if (!is.null(template$template_settings$template_is_setup) && 
      template$template_settings$template_is_setup==TRUE) {
    
    if (!empty(template$setup_data$PROGRAM_SETTINGS)) {
      
      program_settings <- template$setup_data$PROGRAM_SETTINGS
      
      valid_monitoring <- dbGetQuery(pool,"select exists(select * from p_rsf.rsf_pfcbl_ids ids
                                                       inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                       where ids.rsf_program_id = $1::int
                                                         and ids.pfcbl_category = 'program'
                                                         and sn.sys_name = $2::text)::bool as valid",
                                     params=list(template$rsf_program_id,
                                                 paste0(unique(program_settings$SYSNAME),collapse="")))
      if (unlist(valid_monitoring)==FALSE) {
        stop(paste0("Monitored indicators are for '",unique(program_settings$SYSNAME)," but template upload is for program ID#",template$rsf_program_id))
      }    
      
      #conn <- poolCheckout(pool)
      #dbBegin(conn)
      #dbRollback(conn)
      poolWithTransaction(pool,function(conn) { 
        dbExecute(conn,"create temp table _temp_settings(setting_name text,
                                                       setting_value text)
                on commit drop;")
        
        dbAppendTable(conn,
                      name="_temp_settings",
                      value=program_settings[,.(setting_name=setting,
                                                setting_value=value)])
        
        dbExecute(conn,"analyze _temp_settings")
        
        dbExecute(conn,"insert into p_rsf.rsf_program_settings(rsf_program_id,setting_name,setting_value)
                      select $1::int as rsf_program_id,ts.setting_name,ts.setting_value
                      from _temp_settings ts
                      on conflict(rsf_program_id,setting_name)
                      do update
                      set setting_value = EXCLUDED.setting_value")
        
      })
      
    }
    
    #This is coming here in the template_upload because system is resolving SYSNAME and this requires name and ID data to have been already uploaded.
    if (!empty(template$setup_data$PROGRAM_INDICATORS)) {
      
      program_indicators <- template$setup_data$PROGRAM_INDICATORS
      if (!setequal(names(program_indicators),c("INDID","FRMID","SYSNAME","indicator_name","monitored","formula_title","is_auto_subscribed"))) {
        status_message(class="error",
                       "Failed to import PROGRAM INDICATORS.  Expected columns: ",paste0(c("INDID","FRMID","SYSNAME","indicator_name","monitored","formula_title"),collapse=", "))
      } else {
        setnames(program_indicators,
                 old=c("INDID","FRMID"),
                 new=c("indicator_id","formula_id"))
        program_indicators[,indicator_id:=as.numeric(indicator_id)]
        program_indicators[,formula_id:=as.numeric(formula_id)]
        program_indicators[,monitored:=as.logical(monitored)]
        program_indicators[,is_auto_subscribed:=as.logical(is_auto_subscribed)]
        #monitored_indicators <- program_indicators[monitored==TRUE] #if there is a misalignment or change, we don't care if they're not monitoring it.
        
        bad_indicators <- fsetdiff(program_indicators[,.(indicator_id,indicator_name)],
                                  template$rsf_indicators[,.(indicator_id,indicator_name)])
        
        if (!empty(bad_indicators)) {
          message(paste0("Template defines monitored indicators but perhaps some have changed names or been deleted? ",
                         paste0(paste0(bad_indicators$indicator_name," (",bad_indicators$indicator_id,")"),collapse=" & ")))
        }
        
        valid_monitoring <- dbGetQuery(pool,"select sn.rsf_pfcbl_id,sn.sys_name
                                             from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                                             where sn.rsf_program_id = $1::int
                                               and sn.sys_name = any(select unnest(string_to_array($2::text,',')))
                                               and sn.pfcbl_category in ('global','program','facility')",
                                       params=list(template$rsf_program_id,
                                                   paste0(unique(program_indicators$SYSNAME),collapse=",")))
        
        if (!setequal(unique(valid_monitoring$sys_name),unique(program_indicators$SYSNAME))) {
          stop("upload indicators is for program ID#",template$rsf_program_id," cannot find these entities: ",
               paste0(setdiff(unique(program_indicators$SYSNAME),unique(valid_monitoring$sys_name)),collapse=", "))
        }    
        
        #if (is.null(program_indicators$formula_id)) program_indicators[,formula_id:=as.numeric(NA)]
        #conn <- poolCheckout(pool)
        #dbBegin(conn)
        #dbRollback(conn)
        poolWithTransaction(pool,function(conn) { 
          dbExecute(conn,"create temp table _temp_indicators(sys_name text,
                                                           indicator_id int,
                                                           is_subscribed bool,
                                                           is_auto_subscribed bool,
                                                           formula_id int,
                                                           rsf_pfcbl_id int)
                  on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_indicators",
                        value=program_indicators[,.(sys_name=SYSNAME,
                                                    indicator_id,
                                                    is_subscribed=monitored,
                                                    is_auto_subscribed,
                                                    formula_id)])
          
          dbExecute(conn,"analyze _temp_indicators")
          
          dbExecute(conn,"delete from _temp_indicators ti
                        where exists(select * from p_rsf.indicators ind
                                     where ind.is_system = true
                                       and ind.indicator_id = ti.indicator_id)
                           or not exists(select * from p_rsf.indicators ind
                                         where ind.indicator_id = ti.indicator_id)")
          
          dbExecute(conn,"update _temp_indicators ti
                        set rsf_pfcbl_id = sn.rsf_pfcbl_id
                        from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                        where sn.sys_name = ti.sys_name")


          dbExecute(conn,"
                  insert into p_rsf.rsf_program_facility_indicators(rsf_pfcbl_id,
                                                                    indicator_id,
                                                                    formula_id,
                                                                    rsf_program_id,
                                                                    rsf_facility_id,
                                                                    is_subscribed,
                                                                    is_auto_subscribed)
                  select 
                  ti.rsf_pfcbl_id,
                  ti.indicator_id,
                  coalesce(indf.formula_id,pd.formula_id) as formula_id,
                  ids.rsf_program_id,
                  ids.rsf_facility_id,
                  ti.is_subscribed,
                  coalesce(ti.is_auto_subscribed,false) as is_auto_subscribed
                  from _temp_indicators ti
                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = ti.rsf_pfcbl_id
                  left join p_rsf.indicator_formulas indf on indf.formula_id = ti.formula_id
                                                         and indf.indicator_id = ti.indicator_id
                  left join p_rsf.indicator_formulas pd on pd.indicator_id = ti.indicator_id
                                                       and pd.is_primary_default = true
                  where ids.rsf_program_id = $1::int
                    and exists(select * from p_rsf.indicators ind 
                               where ind.indicator_id = ti.indicator_id)
                  except 
                  
                  select 
                    rsf_pfcbl_id,
                    indicator_id,
                    formula_id,
                    rsf_program_id,
                    rsf_facility_id,
                    is_subscribed,
                    is_auto_subscribed
                  from p_rsf.rsf_program_facility_indicators pfi
                  where pfi.rsf_program_id = $1::int
                  
                  on conflict(rsf_pfcbl_id,indicator_id)
                  do update
                  set formula_id = EXCLUDED.formula_id,
                      rsf_facility_id = EXCLUDED.rsf_facility_id,
                      is_subscribed = EXCLUDED.is_subscribed,
                      is_auto_subscribed = EXCLUDED.is_auto_subscribed;",
                  params=list(template$rsf_program_id))
          
        })
      }      
    }
    
    if (!empty(template$setup_data$PROGRAM_CHECKS)) {
      program_checks <- template$setup_data$PROGRAM_CHECKS
      if (!setequal(names(program_checks),c("CHKID","FRMID","SYSNAME","check_name","check_class","check_type","check_formula_title","monitored","is_auto_subscribed"))) {

        status_message(class="error",
                       "Program template does not correctly define checks. Manually import checks. Expecting columns: ",
                       paste0(c("CHKID","FRMID","SYSNAME","check_name","check_class","check_type","check_formula_title","monitored"),collapse=", "))
        
      } else {
        
        setnames(program_checks,
                 old=c("CHKID","FRMID"),
                 new=c("indicator_check_id","check_formula_id"))
        
        program_checks[,indicator_check_id:=as.numeric(indicator_check_id)]
        program_checks[,monitored:=as.logical(monitored)]
        program_checks[,is_auto_subscribed:=as.logical(monitored)]
        program_checks[,check_formula_id:=as.numeric(check_formula_id)]
        
        valid_monitoring <- dbGetQuery(pool,"select sn.rsf_pfcbl_id,sn.sys_name
                                             from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                                             where sn.rsf_program_id = $1::int
                                               and sn.sys_name = any(select unnest(string_to_array($2::text,',')))
                                               and sn.pfcbl_category in ('global','program','facility')",
                                       params=list(template$rsf_program_id,
                                                   paste0(unique(program_checks$SYSNAME),collapse=",")))
        if (!setequal(unique(valid_monitoring$sys_name),unique(program_checks$SYSNAME))) {
          stop("upload checks is for program ID#",template$rsf_program_id," cannot find these entities: ",
               paste0(setdiff(unique(program_checks$SYSNAME),unique(valid_monitoring$sys_name)),collapse=", "))
        }    
        
        
        #conn <- poolCheckout(pool)
        #dbBegin(conn)
        #dbRollback(conn)
        poolWithTransaction(pool,function(conn) { 
          
          dbExecute(conn,"create temp table _temp_checks(sys_name text,
                                                       check_formula_id int,
                                                       indicator_check_id int,
                                                       is_subscribed bool,
                                                       is_auto_subscribed bool,
                                                       rsf_pfcbl_id int
                  )
                  on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_checks",
                        value=unique(program_checks[,.(sys_name=SYSNAME,
                                                       check_formula_id,
                                                       indicator_check_id,
                                                       is_subscribed=monitored,
                                                       is_auto_subscribed)]))
          
          dbExecute(conn,"analyze _temp_checks")
          
          dbExecute(conn,"update _temp_checks tc
                        set rsf_pfcbl_id = sn.rsf_pfcbl_id
                        from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
                        where sn.sys_name = tc.sys_name")
          
          dbExecute(conn,"delete from _temp_checks tc
                        where not exists(select * from p_rsf.indicator_check_formulas icf
                                         where icf.check_formula_id = tc.check_formula_id
                                           and icf.indicator_check_id = tc.indicator_check_id)")
          
          dbExecute(conn,"
                  insert into p_rsf.rsf_program_facility_checks(rsf_pfcbl_id,
                                                                 check_formula_id,
                                                                 indicator_check_id,
                                                                 rsf_program_id,
                                                                 rsf_facility_id,
                                                                 is_subscribed,
                                                                 is_auto_subscribed)
                  select 
                    tc.rsf_pfcbl_id,
                    icf.check_formula_id,
                    tc.indicator_check_id,
                    ids.rsf_program_id,
                    ids.rsf_facility_id,
                    tc.is_subscribed,
                    coalesce(tc.is_auto_subscribed,false) as is_auto_subscribed
                  from _temp_checks tc
                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tc.rsf_pfcbl_id
                  inner join p_rsf.indicator_check_formulas icf on icf.check_formula_id = tc.check_formula_id
                  where ids.rsf_program_id = $1::int
                  
                  except
                  
                  select
                   rsf_pfcbl_id,
                   check_formula_id,
                   indicator_check_id,
                   rsf_program_id,
                   rsf_facility_id,
                   is_subscribed,
                   is_auto_subscribed
                  from p_rsf.rsf_program_facility_checks pfc
                  where pfc.rsf_program_id = $1::int
                  
                  on conflict(rsf_pfcbl_id,check_formula_id)
                  do update
                  set rsf_facility_id = EXCLUDED.rsf_facility_id,
                      is_subscribed = EXCLUDED.is_subscribed,
                      is_auto_subscribed = EXCLUDED.is_auto_subscribed",
                  params=list(template$rsf_program_id))
          
        })
      }
    }
    
    if (!empty(template$setup_data$PROGRAM_GUIDANCE)) {
    
    program_guidance <- template$setup_data$PROGRAM_GUIDANCE
    
    sysids <- dbGetQuery(pool,"
                           select 
                             sn.sys_name,
                             sn.rsf_pfcbl_id
                           from p_rsf.rsf_pfcbl_ids ids
                           inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = ids.rsf_pfcbl_id
                           where sn.sys_name = any(select unnest(string_to_array($1::text,','))::text)
                           and ids.rsf_program_id = $2::int",
                           params=list(paste0(unique(program_guidance$SYSNAME),collapse=","),
                                       template$rsf_program_id))
    setDT(sysids)
    program_guidance[sysids,
                     rsf_pfcbl_id:=i.rsf_pfcbl_id,
                     on=.(SYSNAME=sys_name)]
    sysids <- NULL
    
    guidance <- unique(program_guidance[,
                                        .(indicator_check_guidance_id=GUIDANCEID,
                                          indicator_check_id=CHKID,
                                          for_indicator_id=INDID,
                                          guidance,
                                          is_resolving_guidance=autoresolve,
                                          user_id=created_by_user_id,
                                          for_pfcbl_category=FOR,
                                          overwrite_check_class=guidance_class)])
    
    #conn <- poolCheckout(pool)
    #dbBegin(conn)
    #dbRollback(conn)
    poolWithTransaction(pool,function(conn) {
    
      dbExecute(conn,"create temp table _guidance(indicator_check_guidance_id int,
                                                  indicator_check_id int,
                                                  for_indicator_id int,
                                                  guidance text,
                                                  is_resolving_guidance bool,
                                                  user_id text,
                                                  for_pfcbl_category text,
                                                  overwrite_check_class text)
                on commit drop;")  
      
      dbAppendTable(conn,
                    name="_guidance",
                    value=guidance)
      
      dbExecute(conn,"alter table _guidance add primary key(indicator_check_guidance_id)")
      dbExecute(conn,"analyze _guidance")
      
      dbExecute(conn,"insert into p_rsf.indicator_check_guidance(indicator_check_guidance_id,
                                                                 indicator_check_id,
                                                                 for_indicator_id,
                                                                 guidance,
                                                                 is_resolving_guidance,
                                                                 user_id,
                                                                 for_pfcbl_category,
                                                                 overwrite_check_class)
                select
                  indicator_check_guidance_id,
                  indicator_check_id,
                  for_indicator_id,
                  guidance,
                  is_resolving_guidance,
                  user_id,
                  for_pfcbl_category,
                  overwrite_check_class
                from _guidance
                on conflict (indicator_check_guidance_id)
                do update
                set 
                  indicator_check_id = EXCLUDED.indicator_check_id,
                  for_indicator_id = EXCLUDED.for_indicator_id,
                  guidance = EXCLUDED.guidance,
                  is_resolving_guidance = EXCLUDED.is_resolving_guidance,
                  user_id = EXCLUDED.user_id,
                  for_pfcbl_category = EXCLUDED.for_pfcbl_category,
                  overwrite_check_class = EXCLUDED.overwrite_check_class;")
      })
      
      subscriptions <- unique(program_guidance[,
                                        .(rsf_pfcbl_id,
                                          indicator_check_guidance_id=GUIDANCEID,
                                          applied_by_user_id)])
      
      
      poolWithTransaction(pool,function(conn) {
        dbExecute(conn,"create temp table _subscriptions(rsf_pfcbl_id int,
                                                         indicator_check_guidance_id int,
                                                         applied_by_user_id text)
                  on commit drop;")
      
        dbAppendTable(conn,
                      name="_subscriptions",
                      value=subscriptions[!is.na(rsf_pfcbl_id) & 
                                          !is.na(indicator_check_guidance_id)])
        
        dbExecute(conn,"alter table _subscriptions add primary key(rsf_pfcbl_id,indicator_check_guidance_id)")
        dbExecute(conn,"analyze _subscriptions")
        
        dbExecute(conn,"insert into p_rsf.rsf_program_facility_check_guidance(rsf_pfcbl_id,
                                                                              indicator_check_guidance_id,
                                                                              rsf_program_id,
                                                                              rsf_facility_id,
                                                                              applied_by_user_id,
                                                                              application_time)
                  select 
                    sub.rsf_pfcbl_id,
                    sub.indicator_check_guidance_id,
                    ids.rsf_program_id,
                    ids.rsf_facility_id,
                    sub.applied_by_user_id,
                    now()::timestamptz as application_time
                  from _subscriptions sub
                  inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sub.rsf_pfcbl_id
                  where ids.rsf_program_id = $1::int
                  on conflict(rsf_pfcbl_id,indicator_check_guidance_id)
                  do update
                  set 
                    rsf_program_id = EXCLUDED.rsf_program_id,
                    rsf_facility_id = EXCLUDED.rsf_facility_id,
                    applied_by_user_id = EXCLUDED.applied_by_user_id",
                  params=list(template$rsf_program_id))
      })

    }
    
    if (!empty(template$setup_data$PROGRAM_TEMPLATE_ACTIONS)) {
      
      template_actions <- template$setup_data$PROGRAM_TEMPLATE_ACTIONS 
      
      if (!setequal(names(template_actions),c("HEADERID","TEMPLATE","SYSNAME","SHEET_NAME","HEADER_NAME","HEADER_INDEX","ACTION","REMAP_HEADER"))) {
        status_message(class="error",
                       "Failed to import HEADER ACTIONS.  Expected columns: ",paste0(c("HEADERID","TEMPLATE","SYSNAME","SHEET_NAME","HEADER_NAME","HEADER_INDEX","ACTION","REMAP_HEADER"),collapse=", "))
      } else {
        setnames(template_actions,
                 old=c("HEADERID","TEMPLATE","SYSNAME","SHEET_NAME","HEADER_NAME","HEADER_INDEX","ACTION","REMAP_HEADER"),
                 new=c("header_id",
                       "template_name",
                       "sys_name",
                       "template_header_sheet_name",
                       "template_header",
                       "template_header_encounter_index",
                       "action",
                       "remap_header"))
        
        #<NA> is not allowed in header, but "NA" is and may be read-in elsewhere and interpreted as <NA>
        template_actions[is.na(template_header),
                         template_header:="NA"]
        
        template_actions[is.na(template_header_sheet_name),
                         template_header_sheet_name:=""]
        #conn <- poolCheckout(pool)
        #dbBegin(conn)
        #dbRollback(conn)
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"
            create temp table _temp_actions(header_id int,
                                            template_name text,
                                            sys_name text,
                                            template_header_sheet_name text,
                                            template_header text,
                                            template_header_encounter_index int,
                                            action text,
                                            remap_header text)
            on commit drop;")
        
          dbAppendTable(conn,
                        name="_temp_actions",
                        value=template_actions[,
                                               .(header_id,
                                                 template_name,
                                                 sys_name,
                                                 template_header_sheet_name,
                                                 template_header,
                                                 template_header_encounter_index,
                                                 action,
                                                 remap_header)])
          
          dbExecute(conn,"
            with new_headers as (
              select 
              tac.header_id
            from _temp_actions tac
            inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.sys_name = tac.sys_name
            where exists(select * from p_rsf.rsf_program_facility_template_headers fth
                         where fth.header_id = tac.header_id
            						   and fth.rsf_pfcbl_id <> sn.rsf_pfcbl_id)
            )
            update _temp_actions tac
            set header_id = nextval('p_rsf.rsf_program_facility_template_headers_header_id_seq'::regclass)
            from new_headers 
            where new_headers.header_id = tac.header_id")
          
          dbExecute(conn,"
            insert into p_rsf.rsf_program_facility_template_headers(rsf_pfcbl_id,
                                                                    template_id,
                                                                    rsf_program_id,
                                                                    rsf_facility_id,
                                                                    header_id,
                                                                    template_header_sheet_name,
                                                                    template_header,
                                                                    template_header_encounter_index,
                                                                    action,
                                                                    remap_header)
            
            select 
              ids.rsf_pfcbl_id,
              rt.template_id,
              ids.rsf_program_id,
              ids.rsf_facility_id,
              act.header_id,
              act.template_header_sheet_name,
              act.template_header,
              act.template_header_encounter_index,
              act.action,
              ind.indicator_name as remap_header
            from _temp_actions act
            left join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.sys_name = act.sys_name
            left join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = sn.rsf_pfcbl_id
            left join p_rsf.reporting_templates rt on rt.template_name = act.template_name
            left join p_rsf.indicators ind on ind.indicator_name = act.remap_header
            on conflict (header_id)
            do update set template_header_sheet_name = EXCLUDED.template_header_sheet_name,
            template_header = EXCLUDED.template_header,
            template_header_encounter_index = EXCLUDED.template_header_encounter_index,
            action = EXCLUDED.action,
            remap_header = EXCLUDED.remap_header;")
        })
      }

    }
    
    if (!empty(template$setup_data$PROGRAM_FLAGS)) {
      template_flags <- template$setup_data$PROGRAM_FLAGS
     
      expected_headers <- c("ARCID","SYSNAME","INDID","CHKID","check_asof_date","check_status","check_status_user_id","check_status_comment","check_message","CINDID","CCHKID","data_sys_flags","data_value_unit")
      sys_headers <- c("archive_id","sys_name","indicator_id","indicator_check_id","check_asof_date","check_status","check_status_user_id","check_status_comment","check_message","consolidated_from_indicator_id","consolidated_from_indicator_check_id","data_sys_flags","data_value_unit")
      if (!setequal(names(template_flags),
                    expected_headers)) {
        status_message(class="error",
                       "Failed to import PROGARM FLAGS.  Expected columns: ",paste0(expected_headers,collapse=", "))
      } else {
        setnames(template_flags,
                 old=expected_headers,
                 new=sys_headers)
        
        #conn <- poolCheckout(pool)
        #dbBegin(conn)
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"
            create temp table _temp_flags(archive_id int,
                                          sys_name text,
                                          indicator_id int,
                                          indicator_check_id int,
                                          check_asof_date date,
                                          check_status text,
                                          check_status_user_id text,
                                          check_status_comment text,
                                          check_message text,
                                          consolidated_from_indicator_id int,
                                          consolidated_from_indicator_check_id int,
                                          data_sys_flags int,
                                          data_value_unit text)
            on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_flags",
                        value=template_flags[,
                                             .(archive_id,
                                               sys_name,
                                               indicator_id,
                                               indicator_check_id,
                                               check_asof_date,
                                               check_status,
                                               check_status_user_id,
                                               check_status_comment,
                                               check_message,
                                               consolidated_from_indicator_id,
                                               consolidated_from_indicator_check_id,
                                               data_sys_flags,
                                               data_value_unit)])
          
          dbExecute(conn,"
            insert into p_rsf.rsf_data_checks_archive(archive_id,
                                                      sys_name,
                                                      indicator_id,
                                                      indicator_check_id,
                                                      check_asof_date,
                                                      check_status,
                                                      check_status_user_id,
                                                      check_status_comment,
                                                      check_message,
                                                      consolidated_from_indicator_id,
                                                      consolidated_from_indicator_check_id,
                                                      data_sys_flags,
                                                      data_value_unit)
            select 
              archive_id,
              sys_name,
              indicator_id,
              indicator_check_id,
              check_asof_date,
              check_status,
              check_status_user_id,
              check_status_comment,
              check_message,
              consolidated_from_indicator_id,
              consolidated_from_indicator_check_id,
              data_sys_flags,
              data_value_unit
            from _temp_flags
            on conflict do nothing;")
        })
      }
    }
  }
  

  #saveRDS(template,"template.RDS")
  #template <- readRDS("template.RDS")
  #Checks and flags
  {
    
    
    sys_flags <- rbindlist(list(template$pfcbl_data_flags[,.(rsf_pfcbl_id,
                                                             indicator_id,
                                                             reporting_asof_date,
                                                             check_name,
                                                             check_message)],
                                template$pfcbl_reporting_flags[,.(rsf_pfcbl_id,
                                                                  indicator_id,
                                                                  reporting_asof_date,
                                                                  check_name,
                                                                  check_message)]))
    
    if (!empty(sys_flags)) {
      # 
      # reported_data <- poolWithTransaction(pool,function(conn) {
      #   
      #   dbExecute(conn,"
      #     create temp table _temp_sysflags(rsf_pfcbl_id int,
      #                                      indicator_id int,
      #                                      reporting_asof_date date)
      #     on commit drop;
      #   ")
      #   
      #   dbAppendTable(conn,
      #                 name="_temp_sysflags",
      #                 value=unique(sys_flags[,
      #                                        .(rsf_pfcbl_id,
      #                                          indicator_id,
      #                                          reporting_asof_date)]))
      #   
      #   dbGetQuery(conn,"
      #     select 
      #       ts.*
      #     from _temp_sysflags ts
      #     inner join p_rsf.rsf_data rd on rd.rsf_pfcbl_id = ts.rsf_pfcbl_id
      #                                 and rd.indicator_id = ts.indicator_id
      #                                 and rd.reporting_asof_date = ts.reporting_asof_date
      #     where rd.reporting_cohort_id in (
      #       select rc.reporting_cohort_id 
      #       from p_rsf.reporting_cohorts rc 
      #       where rc.parent_reporting_cohort_id = $1::int
      #     
      #       union 
      #     
      #       select lc.reporting_cohort_id
      #       from p_rsf.reporting_cohorts lc
      #       inner join p_rsf.reporting_cohorts cc on cc.parent_reporting_cohort_id = lc.reporting_cohort_id
      #       where lc.linked_reporting_cohort_id = $1::int
      #     )",
      #     params=list(template$reporting_cohort$reporting_cohort_id))
      # })
        
      sys_flags[,
                omit:=TRUE]
      sys_flags[unique(template$pfcbl_data[inserted==TRUE,
                                           .(rsf_pfcbl_id,
                                             indicator_id,
                                             reporting_asof_date)]),
                omit:=FALSE,
                on=.(rsf_pfcbl_id,
                     indicator_id,
                     reporting_asof_date)]
      #entity reporting data is added during upload data and therefore not included in the pfcbl_data$inserted filter.  So ensure these are not excluded!
      sys_flags[indicator_id %in% template$rsf_indicators[indicator_sys_category=="entity_reporting",indicator_id],
                omit:=FALSE]
      
      #flags generically assigned to NA indicator_id will be added onto entity reporting in flags upload
      sys_flags[is.na(indicator_id),
                omit:=FALSE]
      
      sys_flags <- sys_flags[omit==FALSE]
      sys_flags[,
                omit:=NULL]
    }
    
    
    #if an indicator has a setup designation, and it's being reported by this cohort for the 2nd or more times;
    #Then flag a key characteristics change.
    {
      key_characteristics <- template$pfcbl_data[rsf_pfcbl_id %in% template$match_results[match_action != "new",rsf_pfcbl_id] &
                                                 indicator_id %in% template$rsf_indicators[is.na(is_setup)==FALSE,indicator_id] &
                                                 redundancy_rank == 0 &
                                                 inserted==TRUE,
                                                 .(rsf_pfcbl_id,
                                                   indicator_id,
                                                   reporting_asof_date)]
      
      if (!empty(key_characteristics)) {
        
        key_characteristics <- poolWithTransaction(pool,function(conn) {
  
          dbExecute(conn,"
            create temp table _temp_chflags(rsf_pfcbl_id int,
                                            indicator_id int,
                                            reporting_asof_date date)
            on commit drop;
          ")
  
          dbAppendTable(conn,
                        name="_temp_chflags",
                        value=unique(key_characteristics))
  
          dbExecute(conn,"analyze _temp_chflags")
          dbGetQuery(conn,"
            select
              ch.*,
              concat('FROM ',
                     coalesce(previous.data_value,'{MISSING}'),
                     ' reported on ',
                     previous.reporting_asof_date,
                     ' TO ',
                     coalesce(rdc.data_value,'{MISSING}')) as changes
            from _temp_chflags ch
            inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ch.rsf_pfcbl_id
                                                 and rdc.indicator_id = ch.indicator_id
                                                 and rdc.reporting_asof_date = ch.reporting_asof_date
            left join lateral (select 
                               rdp.data_value,
                               rdp.reporting_asof_date
                               from p_rsf.rsf_data_current rdp
                               where rdp.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                 and rdp.indicator_id = rdc.indicator_id
                                 and rdp.reporting_asof_date < rdc.reporting_asof_date
                               order by rdp.reporting_asof_date desc
                               limit 1) as previous on true")
        })
        
        setDT(key_characteristics)
        key_characteristics[,
                            `:=`(check_name="sys_data_key_characteristics_changed",
                                 check_message=paste0("Key change reported: ",changes))]
        key_characteristics <- key_characteristics[,
                                                   .(rsf_pfcbl_id,
                                                     indicator_id,
                                                     reporting_asof_date,
                                                     check_name,
                                                     check_message)]
        
        sys_flags <- rbindlist(list(sys_flags,
                                    key_characteristics))
      }
    }    
    
    #Multiple reporting flagged
    {
      template$pfcbl_data[,has_multiple_reporting:=FALSE]
      template$pfcbl_data[,
                          has_multiple_reporting:=any(inserted==TRUE & 
                                                        redundancy_rank > 0),
                          by=.(rsf_pfcbl_id,
                               indicator_id,
                               reporting_asof_date)]
      
      if (any(template$pfcbl_data$has_multiple_reporting)) {
        multiple_reportings <- template$pfcbl_data[has_multiple_reporting==TRUE &
                                                   indicator_id %in% template$program_indicators[is_system==FALSE,indicator_id]]
        
        
        if (!empty(multiple_reportings)) {
          multiple_reportings <- multiple_reportings[,.(check_message=paste0("These values should be the same: ",
                                                                        paste0(
                                                                          paste0("{",
                                                                                 ifelse(is.na(data_value),
                                                                                        data_submitted,
                                                                                        data_value),
                                                                               "} on row ",data_source_row_id)
                                                                        ),
                                                                        collapse=" AND ALSO ")),
                                                     by=.(rsf_pfcbl_id,
                                                          indicator_id,
                                                          reporting_asof_date)]
          
          multiple_reportings <- multiple_reportings[,.(rsf_pfcbl_id,
                                                        indicator_id,
                                                        reporting_asof_date,
                                                        check_name='sys_flag_multiple_data_points_reported',
                                                        check_message)]
          sys_flags <- rbindlist(list(sys_flags,
                                      multiple_reportings))
        }
      }
    }
    
    #Facility/client data without ammendment
    {
      #If we're uploaded non-calculated facility client data,
      #then check the actual upload under this reporting cohort to see if any data was actually recorded AND ALSO that facility_amendment_date was NOT
      #reported.
      client_facility_indicator_ids <- template$rsf_indicators[data_category %in% c("facility","client") & is_calculated==FALSE,
                                                               indicator_id]
      if (!empty(template$pfcbl_data[indicator_id %in% client_facility_indicator_ids &
                                     inserted==TRUE])) {
        
        amendment_flags <- dbGetQuery(pool,"
          select 
            rd.rsf_pfcbl_id,
            ind.data_category,
            ind.indicator_id,
            concat(ind.indicator_name,' changed to: ',
                   coalesce(rd.data_value,'{MISSING}'),
            			 ' from: ',coalesce(previous.data_value,'{MISSING}'),' [reported in ',previous.reporting_asof_date,']') as check_message
            from p_rsf.reporting_cohorts rc
            inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
            inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
            inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = rd.rsf_pfcbl_id
            																													 and pis.indicator_id = ind.indicator_id
            inner join p_rsf.rsf_pfcbl_id_family facility on facility.child_rsf_pfcbl_id = rd.rsf_pfcbl_id
                                                         and facility.parent_pfcbl_category = 'facility'
            left join lateral (select
                               rdc.data_value,
            									 rdc.reporting_asof_date
            									 from p_rsf.rsf_data_current rdc
            									 where rdc.rsf_pfcbl_id = rd.rsf_pfcbl_id
            										 and rdc.indicator_id = rd.indicator_id
            										 and rdc.reporting_asof_date < rd.reporting_asof_date
            									 order by rdc.reporting_asof_date desc
            									 limit 1) as previous on true																							
            where rc.parent_reporting_cohort_id = $1::int
              and ind.data_category in ('client','facility')
            	and pis.formula_id is NULL
            	and ind.is_periodic_or_flow_reporting = false
              and exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = rd.data_id) -- wasnt a reversion
              and not exists(select * from p_rsf.rsf_pfcbl_ids ids
                             where ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
                               and ids.created_in_reporting_asof_date = rd.reporting_asof_date) -- init date isnt an update
            	
            	and not exists(select * from p_rsf.rsf_data_current rdc
                             inner join p_rsf.indicators inda on inda.indicator_id = rdc.indicator_id
            								 where rdc.rsf_pfcbl_id = facility.parent_rsf_pfcbl_id
            								   and rdc.indicator_id = inda.indicator_id
            								   and rdc.reporting_asof_date = rd.reporting_asof_date
            								   and inda.indicator_sys_category = 'entity_amendment_date')",
                                      params=list(template$reporting_cohort$reporting_cohort_id))
        
        
        if (!empty(amendment_flags)) {
          setDT(amendment_flags)
          amendment_flags[data_category=="facility",check_name:="sys_facility_update_without_amendment"]
          amendment_flags[data_category=="client",check_name:="sys_client_update_without_amendment"]
          
          
          amendment_flags[,reporting_asof_date:=template$reporting_cohort$reporting_asof_date]
          amendment_flags <- amendment_flags[,
                                             .(rsf_pfcbl_id,
                                               indicator_id,
                                               reporting_asof_date,
                                               check_name,
                                               check_message)]
  
          sys_flags <- rbindlist(list(sys_flags,
                                      amendment_flags))
        }
      }
    }
    
    #Identical flow data reported
    {
      flow_reporting <- template$pfcbl_data[inserted==TRUE &
                                            reporting_redundancy_rank==0 &
                                            indicator_id %in% template$rsf_indicators[is_periodic_or_flow_reporting==TRUE,indicator_id],
                                            .(rsf_pfcbl_id,
                                              indicator_id)]
      
      
      flow_repeats <- dbGetQuery(pool,"
        select
          rdc.rsf_pfcbl_id,
          rdc.indicator_id,
          rdc.reporting_asof_date,
          rdc.data_value,
          rdc.data_unit,
          ind.data_type,
          previous.total_data_value,
          previous.since_date,
          previous.reporting_asof_date as previous_asof_date
        from p_rsf.rsf_data_current rdc
        inner join p_rsf.indicators ind on ind.indicator_id = rdc.indicator_id
        inner join lateral (select 
                              NULLIF(rdp.data_value,'0') as data_value,
                              rdp.reporting_asof_date,
                              sum(case when ind.data_type in ('number','currency','percent')
                                   then rdp.data_value::numeric
                                   else 0::numeric
                              end) over() as total_data_value,
                              min(rdp.reporting_asof_date) filter(where nullif(rdp.data_value,'0') is not null) over() as since_date
                            from p_rsf.rsf_data_current rdp
                            where rdp.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                              and rdp.indicator_id = rdc.indicator_id
                              and rdp.reporting_asof_date < rdc.reporting_asof_date
                            order by rdp.reporting_asof_date desc
                            limit 1) as previous on previous.data_value is not distinct from rdc.data_value
        where rdc.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
          and rdc.indicator_id = any(select unnest(string_to_array($2::text,','))::int)
          and rdc.reporting_asof_date = $3::date",
        params=list(paste0(unique(flow_reporting$rsf_pfcbl_id),collapse=","),
                    paste0(unique(flow_reporting$indicator_id),collapse=","),
                    template$reporting_asof_date))
      
      setDT(flow_repeats)
      
      if (!empty(flow_repeats)) {
        
        flow_repeats[,
                       check_name:="sys_data_unchanged_flow_value"]
        flow_repeats[,
                     check_message:=as.character(NA)]
        
        flow_repeats[is.na(check_message) & 
                     data_type=="date",
                     check_messages:=paste0(data_value,
                                            " reported repeatedly on ",reporting_asof_date,
                                            " and on ",previous_asof_date,". ",
                                            "This is a data-flow indicator (an increment or change) ",
                                            " and for DATES, this implies a change in time--specifically a date that occurs during the reporting period.",
                                            " If no change occurred, then {BLANK} should be reported.")]
        
        flow_repeats[is.na(check_message) & 
                     data_type %in% c("number","currency","percent"),
                     check_messages:=paste0(data_value,
                                            ifelse(is.na(data_unit),"",paste0(" ",data_unit)),
                                            " reported repeatedly on ",reporting_asof_date,
                                            " and on ",previous_asof_date,". ",
                                            "This is a data-flow indicator (an increment or change) ",
                                            " with a total sum value of ",
                                            ((suppressWarnings(as.numeric(total_data_value))) + (suppressWarnings(as.numeric(data_value)))),
                                            ifelse(is.na(data_unit),"",paste0(" ",data_unit)),
                                            " since first reported in ",since_date,". ",
                                            "Changing exactly the same amount in consecutive periods is uncommon; and if no change occured, then zero should be reported")]
        
        flow_repeats <- flow_repeats[,
                                           .(rsf_pfcbl_id,
                                             indicator_id,
                                             reporting_asof_date,
                                             check_name,
                                             check_message)]
        
        sys_flags <- rbindlist(list(sys_flags,
                                    flow_repeats))
      }
    }
    #Bad ID formats and Essential Data Not reported
    if (any(template$match_results$match_action=="new")) {
      
      new_ids <- unique(template$match_results[match_action=="new",rsf_pfcbl_id])
      
      bad_ids <- template$pfcbl_data[rsf_pfcbl_id %in% new_ids &
                                     indicator_id %in% template$rsf_indicators[indicator_sys_category=="id",indicator_id] &
                                     is.na(data_value) == FALSE &
                                     validSyscategory_id(data_value)==FALSE, #NA is considered an INVALID format, but don't flag these as invalid formats: blank IDs are flagged differently as "requried" or "essential" fields.
                                     .(rsf_pfcbl_id,
                                       indicator_id,
                                       reporting_asof_date,
                                       data_value)]
      if (!empty(bad_ids)) {
        
        bad_ids[,
                `:=`(check_name="sys_data_format_invalid_id",
                     check_message=paste0("Invalid format for ID \"",data_value,"\". ID values may contain letters, numbers and : - or _ characters.  Loans may specify a sequance number using #"))]
        bad_ids <- bad_ids[,
                           .(rsf_pfcbl_id,
                             indicator_id,
                             reporting_asof_date,
                             check_name,
                             check_message)]
        
        sys_flags <- rbindlist(list(sys_flags,
                                    bad_ids))
        
      }
      
      essential_required <- dbGetQuery(pool,"
        select 
          ids.rsf_pfcbl_id,
          ind.indicator_id,
          ids.created_in_reporting_asof_date as reporting_asof_date,
          ind.indicator_name,
          ind.is_setup,
          case when ind.is_setup = 'required' 
           then 'sys_data_check_essential_field'
           when ind.is_setup = 'optional'
           then 'sys_data_check_required_field'
           else NULL 
          end as check_name		 
        from p_rsf.rsf_pfcbl_ids ids
        inner join p_rsf.view_rsf_pfcbl_indicator_subscriptions pis on pis.rsf_pfcbl_id = ids.rsf_pfcbl_id																									
        inner join p_rsf.indicators ind on ind.indicator_id = pis.indicator_id
        																										 
        left join lateral (select 
                           rdc.data_id,
        									 rdc.data_value
        									 from p_rsf.rsf_data_current rdc
        									where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
        										and rdc.indicator_id = ind.indicator_id
        									order by rdc.reporting_asof_date desc
        									limit 1) as rdc on true
        where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
          and ind.is_setup is not null
          and pis.is_subscribed = true
          and pis.is_calculated = false
          and rdc.data_value is null",
        params=list(paste0(new_ids,collapse=",")))
      
      setDT(essential_required)
      
      if (!empty(essential_required)) {
        essential_required[,
                           check_message:=paste0(indicator_name," is {MISSING} and a reported value is expected")]
        
        essential_required <- essential_required[,
                                               .(rsf_pfcbl_id,
                                                 indicator_id,
                                                 reporting_asof_date,
                                                 check_name,
                                                 check_message)]
        
        sys_flags <- rbindlist(list(sys_flags,
                                    essential_required))
      }
    }
    
    if (!empty(sys_flags)) {
      
      
      check_names <- unique(sys_flags$check_name)
      check_ids <- dbGetQuery(pool,
                             "select 
                                ic.check_name,
                                ic.indicator_check_id
                              from p_rsf.indicator_checks ic
                              where ic.check_name = any(select unnest(string_to_array($1::text,','))::text)",
                             params=list(paste0(check_names,collapse=",")))
      
      setDT(check_ids)
      
      if (!all(check_names %in% check_ids$check_name)) {
        stop("Failed to lookup system check names: ",
             paste0(check_names[-which(check_names %in% check_ids$check_nam,)],
                    collapse=" AND ALSO "))
      }
      
      sys_flags[check_ids,
                indicator_check_id:=i.indicator_check_id,
                on=.(check_name)]
      
      sys_flags <- unique(sys_flags)
      
      sys_flags <- sys_flags[,
                             n:=.N,
                             by=.(rsf_pfcbl_id,
                                  indicator_id,
                                  reporting_asof_date,
                                  indicator_check_id)]
      
      if (any(sys_flags$n > 1)) {
        #sys_flags[n>1 & is.na(data_source_row_id)==FALSE,
        #          check_message:=paste0(check_message," on ROW ",data_source_row_id)]
        sys_flags <- sys_flags[,
                               .(check_message=paste0(check_message,collapse="; AND ALSO ")),
                               by=.(rsf_pfcbl_id,
                                    indicator_id,
                                    reporting_asof_date,
                                    indicator_check_id)]
      } else {
        sys_flags <- sys_flags[,
                               .(rsf_pfcbl_id,
                                 indicator_id,
                                 reporting_asof_date,
                                 indicator_check_id,
                                 check_message)]
      }
     
      sys_flags[is.na(rsf_pfcbl_id),rsf_pfcbl_id:=template$reporting_cohort$reporting_rsf_pfcbl_id]
      sys_flags[is.na(reporting_asof_date),reporting_asof_date:=template$reporting_cohort$reporting_asof_date]
      if (anyNA(sys_flags$indicator_id)) {
        reporting <- dbGetQuery(pool,"
                              select
                                ids.rsf_pfcbl_id,
                                ind.indicator_id
                              from p_rsf.rsf_pfcbl_ids ids 
                              inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                              where ids.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
                                and ind.indicator_sys_category = 'entity_reporting'",
                                params=list(paste0(sys_flags[is.na(indicator_id),unique(rsf_pfcbl_id)],collapse=",")))
        setDT(reporting)
        reporting[,joincondition:=as.numeric(NA)]
        sys_flags[reporting,
                  indicator_id:=i.indicator_id,
                  on=.(rsf_pfcbl_id,
                       indicator_id=joincondition)]
      }
      
      setnames(sys_flags,
               old=c("reporting_asof_date","indicator_id"),
               new=c("check_asof_date","for_indicator_id"))

      sys_flags[,
                check_formula_id:=as.numeric(NA)]
      
      db_rsf_checks_add_update(pool=pool,
                               data_checks=sys_flags,
                               consolidation_threshold=template$get_program_setting("on_upload_check_consolidation_threshold"))
      
    }
  }  
  
  t2 <- Sys.time()
  processed_calculations <- rsf_program_calculate(pool=pool,
                                                  rsf_program_id=template$reporting_cohort$rsf_program_id,
                                                  rsf_indicators=template$rsf_indicators,
                                                  rsf_pfcbl_id.family=template$reporting_cohort$reporting_rsf_pfcbl_id,
                                                  calculate_future=template$get_program_setting("on_upload_perform_future_calculations"),
                                                  reference_asof_date=pmax(template$reporting_cohort$reporting_asof_date,
                                                                           max(template$pfcbl_data$reporting_asof_date)),
                                                  status_message = status_message)

  if(SYS_PRINT_TIMING) debugtime("template_upload"," >> Calculate time: ",format(Sys.time()-t2))
  
  template$calculate_time <- as.numeric(Sys.time()-t2,"secs")
  
  t2 <- Sys.time()
  rsf_program_check(pool=pool,
                    rsf_program_id=template$reporting_cohort$rsf_program_id,
                    rsf_indicators=template$rsf_indicators,
                    rsf_pfcbl_id.family=template$reporting_cohort$reporting_rsf_pfcbl_id,
                    check_future=template$get_program_setting("on_upload_perform_future_checks"),
                    check_consolidation_threshold=template$get_program_setting("on_upload_check_consolidation_threshold"),
                    reference_asof_date=pmax(template$reporting_cohort$reporting_asof_date,
                                             max(template$pfcbl_data$reporting_asof_date)),
                    status_message= status_message)

  if(SYS_PRINT_TIMING) debugtime("template_upload"," >> Check time: ",format(Sys.time()-t2))
  
  template$check_time <- as.numeric(Sys.time()-t2,"secs")
  
  #Restore flags (following checks)
  {
    #restore regular flags
    {
      
      restore_archive_checks <- dbGetQuery(pool,"
        with archive_match as MATERIALIZED (
          select 
            rdc.evaluation_id,
            dca.archive_id,
            rdc.rsf_pfcbl_id,
            dca.sys_name
          from p_rsf.rsf_data_checks rdc
          inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
          inner join p_rsf.rsf_data_checks_archive dca on dca.check_asof_date = rdc.check_asof_date
                                                      and dca.indicator_check_id = rdc.indicator_check_id
          																					  and dca.check_formula_id is not distinct from rdc.check_formula_id
          
          where rdc.rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id
          														 from p_rsf.reporting_cohorts rc 
          														 inner join p_rsf.rsf_pfcbl_id_family fam on fam.parent_rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
          														 where rc.reporting_cohort_id = $1::int)
          
          and dca.sys_name ^@ (select sn.sys_name 
          										 from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
          										 where sn.rsf_pfcbl_id = (select rc.reporting_rsf_pfcbl_id
          																							from p_rsf.reporting_cohorts rc
          																							where rc.reporting_cohort_id = $1::int))
          and p_rsf.rsf_data_value_unit(rd.data_value,rd.data_unit) is not distinct from dca.data_value_unit
        )
        select 
          am.archive_id,
          am.evaluation_id 
        from archive_match am
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = am.rsf_pfcbl_id
        where sn.sys_name = am.sys_name",
        params=list(template$reporting_cohort$reporting_cohort_id))
      
      setDT(restore_archive_checks)
      
      if (!empty(restore_archive_checks)) {
        
        #conn <- poolCheckout(pool)
        #dbBegin(conn)
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"
            create temp table _temp_restore(archive_id int,
                                            evaluation_id int)
            on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_restore",
                        value=restore_archive_checks)
          
          dbExecute(conn,"
          update p_rsf.rsf_data_checks rdc
          set check_status = dca.check_status,
              check_status_comment = dca.check_status_comment,
              check_status_user_id = dca.check_status_user_id
          from _temp_restore tr
          inner join p_rsf.rsf_data_checks_archive dca on dca.archive_id = tr.archive_id
          where tr.evaluation_id = rdc.evaluation_id
            and (rdc.check_status is distinct from dca.check_status
                 or
                 rdc.check_status_comment is distinct from dca.check_status_comment
                 or
                 rdc.check_status_user_id is distinct from dca.check_status_user_id)")
          
          dbExecute(conn,"
          update p_rsf.rsf_data_checks rdc
          set data_sys_flags = dca.data_sys_flags
          from _temp_restore tr
          inner join p_rsf.rsf_data_checks_archive dca on dca.archive_id = tr.archive_id
          where tr.evaluation_id = rdc.evaluation_id
            and rdc.data_sys_flags is distinct from dca.data_sys_flags")
          
          dbExecute(conn,"
            delete from p_rsf.rsf_data_checks_archive dca
            using _temp_restore tr
            where tr.archive_id = dca.archive_id")
          
          dbExecute(conn,"
            delete from p_rsf.rsf_data_checks_archive dca
            where dca.archive_time < (now() - interval '90 days')")
        }) 
      }
    }
    
    #restore data flags
    {
      restore_archive_flags <- dbGetQuery(pool,"
        with restore_flags as materialized (
        	select
        		dca.archive_id,
        		rd.data_id,
        		rd.rsf_pfcbl_id as data_rsf_pfcbl_id,
        		dca.rsf_pfcbl_id as archive_rsf_pfcbl_id,
        		dca.sys_name
        	from p_rsf.rsf_data rd
        	inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
        	inner join p_rsf.rsf_data_checks_archive dca on dca.indicator_id = rd.indicator_id
        	                                            and dca.check_asof_date = rd.reporting_asof_date  
        	where rd.rsf_pfcbl_id = any(select fam.child_rsf_pfcbl_id
                                      from p_rsf.reporting_cohorts rc 
                                      inner join p_rsf.rsf_pfcbl_id_family fam on fam.parent_rsf_pfcbl_id = rc.reporting_rsf_pfcbl_id
                                      where rc.reporting_cohort_id = $1::int)
            and dca.sys_name ^@ (select sn.sys_name 
          										   from p_rsf.view_rsf_pfcbl_id_current_sys_names sn
          										   where sn.rsf_pfcbl_id = (select rc.reporting_rsf_pfcbl_id
          																							  from p_rsf.reporting_cohorts rc
          																							  where rc.reporting_cohort_id = $1::int))        
        	  and rc.is_reported_cohort = true
        		and dca.data_sys_flags is not null	  
        		and dca.data_value_unit = p_rsf.rsf_data_value_unit(rd.data_value,rd.data_unit)
        		
        )
        select 
        	rf.archive_id,            
        	rf.data_id
        from restore_flags rf
        inner join p_rsf.view_rsf_pfcbl_id_current_sys_names sn on sn.rsf_pfcbl_id = rf.data_rsf_pfcbl_id
        where sn.sys_name = rf.sys_name",
        params=list(template$reporting_cohort$reporting_cohort_id))
      
      setDT(restore_archive_flags)
      if (!empty(restore_archive_flags)) {
        
        #dbBegin(conn)
        poolWithTransaction(pool,function(conn) {
          
          dbExecute(conn,"
            create temp table _temp_restore(archive_id int,
                                            data_id int)
            on commit drop;")
          
          dbAppendTable(conn,
                        name="_temp_restore",
                        value=restore_archive_flags)
          
          
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
                                            indicator_check_guidance_id,
                                            check_data_id_is_current,
                                            consolidated_from_indicator_id,
                                            consolidated_from_indicator_check_id,
                                            data_sys_flags)						
          select 
          rd.data_id,
          rd.rsf_pfcbl_id,
          rd.indicator_id,
          rd.reporting_asof_date as check_asof_date,
          dca.indicator_check_id,
          dca.check_formula_id,
          now() as status_time,
          dca.check_message,
          dca.check_status,
          dca.check_status_comment,
          dca.check_status_user_id,
          NULL as indicator_check_guidance_id,
          (dca.data_sys_flags & 4)=4 as check_data_id_is_current, -- revert will make current
          dca.consolidated_from_indicator_id,
          dca.consolidated_from_indicator_check_id,
          dca.data_sys_flags
          from _temp_restore tr
          inner join p_rsf.rsf_data rd on rd.data_id = tr.data_id
          inner join p_rsf.rsf_data_checks_archive dca on dca.archive_id = tr.archive_id
          on conflict do nothing")
          
          dbExecute(conn,"
            delete from p_rsf.rsf_data_checks_archive dca
            using _temp_restore tr
            where tr.archive_id = dca.archive_id")
        }) 
        
        rsf_program_calculate(pool=pool,
                              rsf_program_id=template$reporting_cohort$rsf_program_id,
                              rsf_indicators=template$rsf_indicators,
                              rsf_pfcbl_id.family=template$reporting_cohort$reporting_rsf_pfcbl_id,
                              calculate_future=template$get_program_setting("on_upload_perform_future_calculations"),
                              reference_asof_date=pmax(template$reporting_cohort$reporting_asof_date,
                                                       max(template$pfcbl_data$reporting_asof_date)),
                              status_message = status_message)
        
        rsf_program_check(pool=pool,
                          rsf_program_id=template$reporting_cohort$rsf_program_id,
                          rsf_indicators=template$rsf_indicators,
                          rsf_pfcbl_id.family=template$reporting_cohort$reporting_rsf_pfcbl_id,
                          check_future=template$get_program_setting("on_upload_perform_future_checks"),
                          check_consolidation_threshold=template$get_program_setting("on_upload_check_consolidation_threshold"),
                          reference_asof_date=pmax(template$reporting_cohort$reporting_asof_date,
                                                   max(template$pfcbl_data$reporting_asof_date)),
                          status_message= status_message)
      }
    }
  }
  
  #cohort info updates
  {
    dbExecute(pool,"update p_rsf.reporting_cohorts rc
                    set cohort_processing_completed = true
                    where rc.reporting_cohort_id = $1::int",
              params=list(template$reporting_cohort$reporting_cohort_id))
    
    dbExecute(pool,"
      with counts as (
        select 
          rc.parent_reporting_cohort_id as reporting_cohort_id,
          count(rd.data_id) filter(where rc.is_reported_cohort = true) as data_count_reported,
          count(rd.data_id) filter(where rc.is_calculated_cohort = true) as data_count_calculated,
          count(rdc.data_id) filter(where rc.is_reported_cohort = true) as data_current_count_reported,
          count(rdc.data_id) filter(where rc.is_calculated_cohort = true) as data_current_count_calculated

        from p_rsf.rsf_data rd
        inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id  = rd.reporting_cohort_id
				inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
				left join p_rsf.rsf_data_current rdc on rdc.data_id = rd.data_id

        where rc.parent_reporting_cohort_id = $1::int
				  and ind.is_system = false
        group by rc.parent_reporting_cohort_id
      
        union 
      
        select 
          rc.parent_reporting_cohort_id as reporting_cohort_id,
          count(rd.data_id) filter(where rc.is_reported_cohort = true) as data_count_reported,
          count(rd.data_id) filter(where rc.is_calculated_cohort = true) as data_count_calculated,
          count(rdc.data_id) filter(where rc.is_reported_cohort = true) as data_current_count_reported,
          count(rdc.data_id) filter(where rc.is_calculated_cohort = true) as data_current_count_calculated
				
        from p_rsf.rsf_data rd
        inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id  = rd.reporting_cohort_id
        inner join p_rsf.reporting_cohorts lrc on lrc.reporting_cohort_id = rc.parent_reporting_cohort_id
				inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
				left join p_rsf.rsf_data_current rdc on rdc.data_id = rd.data_id
        where lrc.linked_reporting_cohort_id = $1::int
				  and ind.is_system = false
        group by rc.reporting_cohort_id
      )
      update p_rsf.reporting_cohort_info rci
      set   
        data_count_reported = ct.data_count_reported,
        data_count_calculated = ct.data_count_calculated,
				data_current_count_reported = ct.data_current_count_reported,
				data_current_count_calculated = ct.data_current_count_calculated
      from counts ct
      where rci.reporting_cohort_id = ct.reporting_cohort_id",
      params=list(template$reporting_cohort$reporting_cohort_id))
  }
  
  template$upload_time <- as.numeric(Sys.time()-t1,"secs")

  #status_message(class="info",paste0("Processing time: ",round(as.numeric(difftime(Sys.time(),t1,units="secs")))," seconds\n"))
  
  if(SYS_PRINT_TIMING) debugtime("template_upload","Done!",format(Sys.time()-t1))
  
  return (template)
}