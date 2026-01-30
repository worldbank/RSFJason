template_upload <- function(pool,
                            template,
                            status_message=function(...) {}) 
{
  
  

  {
    {
      
      SYSTEM_CALCULATOR_ACCOUNT <- CALCULATIONS_ENVIRONMENT$SYSTEM_CALCULATOR_ACCOUNT
      
      if (is.null(template$pfcbl_data) || all(is.na(template$pfcbl_data))) stop("Missing pfcbl_data")
      
      status_message(class="none",paste0("Uploading data for: ",template$reporting_cohort$source_reference," ...\n"))
    } #Validating Formats
    
    t1<-Sys.time() 
    uploaded_data <- db_add_update_data_user(pool=pool,
                                             import_id=template$reporting_import$import_id,
                                             upload_data=template$pfcbl_data,
                                             upload_user_id=template$reporting_user_id,
                                             rsf_indicators=template$rsf_indicators)
    template$pfcbl_data <- uploaded_data
    status_message(class="info","\nUploaded ",format(nrow(template$pfcbl_data),big.mark=","), " data points: ",format(round(Sys.time()-t1,2),units="secs"),"\n"); 
  }
  
  
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
    
    #Facility/client data without ammendment
    {
      #If we're uploaded non-calculated facility client data,
      #then check the actual upload under this reporting cohort to see if any data was actually recorded AND ALSO that facility_amendment_date was NOT
      #reported.
      client_facility_indicator_ids <- template$rsf_indicator_subscriptions[data_category %in% c("facility","client") & is_calculated==FALSE,
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
            from p_rsf.reporting_imports ri
            inner join p_rsf.reporting_cohorts rc on rc.import_id = ri.import_id
            inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = rd.rsf_pfcbl_id
            inner join p_rsf.rsf_pfcbl_ids facility on facility.rsf_pfcbl_id = ids.rsf_facility_id
            inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id
            left join lateral (select
                               rdc.data_value,
            									 rdc.reporting_asof_date
            									 from p_rsf.rsf_data_current rdc
            									 where rdc.rsf_pfcbl_id = rd.rsf_pfcbl_id
            										 and rdc.indicator_id = rd.indicator_id
            										 and rdc.reporting_asof_date < rd.reporting_asof_date
            									 order by rdc.reporting_asof_date desc
            									 limit 1) as previous on true																							
            where ri.import_id = $1::int
              and ind.data_category in ('client','facility')
              and ind.is_periodic_or_flow_reporting = false
              and facility.created_in_reporting_asof_date <> rc.reporting_asof_date -- init date isnt an update
              and exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = rd.data_id) -- wasnt a reversion
              
              -- not a formula and/or subscribed formula
              and not exists (select * from p_rsf.view_rsf_setup_indicator_subscriptions sis
                              where sis.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                and sis.indicator_id = rd.indicator_id
                                and sis.formula_id is NOT NULL
                                and sis.is_subscribed is true)
                            
              -- facility hasn't submitted an amendment date
              and not exists(select * from p_rsf.rsf_data_current rdc
                             where rdc.rsf_pfcbl_id = facility.rsf_pfcbl_id
                               and rdc.indicator_id = (select inda.indicator_id from p_rsf.indicators inda
                                                       where inda.indicator_sys_category = 'entity_amendment_date'
                                                         and inda.data_category = facility.pfcbl_category)
                               and rdc.reporting_asof_date = rd.reporting_asof_date)",
                                                  params=list(template$reporting_import$import_id))
        
        
        if (!empty(amendment_flags)) {
          setDT(amendment_flags)
          amendment_flags[data_category=="facility",check_name:="sys_facility_update_without_amendment"]
          amendment_flags[data_category=="client",check_name:="sys_client_update_without_amendment"]
          
          
          amendment_flags[,reporting_asof_date:=template$reporting_import$reporting_asof_date]
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
        from p_rsf.reporting_imports ri
        inner join p_rsf.reporting_cohorts rc on rc.import_id = ri.import_id
        inner join p_rsf.rsf_data rd on rd.reporting_cohort_id = rc.reporting_cohort_id
        inner join p_rsf.rsf_data_current rdc on rdc.data_id = rd.data_id
        inner join p_rsf.indicators ind on ind.indicator_id = rd.indicator_id																		
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
        where ri.import_id = $1::int
          and ind.is_periodic_or_flow_reporting is true  
          and ind.is_system is false",
      params=list(template$reporting_import$import_id))
      
      setDT(flow_repeats)
      
      if (!empty(flow_repeats)) {
        
        flow_repeats[,
                       check_name:="sys_data_unchanged_flow_value"]
        flow_repeats[,
                     check_message:=as.character(NA)]
        
        flow_repeats[is.na(check_message) & 
                     data_type=="date",
                     check_message:=paste0(data_value,
                                            " reported repeatedly on ",reporting_asof_date,
                                            " and on ",previous_asof_date,". ",
                                            "This is a data-flow indicator (an increment or change) ",
                                            " and for DATES, this implies a change in time--specifically a date that occurs during the reporting period.",
                                            " If no change occurred, then {BLANK} should be reported.")]
        
        flow_repeats[is.na(check_message) & 
                     data_type %in% c("number","currency","percent"),
                     check_message:=paste0(data_value,
                                            ifelse(is.na(data_unit),"",paste0(" ",data_unit)),
                                            " reported repeatedly on ",reporting_asof_date,
                                            " and on ",previous_asof_date,". ",
                                            "This is a data-flow indicator (an increment or change) ",
                                            " with a total sum value of ",
                                            ((suppressWarnings(as.numeric(total_data_value))) + (suppressWarnings(as.numeric(data_value)))),
                                            ifelse(is.na(data_unit),"",paste0(" ",data_unit)),
                                            " since first reported in ",since_date,". ",
                                            "Changing exactly the same amount in consecutive periods is uncommon; and if no change occured, then zero should be reported")]
        
        flow_repeats[is.na(check_message),
                     check_message:=paste0(ifelse(is.na(data_value),"{MISSING}",data_value),
                                            ifelse(is.na(data_unit),"",paste0(" ",data_unit)),
                                            " reported repeatedly on ",reporting_asof_date,
                                            " and on ",previous_asof_date,". ",
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
    
    #Missing (expected) facility setups
    {
      if (template$reporting_import$import_pfcbl_category %in% c("facility","client")) {
        
        missing_terms <- dbGetQuery(pool,"
          with ids as (
            select ids.rsf_pfcbl_id
            from p_rsf.rsf_pfcbl_ids ids
            where ids.rsf_facility_id = $1::int
              and ids.pfcbl_category in ('facility','client')
          )
          select distinct
          ftm.rsf_facility_id as rsf_pfcbl_id,
          ftm.parameter_id as indicator_id,
          ftm.metric,
          ftm.metric_name,
          ftm.parameter_name,
          ftm.formula_title,
          ftm.reporting_asof_date,
          ftm.is_unreported,
          array_to_string(ftm.parameter_ids,',') as parameter_ids
          from p_rsf.view_rsf_setup_facility_terms_missing ftm
          where ftm.rsf_facility_id = any(select ids.rsf_pfcbl_id from ids)",
          params=list(template$reporting_import$import_rsf_pfcbl_id))
        
        if (!empty(missing_terms)) {
          #order by ftm.rsf_facility_id,ftm.metric,ftm.metric_id,ftm.formula_id,ftm.parameter_id,ftm.reporting_asof_date desc
          missing_terms[["parameter_ids"]] <- strsplit(missing_terms$parameter_ids,",",fixed=T)
          setDT(missing_terms)
          setorder(missing_terms,
                   rsf_pfcbl_id,
                   metric,
                   metric_name,
                   parameter_name,
                   -reporting_asof_date)
          
          missing_terms <- missing_terms[is_unreported==TRUE | reporting_asof_date <= template$reporting_import$reporting_asof_date]
          
          
          
          
          missing_terms[,
                        reporting_asof_date:=template$reporting_import$reporting_asof_date]
          
          uploaded_parameters <- sapply(missing_terms$parameter_ids,FUN=function(ids,uploads) { any(ids %in% uploads,na.rm=F) },
                                        uploads=unique(template$pfcbl_data$indicator_id))
          
          missing_terms[,
                        affected:=uploaded_parameters]
          
          missing_terms <- missing_terms[affected==TRUE]
          missing_terms[,check_message:=paste0("Facility term is missing and monitored at Facility setup.\n",
                                               "Enter value for '",parameter_name,"' in Facility Setup\n",
                                               "Or unsubscribe from ",toupper(metric)," that will fail to compute: ",metric_name,"[",formula_title,"]")]
          
          missing_terms <- missing_terms[,
                                       .(rsf_pfcbl_id,
                                         indicator_id,
                                         reporting_asof_date,
                                         check_name="sys_facility_missing_term",
                                         check_message)]
          
          sys_flags <- rbindlist(list(sys_flags,
                                      missing_terms))
        }
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
          sis.rsf_pfcbl_id,
          ind.indicator_id,
          ind.indicator_name,
          ind.is_setup,
          case 
           when ind.is_setup = 'required' 
           then 'sys_data_check_essential_field'
           
           when ind.is_setup = 'optional'
           then 'sys_data_check_required_field'
           
           else NULL 
          end as check_name		 
        from p_rsf.view_rsf_setup_indicator_subscriptions sis 
        inner join p_rsf.indicators ind on ind.indicator_id = sis.indicator_id
        																										 
        left join lateral (select 
                           rdc.data_id,
        									 rdc.data_value
        									 from p_rsf.rsf_data_current rdc
        									where rdc.rsf_pfcbl_id = sis.rsf_pfcbl_id
        										and rdc.indicator_id = ind.indicator_id
        									order by rdc.reporting_asof_date desc
        									limit 1) as rdc on true
        where sis.rsf_pfcbl_id = any(select unnest(string_to_array($1::text,','))::int)
          and sis.filter_matched_pfcbl_indicators is true
          and ind.is_setup is not null
          and sis.is_subscribed = true
          and sis.is_calculated = false
          and rdc.data_value is null",
        params=list(paste0(new_ids,collapse=",")))
      
      setDT(essential_required)
      
      if (!empty(essential_required)) {
        essential_required[,
                           check_message:=paste0(indicator_name," is {MISSING} and a reported value is expected")]
        
        essential_required <- essential_required[,
                                               .(rsf_pfcbl_id,
                                                 indicator_id,
                                                 reporting_asof_date=template$reporting_import$reporting_asof_date,
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
      
      #Grouping lots of similar messages with AND ALSO is not helpful...removed.  If lots of similar flags, better to download Excel extract and review.  Else messages are tough to differentiate
      sys_flags <- sys_flags[,
                             .(rsf_pfcbl_id,
                               indicator_id,
                               reporting_asof_date,
                               indicator_check_id,
                               check_message)]
      
      sys_flags[is.na(rsf_pfcbl_id),rsf_pfcbl_id:=template$reporting_import$import_rsf_pfcbl_id]
      sys_flags[is.na(reporting_asof_date),reporting_asof_date:=template$reporting_import$reporting_asof_date]
      
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
                                                  rsf_indicators=template$rsf_indicators,
                                                  rsf_pfcbl_id.family=template$reporting_import$import_rsf_pfcbl_id,
                                                  for_import_id=template$reporting_import$import_id,
                                                  calculate_future=template$get_program_setting("on_upload_perform_future_calculations"),
                                                  reference_asof_date=pmax(template$reporting_import$reporting_asof_date,
                                                                           max(template$pfcbl_data$reporting_asof_date)),
                                                  status_message = status_message)

  if(SYS_PRINT_TIMING) debugtime("template_upload"," >> Calculate time: ",format(Sys.time()-t2))
  
  template$calculate_time <- as.numeric(Sys.time()-t2,"secs")
  
  t2 <- Sys.time()
  rsf_program_check(pool=pool,
                    rsf_indicators=template$rsf_indicators,
                    rsf_pfcbl_id.family=template$reporting_import$import_rsf_pfcbl_id,
                    check_future=template$get_program_setting("on_upload_perform_future_checks"),
                    check_consolidation_threshold=template$get_program_setting("on_upload_check_consolidation_threshold"),
                    reference_asof_date=pmax(template$reporting_import$reporting_asof_date,
                                             max(template$pfcbl_data$reporting_asof_date)),
                    status_message= status_message)

  if(SYS_PRINT_TIMING) debugtime("template_upload"," >> Check time: ",format(Sys.time()-t2))
  
  template$check_time <- as.numeric(Sys.time()-t2,"secs")
  
  #Restore flags (following checks)
  {
    #restore regular flags
    {
      
      #conn <- poolCheckout(pool)
      #dbBegin(conn)
      #dbRollback(conn)
      restored <- poolWithTransaction(pool,function(conn) {
        
        dbExecute(conn,"
          create temp table _temp_restore(archive_id int,
                                          evaluation_id int)
          on commit drop;")
        
        
        dbExecute(conn,"
          with archive_match as (
            select distinct on (dca.archive_id)
              rdc.evaluation_id,
              dca.archive_id,
              rdc.rsf_pfcbl_id
            from p_rsf.rsf_data_checks rdc
            inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
            inner join lateral (select nai.sys_name
                                from p_rsf.rsf_data_current_names_and_ids nai
                                where nai.rsf_pfcbl_id = rdc.rsf_pfcbl_id
                                  and nai.reporting_asof_date <= rdc.check_asof_date
                                order by nai.reporting_asof_date desc
                                limit 1) as sn on true
                                
            inner join p_rsf.rsf_data_checks_archive dca on dca.sys_name = sn.sys_name
                                                        and dca.check_asof_date = rdc.check_asof_date
                                                        and dca.indicator_check_id = rdc.indicator_check_id
                                                        and dca.indicator_id = rdc.indicator_id
                                                        
            where rdc.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id 
                                         from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                         where ft.from_rsf_pfcbl_id = $1::int
                                           and ft.pfcbl_hierarchy <> 'parent')
                                           
              and dca.check_formula_id is not distinct from rdc.check_formula_id          														 
              and p_rsf.rsf_data_value_unit(rd.data_value,rd.data_unit) is not distinct from dca.data_value_unit
              order by dca.archive_id,dca.archive_time desc nulls last
          )
          insert into  _temp_restore(archive_id,
                                     evaluation_id)
          select 
            am.archive_id,
            am.evaluation_id 
          from archive_match am",
          params=list(template$reporting_import$import_rsf_pfcbl_id))
        
        
        #Can't restore is because evaluation_id already used and conflict would arise.
        #This can only exist in cross-database restores
        dbExecute(conn,"
                  delete from _temp_restore tr 
                  where exists(select * from p_rsf.rsf_data_checks_archive dca where dca.archive_id = tr.archive_id) 
                    and exists(select * from p_rsf.rsf_data_checks rdc where rdc.evaluation_id   = tr.archive_id) 
                    and tr.archive_id <> tr.evaluation_id
                  ")
        
        restored <- dbGetQuery(conn,"
          select exists(select * from _temp_restore)::bool as restored")
        
        restored <- as.logical(unlist(restored))
        
        if (restored==TRUE) {

          dbExecute(conn,"
          update p_rsf.rsf_data_checks rdc
          set evaluation_id = tr.archive_id, -- this reverts to the original evaluation_id that was archived and also prevents database from checking user rights to update
              check_status = dca.check_status,
              check_status_comment = coalesce(dca.check_status_comment,'{MISSING COMMENT}'),
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
        }
        
        dbExecute(conn,"
          delete from p_rsf.rsf_data_checks_archive dca
          where dca.archive_time < (now() - interval '90 days')")
        
        return (restored)
      }) 
    
    }
    
    #restore data flags
    {
      
      mod_flag <- dbGetQuery(pool,"
        select indicator_check_id
        from p_rsf.indicator_checks
        where check_name = 'sys_data_status_modified'")
      
      if (empty(mod_flag)) {
        stop("Failed to find system check with name 'sys_data_status_modified' -- this is a required system check that must exist in the database.  Check with Database Administrator of the check was deleted or name changed")
      }
      
      #dbBegin(conn)
      #dbRollback(conn)
      restored <- poolWithTransaction(pool,function(conn) {
        
        dbExecute(conn,"
          create temp table _temp_restore(archive_id int,
                                          data_id int)
          on commit drop;")
        
        
        dbExecute(conn,"
          with restore_flags as (
          	select
          		dca.archive_id,
          		rd.data_id,
          		rd.rsf_pfcbl_id as data_rsf_pfcbl_id,
          		dca.rsf_pfcbl_id as archive_rsf_pfcbl_id,
          		dca.sys_name
          	from p_rsf.rsf_data rd
          	inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rd.reporting_cohort_id
          	inner join lateral (select nai.sys_name
                                from p_rsf.rsf_data_current_names_and_ids nai
                                where nai.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                  and nai.reporting_asof_date <= rd.reporting_asof_date
                                order by nai.reporting_asof_date desc
                                limit 1) as sn on true
                                
            inner join p_rsf.rsf_data_checks_archive dca on dca.sys_name = sn.sys_name
                                                        and dca.check_asof_date = rd.reporting_asof_date
                                                        and dca.indicator_check_id = $2::int
                                                        and dca.indicator_id = rd.indicator_id
                                                        
          	where rd.rsf_pfcbl_id = any(select ft.to_family_rsf_pfcbl_id 
                                         from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                         where ft.from_rsf_pfcbl_id = $1::int
                                           and ft.pfcbl_hierarchy <> 'parent')
                                        
          	  and rc.is_reported_cohort = true
          		and dca.data_sys_flags is not null 
          		and dca.data_value_unit = p_rsf.rsf_data_value_unit(rd.data_value,rd.data_unit)
          		
          )
          insert into _temp_restore(archive_id,
                                    data_id)
          select 
          	rf.archive_id,            
          	rf.data_id
          from restore_flags rf",
          params=list(template$reporting_import$import_rsf_pfcbl_id,
                      mod_flag$indicator_check_id))

      restored <- dbGetQuery(conn,"
        select exists(select * from _temp_restore)::bool as restored
      ")
      
      restored <- as.logical(unlist(restored))
      
      if (restored==FALSE) {
        return (FALSE)
      } else {
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
                                          check_data_id_is_current,
                                          
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
        (dca.data_sys_flags & 4)=4 as check_data_id_is_current, -- revert will make current
        dca.data_sys_flags
        from _temp_restore tr
        inner join p_rsf.rsf_data rd on rd.data_id = tr.data_id
        inner join p_rsf.rsf_data_checks_archive dca on dca.archive_id = tr.archive_id
        on conflict do nothing")
        
        dbExecute(conn,"
          delete from p_rsf.rsf_data_checks_archive dca
          using _temp_restore tr
          where tr.archive_id = dca.archive_id")
        
        return (TRUE)
      }
    }) 
      
      if (restored==TRUE) {
        rsf_program_calculate(pool=pool,
                              rsf_indicators=template$rsf_indicators,
                              rsf_pfcbl_id.family=template$reporting_import$import_rsf_pfcbl_id,
                              calculate_future=template$get_program_setting("on_upload_perform_future_calculations"),
                              reference_asof_date=pmax(template$reporting_import$reporting_asof_date,
                                                       max(template$pfcbl_data$reporting_asof_date)),
                              status_message = status_message)
        
        rsf_program_check(pool=pool,
                          rsf_indicators=template$rsf_indicators,
                          rsf_pfcbl_id.family=template$reporting_import$import_rsf_pfcbl_id,
                          check_future=template$get_program_setting("on_upload_perform_future_checks"),
                          
                          reference_asof_date=pmax(template$reporting_import$reporting_asof_date,
                                                   max(template$pfcbl_data$reporting_asof_date)),
                          status_message= status_message)
      }
      
    }
  }
  
  #cohort info updates
  {
    dbExecute(pool,"update p_rsf.reporting_imports ri
                    set import_completed = true
                    where ri.import_id = $1::int",
              params=list(template$reporting_import$import_id))
  }
  
  template$upload_time <- as.numeric(Sys.time()-t1,"secs")

  #status_message(class="info",paste0("Processing time: ",round(as.numeric(difftime(Sys.time(),t1,units="secs")))," seconds\n"))
  
  if(SYS_PRINT_TIMING) debugtime("template_upload","Done!",format(Sys.time()-t1))
  
  return (template)
}