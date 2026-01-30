rsf_program_check <- function(pool,
                             rsf_indicators,
                             rsf_pfcbl_id.family,
                             check_future=TRUE,
                             check_consolidation_threshold=NA,
                             reference_asof_date=NULL,
                             status_message) {
  
  t1 <- Sys.time()

  #rsf_pfcbl_id.family <- 59268
  
  limit_date <- NA
  if (check_future==FALSE &&
      is.null(reference_asof_date)==FALSE) {
    limit_date <- ymd(reference_asof_date)
    status_message(class="warning",
                   "Setting check_future set to FALSE.  Skipping calculations for after ",as.character(reference_asof_date),": update program setting to avoid this warning\n")
    
  }
  
  if (any(is.na(check_consolidation_threshold)) || 
      is.null(check_consolidation_threshold)    ||
      length(check_consolidation_threshold) > 1 ||
      any(check_consolidation_threshold <= 1)) check_consolidation_threshold <- NA
  
  stale_checks <- db_program_get_stale_checks(pool=pool,
                                              rsf_pfcbl_id.family=rsf_pfcbl_id.family,
                                              limit_future=limit_date)
  
  
  if (empty(stale_checks)) return (NULL) #nothing to check!
  
  #In case it was NULL and defaulted to program-level
  rsf_pfcbl_id.family <- unique(stale_checks$rsf_pfcbl_id.family)
  
  check_groups <- sort(unique(stale_checks$check_group))
  
  if (all(is.na(reference_asof_date))) reference_asof_date <- NULL
  
  #group <- 1
  #group <- 10
  #group <- group+1
  check_results <- NULL
  computed_count <- 0
  for (group in check_groups) {
    
    perform_checks <- stale_checks[check_group==group,
                                   .(check_rsf_pfcbl_ids,
                                     check_asof_date,
                                     entity_local_currency_unit,
                                     current_evaluation_ids,
                                     check_formula_id)]
    
    #using_check_cohort_id <- using_reporting_cohort_id
    current_check_asof_date <- unique(as.character(perform_checks$check_asof_date))
    
    status_message(class="none",
                   paste0(group,"/",max(check_groups),
                          " Performing: ",format(length(unlist(perform_checks$check_rsf_pfcbl_ids,recursive = F)),big.mark = ","),
                          " checks for ",perform_checks$check_asof_date[[1]]," ...\n"))
    
    computed_checks <- rsf_program_perform_checks(pool=pool,
                                                  rsf_indicators=rsf_indicators,
                                                  perform_checks=perform_checks,
                                                  perform.test = FALSE,
                                                  status_message=status_message)
   
    #perform checks will return NULL if no valid checks are passed to it
    #eg, perform check for these IDs but none of those IDs exist (yet) in the current timeline.  This can happen if system is prompted to recheck historic data, for example
    #where initially those IDs didn't exit yet and therefore didn't prompt the check.  But retrospectively they do exist and prompt the check, but don't exist in the timeline
    #and are filtered out here and result in a NULL result.
    if (empty(computed_checks)) next;
    
    computed_count <- computed_count + nrow(computed_checks)
    
    for (chk in 1:nrow(perform_checks)) {
      check <- perform_checks[chk,
                              .(current_evaluation_ids,
                                check_formula_id,
                                check_asof_date)]
      
      eids <- na.omit(unlist(check$current_evaluation_ids,recursive = F))
      
      if (length(eids)==0) next;
      
      existing_checks <- dbGetQuery(pool,
                                   "select 
                                      rdc.rsf_pfcbl_id,
                                      rdc.evaluation_id
                                    from p_rsf.rsf_data_checks rdc
                                    where rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)
                                      and rdc.check_status = 'active'
                                   ",params=list(paste0(eids,collapse=",")))
      setDT(existing_checks)
      obsolete_evaluations <- computed_checks[check_asof_date == check$check_asof_date &
                                              check_formula_id == check$check_formula_id &
                                              flag_status == FALSE, #Computed to be FALSE but existing check exists
                                              .(rsf_pfcbl_id)
                                              ][existing_checks,
                                                on=.(rsf_pfcbl_id),
                                                evaluation_id,
                                                nomatch=NULL]
      if (length(obsolete_evaluations) > 0) {
        dbExecute(pool,"
                 update p_rsf.rsf_data_checks rdc
                 set check_status = 'resolved',
                     check_status_comment = concat('Resolved by System: flag no longer exists following data correction'),
                     check_status_user_id = (select account_id from p_rsf.view_account_info where is_system_account = true and users_name = 'RSF SYS Calculator'),
                     status_time = TIMEOFDAY()::timestamptz
                 where rdc.check_status = 'active'
                   and rdc.evaluation_id = any(select unnest(string_to_array($1::text,','))::int)",
                  params=list(paste0(unique(obsolete_evaluations),collapse=",")))
      }
      
      existing_checks <- NULL
      check <- NULL

    }
    
    computed_checks <- computed_checks[is.na(flag_status) | #so it can be flagged later
                                       flag_status == TRUE]
    perform_checks <- NULL
    
    check_results <- rbindlist(list(check_results,
                                    computed_checks))
    computed_checks <- NULL
  }
  
  if(SYS_PRINT_TIMING) debugtime("rsf_program_check","Check Groups performed in ",format(Sys.time()-t1))
  
  #check_results[flag_status==TRUE]
  #lobstr::obj_size(stale_checks);lobstr::obj_size(check_results)
  #saveRDS(check_results,file="check_results.RDS")
  #saveRDS(stale_checks,file="stale_checks.RDS")
  #check_results <- readRDS("check_results.RDS")
  #stale_checks <- readRDS("stale_checks.RDS")
  
  #pulled-in later
  check_results[,
                indicator_check_id:=as.numeric(NA)]
  
  if (anyNA(check_results$flag_status)) {
    #print(checks[is.na(flag_status)])

    failed_check_id <- dbGetQuery(pool,
                                  "select indicator_check_id
                                    from p_rsf.indicator_checks
                                    where check_name = 'sys_checker_failed'")
    
    failed_check_id <- as.numeric(unlist(failed_check_id))
    if (length(failed_check_id)==0) stop("Failed to lookup indicator_check_id named 'sys_checker_failed' -- verify name exists in database")
    
    
    failed_checks <- check_results[is.na(flag_status)==TRUE]
    
    failed_checks[,
                  `:=`(flag_status=TRUE,
                       check_formula_id=as.numeric(NA), #turning into a sys flag
                       indicator_check_id=failed_check_id)]

    setcolorder(failed_checks,
                neworder = names(check_results))
    
    check_results <- rbindlist(list(check_results,
                             failed_checks))
      
  }
  check_results <- check_results[is.na(flag_status)==FALSE]
  
  #If there are any TRUE checks that have flagged a data point... Upload it!
  #And condolidate at the consolidation threshold.
  t2 <- Sys.time()
  if (any(check_results$flag_status==TRUE,na.rm=T)) {
    
    #get the (current)indicator on which to apply the flag.
    #conn <- poolCheckout(pool)
    #dbBegin(conn)
    #dbRollback(conn)
    
    #This query identifies which indicator to flag based on most recently-updated parameter of the flag formula
    #The check formula is disassociated with which data point receives the flag.  This will flag the current value.
#     check_results_indicators <- poolWithTransaction(pool,function(conn) {
#       dbExecute(conn,"
#         create temp table _temp_checks(rsf_pfcbl_id int,
#                                        check_asof_date date,
#                                        check_formula_id int)
#         on commit drop;")
#       
#       dbAppendTable(conn,
#                     name="_temp_checks",
#                     value=check_results[flag_status==TRUE,
#                                         .(rsf_pfcbl_id,
#                                           check_asof_date,
#                                           check_formula_id)])
#       
#       dbExecute(conn,"analyze _temp_checks")
#       
#       #efficiency opportuniy to first select only those that have one parameter at for_pfcbl_category level, as no need to query latest parameter on which to apply  
#       dbGetQuery(conn,"
#         select distinct on (tc.rsf_pfcbl_id,tc.check_asof_date,tc.check_formula_id)
#         	tc.rsf_pfcbl_id,tc.check_asof_date,tc.check_formula_id,
#         	cfp.indicator_check_id,
#         	coalesce(rdc.indicator_id,
#                    rpr.reporting_indicator_id) as for_indicator_id
#         from _temp_checks tc
#         inner join p_rsf.rsf_pfcbl_reporting rpr on rpr.rsf_pfcbl_id = tc.rsf_pfcbl_id
#                                                 and rpr.reporting_asof_date = tc.check_asof_date
#         inner join p_rsf.indicator_check_formula_parameters cfp on cfp.check_formula_id = tc.check_formula_id
#         inner join p_rsf.indicators ind on ind.indicator_id = cfp.parameter_indicator_id
#         inner join p_rsf.compute_check_to_parameter_rsf_pfcbl_ids pids on pids.from_check_rsf_pfcbl_id = tc.rsf_pfcbl_id
#         																														  and pids.from_check_formula_id = tc.check_formula_id
#         																															and pids.to_parameter_pfcbl_category = cfp.for_pfcbl_category -- only assign at own category
#         left join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = pids.to_parameter_rsf_pfcbl_id
# 											                      and rdc.indicator_id = cfp.parameter_indicator_id
# 											                      and rdc.reporting_asof_date = tc.check_asof_date	
#         order by
#         tc.rsf_pfcbl_id,tc.check_asof_date,tc.check_formula_id,
#         rdc.data_id is not null desc,
#         ind.indicator_sys_category is distinct from 'entity_reporting' desc,
#         ind.indicator_name asc")
#       
#     })
# 
#     setDT(check_results_indicators)
# 
#     check_results[check_results_indicators,
#                   `:=`(indicator_check_id=i.indicator_check_id,
#                        for_indicator_id=i.for_indicator_id),
#                   on=.(rsf_pfcbl_id,
#                        check_asof_date,
#                        check_formula_id)]
    
    
    db_rsf_checks_add_update(pool=pool,
                             data_checks=check_results[flag_status==TRUE,
                                                       .(rsf_pfcbl_id,
                                                         for_indicator_id=as.numeric(NA),
                                                         indicator_check_id=as.numeric(NA),
                                                         check_formula_id,
                                                         check_asof_date,
                                                         check_message)],
                             consolidation_threshold=check_consolidation_threshold)
    
  }
  
  if(SYS_PRINT_TIMING) debugtime("rsf_program_check","Verified done!",nrow(stale_checks)," in ",format(Sys.time()-t2))
  
 
  expected_checks <- dbGetQuery(pool,"
                                select count(*)::int as pending_evaluations
                                from p_rsf.view_rsf_pfcbl_id_family_tree ft
                                inner join p_rsf.rsf_data_check_evaluations dce on dce.rsf_pfcbl_id = ft.to_family_rsf_pfcbl_id
                                where ft.from_rsf_pfcbl_id = $1::int",
                                params=list(rsf_pfcbl_id.family))
  
  expected_checks <- as.numeric(unlist(expected_checks))
  if (computed_count==expected_checks) {
    t2 <- Sys.time()
    dbExecute(pool,"
              with ids as MATERIALIZED (
                select ft.to_family_rsf_pfcbl_id as rsf_pfcbl_id
                from p_rsf.view_rsf_pfcbl_id_family_tree ft
                where ft.from_rsf_pfcbl_id = $1::int
              )
              delete from p_rsf.rsf_data_check_evaluations dce
              where dce.rsf_pfcbl_id = any(select rsf_pfcbl_id from ids)",
              params=list(rsf_pfcbl_id.family))
    
    if(SYS_PRINT_TIMING) debugtime("rsf_program_check","Verified done!",expected_checks," in ",format(Sys.time()-t2))
  } else {
    actual_checks <- poolWithTransaction(pool,function(conn) {
      dbExecute(conn,"create temp table _temp_ids(rsf_pfcbl_id int,
                                                  check_formula_id int,
                                                  check_asof_date date)
                      on commit drop;")
      
      dbAppendTable(conn,
                    name="_temp_ids",
                    value=stale_checks[,
                                       .(rsf_pfcbl_id=unlist(check_rsf_pfcbl_ids,recursive=F)),
                                       by=.(check_formula_id,
                                            check_asof_date)][,.(rsf_pfcbl_id,
                                                                 check_formula_id,
                                                                    check_asof_date)])
      
      dbExecute(conn,"alter table _temp_ids add primary key(rsf_pfcbl_id,check_formula_id,check_asof_date)")
      dbExecute(conn,"analyze _temp_ids")
      actual_checks <- dbExecute(conn,"
                delete from p_rsf.rsf_data_check_evaluations dce
                where exists(select * from _temp_ids ids
                              where ids.rsf_pfcbl_id = dce.rsf_pfcbl_id
                                and ids.check_formula_id = dce.check_formula_id
                                and ids.check_asof_date = dce.check_asof_date)")
      actual_checks
    })
  }
  
  #lobstr::obj_size(checked)
  #lobstr::obj_size(stale_checks)
  #chk_dates <- sort(unique(as.character(stale_checks$check_asof_date)))
  
  if(SYS_PRINT_TIMING) debugtime("rsf_program_check","Done!",format(Sys.time()-t1))
#t2 <- Sys.time()  
#t2-t1
#With Extended loop: (1) Time difference of 2.742337 mins (2) Time difference of 2.910265 mins
#Without Extended loop: (1) Time difference of 3.216619 mins (2) Time difference of 3.051032 mins
}
