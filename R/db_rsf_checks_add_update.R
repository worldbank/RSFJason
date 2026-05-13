#NEW
db_rsf_checks_add_update <- function(pool,
                                     data_checks,
                                     for_import_id=NA,
                                     consolidation_threshold=NA) 
{

  
  #setups
  {
  
    if (empty(data_checks)) return (NULL)
    if (anyNA(data_checks$check_asof_date)) stop("NA check_asof_date not allowed")
    #if (anyNA(data_checks$indicator_check_id)) stop("NA indicator_check_id not allowed")
    if (anyNA(data_checks$rsf_pfcbl_id)) stop("NA rsf_pfcbl_id not allowed")
    
    for_import_id <- suppressWarnings(as.numeric(for_import_id))
    #if (anyNA(data_checks$for_indicator_id)) stop("NA for indicator_id not allowed: ensure user checks identify indicator_id on which to apply before upload")
    #if (!empty(data_checks[is.na(indicator_id) & is.na(check_formula_id)])) stop("system checks must submit NA check_formula_id and a value for indicator_id and user checks, the inverse")
    #if (length(using_reporting_cohort_id) != 1) { stop("Only one using_reporting_cohort_id value allowed, NA by default") }
    #lobstr::obj_size(data_checks)
    t10 <- Sys.time()
   
    expected_cols <- c("rsf_pfcbl_id",
                       "for_indicator_id",
                       "check_asof_date",
                       "check_formula_id",
                       "indicator_check_id",
                       "check_message")
    
    if (length((bad_cols <- setdiff(union(names(data_checks),expected_cols),names(data_checks))))>0) {
      stop(paste0("data_checks must define columns: ",paste0(bad_cols,collapse=", ")))
    }
    
    #Because these fields may (or may not) come from other preocesses.
    if (!any(names(data_checks)=="variance")) {
      data_checks[,
                  variance:=as.numeric(NA)]
    }
    if (!any(names(data_checks)=="data_check_value")) {
      data_checks[,
                  data_check_value:=as.character(NA)]
    }
    if (!any(names(data_checks)=="data_check_unit")) {
      data_checks[,
                  data_check_unit:=as.character(NA)]
    }
  
  }  
  
  #Group checks (to consolidate multi-messages and to speed uploads)
  {
   
    #Ie, there was one check. We found guidance to ignore it.  We ignored it here before uploading and removing from
    #checks table.  Now checks table is empty.  So nothing more to do.
    if (empty(data_checks)) {
      return (FALSE)
    }
    
    data_checks[is.na(check_message),
                check_message:="Warning: message undefined for this check. Update in admin section"]
    
    #for IO speed, consolidate by inidicator_id,date and check
    #eg, for OBA testing and 28 million checks and lobstr::obj_size(data_checks)=111.55
    #consolidated 41.90MB
    
    #system flag that has no formula ID
    data_checks[is.na(check_formula_id),
                check_formula_id:=-1]
    
    #Now that we're consolidating checks, let maximum of 100
    data_checks[,
                rsf_pfcbl_id:=as.numeric(rsf_pfcbl_id)]
    
    data_checks <- data_checks[,
                               .(rsf_pfcbl_id,
                                 for_indicator_id,
                                 check_asof_date,
                                 indicator_check_id,
                                 check_formula_id,
                                 check_message,
                                 variance,
                                 data_check_value,
                                 data_check_unit,
                                 flag_id=1:.N)]
    
    # #mostly differentiates system checks (with an NA formula_id)
    # #and user defined checks that have a formula_id -- and also will have an indicator_check_id, but this will be automatically asigned
    # data_checks[!is.na(check_formula_id),
    #             indicator_check_id:=NA]
    # 
    # data_checks[!is.na(indicator_check_id),
    #             check_formula_id:=NA]  
  }
  
  #conn <- poolCheckout(pool)
  #dbBegin(conn);
  #dbRollback(conn);
  #poolReturn(conn)
  
  
t20 <- Sys.time()  
  nx <- poolWithTransaction(pool,function(conn) {
    
    t30 <- Sys.time()
        {
          dbExecute(conn,"
                    create temp table _temp_data_checks(data_id int,
                                                        rsf_pfcbl_id int,
                                                        indicator_id int,
                                                        check_asof_date date,
                                                        indicator_check_id int,
                                                        check_formula_id int,
                                                        status_time timestamptz,
                                                        check_message text,
                                                        check_status text,
                                                        check_status_comment text,
                                                        check_status_user_id text,
                                                        check_data_id_is_current bool,
                                                        data_check_value text,
                                                        data_check_unit text)
                    on commit drop")
  
          dbExecute(conn,"create TEMP table _temp_add_checks(rsf_pfcbl_id int,
                                                             for_indicator_id int,
                                                             check_asof_date date,
                                                             indicator_check_id int,
                                                             check_formula_id int,
                                                             check_message text,
                                                             variance numeric,
                                                             data_check_value text,
                                                             data_check_unit text,
                                                             data_id int,
                                                             flag_id int)
                      on commit drop;")

          dbAppendTable(conn,
                        name="_temp_add_checks",
                        value=data_checks)     
          
          dbExecute(conn,"analyze _temp_add_checks;")

          #if check is being applied BEFORE entity exists
          #OR
          #after it has been deactivated.
          dbExecute(conn,"
            delete from _temp_add_checks tac
            where exists(select * from p_rsf.rsf_pfcbl_ids ids
                         where ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
                           and (tac.check_asof_date < ids.created_in_reporting_asof_date 
                                or
                                (ids.deactivated_in_reporting_asof_date is not NULL AND
                                 ids.deactivated_in_reporting_asof_date > tac.check_asof_date))
                      )
          ")
          
          
          
          #          
          # dbExecute(conn,"
          #           delete from _temp_add_checks tac
          #           where not exists(select * from p_rsf.rsf_pfcbl_reporting rpr 
          #                            where rpr.rsf_pfcbl_id = tac.rsf_pfcbl_id 
          #                              and rpr.reporting_asof_date = tac.check_asof_date)")
          
        }   
    #if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","write data in ",format(Sys.time()-t30))  
    
        {
          
          
          dbExecute(conn,"update _temp_add_checks tac
                          set indicator_check_id = icf.indicator_check_id
                          from p_rsf.indicator_check_formulas icf
                          where icf.check_formula_id = tac.check_formula_id
                            and tac.indicator_check_id is NULL")
          
          #defaults to -1 formula_id so set it to NULL.
          dbExecute(conn,"update _temp_add_checks tac
                          set check_formula_id = NULL
                          where tac.check_formula_id is NOT NULL 
                            AND (exists(select * from p_rsf.indicator_checks ic 
                                       where tac.indicator_check_id = ic.indicator_check_id
                                         and ic.is_system is true)
                             or not exists(select * from p_rsf.indicator_check_formulas icf
                                           where icf.indicator_check_id = tac.indicator_check_id))")
          
          
          #This will apply the check onto the most recently-updated indicator used by the check
          #Will do so ONLY for flags with a check_formula_id (ie, not reporting/system flags)
          dbExecute(conn,"
            update _temp_add_checks tic
            set for_indicator_id = updated.indicator_id,
                data_id = updated.data_id
            from _temp_add_checks tac
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
            left join lateral (select
                                cfp.parameter_indicator_id as indicator_id,
                                rdc.data_id
                               from p_rsf.indicator_check_formula_parameters cfp
                               left join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                   and rdc.indicator_id = cfp.parameter_indicator_id
                                                                   and rdc.reporting_asof_date <= tac.check_asof_date
                               where cfp.check_formula_id = tac.check_formula_id
                                 and cfp.parameter_pfcbl_category = ids.pfcbl_category
                                 and cfp.is_calculation_trigger_parameter is true
                               order by
                                rdc.reporting_asof_date desc, -- most recently-updated data point.
                                cfp.parameter_trigger_by_reporting asc, -- false first (invariably an entity_reporting indicator) and least meaningful flag destination; defult set below
                                cfp.parameter_indicator_id asc,
                                rdc.data_id desc
                               limit 1) as updated on true
            where tic.for_indicator_id is NULL
              and tic.check_formula_id is NOT NULL -- this is only meaningful for non-system checks that have formulas.
              and tac.flag_id = tic.flag_id")
          
          # dbExecute(conn,"update _temp_add_checks tac
          #           set for_indicator_id = ind.indicator_id
          #           from p_rsf.rsf_pfcbl_ids ids 
          #           inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
          #           where ind.indicator_sys_category = 'entity_reporting'
          #             and tac.for_indicator_id is NULL")
          # 
          # dbExecute(conn,"update _temp_add_checks tac
          #                 set data_id = (select rdc.data_id
          #                                from p_rsf.rsf_data_current rdc
          #                                where rdc.rsf_pfcbl_id = tac.rsf_pfcbl_id
          #                                  and rdc.indicator_id = tac.for_indicator_id
          #                                  and rdc.reporting_asof_date <= tac.check_asof_date
          #                                order by
          #                                  rdc.reporting_asof_date desc
          #                                limit 1)
          #                 where tac.data_id is null")
          # 
          
          
          #Will assign to entity_reporting when previously, no for_indicator_id is assigned
          #(1) It's a system indicator with no check_formula_id
          #(2) The indicator formula was triggered -- and a flag found -- but no formula-entity-level indicators were actually modified this reporting period (maybe a parent metric update and triggered a child-level flag?)
          dbExecute(conn,"
                    update _temp_add_checks tic
                    set for_indicator_id = coalesce(tac.for_indicator_id,ind.indicator_id), -- if an indicator_id hasn't been asigned, then assign to entity_reporting
                        data_id = rdc.data_id                                               -- BUT if an explicit for_indicator IS set and the data_id is not, then apply the data
                                                                                            -- to entity_reporting but keep the flag on a DIFFERENT indicator
                    from _temp_add_checks tac
                    inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
                    inner join p_rsf.indicators ind on ind.data_category = ids.pfcbl_category
                                                   and ind.indicator_sys_category = 'entity_reporting'
                    inner join lateral (select 
                                          rdc.data_id
                                        from p_rsf.rsf_data_current rdc
                                        where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                          and rdc.indicator_id in (tac.for_indicator_id,ind.indicator_id)
                                          and rdc.reporting_asof_date <= tac.check_asof_date
                                        order by 
                                        rdc.indicator_id is not distinct from tac.for_indicator_id desc, -- tagged indicator is priority; after, entity reporting default.
                                        rdc.reporting_asof_date desc
                                        limit 1) as rdc on true
                    where (tic.for_indicator_id is NULL or tic.data_id is NULL)
                      and tic.flag_id = tac.flag_id")
          
          dbExecute(conn,"delete from _temp_add_checks tic where data_id is NULL")
          
          #This can happen in the very rare cases where the for_indicator_id doesn't match the rsf_data's indicator_id entry for that data_id
          #So we'll take the data_id from the last entity_reporting and leave the indicator assigned to the for_indicator_id that doesn't actually exists
          #This can occur when a facility unsubscribes to an indicator, for example but system seeks to flag it anyway (such as for templates that submit indicators that are configured to ignore)
          # dbExecute(conn,"
          #   update _temp_add_checks tac
          #   set for_indicator_id = updated.indicator_id,
          #       data_id = updated.data_id
          #   from p_rsf.rsf_pfcbl_reporting rpr
          #   left join lateral (select rdc.indicator_id,rdc.data_id
          #                      from p_rsf.rsf_data_current rdc 
          #                      where rdc.rsf_pfcbl_id = rpr.rsf_pfcbl_id
          #                        and rdc.indicator_id = rpr.reporting_indicator_id
          #                        and rdc.reporting_asof_date <= rpr.reporting_asof_Date
          #                      order by 
          #                       rdc.reporting_asof_date desc,
          #                       rdc.data_id desc
          #                      limit 1) as updated on true
          #   where rpr.rsf_pfcbl_id = tac.rsf_pfcbl_id
          #     and rpr.reporting_asof_date = tac.check_asof_date
          #     and tac.data_id is NULL")
          
          #if a flag is generated on an indicator/data point that the entity has NEVER reported on before,
          #then reassign it to the entity's reporting indicator for this period.
          non_reporting_data <- dbGetQuery(conn,"select exists(select * from _temp_add_checks where data_id is NULL or for_indicator_id is null)::bool")
          if (unlist(non_reporting_data)==TRUE) {
            
            x <- dbGetQuery(conn,"select * from _temp_add_checks where data_id is NULL or for_indicator_id is null")
            print(x)
            stop("_temp_add_checks failed to resolve data_id to apply flag.  see logs for details.")
          
        }
        }
          #x<-dbGetQuery(conn,"select tac.*,ind.indicator_name,ind.data_category,ids.pfcbl_category from _temp_add_checks tac inner join p_rsf.indicators ind on ind.indicator_id = tac.for_indicator_id inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id");setDT(x)
        {
          dbExecute(conn,"
                    update _temp_add_checks tac
                    set check_formula_id = NULL
                    where exists(select * from p_rsf.indicator_checks ic
                                 where ic.indicator_check_id = tac.indicator_check_id
                                   and ic.is_system = true)
                      and check_formula_id is distinct from NULL")

          #delete where data already has this flag and this message (its redundant)
          dbExecute(conn,"
                  delete from _temp_add_checks tac
                  where exists(select * from p_rsf.rsf_data_checks rdc
                               where rdc.data_id = tac.data_id
                                 and rdc.check_asof_date = tac.check_asof_date
                                 and rdc.indicator_check_id = tac.indicator_check_id::int
                                 and rdc.check_message is not distinct from tac.check_message)")
          
          dbExecute(conn,"
                  delete from _temp_add_checks tac
                  where exists(select * from p_rsf.rsf_data_checks rdc
                               where rdc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                 and rdc.check_asof_date = tac.check_asof_date
                                 and rdc.indicator_check_id = tac.indicator_check_id::int
                                 and rdc.check_formula_id is not distinct from tac.check_formula_id
                                 and tac.check_formula_id is not null
                                 and rdc.check_data_id_is_current = true)")
          
          #when for_import_id is defined by call to this function, it's from template system checks
          #if there's the same issue in the template without any change in the flag, no need to re-flag it each upload: first flag suffices.
          if (!is.na(for_import_id)) {
            dbExecute(conn,"
                      with last_checks as (
                        select distinct on (chk.rsf_pfcbl_id,chk.indicator_id,chk.indicator_check_id)
                        chk.rsf_pfcbl_id,chk.indicator_id,chk.indicator_check_id,chk.check_message
                        from p_rsf.rsf_data_checks chk
                        inner join _temp_add_checks tac on tac.rsf_pfcbl_id = chk.rsf_pfcbl_id
                                                       and tac.for_indicator_id = chk.indicator_id
                                                       and tac.indicator_check_id = chk.indicator_check_id
                                                       and chk.check_asof_date <= tac.check_asof_date
                        where chk.check_formula_id is null
                        order by 
                        chk.rsf_pfcbl_id,
                        chk.indicator_id,
                        chk.indicator_check_id,
                        chk.check_data_id_is_current desc,
                        chk.check_asof_date desc
                      )
                      delete from _temp_add_checks tac
                      using last_checks lc
                      where lc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                        and lc.indicator_id = tac.for_indicator_id
                        and lc.indicator_check_id = tac.indicator_check_id
                        and lc.check_message is not distinct from tac.check_message;
                      ")
          }
          
          nothing <- dbGetQuery(conn,"select not exists(select * from _temp_add_checks)::bool")
          
          if (unlist(nothing)==TRUE) {
            return (0);
          }
          
        }
    
    
          {
          #IMPORTANT!
          #_temp_add_checks variance is alraedy multiplied by 100 so variance of 0.8 is 0.8% not 80%
          #and setup config variance is NOT multiplied by 100.
          #Comparing these means config is *100 ONLY
          nx <- dbExecute(conn,"
                          insert into _temp_data_checks(data_id,
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
                      																	data_check_value,
                                                        data_check_unit)																
																	
                          select 
                          	tac.data_id,
                          	tac.rsf_pfcbl_id,
                          	tac.for_indicator_id,
                          	tac.check_asof_date,
                          	tac.indicator_check_id,
                            tac.check_formula_id,
                          	(timeofday())::timestamptz as status_time,
                          	tac.check_message,
                          	
                          	case 
                          	  when coalesce(tac.variance,0) < coalesce(scc.config_threshold,0) then 'resolved'
                          	  when scc.config_auto_resolve is not null and scc.config_auto_resolve is true then 'resolved'
                          	  when scc.config_auto_resolve is not null and scc.config_auto_resolve is false then 'active'
                          		when coalesce(icf.auto_resolve,ic.auto_resolve_system_check,false) is true then 'resolved'
                          		else 'active'
                          	end as check_status,
                          	
                          	case 
                          	  when coalesce(tac.variance,0) < coalesce(scc.config_threshold,0) 
                          	  then concat('Variance ',round(tac.variance,2),'% below threshold ',round(scc.config_threshold,2),'% Resolved [',ssc_vai.users_name,']: ',scc.config_comments)
                          	  
                          	  when scc.config_auto_resolve is not null and scc.config_auto_resolve is true 
                          	  then concat('Auto Resolved [',ssc_vai.users_name,']: ',scc.config_comments)
                          	  
                          	  when scc.config_auto_resolve is not null and scc.config_auto_resolve is false
                          	  then concat('Review [',ssc_vai.users_name,']: ',scc.config_comments)
                          	  
                          	  when coalesce(icf.auto_resolve,ic.auto_resolve_system_check,false) is true then 'Auto-resolved by SYSTEM [default]'

                          		else NULL::text
                          	end as check_status_comment,
                          	
                          	case 
                          		when scc.comments_user_id is not null then scc.comments_user_id
                          		
                          		when coalesce(icf.auto_resolve,ic.auto_resolve_system_check,false) is true 
                          		then (select account_id from p_rsf.view_account_info where is_system_account = true and users_name = 'RSF SYS Calculator')
                          		
                          		else NULL
                          	end as check_status_user_id,

                          	NULL as check_data_id_is_current, -- before trigger will set this
                          	tac.data_check_value,
                            tac.data_check_unit

                          from _temp_add_checks tac
                          --left join p_rsf.rsf_data rd on rd.data_id = tac.data_id and rd.reporting_asof_date = tac.check_asof_date
                          left join p_rsf.indicator_checks ic on ic.indicator_check_id = tac.indicator_check_id
                          left join p_rsf.indicator_check_formulas icf on icf.check_formula_id = tac.check_formula_id
                          left join p_rsf.view_rsf_setup_check_config scc on scc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                                                         and scc.for_indicator_id = tac.for_indicator_id
                                                                         and scc.indicator_check_id = ic.indicator_check_id
                                                                         and scc.check_formula_id is not distinct from tac.check_formula_id
                          left join p_rsf.view_account_info ssc_vai on ssc_vai.account_id = scc.comments_user_id")
          
          
          #if(SYS_PRINT_TIMING)  debugtime("db_rsf_checks_add_update","staged",format(nx,big.mark = ",")," checks in ",format(Sys.time()-t1))
          #x <- dbGetQuery(conn,"select * from _temp_data_checks");setDT(x);x
        }
        
        {
          nx <- dbExecute(conn,"
                          
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
                                																	data_check_value,
                                                                  data_check_unit,
                                                                  for_import_id)																
                                select 
                                  data_id,
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
  																NULL as check_data_id_is_current,
  																data_check_value,
                                  data_check_unit,
                                  NULLIF($1::text,'NA')::int as for_import_id


                                from _temp_data_checks	
                                on conflict do nothing
                          ",params=list(for_import_id))
          
        }
      
      nx    
    
    })
  
  if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","uploading"," in ",format(Sys.time()-t20))  
  
  if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","Done! uploading",nx," in ",format(Sys.time()-t10))
  return(TRUE)
}
