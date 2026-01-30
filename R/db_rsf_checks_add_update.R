#NEW
db_rsf_checks_add_update <- function(pool,
                                     data_checks,
                                     consolidation_threshold=NA) 
{

  
  #setups
  {
  
    if (empty(data_checks)) return (NULL)
    if (anyNA(data_checks$check_asof_date)) stop("NA check_asof_date not allowed")
    #if (anyNA(data_checks$indicator_check_id)) stop("NA indicator_check_id not allowed")
    if (anyNA(data_checks$rsf_pfcbl_id)) stop("NA rsf_pfcbl_id not allowed")
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
    
    if (!any(names(data_checks)=="variance")) {
      data_checks[,
                  variance:=as.numeric(NA)]
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
                                 variance)]
    
    
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
                                                        check_data_id_is_current bool)
                    on commit drop")
  
          dbExecute(conn,"create TEMP table _temp_add_checks(rsf_pfcbl_id int,
                                                             for_indicator_id int,
                                                             check_asof_date date,
                                                             indicator_check_id int,
                                                             check_formula_id int,
                                                             check_message text,
                                                             variance numeric,
                                                             data_id int)
                      on commit drop;")

          dbAppendTable(conn,
                        name="_temp_add_checks",
                        value=data_checks)     
          
          dbExecute(conn,"analyze _temp_add_checks;")
          
          dbExecute(conn,"
                    delete from _temp_add_checks tac
                    where not exists(select * from p_rsf.rsf_pfcbl_reporting rpr 
                                     where rpr.rsf_pfcbl_id = tac.rsf_pfcbl_id 
                                       and rpr.reporting_asof_date = tac.check_asof_date)")
          
        }   
    if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","write data in ",format(Sys.time()-t30))  
    
        {
          
          
          dbExecute(conn,"update _temp_add_checks tac
                          set indicator_check_id = icf.indicator_check_id
                          from p_rsf.indicator_check_formulas icf
                          where icf.check_formula_id = tac.check_formula_id
                            and tac.indicator_check_id is NULL")
          
          dbExecute(conn,"
            update _temp_add_checks tic
            set for_indicator_id = updated.indicator_id,
                data_id = updated.data_id
            from _temp_add_checks tac
            inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
            left join lateral (select 
                                rdc.indicator_id,
                                rdc.data_id
                               from p_rsf.indicator_check_formula_parameters cfp 
                               inner join p_rsf.rsf_data_current rdc on rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
                                                                    and rdc.indicator_id = cfp.parameter_indicator_id
                                                                    and rdc.reporting_asof_date <= tac.check_asof_date
                               where cfp.check_formula_id = tac.check_formula_id
                                 and cfp.parameter_pfcbl_category = ids.pfcbl_category
                                 and cfp.is_calculation_trigger_parameter is true
                               order by 
                                rdc.reporting_asof_date desc,
                                cfp.parameter_indicator_id asc,
                                rdc.data_id desc
                               limit 1) as updated on true
            where tic.for_indicator_id is NULL
              and tac.rsf_pfcbl_id = tic.rsf_pfcbl_id
              and tac.check_formula_id = tic.check_formula_id
              and tac.check_asof_date = tic.check_asof_date")
          
          
          dbExecute(conn,"update _temp_add_checks tac
                          set data_id = (select rdc.data_id
                                         from p_rsf.rsf_data_current rdc
                                         where rdc.rsf_pfcbl_id = tac.rsf_pfcbl_id
                                           and rdc.indicator_id = tac.for_indicator_id
                                           and rdc.reporting_asof_date <= tac.check_asof_date
                                         order by
                                           rdc.reporting_asof_date desc
                                         limit 1)
                          where tac.data_id is null")
          
          dbExecute(conn,"
            update _temp_add_checks tac
            set for_indicator_id = updated.indicator_id,
                data_id = updated.data_id
            from p_rsf.rsf_pfcbl_reporting rpr
            left join lateral (select rdc.indicator_id,rdc.data_id
                               from p_rsf.rsf_data_current rdc 
                               where rdc.rsf_pfcbl_id = rpr.rsf_pfcbl_id
                                 and rdc.indicator_id = rpr.reporting_indicator_id
                                 and rdc.reporting_asof_date <= rpr.reporting_asof_Date
                               order by 
                                rdc.reporting_asof_date desc,
                                rdc.data_id desc
                               limit 1) as updated on true
            where rpr.rsf_pfcbl_id = tac.rsf_pfcbl_id
              and rpr.reporting_asof_date = tac.check_asof_date
              and tac.data_id is NULL")
          
          #if a flag is generated on an indicator/data point that the entity has NEVER reported on before,
          #then reassign it to the entity's reporting indicator for this period.
          non_reporting_data <- dbGetQuery(conn,"select exists(select * from _temp_add_checks where data_id is NULL or for_indicator_id is null)::bool")
          if (unlist(non_reporting_data)==TRUE) {
            
            x <- dbGetQuery(conn,"select * from _temp_add_checks where data_id is NULL or for_indicator_id is null")
            print(x)
            stop("_temp_add_checks failed to resolve data_id to apply flag.  see logs for details.")
            
          #   dbExecute(conn,"
          #                         with reassign_checks as MATERIALIZED (
          #                           select
          #                           	tac.rsf_pfcbl_id,
          #                           	concat(c_ind.data_category,' has never reported \"',
          #                           	       c_ind.indicator_name,'\" but system checker flagged it for ',
          #                           				 c_ic.check_name,': ',
          #                           				 tac.check_message) as check_message,
          #                           	r_ind.indicator_id as reassign_indicator_id,
          #                           	r_ic.indicator_check_id as reassign_check_id,
          #                           	reporting.data_id,
          #                           	tac.for_indicator_id,
          #                           	tac.check_asof_date,
          #                           	tac.indicator_check_id
          #                           
          #                           from _temp_add_checks tac
          #                           inner join p_rsf.indicators c_ind on c_ind.indicator_id = tac.for_indicator_id
          #                           inner join p_rsf.rsf_pfcbl_ids ids on ids.rsf_pfcbl_id = tac.rsf_pfcbl_id
          #                           inner join p_rsf.indicators r_ind on r_ind.data_category = ids.pfcbl_category
          #                           inner join p_rsf.indicator_checks c_ic on c_ic.indicator_check_id = tac.indicator_check_id
          #                           inner join p_rsf.indicator_checks r_ic on r_ic.check_class = c_ic.check_class
          #                                                                 and r_ic.check_name ~ 'sys_unreported_data_flag'
          #                           inner join lateral (select rdc.data_id 
          #                                               from p_rsf.rsf_data_current rdc 
          #                                               where rdc.rsf_pfcbl_id = ids.rsf_pfcbl_id
          #                           											and rdc.indicator_id = r_ind.indicator_id
          #                           											and rdc.reporting_asof_date <= tac.check_asof_date::date
          #                           										order by rdc.reporting_asof_date desc
          #                           										limit 1) as reporting on true
          #                           where tac.data_id is NULL
          #                           	and r_ind.indicator_sys_category = 'entity_reporting'
          #                         )
          #                         update _temp_add_checks tac
          #                         set check_message = rc.check_message,
          #                             for_indicator_id = rc.reassign_indicator_id,
          #                         		indicator_check_id = rc.reassign_check_id,
          #                         		data_id = rc.data_id
          #                         from reassign_checks rc
          #                         where tac.data_id is NULL
          #                           and tac.rsf_pfcbl_id = rc.rsf_pfcbl_id
          #                         	and tac.for_indicator_id = rc.for_indicator_id
          #                         	and tac.indicator_check_id = rc.indicator_check_id
          #                         	and tac.check_asof_date = rc.check_asof_date")
          # }
          
          
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

          #delete if guidance is set to ignore
          # dbExecute(conn,"
          #           delete from _temp_add_checks tac
          #           where exists(select * from p_rsf.indicator_check_guidance icg
          #                        where icg.indicator_check_guidance_id = tac.guidance_id
          #                          and icg.is_ignoring_guidance = true)")
          
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
          
          nothing <- dbGetQuery(conn,"select not exists(select * from _temp_add_checks)::bool")
          
          if (unlist(nothing)==TRUE) {
            return (0);
          }
          
          
          
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
                      																	check_data_id_is_current)																
																	
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
                          	  then concat('Variance ',round(100*tac.variance,2),'% below threshold ',round(100*scc.config_threshold,2),'% Resolved [',ssc_vai.users_name,']: ',scc.config_comments)
                          	  
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

                          	exists(select * from p_rsf.rsf_data_current rdc where rdc.data_id = tac.data_id) as check_data_id_is_current
                          from _temp_add_checks tac
                          left join p_rsf.rsf_data rd on rd.data_id = tac.data_id
                                                     and rd.reporting_asof_date = tac.check_asof_date
                          left join p_rsf.indicator_checks ic on ic.indicator_check_id = tac.indicator_check_id
                          left join p_rsf.indicator_check_formulas icf on icf.check_formula_id = tac.check_formula_id
                          left join p_rsf.view_rsf_setup_check_config scc on scc.rsf_pfcbl_id = rd.rsf_pfcbl_id
                                                                         and scc.for_indicator_id = rd.indicator_id
                                                                         and scc.indicator_check_id = ic.indicator_check_id
                                                                         and ic.is_system is true
                          left join p_rsf.view_account_info ssc_vai on ssc_vai.account_id = scc.comments_user_id")
          
          
          #if(SYS_PRINT_TIMING)  debugtime("db_rsf_checks_add_update","staged",format(nx,big.mark = ",")," checks in ",format(Sys.time()-t1))
          
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
                                																	check_data_id_is_current)																
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
  																check_data_id_is_current

                                from _temp_data_checks	
                                on conflict do nothing
                          ")
          
        }
      
      nx    
    }
    })
  
  if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","uploading"," in ",format(Sys.time()-t20))  
  
  if(SYS_PRINT_TIMING) debugtime("db_rsf_checks_add_update","Done! uploading",nx," in ",format(Sys.time()-t10))
  return(TRUE)
}
