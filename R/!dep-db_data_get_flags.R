db_data_get_flags <- function(pool,data_id) {

  check_flags <- dbGetQuery(pool,"
                                  select 
                                  rdc.evaluation_id,
                                  rdc.data_id,
                                  rdc.indicator_check_id,
                                  rd.indicator_id as applied_on_indicator_id,
                                  ic.check_name,
                                  ic.definition,
                                  coalesce(icg.overwrite_check_class,ic.check_class) as check_class,
                                  ic.is_system,
                                  case when coalesce(icg.overwrite_check_class,ic.check_class) = 'critical' then 0
                                       when coalesce(icg.overwrite_check_class,ic.check_class) = 'error' then 1
                                       when coalesce(icg.overwrite_check_class,ic.check_class) = 'warning' then 2
                                       when coalesce(icg.overwrite_check_class,ic.check_class) = 'info' then 3
                                       else 4 
                                  end as flag_priority,
                                  rdc.check_asof_date as evaluation_asof_date,
                                  rdc.evaluated_by_cohort_id,
                                  rc.source_name,
                                  rdc.check_message,
                                  rdc.check_status,
                                  rdc.check_status_comment,
                                  vai.users_name as check_status_users_name
                                  from p_rsf.rsf_data_checks rdc
                                  inner join p_rsf.indicator_checks ic on ic.indicator_check_id = rdc.indicator_check_id
                                  inner join p_rsf.reporting_cohorts rc on rc.reporting_cohort_id = rdc.evaluated_by_cohort_id
                                  inner join p_rsf.rsf_data rd on rd.data_id = rdc.data_id
                                  left join p_rsf.view_account_info vai on vai.account_id = rdc.check_status_user_id
                                  left join p_rsf.indicator_check_guidance icg on icg.indicator_check_guidance_id = rdc.indicator_check_guidance_id
                                  where rdc.data_id = $1::int",
                            params=list(data_id))
  
  setDT(check_flags)
  
  if (empty(check_flags)) return (check_flags)
  
  check_flags[,evaluations_count:=.N,by=.(data_id,indicator_check_id)]
  setorder(check_flags,
           flag_priority,
           check_name,
           evaluation_asof_date)
  return (check_flags)
}